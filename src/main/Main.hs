{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main stack tool entry point.

module Main where

import           Control.Exception
import qualified Control.Exception.Lifted as EL
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (ask, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Attoparsec.Args (withInterpreterArgs)
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           Data.List
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable (sequenceA)
import           Network.HTTP.Client
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Simple
import           Options.Applicative.Types (readerAsk)
import           Path
import qualified Paths_stack as Meta
import           Plugins
import           Prelude hiding (pi)
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import           Stack.Constants
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.Exec
import           Stack.Fetch
import           Stack.FileWatch
import           Stack.Ide
import           Stack.Iface (iface)
import qualified Stack.Image as Image
import           Stack.Init
import           Stack.New
import           Stack.Options
import           Stack.Package (getCabalFileName)
import qualified Stack.PackageIndex
import           Stack.Ghci
import           Stack.SDist (getSDistTarball)
import           Stack.Setup
import           Stack.Solver (solveExtraDeps)
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           Stack.Upgrade
import qualified Stack.Upload as Upload
import           System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.FileLock (lockFile, tryLockFile, unlockFile, SharedExclusive(Exclusive), FileLock)
import           System.FilePath (dropTrailingPathSeparator)
import           System.IO (hIsTerminalDevice, stderr, stdin, stdout, hSetBuffering, BufferMode(..))
import           System.Process.Read

-- | Commandline dispatcher.
main :: IO ()
main = withInterpreterArgs stackProgName $ \args isInterpreter ->
  do -- Line buffer the output by default, particularly for non-terminal runs.
     -- See https://github.com/commercialhaskell/stack/pull/360
     hSetBuffering stdout LineBuffering
     hSetBuffering stdin  LineBuffering
     hSetBuffering stderr NoBuffering
     when False $ do -- https://github.com/commercialhaskell/stack/issues/322
       plugins <- findPlugins (T.pack stackProgName)
       tryRunPlugin plugins
     progName <- getProgName
     isTerminal <- hIsTerminalDevice stdout
     execExtraHelp args
                   dockerHelpOptName
                   (dockerOptsParser True)
                   ("Only showing --" ++ Docker.dockerCmdName ++ "* options.")
     let versionString' = $(simpleVersion Meta.version)
     eGlobalRun <- try $
       simpleOptions
         versionString'
         "stack - The Haskell Tool Stack"
         ""
         (extraHelpOption progName (Docker.dockerCmdName ++ "*") dockerHelpOptName <*>
          globalOptsParser isTerminal)
         (do addCommand "build"
                        "Build the project(s) in this directory/configuration"
                        (buildCmd DoNothing)
                        (buildOptsParser Build False)
             addCommand "install"
                        "Identical to 'build --copy-bins', not actually a managed installation tool!"
                        installCmd
                        (buildOptsParser Build True)
             addCommand "uninstall"
                        "DEPRECATED: This command performs no actions, and is present for documentation only"
                        uninstallCmd
                        (many $ strArgument $ metavar "IGNORED")
             addCommand "test"
                        "Build and test the project(s) in this directory/configuration"
                        (\(bopts, topts) ->
                             let bopts' = if toCoverage topts
                                             then bopts { boptsGhcOptions = "-fhpc" : boptsGhcOptions bopts}
                                             else bopts
                             in buildCmd (DoTests topts) bopts')
                        ((,) <$> buildOptsParser Test False <*> testOptsParser)
             addCommand "bench"
                        "Build and benchmark the project(s) in this directory/configuration"
                        (\(bopts, beopts) -> buildCmd (DoBenchmarks beopts) bopts)
                        ((,) <$> buildOptsParser Bench False <*> benchOptsParser)
             addCommand "haddock"
                        "Generate haddocks for the project(s) in this directory/configuration"
                        (buildCmd DoNothing)
                        (buildOptsParser Haddock False)
             addCommand "new"
                        "Create a brand new project"
                        newCmd
                        newOptsParser
             addCommand "init"
                        "Initialize a stack project based on one or more cabal packages"
                        initCmd
                        initOptsParser
             addCommand "solver"
                        "Use a dependency solver to try and determine missing extra-deps"
                        solverCmd
                        solverOptsParser
             addCommand "setup"
                        "Get the appropriate GHC for your project"
                        setupCmd
                        setupParser
             addCommand "path"
                        "Print out handy path information"
                        pathCmd
                        (fmap
                             catMaybes
                             (sequenceA
                                  (map
                                      (\(desc,name,_) ->
                                           flag Nothing
                                                (Just name)
                                                (long (T.unpack name) <>
                                                 help desc))
                                      paths)))
             addCommand "unpack"
                        "Unpack one or more packages locally"
                        unpackCmd
                        (some $ strArgument $ metavar "PACKAGE")
             addCommand "update"
                        "Update the package index"
                        updateCmd
                        (pure ())
             addCommand "upgrade"
                        "Upgrade to the latest stack (experimental)"
                        upgradeCmd
                        (switch
                            ( long "git"
                           <> help "Clone from Git instead of downloading from Hackage (more dangerous)"
                            ))
             addCommand "upload"
                        "Upload a package to Hackage"
                        uploadCmd
                        (many $ strArgument $ metavar "TARBALL/DIR")
             addCommand "sdist"
                        "Create source distribution tarballs"
                        sdistCmd
                        (many $ strArgument $ metavar "DIR")
             addCommand "dot"
                        "Visualize your project's dependency graph using Graphviz dot"
                        dotCmd
                        dotOptsParser
             addCommand "exec"
                        "Execute a command"
                        execCmd
                        (execOptsParser Nothing)
             addCommand "ghc"
                        "Run ghc"
                        execCmd
                        (execOptsParser $ Just "ghc")
             addCommand "ghci"
                        "Run ghci in the context of project(s)"
                        ghciCmd
                        ((,,,,) <$>
                         fmap (map T.pack)
                              (many (strArgument
                                       (metavar "TARGET" <>
                                        help "If none specified, use all packages defined in current directory"))) <*>
                         argsOption (long "ghc-options" <>
                                      metavar "OPTION" <>
                                      help "Additional options passed to GHCi" <>
                                      value []) <*>
                         strOption (long "with-ghc" <>
                                    metavar "GHC" <>
                                    help "Use this command for the GHC to run" <>
                                    value "ghc" <>
                                    showDefault) <*>
                         flag False True (long "no-load" <>
                                         help "Don't load modules on start-up") <*>
                         packagesParser)
             addCommand "runghc"
                        "Run runghc"
                        execCmd
                        (execOptsParser $ Just "runghc")
             addCommand "clean"
                        "Clean the local packages"
                        cleanCmd
                        (pure ())
             addCommand "iface"
                        "Display TH dependencies"
                        ifaceCmd
                        (pure ())
             addCommand "list-dependencies"
                        "List the dependencies"
                        listDependenciesCmd
                        (T.pack <$> strOption (long "separator" <>
                                               metavar "SEP" <>
                                               help ("Separator between package name " <>
                                                     "and package version.") <>
                                               value " " <>
                                               showDefault))
             addSubCommands
                 "ide"
                 "IDE-specific commands"
                 (do addCommand
                         "start"
                         "Start the ide-backend service"
                         ideCmd
                         (((,) <$>
                           fmap (map T.pack)
                                (many (strArgument
                                         (metavar "TARGET" <>
                                          help "If none specified, use all packages defined in current directory"))) <*>
                           argsOption (long "ghc-options" <>
                                       metavar "OPTION" <>
                                       help "Additional options passed to GHCi" <>
                                       value [])))
                     addCommand
                         "packages"
                         "List all available local loadable packages"
                         packagesCmd
                         (pure ()))
             addSubCommands
               Docker.dockerCmdName
               "Subcommands specific to Docker use"
               (do addCommand Docker.dockerPullCmdName
                              "Pull latest version of Docker image from registry"
                              dockerPullCmd
                              (pure ())
                   addCommand "reset"
                              "Reset the Docker sandbox"
                              dockerResetCmd
                              (flag False True (long "keep-home" <>
                                               help "Do not delete sandbox's home directory"))
                   addCommand Docker.dockerCleanupCmdName
                              "Clean up Docker images and containers"
                              dockerCleanupCmd
                              dockerCleanupOptsParser)
             addSubCommands
               Image.imgCmdName
               "Subcommands specific to imaging (EXPERIMENTAL)"
               (addCommand Image.imgDockerCmdName
                "Build a Docker image for the project"
                imgDockerCmd
                (pure ())))
             -- commandsFromPlugins plugins pluginShouldHaveRun) https://github.com/commercialhaskell/stack/issues/322
     case eGlobalRun of
       Left (exitCode :: ExitCode) -> do
         when isInterpreter $
           putStrLn $ concat
             [ "\nIf you are trying to use "
             , stackProgName
             , " as a script interpreter, a\n'-- "
             , stackProgName
             , " [options] runghc [options]' comment is required."
             , "\nSee https://github.com/commercialhaskell/stack/wiki/Script-interpreter" ]
         throwIO exitCode
       Right (global,run) -> do
         when (globalLogLevel global == LevelDebug) $ putStrLn versionString'
         run global `catch` \e -> do
            -- This special handler stops "stack: " from being printed before the
            -- exception
            case fromException e of
                Just ec -> exitWith ec
                Nothing -> do
                    printExceptionStderr e
                    exitFailure
  where
    dockerHelpOptName = Docker.dockerCmdName ++ "-help"

-- Try to run a plugin
tryRunPlugin :: Plugins -> IO ()
tryRunPlugin plugins = do
  args <- getArgs
  case dropWhile (List.isPrefixOf "-") args of
    ((T.pack -> name):args')
      | isJust (lookupPlugin plugins name) -> do
          callPlugin plugins name args' `catch` onPluginErr
          exitSuccess
    _ -> return ()
-- TODO(danburton): use logger
onPluginErr :: PluginException -> IO ()
onPluginErr (PluginNotFound _ name) = do
  T.hPutStr stderr $ "Stack plugin not found: " <> name
  exitFailure
onPluginErr (PluginExitFailure _ i) = do
  exitWith (ExitFailure i)

-- TODO(danburton): improve this, although it should never happen
pluginShouldHaveRun :: Plugin -> GlobalOpts -> IO ()
pluginShouldHaveRun _plugin _globalOpts = do
  fail "Plugin should have run"

-- | Print out useful path information in a human-readable format (and
-- support others later).
pathCmd :: [Text] -> GlobalOpts -> IO ()
pathCmd keys go =
    withBuildConfigAndLock
        go
        (\_ ->
         do env <- ask
            let cfg = envConfig env
                bc = envConfigBuildConfig cfg
            menv <- getMinimalEnvOverride
            snap <- packageDatabaseDeps
            local <- packageDatabaseLocal
            snaproot <- installationRootDeps
            localroot <- installationRootLocal
            distDir <- distRelativeDir
            forM_
                (filter
                     (\(_,key,_) ->
                           null keys || elem key keys)
                     paths)
                (\(_,key,path) ->
                      liftIO $ T.putStrLn
                          ((if length keys == 1
                               then ""
                               else key <> ": ") <>
                           path
                               (PathInfo
                                    bc
                                    menv
                                    snap
                                    local
                                    snaproot
                                    localroot
                                    distDir))))

-- | Passed to all the path printers as a source of info.
data PathInfo = PathInfo
    {piBuildConfig :: BuildConfig
    ,piEnvOverride :: EnvOverride
    ,piSnapDb :: Path Abs Dir
    ,piLocalDb :: Path Abs Dir
    ,piSnapRoot :: Path Abs Dir
    ,piLocalRoot :: Path Abs Dir
    ,piDistDir :: Path Rel Dir
    }

-- | The paths of interest to a user. The first tuple string is used
-- for a description that the optparse flag uses, and the second
-- string as a machine-readable key and also for @--foo@ flags. The user
-- can choose a specific path to list like @--global-stack-root@. But
-- really it's mainly for the documentation aspect.
--
-- When printing output we generate @PathInfo@ and pass it to the
-- function to generate an appropriate string.  Trailing slashes are
-- removed, see #506
paths :: [(String, Text, PathInfo -> Text)]
paths =
    [ ( "Global stack root directory"
      , "global-stack-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (configStackRoot (bcConfig (piBuildConfig pi)))))
    , ( "Project root (derived from stack.yaml file)"
      , "project-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (bcRoot (piBuildConfig pi))))
    , ( "Configuration location (where the stack.yaml file is)"
      , "config-location"
      , \pi ->
             T.pack (toFilePathNoTrailing (bcStackYaml (piBuildConfig pi))))
    , ( "PATH environment variable"
      , "bin-path"
      , \pi ->
             T.pack (intercalate ":" (eoPath (piEnvOverride pi))))
    , ( "Installed GHCs (unpacked and archives)"
      , "ghc-paths"
      , \pi ->
             T.pack (toFilePathNoTrailing (configLocalPrograms (bcConfig (piBuildConfig pi)))))
    , ( "Local bin path where stack installs executables"
      , "local-bin-path"
      , \pi ->
             T.pack (toFilePathNoTrailing (configLocalBin (bcConfig (piBuildConfig pi)))))
    , ( "Extra include directories"
      , "extra-include-dirs"
      , \pi ->
             T.intercalate
                 ", "
                 (Set.elems (configExtraIncludeDirs (bcConfig (piBuildConfig pi)))))
    , ( "Extra library directories"
      , "extra-library-dirs"
      , \pi ->
             T.intercalate ", " (Set.elems (configExtraLibDirs (bcConfig (piBuildConfig pi)))))
    , ( "Snapshot package database"
      , "snapshot-pkg-db"
      , \pi ->
             T.pack (toFilePathNoTrailing (piSnapDb pi)))
    , ( "Local project package database"
      , "local-pkg-db"
      , \pi ->
             T.pack (toFilePathNoTrailing (piLocalDb pi)))
    , ( "Snapshot installation root"
      , "snapshot-install-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (piSnapRoot pi)))
    , ( "Local project installation root"
      , "local-install-root"
      , \pi ->
             T.pack (toFilePathNoTrailing (piLocalRoot pi)))
    , ( "Dist work directory"
      , "dist-dir"
      , \pi ->
             T.pack (toFilePathNoTrailing (piDistDir pi)))]
  where toFilePathNoTrailing = dropTrailingPathSeparator . toFilePath

data SetupCmdOpts = SetupCmdOpts
    { scoGhcVersion :: !(Maybe Version)
    , scoForceReinstall :: !Bool
    , scoUpgradeCabal :: !Bool
    }

setupParser :: Parser SetupCmdOpts
setupParser = SetupCmdOpts
    <$> (optional $ argument readVersion
            (metavar "GHC_MAJOR_VERSION" <>
             help ("Major version of GHC to install, e.g. 7.10. " ++
                   "The default is to install the version implied by the resolver.")))
    <*> boolFlags False
            "reinstall"
            "reinstalling GHC, even if available (implies no-system-ghc)"
            idm
    <*> boolFlags False
            "upgrade-cabal"
            "installing the newest version of the Cabal library globally"
            idm
  where
    readVersion = do
        s <- readerAsk
        case parseVersionFromString s of
            Nothing -> readerError $ "Invalid version: " ++ s
            Just x -> return x

setupCmd :: SetupCmdOpts -> GlobalOpts -> IO ()
setupCmd SetupCmdOpts{..} go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  withUserFileLock (configStackRoot $ lcConfig lc) $ \lk ->
   runStackTGlobal manager (lcConfig lc) go $
      Docker.reexecWithOptionalContainer
          (lcProjectRoot lc)
          Nothing
          (runStackLoggingTGlobal manager go $ do
              (ghc, mstack) <-
                  case scoGhcVersion of
                      Just v -> return (v, Nothing)
                      Nothing -> do
                          bc <- lcLoadBuildConfig lc globalResolver
                          return (bcGhcVersionExpected bc, Just $ bcStackYaml bc)
              mpaths <- runStackTGlobal manager (lcConfig lc) go $
                  ensureGHC SetupOpts
                  { soptsInstallIfMissing = True
                  , soptsUseSystem =
                    (configSystemGHC $ lcConfig lc)
                    && not scoForceReinstall
                  , soptsExpected = ghc
                  , soptsStackYaml = mstack
                  , soptsForceReinstall = scoForceReinstall
                  , soptsSanityCheck = True
                  , soptsSkipGhcCheck = False
                  , soptsSkipMsys = configSkipMsys $ lcConfig lc
                  , soptsUpgradeCabal = scoUpgradeCabal
                  }
              case mpaths of
                  Nothing -> $logInfo "stack will use the GHC on your PATH"
                  Just _ -> $logInfo "stack will use a locally installed GHC"
              $logInfo "For more information on paths, see 'stack path' and 'stack exec env'"
              $logInfo "To use this GHC and packages outside of a project, consider using:"
              $logInfo "stack ghc, stack ghci, stack runghc, or stack exec"
              )
          Nothing
          (Just $ liftIO$ unlockFile lk)

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: (MonadBaseControl IO m, MonadIO m)
                 => Path Abs Dir
                 -> (FileLock -> m a)
                 -> m a
withUserFileLock dir act = do
    let lockfile = $(mkRelFile "lockfile")
    let pth = dir </> lockfile
    liftIO $ createDirectoryIfMissing True (toFilePath dir)
    -- Just in case of asynchronous exceptions, we need to be careful
    -- when using tryLockFile here:
    EL.bracket (liftIO $ tryLockFile (toFilePath pth) Exclusive)
               (\fstTry -> maybe (return ()) (liftIO . unlockFile) fstTry)
               (\fstTry ->
                case fstTry of
                  Just lk -> EL.finally (act lk) (liftIO $ unlockFile lk)
                  Nothing ->
                    do liftIO $ putStrLn $ "Failed to grab lock ("++show pth++
                                           "); other stack instance running.  Waiting..."
                       EL.bracket (liftIO $ lockFile (toFilePath pth) Exclusive)
                                  (liftIO . unlockFile)
                                  (\lk -> do
                                    liftIO $ putStrLn "Lock acquired, proceeding."
                                    act lk))

withConfigAndLock :: GlobalOpts
           -> StackT Config IO ()
           -> IO ()
withConfigAndLock go@GlobalOpts{..} inner = do
    (manager, lc) <- loadConfigWithOpts go
    withUserFileLock (configStackRoot $ lcConfig lc) $ \lk ->
     runStackTGlobal manager (lcConfig lc) go $
        Docker.reexecWithOptionalContainer (lcProjectRoot lc)
            Nothing
            (runStackTGlobal manager (lcConfig lc) go inner)
            Nothing
            (Just $ liftIO $ unlockFile lk)

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withBuildConfig :: GlobalOpts
               -> (StackT EnvConfig IO ())
               -> IO ()
withBuildConfig go inner =
    withBuildConfigAndLock go (\lk -> do liftIO $ unlockFile lk
                                         inner)

withBuildConfigAndLock :: GlobalOpts
                 -> (FileLock -> StackT EnvConfig IO ())
                 -> IO ()
withBuildConfigAndLock go inner =
    withBuildConfigExt go Nothing inner Nothing

withBuildConfigExt
    :: GlobalOpts
    -> Maybe (StackT Config IO ())
    -- ^ Action to perform after before build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> (FileLock -> StackT EnvConfig IO ())
    -- ^ Action that uses the build config.  If Docker is enabled for builds,
    -- this will be run in a Docker container.
    -> Maybe (StackT Config IO ())
    -- ^ Action to perform after the build.  This will be run on the host
    -- OS even if Docker is enabled for builds.  The build config is not
    -- available in this action, since that would require build tools to be
    -- installed on the host OS.
    -> IO ()
withBuildConfigExt go@GlobalOpts{..} mbefore inner mafter = do
    (manager, lc) <- loadConfigWithOpts go

    withUserFileLock (configStackRoot $ lcConfig lc) $ \lk0 -> do
      -- A local bit of state for communication between callbacks:
      curLk <- newIORef lk0
      let inner' lk =
            -- Locking policy:  This is only used for build commands, which
            -- only need to lock the snapshot, not the global lock.  We
            -- trade in the lock here.
            do dir <- installationRootDeps
               -- Hand-over-hand locking:
               withUserFileLock dir $ \lk2 -> do
                 liftIO $ writeIORef curLk lk2
                 liftIO $ unlockFile lk
                 inner lk2

      let inner'' lk = do
              bconfig <- runStackLoggingTGlobal manager go $
                  lcLoadBuildConfig lc globalResolver
              envConfig <-
                 runStackTGlobal
                     manager bconfig go
                     setupEnv
              runStackTGlobal
                  manager
                  envConfig
                  go
                  (inner' lk)

      runStackTGlobal manager (lcConfig lc) go $
         Docker.reexecWithOptionalContainer (lcProjectRoot lc) mbefore (inner'' lk0) mafter
                                            (Just $ liftIO $
                                             do lk' <- readIORef curLk
                                                unlockFile lk')

cleanCmd :: () -> GlobalOpts -> IO ()
cleanCmd () go = withBuildConfigAndLock go (\_ -> clean)

-- | Helper for build and install commands
buildCmdHelper :: StackT EnvConfig IO () -- ^ do before build
               -> FinalAction -> BuildOpts -> GlobalOpts -> IO ()
buildCmdHelper beforeBuild finalAction opts go
    | boptsFileWatch opts = fileWatch inner
    | otherwise = inner $ const $ return ()
  where
    inner setLocalFiles = withBuildConfigAndLock go $ \lk -> do
        beforeBuild
        Stack.Build.build
            setLocalFiles
            (Just lk)
            opts { boptsFinalAction = finalAction
                 , boptsEnableTests =
                       case finalAction of
                         DoTests{} -> True
                         _ -> False
                 , boptsEnableBenchmarks =
                       case finalAction of
                         DoBenchmarks{} -> True
                         _ -> False
                 }

-- | Build the project.
buildCmd :: FinalAction -> BuildOpts -> GlobalOpts -> IO ()
buildCmd = buildCmdHelper (return ())

-- | Install
installCmd :: BuildOpts -> GlobalOpts -> IO ()
installCmd =
    buildCmdHelper warning DoNothing
  where
    warning = do
        $logInfo "NOTE: the install command will copy executables to a destination directory"
        $logInfo "It is functionally equivalent to the --copy-bins option"

copyCmd :: BuildOpts -> GlobalOpts -> IO ()
copyCmd opts = buildCmdHelper (return ()) DoNothing opts { boptsInstallExes = True }

uninstallCmd :: [String] -> GlobalOpts -> IO ()
uninstallCmd _ go = withConfigAndLock go $ do
    $logError "stack does not manage installations in global locations"
    $logError "The only global mutation stack performs is executable copying"
    $logError "For the default executable destination, please run 'stack path --local-bin-path'"

-- | Unpack packages to the filesystem
unpackCmd :: [String] -> GlobalOpts -> IO ()
unpackCmd names go = withConfigAndLock go $ do
    menv <- getMinimalEnvOverride
    Stack.Fetch.unpackPackages menv "." names

-- | Update the package index
updateCmd :: () -> GlobalOpts -> IO ()
updateCmd () go = withConfigAndLock go $
    getMinimalEnvOverride >>= Stack.PackageIndex.updateAllIndices

upgradeCmd :: Bool -> GlobalOpts -> IO ()
upgradeCmd fromGit go = withConfigAndLock go $
    upgrade fromGit (globalResolver go)

-- | Upload to Hackage
uploadCmd :: [String] -> GlobalOpts -> IO ()
uploadCmd [] _ = error "To upload the current package, please run 'stack upload .'"
uploadCmd args go = do
    let partitionM _ [] = return ([], [])
        partitionM f (x:xs) = do
            r <- f x
            (as, bs) <- partitionM f xs
            return $ if r then (x:as, bs) else (as, x:bs)
    (files, nonFiles) <- partitionM doesFileExist args
    (dirs, invalid) <- partitionM doesDirectoryExist nonFiles
    when (not (null invalid)) $ error $
        "stack upload expects a list sdist tarballs or cabal directories.  Can't find " ++
        show invalid
    let getUploader :: (HasStackRoot config, HasPlatform config, HasConfig config) => StackT config IO Upload.Uploader
        getUploader = do
            config <- asks getConfig
            manager <- asks envManager
            let uploadSettings =
                    Upload.setGetManager (return manager) $
                    Upload.defaultUploadSettings
            liftIO $ Upload.mkUploader config uploadSettings
    if null dirs
        then withConfigAndLock go $ do
            uploader <- getUploader
            liftIO $ forM_ files (canonicalizePath >=> Upload.upload uploader)
        else withBuildConfigAndLock go $ \_ -> do
            uploader <- getUploader
            liftIO $ forM_ files (canonicalizePath >=> Upload.upload uploader)
            forM_ dirs $ \dir -> do
                pkgDir <- parseAbsDir =<< liftIO (canonicalizePath dir)
                (tarName, tarBytes) <- getSDistTarball pkgDir
                liftIO $ Upload.uploadBytes uploader tarName tarBytes

sdistCmd :: [String] -> GlobalOpts -> IO ()
sdistCmd dirs go =
    withBuildConfig go $ do -- No locking needed.
        -- If no directories are specified, build all sdist tarballs.
        dirs' <- if null dirs
            then asks (Map.keys . envConfigPackages . getEnvConfig)
            else mapM (parseAbsDir <=< liftIO . canonicalizePath) dirs
        forM_ dirs' $ \dir -> do
            (tarName, tarBytes) <- getSDistTarball dir
            distDir <- distDirFromDir dir
            tarPath <- fmap (distDir </>) $ parseRelFile tarName
            liftIO $ L.writeFile (toFilePath tarPath) tarBytes
            $logInfo $ "Wrote sdist tarball to " <> T.pack (toFilePath tarPath)

-- | Execute a command.
execCmd :: ExecOpts -> GlobalOpts -> IO ()
execCmd ExecOpts {..} go@GlobalOpts{..} =
    case eoExtra of
        ExecOptsPlain -> do
            (manager,lc) <- liftIO $ loadConfigWithOpts go
            withUserFileLock (configStackRoot $ lcConfig lc) $ \lk ->
             runStackTGlobal manager (lcConfig lc) go $
                Docker.execWithOptionalContainer
                    (lcProjectRoot lc)
                    (return (eoCmd, eoArgs, [], id))
                    -- Unlock before transferring control away, whether using docker or not:
                    (Just $ liftIO $ unlockFile lk)
                    (runStackTGlobal manager (lcConfig lc) go $ do
                        exec plainEnvSettings eoCmd eoArgs)
                    Nothing
                    Nothing -- Unlocked already above.
        ExecOptsEmbellished {..} ->
           withBuildConfigAndLock go $ \lk -> do
               let targets = concatMap words eoPackages
               unless (null targets) $
                   Stack.Build.build (const $ return ()) (Just lk) defaultBuildOpts
                       { boptsTargets = map T.pack targets
                       }
               liftIO $ unlockFile lk -- Unlock before transferring control away.
               exec eoEnvSettings eoCmd eoArgs

-- | Run GHCi in the context of a project.
ghciCmd :: ([Text], [String], FilePath, Bool, [String]) -> GlobalOpts -> IO ()
ghciCmd (targets,args,path,noload,packages) go@GlobalOpts{..} = do
  withBuildConfigAndLock go $ \lk -> do
    let packageTargets = concatMap words packages
    unless (null packageTargets) $
       Stack.Build.build (const $ return ()) (Just lk) defaultBuildOpts
           { boptsTargets = map T.pack packageTargets
           }
    liftIO $ unlockFile lk -- Don't hold the lock while in the GHCI.
    ghci targets args path noload

-- | Run ide-backend in the context of a project.
ideCmd :: ([Text], [String]) -> GlobalOpts -> IO ()
ideCmd (targets,args) go@GlobalOpts{..} =
    withBuildConfig go $ -- No locking needed.
      ide targets args

-- | Run ide-backend in the context of a project.
packagesCmd :: () -> GlobalOpts -> IO ()
packagesCmd () go@GlobalOpts{..} =
    withBuildConfig go $
      do econfig <- asks getEnvConfig
         locals <-
             forM (M.toList (envConfigPackages econfig)) $
             \(dir,_) ->
                  do cabalfp <- getCabalFileName dir
                     parsePackageNameFromFilePath cabalfp
         forM_ locals (liftIO . putStrLn . packageNameString)

-- | Pull the current Docker image.
dockerPullCmd :: () -> GlobalOpts -> IO ()
dockerPullCmd _ go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock (configStackRoot $ lcConfig lc) $ \_ ->
     runStackTGlobal manager (lcConfig lc) go $
       Docker.preventInContainer Docker.pull

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> GlobalOpts -> IO ()
dockerResetCmd keepHome go@GlobalOpts{..} = do
    (manager,lc) <- liftIO (loadConfigWithOpts go)
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock (configStackRoot $ lcConfig lc) $ \_ ->
     runStackLoggingTGlobal manager go $
        Docker.preventInContainer $ Docker.reset (lcProjectRoot lc) keepHome

-- | Cleanup Docker images and containers.
dockerCleanupCmd :: Docker.CleanupOpts -> GlobalOpts -> IO ()
dockerCleanupCmd cleanupOpts go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock (configStackRoot $ lcConfig lc) $ \_ ->
     runStackTGlobal manager (lcConfig lc) go $
        Docker.preventInContainer $
            Docker.cleanup cleanupOpts

imgDockerCmd :: () -> GlobalOpts -> IO ()
imgDockerCmd () go@GlobalOpts{..} = do
    withBuildConfigExt
        go
        Nothing
        (\lk ->
         do Stack.Build.build
                (const (return ()))
                (Just lk)
                defaultBuildOpts
            Image.stageContainerImageArtifacts)
        (Just Image.createContainerImageFromStage)

-- | Load the configuration with a manager. Convenience function used
-- throughout this module.
loadConfigWithOpts :: GlobalOpts -> IO (Manager,LoadConfig (StackLoggingT IO))
loadConfigWithOpts go@GlobalOpts{..} = do
    manager <- newTLSManager
    mstackYaml <-
        case globalStackYaml of
            Nothing -> return Nothing
            Just fp -> do
                path <- canonicalizePath fp >>= parseAbsFile
                return $ Just path
    lc <- runStackLoggingTGlobal
              manager
              go
              (loadConfig globalConfigMonoid mstackYaml)
    return (manager,lc)

-- | Project initialization
initCmd :: InitOpts -> GlobalOpts -> IO ()
initCmd initOpts go = withConfigAndLock go $ initProject initOpts

-- | Project creation
newCmd :: NewOpts -> GlobalOpts -> IO ()
newCmd newOpts go@GlobalOpts{..} = withConfigAndLock go $ do
    newProject newOpts
    initProject (newOptsInitOpts newOpts)

-- | Fix up extra-deps for a project
solverCmd :: Bool -- ^ modify stack.yaml automatically?
          -> GlobalOpts
          -> IO ()
solverCmd fixStackYaml go =
    withBuildConfigAndLock go (\_ -> solveExtraDeps fixStackYaml)

-- | Visualize dependencies
dotCmd :: DotOpts -> GlobalOpts -> IO ()
dotCmd dotOpts go = withBuildConfigAndLock go (\_ -> dot dotOpts)

ifaceCmd :: () -> GlobalOpts -> IO ()
ifaceCmd () go = withBuildConfigAndLock go (\_ -> iface)

-- | List the dependencies
listDependenciesCmd :: Text -> GlobalOpts -> IO ()
listDependenciesCmd sep go = withBuildConfig go (listDependencies sep')
  where sep' = T.replace "\\t" "\t" (T.replace "\\n" "\n" sep)
