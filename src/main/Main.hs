{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main stack tool entry point.

module Main (main) where

import           Control.Exception
import qualified Control.Exception.Lifted as EL
import           Control.Monad hiding (mapM, forM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (ask, asks, runReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Attoparsec.Args (withInterpreterArgs, parseArgs, EscapingMode (Escaping))
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           Data.List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Maybe.Extra (mapMaybeA)
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Data.Typeable (Typeable)
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           GHC.IO.Encoding (mkTextEncoding, textEncodingName)
import           Network.HTTP.Client
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Complicated
import           Options.Applicative.Simple (simpleVersion)
import           Options.Applicative.Types (readerAsk)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import qualified Paths_stack as Meta
import           Prelude hiding (pi, mapM)
import           Stack.Build
import           Stack.Clean (CleanOpts, clean)
import           Stack.Config
import           Stack.ConfigCmd as ConfigCmd
import           Stack.Constants
import           Stack.Coverage
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.Exec
import           Stack.Fetch
import           Stack.FileWatch
import           Stack.GhcPkg (getGlobalDB, mkGhcPackagePath)
import           Stack.Ghci
import           Stack.Ide
import qualified Stack.Image as Image
import           Stack.Init
import           Stack.New
import           Stack.Options
import           Stack.Package (getCabalFileName)
import qualified Stack.PackageIndex
import           Stack.SDist (getSDistTarball, checkSDistTarball, checkSDistTarball')
import           Stack.Setup
import           Stack.Solver (solveExtraDeps)
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           Stack.Upgrade
import qualified Stack.Upload as Upload
import           System.Directory (canonicalizePath, doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import           System.Environment (getEnvironment, getProgName)
import           System.Exit
import           System.FileLock (lockFile, tryLockFile, unlockFile, SharedExclusive(Exclusive), FileLock)
import           System.FilePath (searchPathSeparator)
import           System.IO (hIsTerminalDevice, stderr, stdin, stdout, hSetBuffering, BufferMode(..), hPutStrLn, Handle, hGetEncoding, hSetEncoding)
import           System.Process.Read

-- | Change the character encoding of the given Handle to transliterate
-- on unsupported characters instead of throwing an exception
hSetTranslit :: Handle -> IO ()
hSetTranslit h = do
    menc <- hGetEncoding h
    case fmap textEncodingName menc of
        Just name
          | '/' `notElem` name -> do
              enc' <- mkTextEncoding $ name ++ "//TRANSLIT"
              hSetEncoding h enc'
        _ -> return ()

-- | Commandline dispatcher.
main :: IO ()
main = withInterpreterArgs stackProgName $ \args isInterpreter -> do
     -- Line buffer the output by default, particularly for non-terminal runs.
     -- See https://github.com/commercialhaskell/stack/pull/360
     hSetBuffering stdout LineBuffering
     hSetBuffering stdin  LineBuffering
     hSetBuffering stderr LineBuffering
     hSetTranslit stdout
     hSetTranslit stderr
     progName <- getProgName
     isTerminal <- hIsTerminalDevice stdout
     execExtraHelp args
                   dockerHelpOptName
                   (dockerOptsParser False)
                   ("Only showing --" ++ Docker.dockerCmdName ++ "* options.")
     let commitCount = $gitCommitCount
         versionString' = concat $ concat
            [ [$(simpleVersion Meta.version)]
              -- Leave out number of commits for --depth=1 clone
              -- See https://github.com/commercialhaskell/stack/issues/792
            , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                                    commitCount /= ("UNKNOWN" :: String)]
            , [" ", display buildArch]
            ]

     let globalOpts hide =
             extraHelpOption hide progName (Docker.dockerCmdName ++ "*") dockerHelpOptName <*>
             globalOptsParser hide
         addCommand' cmd title footerStr constr =
             addCommand cmd title footerStr constr (globalOpts True)
         addSubCommands' cmd title footerStr =
             addSubCommands cmd title footerStr (globalOpts True)
     eGlobalRun <- try $
       complicatedOptions
         Meta.version
         (Just versionString')
         "stack - The Haskell Tool Stack"
         ""
         (globalOpts False)
         (do addCommand' "build"
                        "Build the package(s) in this directory/configuration"
                        cmdFooter
                        buildCmd
                        (buildOptsParser Build)
             addCommand' "install"
                        "Shortcut for 'build --copy-bins'"
                        cmdFooter
                        buildCmd
                        (buildOptsParser Install)
             addCommand' "uninstall"
                        "DEPRECATED: This command performs no actions, and is present for documentation only"
                        cmdFooter
                        uninstallCmd
                        (many $ strArgument $ metavar "IGNORED")
             addCommand' "test"
                        "Shortcut for 'build --test'"
                        cmdFooter
                        buildCmd
                        (buildOptsParser Test)
             addCommand' "bench"
                        "Shortcut for 'build --bench'"
                        cmdFooter
                        buildCmd
                        (buildOptsParser Bench)
             addCommand' "haddock"
                        "Shortcut for 'build --haddock'"
                        cmdFooter
                        buildCmd
                        (buildOptsParser Haddock)
             addCommand' "new"
                        "Create a new project from a template. Run `stack templates' to see available templates."
                        cmdFooter
                        newCmd
                        newOptsParser
             addCommand' "templates"
                        "List the templates available for `stack new'."
                        cmdFooter
                        templatesCmd
                        (pure ())
             addCommand' "init"
                        "Initialize a stack project based on one or more cabal packages"
                        cmdFooter
                        initCmd
                        initOptsParser
             addCommand' "solver"
                        "Use a dependency solver to try and determine missing extra-deps"
                        cmdFooter
                        solverCmd
                        solverOptsParser
             addCommand' "setup"
                        "Get the appropriate GHC for your project"
                        cmdFooter
                        setupCmd
                        setupParser
             addCommand' "path"
                        "Print out handy path information"
                        cmdFooter
                        pathCmd
                        (mapMaybeA
                            (\(desc,name,_) ->
                                 flag Nothing
                                      (Just name)
                                      (long (T.unpack name) <>
                                       help desc))
                            paths)
             addCommand' "unpack"
                        "Unpack one or more packages locally"
                        cmdFooter
                        unpackCmd
                        (some $ strArgument $ metavar "PACKAGE")
             addCommand' "update"
                        "Update the package index"
                        cmdFooter
                        updateCmd
                        (pure ())
             addCommand' "upgrade"
                        "Upgrade to the latest stack (experimental)"
                        cmdFooter
                        upgradeCmd
                        ((,) <$> switch
                                  ( long "git"
                                 <> help "Clone from Git instead of downloading from Hackage (more dangerous)" )
                             <*> strOption
                                  ( long "git-repo"
                                 <> help "Clone from specified git repository"
                                 <> value "https://github.com/commercialhaskell/stack"
                                 <> showDefault ))
             addCommand' "upload"
                        "Upload a package to Hackage"
                        cmdFooter
                        uploadCmd
                        ((,,)
                         <$> many (strArgument $ metavar "TARBALL/DIR")
                         <*> optional pvpBoundsOption
                         <*> ignoreCheckSwitch)
             addCommand' "sdist"
                        "Create source distribution tarballs"
                        cmdFooter
                        sdistCmd
                        ((,,)
                         <$> many (strArgument $ metavar "DIR")
                         <*> optional pvpBoundsOption
                         <*> ignoreCheckSwitch)
             addCommand' "dot"
                        "Visualize your project's dependency graph using Graphviz dot"
                        cmdFooter
                        dotCmd
                        dotOptsParser
             addCommand' "exec"
                        "Execute a command"
                        cmdFooter
                        execCmd
                        (execOptsParser Nothing)
             addCommand' "ghc"
                        "Run ghc"
                        cmdFooter
                        execCmd
                        (execOptsParser $ Just ExecGhc)
             addCommand' "ghci"
                        "Run ghci in the context of package(s) (experimental)"
                        cmdFooter
                        ghciCmd
                        ghciOptsParser
             addCommand' "repl"
                        "Run ghci in the context of package(s) (experimental) (alias for 'ghci')"
                        cmdFooter
                        ghciCmd
                        ghciOptsParser
             addCommand' "runghc"
                        "Run runghc"
                        cmdFooter
                        execCmd
                        (execOptsParser $ Just ExecRunGhc)
             addCommand' "runhaskell"
                        "Run runghc (alias for 'runghc')"
                        cmdFooter
                        execCmd
                        (execOptsParser $ Just ExecRunGhc)
             addCommand' "eval"
                        "Evaluate some haskell code inline. Shortcut for 'stack exec ghc -- -e CODE'"
                        cmdFooter
                        evalCmd
                        (evalOptsParser "CODE")
             addCommand' "clean"
                        "Clean the local packages"
                        cmdFooter
                        cleanCmd
                        cleanOptsParser
             addCommand' "list-dependencies"
                        "List the dependencies"
                        cmdFooter
                        listDependenciesCmd
                        (textOption (long "separator" <>
                                     metavar "SEP" <>
                                     help ("Separator between package name " <>
                                           "and package version.") <>
                                     value " " <>
                                     showDefault))
             addCommand' "query"
                        "Query general build information (experimental)"
                        cmdFooter
                        queryCmd
                        (many $ strArgument $ metavar "SELECTOR...")
             addSubCommands'
                 "ide"
                 "IDE-specific commands"
                 cmdFooter
                 (do addCommand'
                         "start"
                         "Start the ide-backend service"
                         cmdFooter
                         ideCmd
                         ((,) <$> many (textArgument
                                          (metavar "TARGET" <>
                                           help ("If none specified, use all " <>
                                                 "packages defined in current directory")))
                              <*> argsOption (long "ghc-options" <>
                                              metavar "OPTION" <>
                                              help "Additional options passed to GHCi" <>
                                              value []))
                     addCommand'
                         "packages"
                         "List all available local loadable packages"
                         cmdFooter
                         packagesCmd
                         (pure ())
                     addCommand'
                         "load-targets"
                         "List all load targets for a package target"
                         cmdFooter
                         targetsCmd
                         (textArgument
                            (metavar "TARGET")))
             addSubCommands'
               Docker.dockerCmdName
               "Subcommands specific to Docker use"
               cmdFooter
               (do addCommand' Docker.dockerPullCmdName
                              "Pull latest version of Docker image from registry"
                              cmdFooter
                              dockerPullCmd
                              (pure ())
                   addCommand' "reset"
                              "Reset the Docker sandbox"
                              cmdFooter
                              dockerResetCmd
                              (switch (long "keep-home" <>
                                       help "Do not delete sandbox's home directory"))
                   addCommand' Docker.dockerCleanupCmdName
                              "Clean up Docker images and containers"
                              cmdFooter
                              dockerCleanupCmd
                              dockerCleanupOptsParser)
             addSubCommands'
                ConfigCmd.cfgCmdName
                "Subcommands specific to modifying stack.yaml files"
                cmdFooter
                (addCommand' ConfigCmd.cfgCmdSetName
                            "Sets a field in the project's stack.yaml to value"
                            cmdFooter
                            cfgSetCmd
                            configCmdSetParser)
             addSubCommands'
               Image.imgCmdName
               "Subcommands specific to imaging (EXPERIMENTAL)"
               cmdFooter
               (addCommand' Image.imgDockerCmdName
                "Build a Docker image for the project"
                cmdFooter
                imgDockerCmd
                (boolFlags True
                    "build"
                    "building the project before creating the container"
                    idm))
             addSubCommands'
               "hpc"
               "Subcommands specific to Haskell Program Coverage"
               cmdFooter
               (addCommand' "report"
                            "Generate HPC report a combined HPC report"
                            cmdFooter
                            hpcReportCmd
                            hpcReportOptsParser))
     case eGlobalRun of
       Left (exitCode :: ExitCode) -> do
         when isInterpreter $
           hPutStrLn stderr $ concat
             [ "\nIf you are trying to use "
             , stackProgName
             , " as a script interpreter, a\n'-- "
             , stackProgName
             , " [options] runghc [options]' comment is required."
             , "\nSee https://github.com/commercialhaskell/stack/blob/release/doc/GUIDE.md#ghcrunghc" ]
         throwIO exitCode
       Right (globalMonoid,run) -> do
         let global = globalOptsFromMonoid isTerminal globalMonoid
         when (globalLogLevel global == LevelDebug) $ hPutStrLn stderr versionString'
         case globalReExecVersion global of
             Just expectVersion
                 | expectVersion /= showVersion Meta.version ->
                     throwIO $ InvalidReExecVersion expectVersion (showVersion Meta.version)
             _ -> return ()
         run global `catch` \e ->
            -- This special handler stops "stack: " from being printed before the
            -- exception
            case fromException e of
                Just ec -> exitWith ec
                Nothing -> do
                    printExceptionStderr e
                    exitFailure
  where
    ignoreCheckSwitch = switch (long "ignore-check" <> help "Do not check package for common mistakes")
    dockerHelpOptName = Docker.dockerCmdName ++ "-help"
    cmdFooter = "Run 'stack --help' for global options that apply to all subcommands."

-- | Print out useful path information in a human-readable format (and
-- support others later).
pathCmd :: [Text] -> GlobalOpts -> IO ()
pathCmd keys go =
    withBuildConfig
        go
        (do env <- ask
            let cfg = envConfig env
                bc = envConfigBuildConfig cfg
            -- This is the modified 'bin-path',
            -- including the local GHC or MSYS if not configured to operate on
            -- global GHC.
            -- It was set up in 'withBuildConfigAndLock -> withBuildConfigExt -> setupEnv'.
            -- So it's not the *minimal* override path.
            menv <- getMinimalEnvOverride
            snap <- packageDatabaseDeps
            local <- packageDatabaseLocal
            extra <- packageDatabaseExtra
            global <- getGlobalDB menv =<< getWhichCompiler
            snaproot <- installationRootDeps
            localroot <- installationRootLocal
            distDir <- distRelativeDir
            hpcDir <- hpcReportDir
            forM_
                -- filter the chosen paths in flags (keys),
                -- or show all of them if no specific paths chosen.
                (filter
                     (\(_,key,_) ->
                           null keys || elem key keys)
                     paths)
                (\(_,key,path) ->
                      liftIO $ T.putStrLn
                          -- If a single path type is requested, output it directly.
                          -- Otherwise, name all the paths.
                          ((if length keys == 1
                               then ""
                               else key <> ": ") <>
                           path
                               (PathInfo
                                    bc
                                    menv
                                    snap
                                    local
                                    global
                                    snaproot
                                    localroot
                                    distDir
                                    hpcDir
                                    extra))))

-- | Passed to all the path printers as a source of info.
data PathInfo = PathInfo
    { piBuildConfig :: BuildConfig
    , piEnvOverride :: EnvOverride
    , piSnapDb      :: Path Abs Dir
    , piLocalDb     :: Path Abs Dir
    , piGlobalDb    :: Path Abs Dir
    , piSnapRoot    :: Path Abs Dir
    , piLocalRoot   :: Path Abs Dir
    , piDistDir     :: Path Rel Dir
    , piHpcDir      :: Path Abs Dir
    , piExtraDbs    :: [Path Abs Dir]
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
      , T.pack . toFilePathNoTrailingSep . configStackRoot . bcConfig . piBuildConfig )
    , ( "Project root (derived from stack.yaml file)"
      , "project-root"
      , T.pack . toFilePathNoTrailingSep . bcRoot . piBuildConfig )
    , ( "Configuration location (where the stack.yaml file is)"
      , "config-location"
      , T.pack . toFilePath . bcStackYaml . piBuildConfig )
    , ( "PATH environment variable"
      , "bin-path"
      , T.pack . intercalate [searchPathSeparator] . eoPath . piEnvOverride )
    , ( "Installed GHCs (unpacked and archives)"
      , "ghc-paths"
      , T.pack . toFilePathNoTrailingSep . configLocalPrograms . bcConfig . piBuildConfig )
    , ( "Local bin path where stack installs executables"
      , "local-bin-path"
      , T.pack . toFilePathNoTrailingSep . configLocalBin . bcConfig . piBuildConfig )
    , ( "Extra include directories"
      , "extra-include-dirs"
      , T.intercalate ", " . Set.elems . configExtraIncludeDirs . bcConfig . piBuildConfig )
    , ( "Extra library directories"
      , "extra-library-dirs"
      , T.intercalate ", " . Set.elems . configExtraLibDirs . bcConfig . piBuildConfig )
    , ( "Snapshot package database"
      , "snapshot-pkg-db"
      , T.pack . toFilePathNoTrailingSep . piSnapDb )
    , ( "Local project package database"
      , "local-pkg-db"
      , T.pack . toFilePathNoTrailingSep . piLocalDb )
    , ( "Global package database"
      , "global-pkg-db"
      , T.pack . toFilePathNoTrailingSep . piGlobalDb )
    , ( "GHC_PACKAGE_PATH environment variable"
      , "ghc-package-path"
      , \pi -> mkGhcPackagePath True (piLocalDb pi) (piSnapDb pi) (piExtraDbs pi) (piGlobalDb pi))
    , ( "Snapshot installation root"
      , "snapshot-install-root"
      , T.pack . toFilePathNoTrailingSep . piSnapRoot )
    , ( "Local project installation root"
      , "local-install-root"
      , T.pack . toFilePathNoTrailingSep . piLocalRoot )
    , ( "Snapshot documentation root"
      , "snapshot-doc-root"
      , \pi -> T.pack (toFilePathNoTrailingSep (piSnapRoot pi </> docDirSuffix)))
    , ( "Local project documentation root"
      , "local-doc-root"
      , \pi -> T.pack (toFilePathNoTrailingSep (piLocalRoot pi </> docDirSuffix)))
    , ( "Dist work directory"
      , "dist-dir"
      , T.pack . toFilePathNoTrailingSep . piDistDir )
    , ( "Where HPC reports and tix files are stored"
      , "local-hpc-root"
      , T.pack . toFilePathNoTrailingSep . piHpcDir ) ]

data SetupCmdOpts = SetupCmdOpts
    { scoCompilerVersion :: !(Maybe CompilerVersion)
    , scoForceReinstall  :: !Bool
    , scoUpgradeCabal    :: !Bool
    , scoStackSetupYaml  :: !String
    , scoGHCBindistURL   :: !(Maybe String)
    }

setupParser :: Parser SetupCmdOpts
setupParser = SetupCmdOpts
    <$> optional (argument readVersion
            (metavar "GHC_VERSION" <>
             help ("Version of GHC to install, e.g. 7.10.2. " ++
                   "The default is to install the version implied by the resolver.")))
    <*> boolFlags False
            "reinstall"
            "reinstalling GHC, even if available (implies no-system-ghc)"
            idm
    <*> boolFlags False
            "upgrade-cabal"
            "installing the newest version of the Cabal library globally"
            idm
    <*> strOption
            ( long "stack-setup-yaml"
           <> help "Location of the main stack-setup.yaml file"
           <> value defaultStackSetupYaml
           <> showDefault )
    <*> optional (strOption
            (long "ghc-bindist"
           <> metavar "URL"
           <> help "Alternate GHC binary distribution (requires custom --ghc-variant)"))
  where
    readVersion = do
        s <- readerAsk
        case parseCompilerVersion ("ghc-" <> T.pack s) of
            Nothing ->
                case parseCompilerVersion (T.pack s) of
                    Nothing -> readerError $ "Invalid version: " ++ s
                    Just x -> return x
            Just x -> return x

setupCmd :: SetupCmdOpts -> GlobalOpts -> IO ()
setupCmd SetupCmdOpts{..} go@GlobalOpts{..} = do
  (manager,lc) <- loadConfigWithOpts go
  withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk ->
   runStackTGlobal manager (lcConfig lc) go $
      Docker.reexecWithOptionalContainer
          (lcProjectRoot lc)
          Nothing
          (runStackLoggingTGlobal manager go $ do
              (wantedCompiler, compilerCheck, mstack) <-
                  case scoCompilerVersion of
                      Just v -> return (v, MatchMinor, Nothing)
                      Nothing -> do
                          bc <- lcLoadBuildConfig lc globalResolver globalCompiler
                          return ( bcWantedCompiler bc
                                 , configCompilerCheck (lcConfig lc)
                                 , Just $ bcStackYaml bc
                                 )
              miniConfig <- loadMiniConfig (lcConfig lc)
              mpaths <- runStackTGlobal manager miniConfig go $
                  ensureCompiler SetupOpts
                  { soptsInstallIfMissing = True
                  , soptsUseSystem =
                    configSystemGHC (lcConfig lc) && not scoForceReinstall
                  , soptsWantedCompiler = wantedCompiler
                  , soptsCompilerCheck = compilerCheck
                  , soptsStackYaml = mstack
                  , soptsForceReinstall = scoForceReinstall
                  , soptsSanityCheck = True
                  , soptsSkipGhcCheck = False
                  , soptsSkipMsys = configSkipMsys $ lcConfig lc
                  , soptsUpgradeCabal = scoUpgradeCabal
                  , soptsResolveMissingGHC = Nothing
                  , soptsStackSetupYaml = scoStackSetupYaml
                  , soptsGHCBindistURL = scoGHCBindistURL
                  }
              let compiler = case wantedCompiler of
                      GhcVersion _ -> "GHC"
                      GhcjsVersion {} -> "GHCJS"
              case mpaths of
                  Nothing -> $logInfo $ "stack will use the " <> compiler <> " on your PATH"
                  Just _ -> $logInfo $ "stack will use a locally installed " <> compiler
              $logInfo "For more information on paths, see 'stack path' and 'stack exec env'"
              $logInfo $ "To use this " <> compiler <> " and packages outside of a project, consider using:"
              $logInfo "stack ghc, stack ghci, stack runghc, or stack exec"
              )
          Nothing
          (Just $ munlockFile lk)

-- | Unlock a lock file, if the value is Just
munlockFile :: MonadIO m => Maybe FileLock -> m ()
munlockFile Nothing = return ()
munlockFile (Just lk) = liftIO $ unlockFile lk

-- | Enforce mutual exclusion of every action running via this
-- function, on this path, on this users account.
--
-- A lock file is created inside the given directory.  Currently,
-- stack uses locks per-snapshot.  In the future, stack may refine
-- this to an even more fine-grain locking approach.
--
withUserFileLock :: (MonadBaseControl IO m, MonadIO m)
                 => GlobalOpts
                 -> Path Abs Dir
                 -> (Maybe FileLock -> m a)
                 -> m a
withUserFileLock go@GlobalOpts{} dir act = do
    env <- liftIO getEnvironment
    let toLock = lookup "STACK_LOCK" env == Just "true"
    if toLock
        then do
            let lockfile = $(mkRelFile "lockfile")
            let pth = dir </> lockfile
            liftIO $ createDirectoryIfMissing True (toFilePath dir)
            -- Just in case of asynchronous exceptions, we need to be careful
            -- when using tryLockFile here:
            EL.bracket (liftIO $ tryLockFile (toFilePath pth) Exclusive)
                       (maybe (return ()) (liftIO . unlockFile))
                       (\fstTry ->
                        case fstTry of
                          Just lk -> EL.finally (act $ Just lk) (liftIO $ unlockFile lk)
                          Nothing ->
                            do let chatter = globalLogLevel go /= LevelOther "silent"
                               when chatter $
                                 liftIO $ hPutStrLn stderr $ "Failed to grab lock ("++show pth++
                                                     "); other stack instance running.  Waiting..."
                               EL.bracket (liftIO $ lockFile (toFilePath pth) Exclusive)
                                          (liftIO . unlockFile)
                                          (\lk -> do
                                            when chatter $
                                              liftIO $ hPutStrLn stderr "Lock acquired, proceeding."
                                            act $ Just lk))
        else act Nothing

withConfigAndLock :: GlobalOpts
           -> StackT Config IO ()
           -> IO ()
withConfigAndLock go@GlobalOpts{..} inner = do
    (manager, lc) <- loadConfigWithOpts go
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk ->
     runStackTGlobal manager (lcConfig lc) go $
        Docker.reexecWithOptionalContainer (lcProjectRoot lc)
            Nothing
            (runStackTGlobal manager (lcConfig lc) go inner)
            Nothing
            (Just $ munlockFile lk)

-- For now the non-locking version just unlocks immediately.
-- That is, there's still a serialization point.
withBuildConfig :: GlobalOpts
               -> StackT EnvConfig IO ()
               -> IO ()
withBuildConfig go inner =
    withBuildConfigAndLock go (\lk -> do munlockFile lk
                                         inner)

withBuildConfigAndLock :: GlobalOpts
                 -> (Maybe FileLock -> StackT EnvConfig IO ())
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
    -> (Maybe FileLock -> StackT EnvConfig IO ())
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

    withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk0 -> do
      -- A local bit of state for communication between callbacks:
      curLk <- newIORef lk0
      let inner' lk =
            -- Locking policy:  This is only used for build commands, which
            -- only need to lock the snapshot, not the global lock.  We
            -- trade in the lock here.
            do dir <- installationRootDeps
               -- Hand-over-hand locking:
               withUserFileLock go dir $ \lk2 -> do
                 liftIO $ writeIORef curLk lk2
                 liftIO $ munlockFile lk
                 inner lk2

      let inner'' lk = do
              bconfig <- runStackLoggingTGlobal manager go $
                  lcLoadBuildConfig lc globalResolver globalCompiler
              envConfig <-
                 runStackTGlobal
                     manager bconfig go
                     (setupEnv Nothing)
              runStackTGlobal
                  manager
                  envConfig
                  go
                  (inner' lk)

      runStackTGlobal manager (lcConfig lc) go $
         Docker.reexecWithOptionalContainer (lcProjectRoot lc) mbefore (inner'' lk0) mafter
                                            (Just $ liftIO $
                                             do lk' <- readIORef curLk
                                                munlockFile lk')

cleanCmd :: CleanOpts -> GlobalOpts -> IO ()
cleanCmd opts go = withBuildConfigAndLock go (const (clean opts))

-- | Helper for build and install commands
buildCmd :: BuildOpts -> GlobalOpts -> IO ()
buildCmd opts go = do
  when (any (("-prof" `elem`) . either (const []) id . parseArgs Escaping) (boptsGhcOptions opts)) $ do
    hPutStrLn stderr "When building with stack, you should not use the -prof GHC option"
    hPutStrLn stderr "Instead, please use --library-profiling and --executable-profiling"
    hPutStrLn stderr "See: https://github.com/commercialhaskell/stack/issues/1015"
    error "-prof GHC option submitted"
  case boptsFileWatch opts of
    FileWatchPoll -> fileWatchPoll inner
    FileWatch -> fileWatch inner
    NoFileWatch -> inner $ const $ return ()
  where
    inner setLocalFiles = withBuildConfigAndLock go $ \lk ->
        Stack.Build.build setLocalFiles lk opts

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

upgradeCmd :: (Bool, String) -> GlobalOpts -> IO ()
upgradeCmd (fromGit, repo) go = withConfigAndLock go $
    upgrade (if fromGit then Just repo else Nothing) (globalResolver go)

-- | Upload to Hackage
uploadCmd :: ([String], Maybe PvpBounds, Bool) -> GlobalOpts -> IO ()
uploadCmd ([], _, _) _ = error "To upload the current package, please run 'stack upload .'"
uploadCmd (args, mpvpBounds, ignoreCheck) go = do
    let partitionM _ [] = return ([], [])
        partitionM f (x:xs) = do
            r <- f x
            (as, bs) <- partitionM f xs
            return $ if r then (x:as, bs) else (as, x:bs)
    (files, nonFiles) <- partitionM doesFileExist args
    (dirs, invalid) <- partitionM doesDirectoryExist nonFiles
    unless (null invalid) $ error $
        "stack upload expects a list sdist tarballs or cabal directories.  Can't find " ++
        show invalid
    let getUploader :: (HasStackRoot config, HasPlatform config, HasConfig config) => StackT config IO Upload.Uploader
        getUploader = do
            config <- asks getConfig
            manager <- asks envManager
            let uploadSettings =
                    Upload.setGetManager (return manager) Upload.defaultUploadSettings
            liftIO $ Upload.mkUploader config uploadSettings
    withBuildConfigAndLock go $ \_ -> do
        uploader <- getUploader
        unless ignoreCheck $
            mapM_ (parseRelAsAbsFile >=> checkSDistTarball) files
        liftIO $ forM_ files (canonicalizePath >=> Upload.upload uploader)
        unless (null dirs) $
            forM_ dirs $ \dir -> do
                pkgDir <- parseRelAsAbsDir dir
                (tarName, tarBytes) <- getSDistTarball mpvpBounds pkgDir
                unless ignoreCheck $ checkSDistTarball' tarName tarBytes
                liftIO $ Upload.uploadBytes uploader tarName tarBytes

sdistCmd :: ([String], Maybe PvpBounds, Bool) -> GlobalOpts -> IO ()
sdistCmd (dirs, mpvpBounds, ignoreCheck) go =
    withBuildConfig go $ do -- No locking needed.
        -- If no directories are specified, build all sdist tarballs.
        dirs' <- if null dirs
            then asks (Map.keys . envConfigPackages . getEnvConfig)
            else mapM (parseAbsDir <=< liftIO . canonicalizePath) dirs
        forM_ dirs' $ \dir -> do
            (tarName, tarBytes) <- getSDistTarball mpvpBounds dir
            distDir <- distDirFromDir dir
            tarPath <- (distDir </>) <$> parseRelFile tarName
            liftIO $ createTree $ parent tarPath
            liftIO $ L.writeFile (toFilePath tarPath) tarBytes
            unless ignoreCheck (checkSDistTarball tarPath)
            $logInfo $ "Wrote sdist tarball to " <> T.pack (toFilePath tarPath)

-- | Execute a command.
execCmd :: ExecOpts -> GlobalOpts -> IO ()
execCmd ExecOpts {..} go@GlobalOpts{..} =
    case eoExtra of
        ExecOptsPlain -> do
            (cmd, args) <- case (eoCmd, eoArgs) of
                 (ExecCmd cmd, args) -> return (cmd, args)
                 (ExecGhc, args) -> return ("ghc", args)
                 (ExecRunGhc, args) -> return ("runghc", args)
            (manager,lc) <- liftIO $ loadConfigWithOpts go
            withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk ->
             runStackTGlobal manager (lcConfig lc) go $
                Docker.reexecWithOptionalContainer
                    (lcProjectRoot lc)
                    -- Unlock before transferring control away, whether using docker or not:
                    (Just $ munlockFile lk)
                    (runStackTGlobal manager (lcConfig lc) go $
                        exec plainEnvSettings cmd args)
                    Nothing
                    Nothing -- Unlocked already above.
        ExecOptsEmbellished {..} ->
           withBuildConfigAndLock go $ \lk -> do
               (cmd, args) <- case (eoCmd, eoArgs) of
                   (ExecCmd cmd, args) -> return (cmd, args)
                   (ExecGhc, args) -> execCompiler "" args
                    -- NOTE: this won't currently work for GHCJS, because it doesn't have
                    -- a runghcjs binary. It probably will someday, though.
                   (ExecRunGhc, args) -> execCompiler "run" args
               let targets = concatMap words eoPackages
               unless (null targets) $
                   Stack.Build.build (const $ return ()) lk defaultBuildOpts
                       { boptsTargets = map T.pack targets
                       }
               munlockFile lk -- Unlock before transferring control away.
               exec eoEnvSettings cmd args
  where
    execCompiler cmdPrefix args = do
        wc <- getWhichCompiler
        let cmd = cmdPrefix ++ compilerExeName wc
        return (cmd, args)

-- | Evaluate some haskell code inline.
evalCmd :: EvalOpts -> GlobalOpts -> IO ()
evalCmd EvalOpts {..} go@GlobalOpts {..} = execCmd execOpts go
    where
      execOpts =
          ExecOpts { eoCmd = ExecGhc
                   , eoArgs = ["-e", evalArg]
                   , eoExtra = evalExtra
                   }

-- | Run GHCi in the context of a project.
ghciCmd :: GhciOpts -> GlobalOpts -> IO ()
ghciCmd ghciOpts go@GlobalOpts{..} =
  withBuildConfigAndLock go $ \lk -> do
    let packageTargets = concatMap words (ghciAdditionalPackages ghciOpts)
    unless (null packageTargets) $
       Stack.Build.build (const $ return ()) lk defaultBuildOpts
           { boptsTargets = map T.pack packageTargets
           }
    munlockFile lk -- Don't hold the lock while in the GHCI.
    ghci ghciOpts

-- | Run ide-backend in the context of a project.
ideCmd :: ([Text], [String]) -> GlobalOpts -> IO ()
ideCmd (targets,args) go@GlobalOpts{..} =
    withBuildConfig go $ -- No locking needed.
      ide targets args

-- | List packages in the project.
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

-- | List load targets for a package target.
targetsCmd :: Text -> GlobalOpts -> IO ()
targetsCmd target go@GlobalOpts{..} =
    withBuildConfig go $
    do let bopts = defaultBuildOpts { boptsTargets = [target] }
       (_realTargets,_,pkgs) <- ghciSetup bopts False Nothing
       pwd <- getWorkingDir
       targets <-
           fmap
               (concat . snd . unzip)
               (mapM (getPackageOptsAndTargetFiles pwd) pkgs)
       forM_ targets (liftIO . putStrLn)

-- | Pull the current Docker image.
dockerPullCmd :: () -> GlobalOpts -> IO ()
dockerPullCmd _ go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_ ->
     runStackTGlobal manager (lcConfig lc) go $
       Docker.preventInContainer Docker.pull

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> GlobalOpts -> IO ()
dockerResetCmd keepHome go@GlobalOpts{..} = do
    (manager,lc) <- liftIO (loadConfigWithOpts go)
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_ ->
     runStackLoggingTGlobal manager go $
        Docker.preventInContainer $ Docker.reset (lcProjectRoot lc) keepHome

-- | Cleanup Docker images and containers.
dockerCleanupCmd :: Docker.CleanupOpts -> GlobalOpts -> IO ()
dockerCleanupCmd cleanupOpts go@GlobalOpts{..} = do
    (manager,lc) <- liftIO $ loadConfigWithOpts go
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_ ->
     runStackTGlobal manager (lcConfig lc) go $
        Docker.preventInContainer $
            Docker.cleanup cleanupOpts

cfgSetCmd :: ConfigCmd.ConfigCmdSet -> GlobalOpts -> IO ()
cfgSetCmd co go@GlobalOpts{..} =
    withBuildConfigAndLock
        go
        (\_ -> do env <- ask
                  runReaderT
                      (cfgCmdSet co)
                      env)

imgDockerCmd :: Bool -> GlobalOpts -> IO ()
imgDockerCmd rebuild go@GlobalOpts{..} =
    withBuildConfigExt
        go
        Nothing
        (\lk ->
              do when rebuild $ Stack.Build.build
                         (const (return ()))
                         lk
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
    lc <- runStackLoggingTGlobal manager go $ do
        lc <- loadConfig globalConfigMonoid mstackYaml
        -- If we have been relaunched in a Docker container, perform in-container initialization
        -- (switch UID, etc.).  We do this after first loading the configuration since it must
        -- happen ASAP but needs a configuration.
        case globalDockerEntrypoint of
            Just de -> Docker.entrypoint (lcConfig lc) de
            Nothing -> return ()
        return lc
    return (manager,lc)

-- | Project initialization
initCmd :: InitOpts -> GlobalOpts -> IO ()
initCmd initOpts go =
    withConfigAndLock go $
    do pwd <- getWorkingDir
       config <- asks getConfig
       miniConfig <- loadMiniConfig config
       runReaderT (initProject pwd initOpts) miniConfig

-- | Create a project directory structure and initialize the stack config.
newCmd :: (NewOpts,InitOpts) -> GlobalOpts -> IO ()
newCmd (newOpts,initOpts) go@GlobalOpts{..} =
    withConfigAndLock go $
    do dir <- new newOpts
       config <- asks getConfig
       miniConfig <- loadMiniConfig config
       runReaderT (initProject dir initOpts) miniConfig

-- | List the available templates.
templatesCmd :: () -> GlobalOpts -> IO ()
templatesCmd _ go@GlobalOpts{..} = withConfigAndLock go listTemplates

-- | Fix up extra-deps for a project
solverCmd :: Bool -- ^ modify stack.yaml automatically?
          -> GlobalOpts
          -> IO ()
solverCmd fixStackYaml go =
    withBuildConfigAndLock go (\_ -> solveExtraDeps fixStackYaml)

-- | Visualize dependencies
dotCmd :: DotOpts -> GlobalOpts -> IO ()
dotCmd dotOpts go = withBuildConfigAndLock go (\_ -> dot dotOpts)

-- | List the dependencies
listDependenciesCmd :: Text -> GlobalOpts -> IO ()
listDependenciesCmd sep go = withBuildConfig go (listDependencies sep')
  where sep' = T.replace "\\t" "\t" (T.replace "\\n" "\n" sep)

-- | Query build information
queryCmd :: [String] -> GlobalOpts -> IO ()
queryCmd selectors go = withBuildConfig go $ queryBuildInfo $ map T.pack selectors

-- | Generate a combined HPC report
hpcReportCmd :: HpcReportOpts -> GlobalOpts -> IO ()
hpcReportCmd hropts go = withBuildConfig go $ generateHpcReportForTargets hropts

data MainException = InvalidReExecVersion String String
     deriving (Typeable)
instance Exception MainException
instance Show MainException where
    show (InvalidReExecVersion expected actual) = concat
        [ "When re-executing '"
        , stackProgName
        , "' in a container, the incorrect version was found\nExpected: "
        , expected
        , "; found: "
        , actual]
