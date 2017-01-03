{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Main stack tool entry point.

module Main (main) where

#ifndef HIDE_DEP_VERSIONS
import qualified Build_stack
#endif
import           Control.Exception
import           Control.Monad hiding (mapM, forM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (local)
import           Control.Monad.Trans.Either (EitherT)
import           Control.Monad.Writer.Lazy (Writer)
import           Data.Attoparsec.Args (parseArgs, EscapingMode (Escaping))
import           Data.Attoparsec.Interpreter (getInterpreterArgs)
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Data.Typeable (Typeable)
import           Data.Version (showVersion)
import           System.Process.Read
#ifdef USE_GIT_INFO
import           Development.GitRev (gitCommitCount, gitHash)
#endif
import           Distribution.System (buildArch, buildPlatform)
import           Distribution.Text (display)
import           GHC.IO.Encoding (mkTextEncoding, textEncodingName)
import           Lens.Micro
import           Options.Applicative
import           Options.Applicative.Help (errorHelp, stringChunk, vcatChunks)
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Complicated
#ifdef USE_GIT_INFO
import           Options.Applicative.Simple (simpleVersion)
#endif
import           Options.Applicative.Types (ParserHelp(..))
import           Path
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
import           Stack.GhcPkg (findGhcPkgField)
import qualified Stack.Nix as Nix
import           Stack.Fetch
import           Stack.FileWatch
import           Stack.Ghci
import           Stack.Hoogle
import qualified Stack.IDE as IDE
import qualified Stack.Image as Image
import           Stack.Init
import           Stack.New
import           Stack.Options.BuildParser
import           Stack.Options.CleanParser
import           Stack.Options.DockerParser
import           Stack.Options.DotParser
import           Stack.Options.ExecParser
import           Stack.Options.GhciParser
import           Stack.Options.GlobalParser
import           Stack.Options.HpcReportParser
import           Stack.Options.NewParser
import           Stack.Options.NixParser
import           Stack.Options.SolverParser
import           Stack.Options.Utils
import qualified Stack.PackageIndex
import qualified Stack.Path
import           Stack.Runners
import           Stack.SDist (getSDistTarball, checkSDistTarball, checkSDistTarball')
import           Stack.SetupCmd
import qualified Stack.Sig as Sig
import           Stack.Solver (solveExtraDeps)
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Compiler
import           Stack.Types.StackT
import           Stack.Upgrade
import qualified Stack.Upload as Upload
import qualified System.Directory as D
import           System.Environment (getProgName, getArgs, withArgs)
import           System.Exit
import           System.FilePath (pathSeparator)
import           System.IO (hIsTerminalDevice, stderr, stdin, stdout, hSetBuffering, BufferMode(..), hPutStrLn, Handle, hGetEncoding, hSetEncoding)

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

versionString' :: String
#ifdef USE_GIT_INFO
versionString' = concat $ concat
    [ [$(simpleVersion Meta.version)]
      -- Leave out number of commits for --depth=1 clone
      -- See https://github.com/commercialhaskell/stack/issues/792
    , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                          commitCount /= ("UNKNOWN" :: String)]
    , [" ", display buildArch]
    , [depsString]
    ]
  where
    commitCount = $gitCommitCount
#else
versionString' =
    showVersion Meta.version
    ++ ' ' : display buildArch
    ++ depsString
  where
#endif
#ifdef HIDE_DEP_VERSIONS
    depsString = " hpack-" ++ VERSION_hpack
#else
    depsString = "\nCompiled with:\n" ++ unlines (map ("- " ++) Build_stack.deps)
#endif

main :: IO ()
main = do
  -- Line buffer the output by default, particularly for non-terminal runs.
  -- See https://github.com/commercialhaskell/stack/pull/360
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr LineBuffering
  hSetTranslit stdout
  hSetTranslit stderr
  args <- getArgs
  progName <- getProgName
  isTerminal <- hIsTerminalDevice stdout
  execExtraHelp args
                Docker.dockerHelpOptName
                (dockerOptsParser False)
                ("Only showing --" ++ Docker.dockerCmdName ++ "* options.")
  execExtraHelp args
                Nix.nixHelpOptName
                (nixOptsParser False)
                ("Only showing --" ++ Nix.nixCmdName ++ "* options.")

  eGlobalRun <- try $ commandLineHandler progName False
  case eGlobalRun of
    Left (exitCode :: ExitCode) ->
      throwIO exitCode
    Right (globalMonoid,run) -> do
      let global = globalOptsFromMonoid isTerminal globalMonoid
      when (globalLogLevel global == LevelDebug) $ hPutStrLn stderr versionString'
      case globalReExecVersion global of
          Just expectVersion -> do
              expectVersion' <- parseVersionFromString expectVersion
              unless (checkVersion MatchMinor expectVersion' (fromCabalVersion Meta.version))
                  $ throwIO $ InvalidReExecVersion expectVersion (showVersion Meta.version)
          _ -> return ()
      run global `catch` \e ->
          -- This special handler stops "stack: " from being printed before the
          -- exception
          case fromException e of
              Just ec -> exitWith ec
              Nothing -> do
                  printExceptionStderr e
                  exitFailure

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
vcatErrorHelp (ParserHelp e1 _ _ _ _) (ParserHelp e2 h2 u2 b2 f2) =
  ParserHelp (vcatChunks [e2, e1]) h2 u2 b2 f2

commandLineHandler
  :: String
  -> Bool
  -> IO (GlobalOptsMonoid, GlobalOpts -> IO ())
commandLineHandler progName isInterpreter = complicatedOptions
  Meta.version
  (Just versionString')
  VERSION_hpack
  "stack - The Haskell Tool Stack"
  ""
  "stack's documentation is available at https://docs.haskellstack.org/"
  (globalOpts OuterGlobalOpts)
  (Just failureCallback)
  addCommands
  where
    failureCallback f args =
      case stripPrefix "Invalid argument" (fst (renderFailure f "")) of
          Just _ -> if isInterpreter
                    then parseResultHandler args f
                    else secondaryCommandHandler args f
                        >>= interpreterHandler args
          Nothing -> parseResultHandler args f

    parseResultHandler args f =
      if isInterpreter
      then do
        let hlp = errorHelp $ stringChunk
              (unwords ["Error executing interpreter command:"
                        , progName
                        , unwords args])
        handleParseResult (overFailure (vcatErrorHelp hlp) (Failure f))
      else handleParseResult (Failure f)

    addCommands = do
      unless isInterpreter (do
        addBuildCommand' "build"
                         "Build the package(s) in this directory/configuration"
                         buildCmd
                         (buildOptsParser Build)
        addBuildCommand' "install"
                         "Shortcut for 'build --copy-bins'"
                         buildCmd
                         (buildOptsParser Install)
        addCommand' "uninstall"
                    "DEPRECATED: This command performs no actions, and is present for documentation only"
                    uninstallCmd
                    (many $ strArgument $ metavar "IGNORED")
        addBuildCommand' "test"
                         "Shortcut for 'build --test'"
                         buildCmd
                         (buildOptsParser Test)
        addBuildCommand' "bench"
                         "Shortcut for 'build --bench'"
                         buildCmd
                         (buildOptsParser Bench)
        addBuildCommand' "haddock"
                         "Shortcut for 'build --haddock'"
                         buildCmd
                         (buildOptsParser Haddock)
        addCommand' "new"
                    "Create a new project from a template. Run `stack templates' to see available templates."
                    newCmd
                    newOptsParser
        addCommand' "templates"
                    "List the templates available for `stack new'."
                    templatesCmd
                    (pure ())
        addCommand' "init"
                    "Create stack project config from cabal or hpack package specifications"
                    initCmd
                    initOptsParser
        addCommand' "solver"
                    "Add missing extra-deps to stack project config"
                    solverCmd
                    solverOptsParser
        addCommand' "setup"
                    "Get the appropriate GHC for your project"
                    setupCmd
                    setupParser
        addCommand' "path"
                    "Print out handy path information"
                    pathCmd
                    Stack.Path.pathParser
        addCommand' "unpack"
                    "Unpack one or more packages locally"
                    unpackCmd
                    (some $ strArgument $ metavar "PACKAGE")
        addCommand' "update"
                    "Update the package index"
                    updateCmd
                    (pure ())
        addCommand' "upgrade"
                    "Upgrade to the latest stack"
                    upgradeCmd
                    upgradeOpts
        addCommand'
            "upload"
            "Upload a package to Hackage"
            uploadCmd
            ((,,,,) <$> many (strArgument $ metavar "TARBALL/DIR") <*>
             optional pvpBoundsOption <*>
             ignoreCheckSwitch <*>
             switch (long "no-signature" <> help "Do not sign & upload signatures") <*>
             strOption
             (long "sig-server" <> metavar "URL" <> showDefault <>
              value "https://sig.commercialhaskell.org" <>
              help "URL"))
        addCommand'
            "sdist"
            "Create source distribution tarballs"
            sdistCmd
            ((,,,,) <$> many (strArgument $ metavar "DIR") <*>
             optional pvpBoundsOption <*>
             ignoreCheckSwitch <*>
             switch (long "sign" <> help "Sign & upload signatures") <*>
             strOption
             (long "sig-server" <> metavar "URL" <> showDefault <>
              value "https://sig.commercialhaskell.org" <>
              help "URL"))
        addCommand' "dot"
                    "Visualize your project's dependency graph using Graphviz dot"
                    dotCmd
                    (dotOptsParser False) -- Default for --external is False.
        addCommand' "ghc"
                    "Run ghc"
                    execCmd
                    (execOptsParser $ Just ExecGhc)
        addCommand' "hoogle"
                    ("Run hoogle, the Haskell API search engine. Use 'stack exec' syntax " ++
                     "to pass Hoogle arguments, e.g. stack hoogle -- --count=20")
                    hoogleCmd
                    ((,,) <$> many (strArgument (metavar "ARG"))
                          <*> boolFlags
                                  True
                                  "setup"
                                  "If needed: install hoogle, build haddocks and generate a hoogle database"
                                  idm
                          <*> switch
                                  (long "rebuild" <>
                                   help "Rebuild the hoogle database"))
        )

      -- These are the only commands allowed in interpreter mode as well
      addCommand' "exec"
                  "Execute a command"
                  execCmd
                  (execOptsParser Nothing)
      addGhciCommand' "ghci"
                      "Run ghci in the context of package(s) (experimental)"
                      ghciCmd
                      ghciOptsParser
      addGhciCommand' "repl"
                      "Run ghci in the context of package(s) (experimental) (alias for 'ghci')"
                      ghciCmd
                      ghciOptsParser
      addCommand' "runghc"
                  "Run runghc"
                  execCmd
                  (execOptsParser $ Just ExecRunGhc)
      addCommand' "runhaskell"
                  "Run runghc (alias for 'runghc')"
                  execCmd
                  (execOptsParser $ Just ExecRunGhc)

      unless isInterpreter (do
        addCommand' "eval"
                    "Evaluate some haskell code inline. Shortcut for 'stack exec ghc -- -e CODE'"
                    evalCmd
                    (evalOptsParser "CODE")
        addCommand' "clean"
                    "Clean the local packages"
                    cleanCmd
                    cleanOptsParser
        addCommand' "list-dependencies"
                    "List the dependencies"
                    listDependenciesCmd
                    listDepsOptsParser
        addCommand' "query"
                    "Query general build information (experimental)"
                    queryCmd
                    (many $ strArgument $ metavar "SELECTOR...")
        addSubCommands'
            "ide"
            "IDE-specific commands"
            (do addCommand'
                    "packages"
                    "List all available local loadable packages"
                    idePackagesCmd
                    (pure ())
                addCommand'
                    "targets"
                    "List all available stack targets"
                    ideTargetsCmd
                    (pure ()))
        addSubCommands'
          Docker.dockerCmdName
          "Subcommands specific to Docker use"
          (do addCommand' Docker.dockerPullCmdName
                          "Pull latest version of Docker image from registry"
                          dockerPullCmd
                          (pure ())
              addCommand' "reset"
                          "Reset the Docker sandbox"
                          dockerResetCmd
                          (switch (long "keep-home" <>
                                   help "Do not delete sandbox's home directory"))
              addCommand' Docker.dockerCleanupCmdName
                          "Clean up Docker images and containers"
                          dockerCleanupCmd
                          dockerCleanupOptsParser)
        addSubCommands'
            ConfigCmd.cfgCmdName
            "Subcommands specific to modifying stack.yaml files"
            (addCommand' ConfigCmd.cfgCmdSetName
                        "Sets a field in the project's stack.yaml to value"
                        cfgSetCmd
                        configCmdSetParser)
        addSubCommands'
            Image.imgCmdName
            "Subcommands specific to imaging"
            (addCommand'
                 Image.imgDockerCmdName
                 "Build a Docker image for the project"
                 imgDockerCmd
                 ((,) <$>
                  boolFlags
                      True
                      "build"
                      "building the project before creating the container"
                      idm <*>
                  many
                      (textOption
                           (long "image" <>
                            help "A specific container image name to build"))))
        addSubCommands'
          "hpc"
          "Subcommands specific to Haskell Program Coverage"
          (addCommand' "report"
                        "Generate unified HPC coverage report from tix files and project targets"
                        hpcReportCmd
                        hpcReportOptsParser)
        )
      where
        ignoreCheckSwitch =
            switch (long "ignore-check"
                    <> help "Do not check package for common mistakes")

        -- addCommand hiding global options
        addCommand' :: String -> String -> (a -> GlobalOpts -> IO ()) -> Parser a
                    -> AddCommand
        addCommand' cmd title constr =
            addCommand cmd title globalFooter constr (globalOpts OtherCmdGlobalOpts)

        addSubCommands' :: String -> String -> AddCommand
                        -> AddCommand
        addSubCommands' cmd title =
            addSubCommands cmd title globalFooter (globalOpts OtherCmdGlobalOpts)

        -- Additional helper that hides global options and shows build options
        addBuildCommand' :: String -> String -> (a -> GlobalOpts -> IO ()) -> Parser a
                         -> AddCommand
        addBuildCommand' cmd title constr =
            addCommand cmd title globalFooter constr (globalOpts BuildCmdGlobalOpts)

        -- Additional helper that hides global options and shows some ghci options
        addGhciCommand' :: String -> String -> (a -> GlobalOpts -> IO ()) -> Parser a
                         -> AddCommand
        addGhciCommand' cmd title constr =
            addCommand cmd title globalFooter constr (globalOpts GhciCmdGlobalOpts)

    globalOpts :: GlobalOptsContext -> Parser GlobalOptsMonoid
    globalOpts kind =
        extraHelpOption hide progName (Docker.dockerCmdName ++ "*") Docker.dockerHelpOptName <*>
        extraHelpOption hide progName (Nix.nixCmdName ++ "*") Nix.nixHelpOptName <*>
        globalOptsParser kind (if isInterpreter
                                then Just $ LevelOther "silent"
                                else Nothing)
        where hide = kind /= OuterGlobalOpts

    globalFooter = "Run 'stack --help' for global options that apply to all subcommands."

type AddCommand =
    EitherT (GlobalOpts -> IO ()) (Writer (Mod CommandFields (GlobalOpts -> IO (), GlobalOptsMonoid))) ()

-- | fall-through to external executables in `git` style if they exist
-- (i.e. `stack something` looks for `stack-something` before
-- failing with "Invalid argument `something'")
secondaryCommandHandler
  :: [String]
  -> ParserFailure ParserHelp
  -> IO (ParserFailure ParserHelp)
secondaryCommandHandler args f =
    -- don't even try when the argument looks like a path or flag
    if elem pathSeparator cmd || "-" `isPrefixOf` head args
       then return f
    else do
      mExternalExec <- D.findExecutable cmd
      case mExternalExec of
        Just ex -> do
          menv <- getEnvOverride buildPlatform
          -- TODO show the command in verbose mode
          -- hPutStrLn stderr $ unwords $
          --   ["Running", "[" ++ ex, unwords (tail args) ++ "]"]
          _ <- runNoLoggingT (exec menv ex (tail args))
          return f
        Nothing -> return $ fmap (vcatErrorHelp (noSuchCmd cmd)) f
  where
    -- FIXME this is broken when any options are specified before the command
    -- e.g. stack --verbosity silent cmd
    cmd = stackProgName ++ "-" ++ head args
    noSuchCmd name = errorHelp $ stringChunk
      ("Auxiliary command not found in path `" ++ name ++ "'")

interpreterHandler
  :: Monoid t
  => [String]
  -> ParserFailure ParserHelp
  -> IO (GlobalOptsMonoid, (GlobalOpts -> IO (), t))
interpreterHandler args f = do
  -- args can include top-level config such as --extra-lib-dirs=... (set by
  -- nix-shell) - we need to find the first argument which is a file, everything
  -- afterwards is an argument to the script, everything before is an argument
  -- to Stack
  (stackArgs, fileArgs) <- spanM (fmap not . D.doesFileExist) args
  case fileArgs of
    (file:fileArgs') -> runInterpreterCommand file stackArgs fileArgs'
    [] -> parseResultHandler (errorCombine (noSuchFile firstArg))
  where
    firstArg = head args

    spanM _ [] = return ([], [])
    spanM p xs@(x:xs') = do
      r <- p x
      if r
      then do
        (ys, zs) <- spanM p xs'
        return (x:ys, zs)
      else
        return ([], xs)

    -- if the first argument contains a path separator then it might be a file,
    -- or a Stack option referencing a file. In that case we only show the
    -- interpreter error message and exclude the command related error messages.
    errorCombine =
      if pathSeparator `elem` firstArg
      then overrideErrorHelp
      else vcatErrorHelp

    overrideErrorHelp (ParserHelp e1 _ _ _ _) (ParserHelp _ h2 u2 b2 f2) =
      ParserHelp e1 h2 u2 b2 f2

    parseResultHandler fn = handleParseResult (overFailure fn (Failure f))
    noSuchFile name = errorHelp $ stringChunk
      ("File does not exist or is not a regular file `" ++ name ++ "'")

    runInterpreterCommand path stackArgs fileArgs = do
      progName <- getProgName
      iargs <- getInterpreterArgs path
      let parseCmdLine = commandLineHandler progName True
          separator = if "--" `elem` iargs then [] else ["--"]
          cmdArgs = stackArgs ++ iargs ++ separator ++ path : fileArgs
       -- TODO show the command in verbose mode
       -- hPutStrLn stderr $ unwords $
       --   ["Running", "[" ++ progName, unwords cmdArgs ++ "]"]
      (a,b) <- withArgs cmdArgs parseCmdLine
      return (a,(b,mempty))

pathCmd :: [Text] -> GlobalOpts -> IO ()
pathCmd keys go = withBuildConfig go (Stack.Path.path keys)

setupCmd :: SetupCmdOpts -> GlobalOpts -> IO ()
setupCmd sco@SetupCmdOpts{..} go@GlobalOpts{..} = do
  lc <- loadConfigWithOpts go
  withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk -> do
    let getCompilerVersion = loadCompilerVersion go lc
    runStackTGlobal (lcConfig lc) go $
      Docker.reexecWithOptionalContainer
          (lcProjectRoot lc)
          Nothing
          (runStackTGlobal (lcConfig lc) go $
           Nix.reexecWithOptionalShell (lcProjectRoot lc) getCompilerVersion $
           runStackTGlobal () go $ do
              (wantedCompiler, compilerCheck, mstack) <-
                  case scoCompilerVersion of
                      Just v -> return (v, MatchMinor, Nothing)
                      Nothing -> do
                          bc <- lcLoadBuildConfig lc globalCompiler
                          return ( view wantedCompilerVersionL bc
                                 , configCompilerCheck (lcConfig lc)
                                 , Just $ view stackYamlL bc
                                 )
              miniConfig <- loadMiniConfig (lcConfig lc)
              runStackTGlobal miniConfig go $
                  setup sco wantedCompiler compilerCheck mstack
              )
          Nothing
          (Just $ munlockFile lk)

cleanCmd :: CleanOpts -> GlobalOpts -> IO ()
cleanCmd opts go = withBuildConfigAndLock go (const (clean opts))

-- | Helper for build and install commands
buildCmd :: BuildOptsCLI -> GlobalOpts -> IO ()
buildCmd opts go = do
  when (any (("-prof" `elem`) . either (const []) id . parseArgs Escaping) (boptsCLIGhcOptions opts)) $ do
    hPutStrLn stderr "When building with stack, you should not use the -prof GHC option"
    hPutStrLn stderr "Instead, please use --library-profiling and --executable-profiling"
    hPutStrLn stderr "See: https://github.com/commercialhaskell/stack/issues/1015"
    error "-prof GHC option submitted"
  case boptsCLIFileWatch opts of
    FileWatchPoll -> fileWatchPoll stderr inner
    FileWatch -> fileWatch stderr inner
    NoFileWatch -> inner $ const $ return ()
  where
    inner setLocalFiles = withBuildConfigAndLock go' $ \lk ->
        Stack.Build.build setLocalFiles lk opts
    -- Read the build command from the CLI and enable it to run
    go' = case boptsCLICommand opts of
               Test -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidTestsL) (Just True) go
               Haddock -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidHaddockL) (Just True) go
               Bench -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidBenchmarksL) (Just True) go
               Install -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidInstallExesL) (Just True) go
               Build -> go -- Default case is just Build

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

upgradeCmd :: UpgradeOpts -> GlobalOpts -> IO ()
upgradeCmd upgradeOpts' go = withGlobalConfigAndLock go $
    upgrade (globalConfigMonoid go)
            (globalResolver go)
#ifdef USE_GIT_INFO
            (find (/= "UNKNOWN") [$gitHash])
#else
            Nothing
#endif
            upgradeOpts'

-- | Upload to Hackage
uploadCmd :: ([String], Maybe PvpBounds, Bool, Bool, String) -> GlobalOpts -> IO ()
uploadCmd ([], _, _, _, _) _ = error "To upload the current package, please run 'stack upload .'"
uploadCmd (args, mpvpBounds, ignoreCheck, don'tSign, sigServerUrl) go = do
    let partitionM _ [] = return ([], [])
        partitionM f (x:xs) = do
            r <- f x
            (as, bs) <- partitionM f xs
            return $ if r then (x:as, bs) else (as, x:bs)
    (files, nonFiles) <- partitionM D.doesFileExist args
    (dirs, invalid) <- partitionM D.doesDirectoryExist nonFiles
    unless (null invalid) $ error $
        "stack upload expects a list sdist tarballs or cabal directories.  Can't find " ++
        show invalid
    let getUploader :: (HasConfig config) => StackT config IO Upload.Uploader
        getUploader = do
            config <- view configL
            liftIO $ Upload.mkUploader config Upload.defaultUploadSettings
    withBuildConfigAndLock go $ \_ -> do
        uploader <- getUploader
        unless ignoreCheck $
            mapM_ (resolveFile' >=> checkSDistTarball) files
        forM_
            files
            (\file ->
                  do tarFile <- resolveFile' file
                     liftIO
                         (Upload.upload uploader (toFilePath tarFile))
                     unless
                         don'tSign
                         (void $
                          Sig.sign
                              sigServerUrl
                              tarFile))
        unless (null dirs) $
            forM_ dirs $ \dir -> do
                pkgDir <- resolveDir' dir
                (tarName, tarBytes) <- getSDistTarball mpvpBounds pkgDir
                unless ignoreCheck $ checkSDistTarball' tarName tarBytes
                liftIO $ Upload.uploadBytes uploader tarName tarBytes
                tarPath <- parseRelFile tarName
                unless
                    don'tSign
                    (void $
                     Sig.signTarBytes
                         sigServerUrl
                         tarPath
                         tarBytes)

sdistCmd :: ([String], Maybe PvpBounds, Bool, Bool, String) -> GlobalOpts -> IO ()
sdistCmd (dirs, mpvpBounds, ignoreCheck, sign, sigServerUrl) go =
    withBuildConfig go $ do -- No locking needed.
        -- If no directories are specified, build all sdist tarballs.
        dirs' <- if null dirs
            then liftM Map.keys getLocalPackages
            else mapM resolveDir' dirs
        forM_ dirs' $ \dir -> do
            (tarName, tarBytes) <- getSDistTarball mpvpBounds dir
            distDir <- distDirFromDir dir
            tarPath <- (distDir </>) <$> parseRelFile tarName
            ensureDir (parent tarPath)
            liftIO $ L.writeFile (toFilePath tarPath) tarBytes
            unless ignoreCheck (checkSDistTarball tarPath)
            $logInfo $ "Wrote sdist tarball to " <> T.pack (toFilePath tarPath)
            when sign (void $ Sig.sign sigServerUrl tarPath)

-- | Execute a command.
execCmd :: ExecOpts -> GlobalOpts -> IO ()
execCmd ExecOpts {..} go@GlobalOpts{..} =
    case eoExtra of
        ExecOptsPlain -> do
            (cmd, args) <- case (eoCmd, eoArgs) of
                 (ExecCmd cmd, args) -> return (cmd, args)
                 (ExecGhc, args) -> return ("ghc", args)
                 (ExecRunGhc, args) -> return ("runghc", args)
            lc <- liftIO $ loadConfigWithOpts go
            withUserFileLock go (configStackRoot $ lcConfig lc) $ \lk -> do
              let getCompilerVersion = loadCompilerVersion go lc
              runStackTGlobal (lcConfig lc) go $
                Docker.reexecWithOptionalContainer
                    (lcProjectRoot lc)
                    -- Unlock before transferring control away, whether using docker or not:
                    (Just $ munlockFile lk)
                    (runStackTGlobal (lcConfig lc) go $ do
                        config <- view configL
                        menv <- liftIO $ configEnvOverride config plainEnvSettings
                        Nix.reexecWithOptionalShell
                            (lcProjectRoot lc)
                            getCompilerVersion
                            (runStackTGlobal (lcConfig lc) go $
                                exec menv cmd args))
                    Nothing
                    Nothing -- Unlocked already above.
        ExecOptsEmbellished {..} ->
           withBuildConfigAndLock go $ \lk -> do
               let targets = concatMap words eoPackages
               unless (null targets) $
                   Stack.Build.build (const $ return ()) lk defaultBuildOptsCLI
                       { boptsCLITargets = map T.pack targets
                       }

               config <- view configL
               menv <- liftIO $ configEnvOverride config eoEnvSettings
               (cmd, args) <- case (eoCmd, eoArgs) of
                   (ExecCmd cmd, args) -> return (cmd, args)
                   (ExecGhc, args) -> getGhcCmd "" menv eoPackages args
                    -- NOTE: this won't currently work for GHCJS, because it doesn't have
                    -- a runghcjs binary. It probably will someday, though.
                   (ExecRunGhc, args) ->
                       getGhcCmd "run" menv eoPackages args
               munlockFile lk -- Unlock before transferring control away.
               exec menv cmd args
  where
      -- return the package-id of the first package in GHC_PACKAGE_PATH
      getPkgId menv wc name = do
          mId <- findGhcPkgField menv wc [] name "id"
          case mId of
              Just i -> return (head $ words (T.unpack i))
              -- should never happen as we have already installed the packages
              _      -> error ("Could not find package id of package " ++ name)

      getPkgOpts menv wc pkgs = do
          ids <- mapM (getPkgId menv wc) pkgs
          return $ map ("-package-id=" ++) ids

      getGhcCmd prefix menv pkgs args = do
          wc <- view $ actualCompilerVersionL.whichCompilerL
          pkgopts <- getPkgOpts menv wc pkgs
          return (prefix ++ compilerExeName wc, pkgopts ++ args)

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
    munlockFile lk -- Don't hold the lock while in the GHCI.
    bopts <- view buildOptsL
    -- override env so running of tests and benchmarks is disabled
    let boptsLocal = bopts
               { boptsTestOpts = (boptsTestOpts bopts) { toDisableRun = True }
               , boptsBenchmarkOpts = (boptsBenchmarkOpts bopts) { beoDisableRun = True }
               }
    local (set buildOptsL boptsLocal)
          (ghci ghciOpts)

-- | List packages in the project.
idePackagesCmd :: () -> GlobalOpts -> IO ()
idePackagesCmd () go =
    withBuildConfig go IDE.listPackages

-- | List targets in the project.
ideTargetsCmd :: () -> GlobalOpts -> IO ()
ideTargetsCmd () go =
    withBuildConfig go IDE.listTargets

-- | Pull the current Docker image.
dockerPullCmd :: () -> GlobalOpts -> IO ()
dockerPullCmd _ go@GlobalOpts{..} = do
    lc <- liftIO $ loadConfigWithOpts go
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_ ->
     runStackTGlobal (lcConfig lc) go $
       Docker.preventInContainer Docker.pull

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> GlobalOpts -> IO ()
dockerResetCmd keepHome go@GlobalOpts{..} = do
    lc <- liftIO (loadConfigWithOpts go)
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_ ->
      runStackTGlobal (lcConfig lc) go $
        Docker.preventInContainer $ Docker.reset (lcProjectRoot lc) keepHome

-- | Cleanup Docker images and containers.
dockerCleanupCmd :: Docker.CleanupOpts -> GlobalOpts -> IO ()
dockerCleanupCmd cleanupOpts go@GlobalOpts{..} = do
    lc <- liftIO $ loadConfigWithOpts go
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (configStackRoot $ lcConfig lc) $ \_ ->
     runStackTGlobal (lcConfig lc) go $
        Docker.preventInContainer $
            Docker.cleanup cleanupOpts

cfgSetCmd :: ConfigCmd.ConfigCmdSet -> GlobalOpts -> IO ()
cfgSetCmd co go@GlobalOpts{..} =
    withMiniConfigAndLock
        go
        (cfgCmdSet co)

imgDockerCmd :: (Bool, [Text]) -> GlobalOpts -> IO ()
imgDockerCmd (rebuild,images) go@GlobalOpts{..} = do
    mProjectRoot <- lcProjectRoot <$> loadConfigWithOpts go
    withBuildConfigExt
        go
        Nothing
        (\lk ->
              do when rebuild $
                     Stack.Build.build
                         (const (return ()))
                         lk
                         defaultBuildOptsCLI
                 Image.stageContainerImageArtifacts mProjectRoot images)
        (Just $ Image.createContainerImageFromStage mProjectRoot images)

-- | Project initialization
initCmd :: InitOpts -> GlobalOpts -> IO ()
initCmd initOpts go = do
    pwd <- getCurrentDir
    withMiniConfigAndLock go (initProject IsInitCmd pwd initOpts (globalResolver go))

-- | Create a project directory structure and initialize the stack config.
newCmd :: (NewOpts,InitOpts) -> GlobalOpts -> IO ()
newCmd (newOpts,initOpts) go@GlobalOpts{..} =
    withMiniConfigAndLock go $ do
        dir <- new newOpts (forceOverwrite initOpts)
        initProject IsNewCmd dir initOpts globalResolver

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
dotCmd dotOpts go = withBuildConfigDot dotOpts go $ dot dotOpts

-- | List the dependencies
listDependenciesCmd :: ListDepsOpts -> GlobalOpts -> IO ()
listDependenciesCmd opts go = withBuildConfigDot (listDepsDotOpts opts) go $ listDependencies opts

-- Plumbing for --test and --bench flags
withBuildConfigDot :: DotOpts -> GlobalOpts -> StackT EnvConfig IO () -> IO ()
withBuildConfigDot opts go f = withBuildConfig go' f
  where
    go' =
        (if dotTestTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidTestsL) (Just True) else id) $
        (if dotBenchTargets opts then set (globalOptsBuildOptsMonoidL.buildOptsMonoidBenchmarksL) (Just True) else id)
        go

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
