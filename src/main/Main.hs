{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

#ifdef USE_GIT_INFO
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | Main stack tool entry point.

module Main (main) where

#ifndef HIDE_DEP_VERSIONS
import qualified Build_stack
#endif
import           Stack.Prelude hiding (Display (..))
import           Control.Monad.Reader (local)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Writer.Lazy (Writer)
import           Data.Attoparsec.Args (parseArgs, EscapingMode (Escaping))
import           Data.Attoparsec.Interpreter (getInterpreterArgs)
import qualified Data.ByteString.Lazy as L
import           Data.IORef.RunOnce (runOnce)
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Version (showVersion)
import           RIO.Process
#ifdef USE_GIT_INFO
import           GitHash (giCommitCount, giHash, tGitInfoCwd)
#endif
import           Distribution.System (buildArch)
import qualified Distribution.Text as Cabal (display)
import           Distribution.Version (mkVersion')
import           GHC.IO.Encoding (mkTextEncoding, textEncodingName)
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
import           Stack.Build
import           Stack.Clean (CleanOpts(..), clean)
import           Stack.Config
import           Stack.ConfigCmd as ConfigCmd
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Coverage
import           Stack.DefaultColorWhen (defaultColorWhen)
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.GhcPkg (findGhcPkgField)
import qualified Stack.Nix as Nix
import           Stack.Fetch
import           Stack.FileWatch
import           Stack.Ghci
import           Stack.Hoogle
import           Stack.Ls
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
import           Stack.Options.ScriptParser
import           Stack.Options.SDistParser
import           Stack.Options.SolverParser
import           Stack.Options.Utils
import qualified Stack.PackageIndex
import qualified Stack.Path
import           Stack.PrettyPrint
import           Stack.Runners
import           Stack.Script
import           Stack.SDist (getSDistTarball, checkSDistTarball, checkSDistTarball', SDistOpts(..))
import           Stack.SetupCmd
import qualified Stack.Sig as Sig
import           Stack.Snapshot (loadResolver)
import           Stack.Solver (solveExtraDeps)
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Compiler
import           Stack.Types.NamedComponent
import           Stack.Types.Nix
import           Stack.Upgrade
import qualified Stack.Upload as Upload
import qualified System.Directory as D
import           System.Environment (getProgName, getArgs, withArgs)
import           System.Exit
import           System.FilePath (isValid, pathSeparator)
import qualified System.FilePath as FP
import           System.IO (stderr, stdin, stdout, BufferMode(..), hPutStrLn, hPrint, hGetEncoding, hSetEncoding)

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
    , [" (" ++ show commitCount ++ " commits)" | commitCount /= 1]
    , [" ", Cabal.display buildArch]
    , [depsString, warningString]
    ]
  where
    commitCount = giCommitCount $$tGitInfoCwd
#else
versionString' =
    showVersion Meta.version
    ++ ' ' : Cabal.display buildArch
    ++ depsString
    ++ warningString
  where
#endif
#ifdef HIDE_DEP_VERSIONS
    depsString = " hpack-" ++ VERSION_hpack
#else
    depsString = "\nCompiled with:\n" ++ unlines (map ("- " ++) Build_stack.deps)
#endif
#ifdef SUPPORTED_BUILD
    warningString = ""
#else
    warningString = unlines
      [ ""
      , "Warning: this is an unsupported build that may use different versions of"
      , "dependencies and GHC than the officially released binaries, and therefore may"
      , "not behave identically.  If you encounter problems, please try the latest"
      , "official build by running 'stack upgrade --force-download'."
      ]
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
  isTerminal <- hIsTerminalDeviceOrMinTTY stdout
  -- On Windows, where applicable, defaultColorWhen has the side effect of
  -- enabling ANSI for ANSI-capable native (ConHost) terminals, if not already
  -- ANSI-enabled.
  defColorWhen <- defaultColorWhen
  execExtraHelp args
                Docker.dockerHelpOptName
                (dockerOptsParser False)
                ("Only showing --" ++ Docker.dockerCmdName ++ "* options.")
  execExtraHelp args
                Nix.nixHelpOptName
                (nixOptsParser False)
                ("Only showing --" ++ Nix.nixCmdName ++ "* options.")

  currentDir <- D.getCurrentDirectory
  eGlobalRun <- try $ commandLineHandler currentDir progName False
  case eGlobalRun of
    Left (exitCode :: ExitCode) ->
      throwIO exitCode
    Right (globalMonoid,run) -> do
      let global = globalOptsFromMonoid isTerminal defColorWhen globalMonoid
      when (globalLogLevel global == LevelDebug) $ hPutStrLn stderr versionString'
      case globalReExecVersion global of
          Just expectVersion -> do
              expectVersion' <- parseVersionFromString expectVersion
              unless (checkVersion MatchMinor expectVersion' (fromCabalVersion (mkVersion' Meta.version)))
                  $ throwIO $ InvalidReExecVersion expectVersion (showVersion Meta.version)
          _ -> return ()
      run global `catch` \e ->
          -- This special handler stops "stack: " from being printed before the
          -- exception
          case fromException e of
              Just ec -> exitWith ec
              Nothing -> do
                  hPrint stderr e
                  exitFailure

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }

commandLineHandler
  :: FilePath
  -> String
  -> Bool
  -> IO (GlobalOptsMonoid, GlobalOpts -> IO ())
commandLineHandler currentDir progName isInterpreter = complicatedOptions
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
                        >>= interpreterHandler currentDir args
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
         (unwords [ "Create a new project from a template."
                  , "Run `stack templates' to see available templates."
                  , "Note: you can also specify a local file or a"
                  , "remote URL as a template."
                  ] )
                    newCmd
                    newOptsParser
        addCommand' "templates"
         (unwords [ "List the templates available for `stack new'."
                  , "Templates are drawn from"
                  , "https://github.com/commercialhaskell/stack-templates"
                  , "Note: `stack new' can also accept a template from a"
                  , "local file or a remote URL."
                  ] )
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
        addCommand' "ls"
                    "List command. (Supports snapshots and dependencies)"
                    lsCmd
                    lsParser
        addCommand' "unpack"
                    "Unpack one or more packages locally"
                    unpackCmd
                    ((,) <$> some (strArgument $ metavar "PACKAGE")
                         <*> optional (textOption $ long "to" <>
                                         help "Optional path to unpack the package into (will unpack into subdirectory)"))
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
            (sdistOptsParser True)
        addCommand'
            "sdist"
            "Create source distribution tarballs"
            sdistCmd
            (sdistOptsParser False)
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
                    ((,,,) <$> many (strArgument (metavar "ARG"))
                          <*> boolFlags
                                  True
                                  "setup"
                                  "If needed: install hoogle, build haddocks and generate a hoogle database"
                                  idm
                          <*> switch
                                  (long "rebuild" <>
                                   help "Rebuild the hoogle database")
                          <*> switch
                                  (long "server" <>
                                   help "Start local Hoogle server"))
        )

      -- These are the only commands allowed in interpreter mode as well
      addCommand' "exec"
                  "Execute a command"
                  execCmd
                  (execOptsParser Nothing)
      addCommand' "run"
                  "Build and run an executable. Defaults to the first available executable if none is provided as the first argument."
                  execCmd
                  (execOptsParser $ Just ExecRun)
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
      addCommand' "script"
                  "Run a Stack Script"
                  scriptCmd
                  scriptOptsParser

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
                    (listDependenciesCmd True)
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
        globalOptsParser currentDir kind
            (if isInterpreter
                -- Silent except when errors occur - see #2879
                then Just LevelError
                else Nothing)
        where hide = kind /= OuterGlobalOpts

    globalFooter = "Run 'stack --help' for global options that apply to all subcommands."

type AddCommand =
    ExceptT (GlobalOpts -> IO ()) (Writer (Mod CommandFields (GlobalOpts -> IO (), GlobalOptsMonoid))) ()

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
        Just ex -> withProcessContextNoLogging $ do
          -- TODO show the command in verbose mode
          -- hPutStrLn stderr $ unwords $
          --   ["Running", "[" ++ ex, unwords (tail args) ++ "]"]
          _ <- exec ex (tail args)
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
  => FilePath
  -> [String]
  -> ParserFailure ParserHelp
  -> IO (GlobalOptsMonoid, (GlobalOpts -> IO (), t))
interpreterHandler currentDir args f = do
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

    overrideErrorHelp h1 h2 = h2 { helpError = helpError h1 }

    parseResultHandler fn = handleParseResult (overFailure fn (Failure f))
    noSuchFile name = errorHelp $ stringChunk
      ("File does not exist or is not a regular file `" ++ name ++ "'")

    runInterpreterCommand path stackArgs fileArgs = do
      progName <- getProgName
      iargs <- getInterpreterArgs path
      let parseCmdLine = commandLineHandler currentDir progName True
          -- Implicit file arguments are put before other arguments that
          -- occur after "--". See #3658
          cmdArgs = stackArgs ++ case break (== "--") iargs of
            (beforeSep, []) -> beforeSep ++ ["--"] ++ [path] ++ fileArgs
            (beforeSep, optSep : afterSep) ->
              beforeSep ++ [optSep] ++ [path] ++ fileArgs ++ afterSep
       -- TODO show the command in verbose mode
       -- hPutStrLn stderr $ unwords $
       --   ["Running", "[" ++ progName, unwords cmdArgs ++ "]"]
      (a,b) <- withArgs cmdArgs parseCmdLine
      return (a,(b,mempty))

pathCmd :: [Text] -> GlobalOpts -> IO ()
pathCmd keys go = withBuildConfig go (Stack.Path.path keys)

setupCmd :: SetupCmdOpts -> GlobalOpts -> IO ()
setupCmd sco@SetupCmdOpts{..} go@GlobalOpts{..} = loadConfigWithOpts go $ \lc -> do
  when (isJust scoUpgradeCabal && nixEnable (configNix (lcConfig lc))) $ do
    throwIO UpgradeCabalUnusable
  withUserFileLock go (view stackRootL lc) $ \lk -> do
    let getCompilerVersion = loadCompilerVersion go lc
    runRIO (lcConfig lc) $
      Docker.reexecWithOptionalContainer
          (lcProjectRoot lc)
          Nothing
          (runRIO (lcConfig lc) $
           Nix.reexecWithOptionalShell (lcProjectRoot lc) getCompilerVersion $ do
           (wantedCompiler, compilerCheck, mstack) <-
               case scoCompilerVersion of
                   Just v -> return (v, MatchMinor, Nothing)
                   Nothing -> do
                       bc <- liftIO $ lcLoadBuildConfig lc globalCompiler
                       return ( view wantedCompilerVersionL bc
                              , configCompilerCheck (lcConfig lc)
                              , Just $ view stackYamlL bc
                              )
           runRIO (loadMiniConfig (lcConfig lc)) $ setup sco wantedCompiler compilerCheck mstack
           )
          Nothing
          (Just $ munlockFile lk)

cleanCmd :: CleanOpts -> GlobalOpts -> IO ()
cleanCmd opts go =
  -- See issues #2010 and #3468 for why "stack clean --full" is not used
  -- within docker.
  case opts of
    CleanFull{} -> withBuildConfigAndLockNoDocker go (const (clean opts))
    CleanShallow{} -> withBuildConfigAndLock go (const (clean opts))

-- | Helper for build and install commands
buildCmd :: BuildOptsCLI -> GlobalOpts -> IO ()
buildCmd opts go = do
  when (any (("-prof" `elem`) . either (const []) id . parseArgs Escaping) (boptsCLIGhcOptions opts)) $ do
    hPutStrLn stderr "Error: When building with stack, you should not use the -prof GHC option"
    hPutStrLn stderr "Instead, please use --library-profiling and --executable-profiling"
    hPutStrLn stderr "See: https://github.com/commercialhaskell/stack/issues/1015"
    exitFailure
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
uninstallCmd _ go = withConfigAndLock go $
    prettyErrorL
      [ flow "stack does not manage installations in global locations."
      , flow "The only global mutation stack performs is executable copying."
      , flow "For the default executable destination, please run"
      , styleShell "stack path --local-bin"
      ]

-- | Unpack packages to the filesystem
unpackCmd :: ([String], Maybe Text) -> GlobalOpts -> IO ()
unpackCmd (names, Nothing) go = unpackCmd (names, Just ".") go
unpackCmd (names, Just dstPath) go = withConfigAndLock go $ do
    mSnapshotDef <- mapM (makeConcreteResolver Nothing >=> loadResolver) (globalResolver go)
    Stack.Fetch.unpackPackages mSnapshotDef (T.unpack dstPath) names

-- | Update the package index
updateCmd :: () -> GlobalOpts -> IO ()
updateCmd () go = withConfigAndLock go Stack.PackageIndex.updateAllIndices

upgradeCmd :: UpgradeOpts -> GlobalOpts -> IO ()
upgradeCmd upgradeOpts' go = withGlobalConfigAndLock go $
    upgrade (globalConfigMonoid go)
            (globalResolver go)
#ifdef USE_GIT_INFO
            (Just (giHash $$tGitInfoCwd))
#else
            Nothing
#endif
            upgradeOpts'

-- | Upload to Hackage
uploadCmd :: SDistOpts -> GlobalOpts -> IO ()
uploadCmd (SDistOpts [] _ _ _ _ _ _) go =
    withConfigAndLock go . prettyErrorL $
        [ flow "To upload the current package, please run"
        , styleShell "stack upload ."
        , flow "(with the period at the end)"
        ]
uploadCmd sdistOpts go = do
    let partitionM _ [] = return ([], [])
        partitionM f (x:xs) = do
            r <- f x
            (as, bs) <- partitionM f xs
            return $ if r then (x:as, bs) else (as, x:bs)
    (files, nonFiles) <- partitionM D.doesFileExist (sdoptsDirsToWorkWith sdistOpts)
    (dirs, invalid) <- partitionM D.doesDirectoryExist nonFiles
    withBuildConfigAndLock go $ \_ -> do
        unless (null invalid) $ do
            let invalidList = bulletedList $ map (styleFile . fromString) invalid
            prettyErrorL
                [ styleShell "stack upload"
                , flow "expects a list of sdist tarballs or package directories."
                , flow "Can't find:"
                , line <> invalidList
                ]
            liftIO exitFailure
        when (null files && null dirs) $ do
            prettyErrorL
                [ styleShell "stack upload"
                , flow "expects a list of sdist tarballs or package directories, but none were specified."
                ]
            liftIO exitFailure
        config <- view configL
        getCreds <- liftIO (runOnce (Upload.loadCreds config))
        mapM_ (resolveFile' >=> checkSDistTarball sdistOpts) files
        forM_
            files
            (\file ->
                  do tarFile <- resolveFile' file
                     liftIO $ do
                       creds <- getCreds
                       Upload.upload creds (toFilePath tarFile)
                     when
                         (sdoptsSign sdistOpts)
                         (void $
                          Sig.sign
                              (sdoptsSignServerUrl sdistOpts)
                              tarFile))
        unless (null dirs) $
            forM_ dirs $ \dir -> do
                pkgDir <- resolveDir' dir
                (tarName, tarBytes, mcabalRevision) <- getSDistTarball (sdoptsPvpBounds sdistOpts) pkgDir
                checkSDistTarball' sdistOpts tarName tarBytes
                liftIO $ do
                  creds <- getCreds
                  Upload.uploadBytes creds tarName tarBytes
                  forM_ mcabalRevision $ uncurry $ Upload.uploadRevision creds
                tarPath <- parseRelFile tarName
                when
                    (sdoptsSign sdistOpts)
                    (void $
                     Sig.signTarBytes
                         (sdoptsSignServerUrl sdistOpts)
                         tarPath
                         tarBytes)

sdistCmd :: SDistOpts -> GlobalOpts -> IO ()
sdistCmd sdistOpts go =
    withBuildConfig go $ do -- No locking needed.
        -- If no directories are specified, build all sdist tarballs.
        dirs' <- if null (sdoptsDirsToWorkWith sdistOpts)
            then do
                dirs <- liftM (map lpvRoot . Map.elems . lpProject) getLocalPackages
                when (null dirs) $ do
                    stackYaml <- view stackYamlL
                    prettyErrorL
                        [ styleShell "stack sdist"
                        , flow "expects a list of targets, and otherwise defaults to all of the project's packages."
                        , flow "However, the configuration at"
                        , display stackYaml
                        , flow "contains no packages, so no sdist tarballs will be generated."
                        ]
                    liftIO exitFailure
                return dirs
            else mapM resolveDir' (sdoptsDirsToWorkWith sdistOpts)
        forM_ dirs' $ \dir -> do
            (tarName, tarBytes, _mcabalRevision) <- getSDistTarball (sdoptsPvpBounds sdistOpts) dir
            distDir <- distDirFromDir dir
            tarPath <- (distDir </>) <$> parseRelFile tarName
            ensureDir (parent tarPath)
            liftIO $ L.writeFile (toFilePath tarPath) tarBytes
            prettyInfoL [flow "Wrote sdist tarball to", display tarPath]
            checkSDistTarball sdistOpts tarPath
            forM_ (sdoptsTarPath sdistOpts) $ copyTarToTarPath tarPath tarName
            when (sdoptsSign sdistOpts) (void $ Sig.sign (sdoptsSignServerUrl sdistOpts) tarPath)
        where
          copyTarToTarPath tarPath tarName targetDir = liftIO $ do
            let targetTarPath = targetDir FP.</> tarName
            D.createDirectoryIfMissing True $ FP.takeDirectory targetTarPath
            D.copyFile (toFilePath tarPath) targetTarPath

-- | Execute a command.
execCmd :: ExecOpts -> GlobalOpts -> IO ()
execCmd ExecOpts {..} go@GlobalOpts{..} =
    case eoExtra of
        ExecOptsPlain -> do
          loadConfigWithOpts go $ \lc ->
            withUserFileLock go (view stackRootL lc) $ \lk -> do
              let getCompilerVersion = loadCompilerVersion go lc
              runRIO (lcConfig lc) $
                Docker.reexecWithOptionalContainer
                    (lcProjectRoot lc)
                    -- Unlock before transferring control away, whether using docker or not:
                    (Just $ munlockFile lk)
                    (withBuildConfigAndLock go $ \buildLock -> do
                        config <- view configL
                        menv <- liftIO $ configProcessContextSettings config plainEnvSettings
                        withProcessContext menv $ do
                            (cmd, args) <- case (eoCmd, eoArgs) of
                                (ExecCmd cmd, args) -> return (cmd, args)
                                (ExecRun, args) -> getRunCmd args
                                (ExecGhc, args) -> return ("ghc", args)
                                (ExecRunGhc, args) -> return ("runghc", args)
                            munlockFile buildLock
                            Nix.reexecWithOptionalShell (lcProjectRoot lc) getCompilerVersion (runRIO (lcConfig lc) $ exec cmd args))
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
              menv <- liftIO $ configProcessContextSettings config eoEnvSettings
              withProcessContext menv $ do
                -- Add RTS options to arguments
                let argsWithRts args = if null eoRtsOptions
                            then args :: [String]
                            else args ++ ["+RTS"] ++ eoRtsOptions ++ ["-RTS"]
                (cmd, args) <- case (eoCmd, argsWithRts eoArgs) of
                    (ExecCmd cmd, args) -> return (cmd, args)
                    (ExecRun, args) -> getRunCmd args
                    (ExecGhc, args) -> getGhcCmd "" eoPackages args
                    -- NOTE: this won't currently work for GHCJS, because it doesn't have
                    -- a runghcjs binary. It probably will someday, though.
                    (ExecRunGhc, args) ->
                        getGhcCmd "run" eoPackages args
                munlockFile lk -- Unlock before transferring control away.

                runWithPath eoCwd $ exec cmd args
  where
      -- return the package-id of the first package in GHC_PACKAGE_PATH
      getPkgId wc name = do
          mId <- findGhcPkgField wc [] name "id"
          case mId of
              Just i -> return (head $ words (T.unpack i))
              -- should never happen as we have already installed the packages
              _      -> liftIO $ do
                  hPutStrLn stderr ("Could not find package id of package " ++ name)
                  exitFailure

      getPkgOpts wc pkgs =
          map ("-package-id=" ++) <$> mapM (getPkgId wc) pkgs

      getRunCmd args = do
          pkgComponents <- liftM (map lpvComponents . Map.elems . lpProject) getLocalPackages
          let executables = filter isCExe $ concatMap Set.toList pkgComponents
          let (exe, args') = case args of
                             []   -> (firstExe, args)
                             x:xs -> case find (\y -> y == (CExe $ T.pack x)) executables of
                                     Nothing -> (firstExe, args)
                                     argExe -> (argExe, xs)
                             where
                                firstExe = listToMaybe executables
          case exe of
              Just (CExe exe') -> do
                Stack.Build.build (const (return ())) Nothing defaultBuildOptsCLI{boptsCLITargets = [T.cons ':' exe']}
                return (T.unpack exe', args')
              _                -> do
                  logError "No executables found."
                  liftIO exitFailure

      getGhcCmd prefix pkgs args = do
          wc <- view $ actualCompilerVersionL.whichCompilerL
          pkgopts <- getPkgOpts wc pkgs
          return (prefix ++ compilerExeName wc, pkgopts ++ args)

      runWithPath :: Maybe FilePath -> RIO EnvConfig () -> RIO EnvConfig ()
      runWithPath path callback = case path of
        Nothing                  -> callback
        Just p | not (isValid p) -> throwIO $ InvalidPathForExec p
        Just p                   -> withUnliftIO $ \ul -> D.withCurrentDirectory p $ unliftIO ul callback

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
dockerPullCmd _ go@GlobalOpts{..} =
    loadConfigWithOpts go $ \lc ->
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (view stackRootL lc) $ \_ ->
     runRIO (lcConfig lc) $
       Docker.preventInContainer Docker.pull

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> GlobalOpts -> IO ()
dockerResetCmd keepHome go@GlobalOpts{..} =
    loadConfigWithOpts go $ \lc ->
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (view stackRootL lc) $ \_ ->
      runRIO (lcConfig lc) $
        Docker.preventInContainer $ Docker.reset (lcProjectRoot lc) keepHome

-- | Cleanup Docker images and containers.
dockerCleanupCmd :: Docker.CleanupOpts -> GlobalOpts -> IO ()
dockerCleanupCmd cleanupOpts go@GlobalOpts{..} =
    loadConfigWithOpts go $ \lc ->
    -- TODO: can we eliminate this lock if it doesn't touch ~/.stack/?
    withUserFileLock go (view stackRootL lc) $ \_ ->
     runRIO (lcConfig lc) $
        Docker.preventInContainer $
            Docker.cleanup cleanupOpts

cfgSetCmd :: ConfigCmd.ConfigCmdSet -> GlobalOpts -> IO ()
cfgSetCmd co go@GlobalOpts{..} =
    withMiniConfigAndLock
        go
        (cfgCmdSet go co)

imgDockerCmd :: (Bool, [Text]) -> GlobalOpts -> IO ()
imgDockerCmd (rebuild,images) go@GlobalOpts{..} = loadConfigWithOpts go $ \lc -> do
    let mProjectRoot = lcProjectRoot lc
    withBuildConfigExt
        False
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
        exists <- doesFileExist $ dir </> stackDotYaml
        when (forceOverwrite initOpts || not exists) $
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

-- | Query build information
queryCmd :: [String] -> GlobalOpts -> IO ()
queryCmd selectors go = withBuildConfig go $ queryBuildInfo $ map T.pack selectors

-- | Generate a combined HPC report
hpcReportCmd :: HpcReportOpts -> GlobalOpts -> IO ()
hpcReportCmd hropts go = withBuildConfig go $ generateHpcReportForTargets hropts

data MainException = InvalidReExecVersion String String
                   | UpgradeCabalUnusable
                   | InvalidPathForExec FilePath
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
    show UpgradeCabalUnusable = "--upgrade-cabal cannot be used when nix is activated"
    show (InvalidPathForExec path) = concat
        [ "Got an invalid --cwd argument for stack exec ("
        , path
        , ")"]
