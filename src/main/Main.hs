{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Main stack tool entry point.

module Main (main) where

import           BuildInfo
import           Stack.Prelude hiding (Display (..))
import           Conduit (runConduitRes, sourceLazy, sinkFileCautious)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Writer.Lazy (Writer)
import           Data.Attoparsec.Args (parseArgs, EscapingMode (Escaping))
import           Data.Attoparsec.Interpreter (getInterpreterArgs)
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Version (showVersion)
import           RIO.Process
import           Distribution.Version (mkVersion')
import           GHC.IO.Encoding (mkTextEncoding, textEncodingName)
import           Options.Applicative
import           Options.Applicative.Help (errorHelp, stringChunk, vcatChunks)
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Complicated
import           Pantry (loadSnapshot)
import           Path
import           Path.IO
import qualified Paths_stack as Meta
import           RIO.PrettyPrint
import qualified RIO.PrettyPrint as PP (style)
import           Stack.Build
import           Stack.Build.Target (NeedTargets(..))
import           Stack.Clean (CleanCommand(..), CleanOpts(..), clean)
import           Stack.Config
import           Stack.ConfigCmd as ConfigCmd
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Coverage
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.GhcPkg (findGhcPkgField)
import qualified Stack.Nix as Nix
import           Stack.FileWatch
import           Stack.Ghci
import           Stack.Hoogle
import           Stack.List
import           Stack.Ls
import qualified Stack.IDE as IDE
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
import           Stack.Options.UploadParser
import           Stack.Options.Utils
import qualified Stack.Path
import           Stack.Runners
import           Stack.Script
import           Stack.SDist (getSDistTarball, checkSDistTarball, checkSDistTarball', SDistOpts(..))
import           Stack.Setup (withNewLocalBuildTargets)
import           Stack.SetupCmd
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.SourceMap
import           Stack.Unpack
import           Stack.Upgrade
import qualified Stack.Upload as Upload
import qualified System.Directory as D
import           System.Environment (getProgName, getArgs, withArgs)
import           System.FilePath (isValid, pathSeparator, takeDirectory)
import qualified System.FilePath as FP
import           System.IO (hPutStrLn, hGetEncoding, hSetEncoding)
import           System.Terminal (hIsTerminalDeviceOrMinTTY)

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
      global <- globalOptsFromMonoid isTerminal globalMonoid
      when (globalLogLevel global == LevelDebug) $ hPutStrLn stderr versionString'
      case globalReExecVersion global of
          Just expectVersion -> do
              expectVersion' <- parseVersionThrowing expectVersion
              unless (checkVersion MatchMinor expectVersion' (mkVersion' Meta.version))
                  $ throwIO $ InvalidReExecVersion expectVersion (showVersion Meta.version)
          _ -> return ()
      withRunnerGlobal global $ run `catch` \e ->
          -- This special handler stops "stack: " from being printed before the
          -- exception
          case fromException e of
              Just ec -> exitWith ec
              Nothing -> do
                  logError $ fromString $ displayException e
                  exitFailure

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }

commandLineHandler
  :: FilePath
  -> String
  -> Bool
  -> IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir progName isInterpreter = complicatedOptions
  (mkVersion' Meta.version)
  (Just versionString')
  hpackVersion
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
         (unwords [ "Show how to find templates available for `stack new'."
                  , "`stack new' can accept a template from a remote repository"
                  , "(default: github), local file or remote URL."
                  , "Note: this downloads the help file."
                  ] )
                    templatesCmd
                    (pure ())
        addCommand' "init"
                    "Create stack project config from cabal or hpack package specifications"
                    initCmd
                    initOptsParser
        addCommand' "setup"
                    "Get the appropriate GHC for your project"
                    setupCmd
                    setupParser
        addCommand' "path"
                    "Print out handy path information"
                    Stack.Path.path
                    Stack.Path.pathParser
        addCommand' "ls"
                    "List command. (Supports snapshots, dependencies and stack's styles)"
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
            uploadOptsParser
        addCommand'
            "sdist"
            "Create source distribution tarballs"
            sdistCmd
            sdistOptsParser
        addCommand' "dot"
                    "Visualize your project's dependency graph using Graphviz dot"
                    dot
                    (dotOptsParser False) -- Default for --external is False.
        addCommand' "ghc"
                    "Run ghc"
                    execCmd
                    (execOptsParser $ Just ExecGhc)
        addCommand' "hoogle"
                    ("Run hoogle, the Haskell API search engine. Use the '-- ARGUMENT(S)' syntax " ++
                     "to pass Hoogle arguments, e.g. stack hoogle -- --count=20, or " ++
                     "stack hoogle -- server --local.")
                    hoogleCmd
                    ((,,,) <$> many (strArgument
                                 (metavar "-- ARGUMENT(S) (e.g. stack hoogle -- server --local)"))
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
                  "Execute a command. If the command is absent, the first of any arguments is taken as the command."
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
      addCommand "script"
                 "Run a Stack Script"
                 globalFooter
                 scriptCmd
                 (\so gom ->
                    gom
                      { globalMonoidResolverRoot = First $ Just $ takeDirectory $ soFile so
                      })
                 (globalOpts OtherCmdGlobalOpts)
                 scriptOptsParser

      unless isInterpreter (do
        addCommand' "eval"
                    "Evaluate some haskell code inline. Shortcut for 'stack exec ghc -- -e CODE'"
                    evalCmd
                    (evalOptsParser "CODE")
        addCommand' "clean"
                    "Delete build artefacts for the project packages."
                    cleanCmd
                    (cleanOptsParser Clean)
        addCommand' "purge"
                    "Delete the project stack working directories (.stack-work by default). Shortcut for 'stack clean --full'"
                    cleanCmd
                    (cleanOptsParser Purge)
        addCommand' "query"
                    "Query general build information (experimental)"
                    queryCmd
                    (many $ strArgument $ metavar "SELECTOR...")
        addCommand' "list"
                    "List package id's in snapshot (experimental)"
                    listCmd
                    (many $ strArgument $ metavar "PACKAGE")
        addSubCommands'
            "ide"
            "IDE-specific commands"
            (let outputFlag = flag
                   IDE.OutputLogInfo
                   IDE.OutputStdout
                   (long "stdout" <>
                    help "Send output to stdout instead of the default, stderr")
                 cabalFileFlag = flag
                   IDE.ListPackageNames
                   IDE.ListPackageCabalFiles
                   (long "cabal-files" <>
                    help "Print paths to package cabal-files instead of package names")
             in
             do addCommand'
                    "packages"
                    "List all available local loadable packages"
                    idePackagesCmd
                    ((,) <$> outputFlag <*> cabalFileFlag)
                addCommand'
                    "targets"
                    "List all available stack targets"
                    ideTargetsCmd
                    outputFlag)
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
                                   help "Do not delete sandbox's home directory")))
        addSubCommands'
            ConfigCmd.cfgCmdName
            "Subcommands for accessing and modifying configuration values"
            (do
               addCommand' ConfigCmd.cfgCmdSetName
                          "Sets a field in the project's stack.yaml to value"
                          (withConfig NoReexec . cfgCmdSet)
                          configCmdSetParser
               addCommand' ConfigCmd.cfgCmdEnvName
                          "Print environment variables for use in a shell"
                          (withConfig YesReexec . withDefaultEnvConfig . cfgCmdEnv)
                          configCmdEnvParser)
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
        addCommand' :: String -> String -> (a -> RIO Runner ()) -> Parser a
                    -> AddCommand
        addCommand' cmd title constr =
            addCommand cmd title globalFooter constr (\_ gom -> gom) (globalOpts OtherCmdGlobalOpts)

        addSubCommands' :: String -> String -> AddCommand
                        -> AddCommand
        addSubCommands' cmd title =
            addSubCommands cmd title globalFooter (globalOpts OtherCmdGlobalOpts)

        -- Additional helper that hides global options and shows build options
        addBuildCommand' :: String -> String -> (a -> RIO Runner ()) -> Parser a
                         -> AddCommand
        addBuildCommand' cmd title constr =
            addCommand cmd title globalFooter constr (\_ gom -> gom) (globalOpts BuildCmdGlobalOpts)

        -- Additional helper that hides global options and shows some ghci options
        addGhciCommand' :: String -> String -> (a -> RIO Runner ()) -> Parser a
                         -> AddCommand
        addGhciCommand' cmd title constr =
            addCommand cmd title globalFooter constr (\_ gom -> gom) (globalOpts GhciCmdGlobalOpts)

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
    ExceptT (RIO Runner ()) (Writer (Mod CommandFields (RIO Runner (), GlobalOptsMonoid))) ()

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
  -> IO (GlobalOptsMonoid, (RIO Runner (), t))
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

setupCmd :: SetupCmdOpts -> RIO Runner ()
setupCmd sco@SetupCmdOpts{..} = withConfig YesReexec $ withBuildConfig $ do
  (wantedCompiler, compilerCheck, mstack) <-
    case scoCompilerVersion of
      Just v -> return (v, MatchMinor, Nothing)
      Nothing -> (,,)
        <$> view wantedCompilerVersionL
        <*> view (configL.to configCompilerCheck)
        <*> (Just <$> view stackYamlL)
  setup sco wantedCompiler compilerCheck mstack

cleanCmd :: CleanOpts -> RIO Runner ()
cleanCmd = withConfig NoReexec . withBuildConfig . clean

-- | Helper for build and install commands
buildCmd :: BuildOptsCLI -> RIO Runner ()
buildCmd opts = do
  when (any (("-prof" `elem`) . fromRight [] . parseArgs Escaping) (boptsCLIGhcOptions opts)) $ do
    logError "Error: When building with stack, you should not use the -prof GHC option"
    logError "Instead, please use --library-profiling and --executable-profiling"
    logError "See: https://github.com/commercialhaskell/stack/issues/1015"
    exitFailure
  local (over globalOptsL modifyGO) $
    case boptsCLIFileWatch opts of
      FileWatchPoll -> fileWatchPoll (inner . Just)
      FileWatch -> fileWatch (inner . Just)
      NoFileWatch -> inner Nothing
  where
    inner
      :: Maybe (Set (Path Abs File) -> IO ())
      -> RIO Runner ()
    inner setLocalFiles = withConfig YesReexec $ withEnvConfig NeedTargets opts $
        Stack.Build.build setLocalFiles
    -- Read the build command from the CLI and enable it to run
    modifyGO =
      case boptsCLICommand opts of
        Test -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidTestsL) (Just True)
        Haddock -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidHaddockL) (Just True)
        Bench -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidBenchmarksL) (Just True)
        Install -> set (globalOptsBuildOptsMonoidL.buildOptsMonoidInstallExesL) (Just True)
        Build -> id -- Default case is just Build

uninstallCmd :: [String] -> RIO Runner ()
uninstallCmd _ = do
    prettyErrorL
      [ flow "stack does not manage installations in global locations."
      , flow "The only global mutation stack performs is executable copying."
      , flow "For the default executable destination, please run"
      , PP.style Shell "stack path --local-bin"
      ]
    liftIO exitFailure

-- | Unpack packages to the filesystem
unpackCmd :: ([String], Maybe Text) -> RIO Runner ()
unpackCmd (names, Nothing) = unpackCmd (names, Just ".")
unpackCmd (names, Just dstPath) = withConfig NoReexec $ do
    mresolver <- view $ globalOptsL.to globalResolver
    mSnapshot <- forM mresolver $ \resolver -> do
      concrete <- makeConcreteResolver resolver
      loc <- completeSnapshotLocation concrete
      loadSnapshot loc
    dstPath' <- resolveDir' $ T.unpack dstPath
    unpackPackages mSnapshot dstPath' names

-- | Update the package index
updateCmd :: () -> RIO Runner ()
updateCmd () = withConfig NoReexec (void (updateHackageIndex Nothing))

upgradeCmd :: UpgradeOpts -> RIO Runner ()
upgradeCmd upgradeOpts' = do
  go <- view globalOptsL
  case globalResolver go of
    Just _ -> do
      logError "You cannot use the --resolver option with the upgrade command"
      liftIO exitFailure
    Nothing ->
      withGlobalProject $
      upgrade
        maybeGitHash
        upgradeOpts'

-- | Upload to Hackage
uploadCmd :: UploadOpts -> RIO Runner ()
uploadCmd (UploadOpts (SDistOpts [] _ _ _ _) _) = do
    prettyErrorL
        [ flow "To upload the current package, please run"
        , PP.style Shell "stack upload ."
        , flow "(with the period at the end)"
        ]
    liftIO exitFailure
uploadCmd uploadOpts = do
    let partitionM _ [] = return ([], [])
        partitionM f (x:xs) = do
            r <- f x
            (as, bs) <- partitionM f xs
            return $ if r then (x:as, bs) else (as, x:bs)
        sdistOpts = uoptsSDistOpts uploadOpts
    (files, nonFiles) <- liftIO $ partitionM D.doesFileExist (sdoptsDirsToWorkWith sdistOpts)
    (dirs, invalid) <- liftIO $ partitionM D.doesDirectoryExist nonFiles
    withConfig YesReexec $ withDefaultEnvConfig $ do
        unless (null invalid) $ do
            let invalidList = bulletedList $ map (PP.style File . fromString) invalid
            prettyErrorL
                [ PP.style Shell "stack upload"
                , flow "expects a list of sdist tarballs or package directories."
                , flow "Can't find:"
                , line <> invalidList
                ]
            exitFailure
        when (null files && null dirs) $ do
            prettyErrorL
                [ PP.style Shell "stack upload"
                , flow "expects a list of sdist tarballs or package directories, but none were specified."
                ]
            exitFailure
        config <- view configL
        let hackageUrl = T.unpack $ configHackageBaseUrl config
            uploadVariant = uoptsUploadVariant uploadOpts
        getCreds <- memoizeRef $ Upload.loadCreds config
        mapM_ (resolveFile' >=> checkSDistTarball sdistOpts) files
        forM_ files $ \file -> do
            tarFile <- resolveFile' file
            creds <- runMemoized getCreds
            Upload.upload hackageUrl creds (toFilePath tarFile) uploadVariant
        forM_ dirs $ \dir -> do
            pkgDir <- resolveDir' dir
            (tarName, tarBytes, mcabalRevision) <- getSDistTarball (sdoptsPvpBounds sdistOpts) pkgDir
            checkSDistTarball' sdistOpts tarName tarBytes
            creds <- runMemoized getCreds
            Upload.uploadBytes hackageUrl creds tarName uploadVariant tarBytes
            forM_ mcabalRevision $ uncurry $ Upload.uploadRevision hackageUrl creds

sdistCmd :: SDistOpts -> RIO Runner ()
sdistCmd sdistOpts =
    withConfig YesReexec $ withDefaultEnvConfig $ do
        -- If no directories are specified, build all sdist tarballs.
        dirs' <- if null (sdoptsDirsToWorkWith sdistOpts)
            then do
                dirs <- view $ buildConfigL.to (map ppRoot . Map.elems . smwProject . bcSMWanted)
                when (null dirs) $ do
                    stackYaml <- view stackYamlL
                    prettyErrorL
                        [ PP.style Shell "stack sdist"
                        , flow "expects a list of targets, and otherwise defaults to all of the project's packages."
                        , flow "However, the configuration at"
                        , pretty stackYaml
                        , flow "contains no packages, so no sdist tarballs will be generated."
                        ]
                    exitFailure
                return dirs
            else mapM resolveDir' (sdoptsDirsToWorkWith sdistOpts)
        forM_ dirs' $ \dir -> do
            (tarName, tarBytes, _mcabalRevision) <- getSDistTarball (sdoptsPvpBounds sdistOpts) dir
            distDir <- distDirFromDir dir
            tarPath <- (distDir </>) <$> parseRelFile tarName
            ensureDir (parent tarPath)
            runConduitRes $
              sourceLazy tarBytes .|
              sinkFileCautious (toFilePath tarPath)
            prettyInfoL [flow "Wrote sdist tarball to", pretty tarPath]
            checkSDistTarball sdistOpts tarPath
            forM_ (sdoptsTarPath sdistOpts) $ copyTarToTarPath tarPath tarName
        where
          copyTarToTarPath tarPath tarName targetDir = liftIO $ do
            let targetTarPath = targetDir FP.</> tarName
            D.createDirectoryIfMissing True $ FP.takeDirectory targetTarPath
            D.copyFile (toFilePath tarPath) targetTarPath

-- | Execute a command.
execCmd :: ExecOpts -> RIO Runner ()
execCmd ExecOpts {..} =
  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
    unless (null targets) $ Stack.Build.build Nothing

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
          (ExecGhc, args) -> getGhcCmd eoPackages args
          (ExecRunGhc, args) -> getRunGhcCmd eoPackages args

      runWithPath eoCwd $ exec cmd args
  where
      ExecOptsExtra {..} = eoExtra

      targets = concatMap words eoPackages
      boptsCLI = defaultBuildOptsCLI
                 { boptsCLITargets = map T.pack targets
                 }

      -- return the package-id of the first package in GHC_PACKAGE_PATH
      getPkgId name = do
          pkg <- getGhcPkgExe
          mId <- findGhcPkgField pkg [] name "id"
          case mId of
              Just i -> return (head $ words (T.unpack i))
              -- should never happen as we have already installed the packages
              _      -> do
                  logError ("Could not find package id of package " <> fromString name)
                  exitFailure

      getPkgOpts pkgs =
          map ("-package-id=" ++) <$> mapM getPkgId pkgs

      getRunCmd args = do
          packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
          pkgComponents <- for (Map.elems packages) ppComponents
          let executables = filter isCExe $ concatMap Set.toList pkgComponents
          let (exe, args') = case args of
                             []   -> (firstExe, args)
                             x:xs -> case find (\y -> y == CExe (T.pack x)) executables of
                                     Nothing -> (firstExe, args)
                                     argExe -> (argExe, xs)
                             where
                                firstExe = listToMaybe executables
          case exe of
              Just (CExe exe') -> do
                withNewLocalBuildTargets [T.cons ':' exe'] $ Stack.Build.build Nothing
                return (T.unpack exe', args')
              _                -> do
                  logError "No executables found."
                  exitFailure

      getGhcCmd pkgs args = do
          pkgopts <- getPkgOpts pkgs
          compiler <- view $ compilerPathsL.to cpCompiler
          return (toFilePath compiler, pkgopts ++ args)

      getRunGhcCmd pkgs args = do
          pkgopts <- getPkgOpts pkgs
          interpret <- view $ compilerPathsL.to cpInterpreter
          return (toFilePath interpret, pkgopts ++ args)

      runWithPath :: Maybe FilePath -> RIO EnvConfig () -> RIO EnvConfig ()
      runWithPath path callback = case path of
        Nothing                  -> callback
        Just p | not (isValid p) -> throwIO $ InvalidPathForExec p
        Just p                   -> withUnliftIO $ \ul -> D.withCurrentDirectory p $ unliftIO ul callback

-- | Evaluate some haskell code inline.
evalCmd :: EvalOpts -> RIO Runner ()
evalCmd EvalOpts {..} = execCmd execOpts
    where
      execOpts =
          ExecOpts { eoCmd = ExecGhc
                   , eoArgs = ["-e", evalArg]
                   , eoExtra = evalExtra
                   }

-- | Run GHCi in the context of a project.
ghciCmd :: GhciOpts -> RIO Runner ()
ghciCmd ghciOpts =
  let boptsCLI = defaultBuildOptsCLI
          -- using only additional packages, targets then get overriden in `ghci`
          { boptsCLITargets = map T.pack (ghciAdditionalPackages  ghciOpts)
          , boptsCLIInitialBuildSteps = True
          , boptsCLIFlags = ghciFlags ghciOpts
          , boptsCLIGhcOptions = map T.pack (ghciGhcOptions ghciOpts)
          }
  in withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
    bopts <- view buildOptsL
    -- override env so running of tests and benchmarks is disabled
    let boptsLocal = bopts
               { boptsTestOpts = (boptsTestOpts bopts) { toDisableRun = True }
               , boptsBenchmarkOpts = (boptsBenchmarkOpts bopts) { beoDisableRun = True }
               }
    local (set buildOptsL boptsLocal)
          (ghci ghciOpts)

-- | List packages in the project.
idePackagesCmd :: (IDE.OutputStream, IDE.ListPackagesCmd) -> RIO Runner ()
idePackagesCmd = withConfig NoReexec . withBuildConfig . uncurry IDE.listPackages

-- | List targets in the project.
ideTargetsCmd :: IDE.OutputStream -> RIO Runner ()
ideTargetsCmd = withConfig NoReexec . withBuildConfig . IDE.listTargets

-- | Pull the current Docker image.
dockerPullCmd :: () -> RIO Runner ()
dockerPullCmd () = withConfig NoReexec $ Docker.preventInContainer Docker.pull

-- | Reset the Docker sandbox.
dockerResetCmd :: Bool -> RIO Runner ()
dockerResetCmd = withConfig NoReexec . Docker.preventInContainer . Docker.reset

-- | Project initialization
initCmd :: InitOpts -> RIO Runner ()
initCmd initOpts = do
    pwd <- getCurrentDir
    go <- view globalOptsL
    withGlobalProject $ withConfig YesReexec (initProject pwd initOpts (globalResolver go))

-- | Create a project directory structure and initialize the stack config.
newCmd :: (NewOpts,InitOpts) -> RIO Runner ()
newCmd (newOpts,initOpts) =
    withGlobalProject $ withConfig YesReexec $ do
        dir <- new newOpts (forceOverwrite initOpts)
        exists <- doesFileExist $ dir </> stackDotYaml
        when (forceOverwrite initOpts || not exists) $ do
            go <- view globalOptsL
            initProject dir initOpts (globalResolver go)

-- | Display instructions for how to use templates
templatesCmd :: () -> RIO Runner ()
templatesCmd () = withConfig NoReexec templatesHelp

-- | Query build information
queryCmd :: [String] -> RIO Runner ()
queryCmd selectors = withConfig YesReexec $ withDefaultEnvConfig $ queryBuildInfo $ map T.pack selectors

-- | List packages
listCmd :: [String] -> RIO Runner ()
listCmd names = withConfig NoReexec $ do
    mresolver <- view $ globalOptsL.to globalResolver
    mSnapshot <- forM mresolver $ \resolver -> do
      concrete <- makeConcreteResolver resolver
      loc <- completeSnapshotLocation concrete
      loadSnapshot loc
    listPackages mSnapshot names

-- | generate a combined HPC report
hpcReportCmd :: HpcReportOpts -> RIO Runner ()
hpcReportCmd hropts = do
    let (tixFiles, targetNames) = partition (".tix" `T.isSuffixOf`) (hroptsInputs hropts)
        boptsCLI = defaultBuildOptsCLI
          { boptsCLITargets = if hroptsAll hropts then [] else targetNames }
    withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $
        generateHpcReportForTargets hropts tixFiles targetNames

data MainException = InvalidReExecVersion String String
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
    show (InvalidPathForExec path) = concat
        [ "Got an invalid --cwd argument for stack exec ("
        , path
        , ")"]
