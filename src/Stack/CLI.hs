{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.CLI
License     : BSD-3-Clause
-}

module Stack.CLI
  ( commandLineHandler
  ) where

import           Data.Attoparsec.Interpreter ( getInterpreterArgs )
import           Data.Char ( toLower )
import qualified Data.List as L
import           Data.List.NonEmpty ( prependList )
import           Options.Applicative
                   ( Parser, ParserFailure, ParserHelp, ParserResult (..)
                   , handleParseResult, help, helpError, idm, long, metavar
                   , overFailure, renderFailure, strArgument, switch
                   )
import           Options.Applicative.Help ( errorHelp, stringChunk, vcatChunks )
import           Options.Applicative.Builder.Extra
                   ( boolFlags, extraHelpOption )
import           Options.Applicative.Complicated
                   ( addCommand, addSubCommands, complicatedOptions )
import           Path ( filename )
import           RIO.NonEmpty ( (<|) )
import qualified RIO.NonEmpty as NE
import qualified RIO.Process ( exec )
import           RIO.Process ( withProcessContextNoLogging )
import           Stack.Build ( buildCmd )
import           Stack.BuildInfo ( hpackVersion, versionString' )
import           Stack.Clean ( CleanCommand (..), cleanCmd )
import           Stack.ConfigCmd
                   ( cfgCmdBuildFiles, cfgCmdBuildFilesName, cfgCmdEnv
                   , cfgCmdEnvName, cfgCmdName, cfgCmdSet, cfgCmdSetName
                   )
import           Stack.Constants
                   ( globalFooter, osIsWindows, relFileStack, relFileStackDotExe
                   , stackProgName
                   )
import           Stack.Coverage ( hpcReportCmd )
import           Stack.Docker
                   ( dockerCmdName, dockerHelpOptName, dockerPullCmdName )
import           Stack.DockerCmd ( dockerPullCmd, dockerResetCmd )
import           Stack.Dot ( dotCmd )
import           Stack.Exec ( SpecialExecCmd (..), execCmd )
import           Stack.Eval ( evalCmd )
import           Stack.Ghci ( ghciCmd )
import           Stack.Hoogle ( hoogleCmd )
import           Stack.IDE ( ideGhcOptionsCmd, idePackagesCmd, ideTargetsCmd )
import           Stack.Init ( initCmd )
import           Stack.List ( listCmd )
import           Stack.Ls ( lsCmd )
import           Stack.New ( newCmd )
import qualified Stack.Nix as Nix
import           Stack.Options.BuildParser ( buildOptsParser )
import           Stack.Options.CleanParser ( cleanOptsParser )
import           Stack.Options.ConfigEnvParser ( configCmdEnvParser )
import           Stack.Options.ConfigSetParser ( configCmdSetParser )
import           Stack.Options.DotParser ( dotOptsParser )
import           Stack.Options.EvalParser ( evalOptsParser )
import           Stack.Options.ExecParser ( execOptsParser )
import           Stack.Options.GhciParser ( ghciOptsParser )
import           Stack.Options.GlobalParser ( globalOptsParser )
import           Stack.Options.HpcReportParser ( hpcReportOptsParser )
import           Stack.Options.IdeParser
                   ( ideGhcOptionsParser, idePackagesParser, ideTargetsParser )
import           Stack.Options.InitParser ( initOptsParser )
import           Stack.Options.LsParser ( lsOptsParser )
import           Stack.Options.NewParser ( newOptsParser )
import           Stack.Options.PathParser ( pathParser )
import           Stack.Options.SDistParser ( sdistOptsParser )
import           Stack.Options.ScriptParser ( scriptOptsParser )
import           Stack.Options.SetupParser ( setupOptsParser )
import           Stack.Options.UnpackParser ( unpackOptsParser )
import           Stack.Options.UpgradeParser ( upgradeOptsParser )
import           Stack.Options.UploadParser ( uploadOptsParser )
import           Stack.Options.Utils ( GlobalOptsContext (..) )
import qualified Stack.Path ( path )
import           Stack.Prelude
import           Stack.Query ( queryCmd )
import           Stack.Runners
                   ( ShouldReexec (..), withBuildConfig, withConfig
                   , withDefaultEnvConfig
                   )
import           Stack.SDist ( sdistCmd )
import           Stack.Script ( ScriptOpts (..), scriptCmd )
import           Stack.SetupCmd ( setupCmd )
import           Stack.Templates ( templatesCmd )
import           Stack.Types.AddCommand ( AddCommand )
import           Stack.Types.BuildOptsCLI ( BuildCommand (..) )
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid (..) )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.Version ( stackVersion )
import           Stack.Uninstall ( uninstallCmd )
import           Stack.Unpack ( unpackCmd )
import           Stack.Update ( updateCmd )
import           Stack.Upgrade ( upgradeCmd )
import           Stack.Upload ( uploadCmd )
import qualified System.Directory as D
import           System.Environment ( withArgs )
import           System.FilePath ( pathSeparator, takeDirectory )

-- | Type representing \'pretty\' exceptions thrown by functions in the
-- "Stack.CLI" module.
data CliPrettyException
  = NoArgumentsBug
  deriving (Show, Typeable)

instance Pretty CliPrettyException where
  pretty NoArgumentsBug = bugPrettyReport "[S-4639]" $
    flow "commandLineHandler: no command line arguments on event of failure."

instance Exception CliPrettyException

-- | Stack's command line handler.
commandLineHandler ::
     FilePath
  -> String
     -- ^ The name of the current Stack executable, as it was invoked.
  -> Maybe (Path Abs File)
     -- ^ The path to the current Stack executable, if the operating system
     -- provides a reliable way to determine it and where a result was
     -- available.
  -> Bool
  -> IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir progName mExecutablePath isInterpreter =
  -- Append the relevant default (potentially affecting the LogLevel) *after*
  -- appending the global options of the `stack` command to the global options
  -- of the subcommand - see #5326.
  first (<> defaultGlobalOpts) <$> complicatedOptions
    stackVersion
    (Just versionString')
    hpackVersion
    "stack - The Haskell Tool Stack"
    ""
    ("Stack's documentation is available at https://docs.haskellstack.org/. \
    \Command '" <> progName <> " COMMAND --help' for help about a Stack command. Stack also \
    \supports the Haskell Error Index at https://errors.haskell.org/.")
    (globalOpts OuterGlobalOpts)
    (Just failureCallback)
    addCommands
 where
  defaultGlobalOpts = if isInterpreter
    then
      -- Silent except when errors occur - see #2879
      mempty { logLevel = First (Just LevelError) }
    else mempty
  failureCallback f args =
    case L.stripPrefix "Invalid argument" (fst (renderFailure f "")) of
      Just _ -> maybe
        (prettyThrowIO NoArgumentsBug)
        ( \args' -> if isInterpreter
            then
              parseResultHandler (NE.toList args') f
            else
              secondaryCommandHandler args' f
                >>= interpreterHandler progName mExecutablePath currentDir args'
        )
        (NE.nonEmpty args)
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

  -- The order of commands below determines the order in which they are listed
  -- in `stack --help`.
  addCommands = do
    unless isInterpreter $ do
      build
      install
      uninstall
      test
      bench
      haddock
      new
      templates
      init
      setup
      path
      ls
      unpack
      update
      upgrade
      upload
      sdist
      dot
      ghc
      hoogle
    -- These are the only commands allowed in interpreter mode as well
    exec
    run
    ghci
    repl
    runghc
    runhaskell
    script
    unless isInterpreter $ do
      eval
      clean
      purge
      query
      list
      ide
      docker
      config
      hpc

  -- Stack's subcommands are listed below in alphabetical order

  bench = addBuildCommand'
    "bench"
    "Shortcut for 'build --bench'."
    buildCmd
    (buildOptsParser Bench)

  build = addBuildCommand'
    "build"
    "Build the package(s) in this directory/configuration."
    buildCmd
    (buildOptsParser Build)

  clean = addCommand'
    "clean"
    "Delete build artefacts for the project packages."
    cleanCmd
    (cleanOptsParser Clean)

  config = addSubCommands'
      cfgCmdName
        "Subcommands for accessing and modifying configuration values."
        ( do
            addCommand'
              cfgCmdSetName
              "Set a key in a configuration file to value."
              (withConfig NoReexec . cfgCmdSet)
              configCmdSetParser
            addCommandWithLocalInstallRootFooter
              cfgCmdEnvName
              "Print environment variables for use in a shell."
              (withConfig YesReexec . withDefaultEnvConfig . cfgCmdEnv)
              configCmdEnvParser
            addCommand'
              cfgCmdBuildFilesName
              "Generate (when applicable) a Cabal file from a package \
              \ description in the Hpack format and/or a lock file for Stack's \
              \project-level configuration."
              -- It is withBuildConfig that yields the desired actions;
              -- cfgCmdBuildFiles itself yields nothing of interest.
              (withConfig YesReexec . withBuildConfig . cfgCmdBuildFiles)
              (pure ())
        )

  docker = addSubCommands'
    dockerCmdName
    "Subcommands specific to Docker use."
    ( do
        addCommand'
          dockerPullCmdName
          "Pull latest version of Docker image from registry."
          dockerPullCmd
          (pure ())
        addCommand'
          "reset"
          "Reset the Docker sandbox."
          dockerResetCmd
          ( switch
              (  long "keep-home"
              <> help "Do not delete sandbox's home directory."
              )
          )
    )

  dot = addCommand'
    "dot"
    "Visualize your project's dependency graph using Graphviz dot."
    dotCmd
    (dotOptsParser False) -- Default for --external is False.

  eval = addCommand'
    "eval"
    "Evaluate some Haskell code inline. Shortcut for \
    \'stack exec ghc -- -e CODE'."
    evalCmd
    (evalOptsParser "CODE")

  exec = addCommandWithLocalInstallRootFooter
    "exec"
    "Execute a command. If the command is absent, the first of any arguments \
    \is taken as the command."
    execCmd
    (execOptsParser Nothing)

  ghc = addCommand'
    "ghc"
    "Run ghc."
    execCmd
    (execOptsParser $ Just ExecGhc)

  ghci = addGhciCommand'
    "ghci"
    "Run ghci in the context of package(s)."
    ghciCmd
    ghciOptsParser

  haddock = addBuildCommand'
    "haddock"
    "Shortcut for 'build --haddock'."
    buildCmd
    (buildOptsParser Haddock)

  hoogle = addCommand'
    "hoogle"
    "Run hoogle, the Haskell API search engine. Use the '-- ARGUMENT(S)' \
    \syntax to pass Hoogle arguments, e.g. 'stack hoogle -- --count=20', \
    \or 'stack hoogle -- server --local'."
    hoogleCmd
    ( (,,,)
        <$> many (strArgument
              ( metavar "-- ARGUMENT(S) (e.g. 'stack hoogle -- server --local')"
              ))
        <*> boolFlags
              True
              "setup"
              "If needed: install Hoogle, build Haddock documentation and \
              \generate a Hoogle database."
              idm
        <*> switch
              (  long "rebuild"
              <> help "Rebuild the Hoogle database."
              )
        <*> switch
              (  long "server"
              <> help "Start local Hoogle server."
              )
      )

  hpc = addSubCommands'
    "hpc"
    "Subcommands specific to Haskell Program Coverage."
    ( addCommand'
        "report"
        "Generate unified HPC coverage report from tix files and project \
        \targets."
        hpcReportCmd
        hpcReportOptsParser
    )

  ide = addSubCommands'
    "ide"
    "IDE-specific commands."
    ( do
        addCommand'
          "packages"
          "List all available local loadable packages."
          idePackagesCmd
          idePackagesParser
        addCommand'
          "targets"
          "List all targets or pick component types to list."
          ideTargetsCmd
          ideTargetsParser
        addCommand'
          "ghc-options"
          "List, on the standard output stream, GHC options and other \
          \information passed to GHCi for a given Haskell source code file."
          ideGhcOptionsCmd
          ideGhcOptionsParser
    )

  init = addCommand'
    "init"
    "Create Stack project configuration from Cabal or Hpack package \
    \specifications. If a snapshot is specified at the command line, the \
    \command will try to use it."
    initCmd
    initOptsParser

  install = addBuildCommand'
    "install"
    "Shortcut for 'build --copy-bins'."
    buildCmd
    (buildOptsParser Install)

  list = addCommand'
    "list"
    "List package versions included in the package index, or in a specified \
    \snapshot (directly or indirectly)."
    listCmd
    (many $ strArgument $ metavar "PACKAGE")

  ls = addCommand'
    "ls"
    "List command. (Supports snapshots, global packages, dependencies, Stack's \
    \styles and installed tools.)"
    lsCmd
    lsOptsParser

  new = addCommand'
    "new"
    "Create a new project from a template. Run 'stack templates' to see \
    \available templates. A local file or a remote URL can be specified as a \
    \template. Will initialise if there is no stack.yaml file. Initialisation \
    \may be forced. If a snapshot is specified at the command line, \
    \initialisation will try to use it."
    newCmd
    newOptsParser

  path = addCommandWithLocalInstallRootFooter
    "path"
    "Print out handy path information."
    Stack.Path.path
    pathParser

  purge = addCommand'
    "purge"
    "Delete the project Stack working directories (.stack-work by \
    \default). Shortcut for 'stack clean --full'."
    cleanCmd
    (cleanOptsParser Purge)

  query = addCommand'
    "query"
    "Query general build information (experimental)."
    queryCmd
    (many $ strArgument $ metavar "SELECTOR...")

  repl = addGhciCommand'
    "repl"
    "Run ghci in the context of package(s) (alias for 'ghci')."
    ghciCmd
    ghciOptsParser

  run = addCommand'
    "run"
    "Build and run an executable. Defaults to the first available \
    \executable if none is provided as the first argument."
    execCmd
    (execOptsParser $ Just ExecRun)

  runghc = addCommand'
    "runghc"
    "Run runghc."
    execCmd
    (execOptsParser $ Just ExecRunGhc)

  runhaskell = addCommand'
    "runhaskell"
    "Run runghc (alias for 'runghc')."
    execCmd
    (execOptsParser $ Just ExecRunGhc)

  script = addCommand
    "script"
    "Run a Stack script."
    globalFooter
    scriptCmd
    (\so gom -> gom { snapshotRoot = First $ Just $ takeDirectory so.file })
    (globalOpts OtherCmdGlobalOpts)
    scriptOptsParser

  sdist = addCommand'
    "sdist"
    "Create source distribution tarballs."
    sdistCmd
    sdistOptsParser

  setup = addCommand'
    "setup"
    "Get the appropriate GHC for your project."
    setupCmd
    setupOptsParser

  templates = addCommand'
    "templates"
    "Show how to find templates available for 'stack new'. 'stack new' \
    \can accept a template from a remote repository (default: github), \
    \local file or remote URL. Note: this downloads the help file."
    templatesCmd
    (pure ())

  test = addBuildCommand'
    "test"
    "Shortcut for 'build --test'."
    buildCmd
    (buildOptsParser Test)

  uninstall = addCommand'
    "uninstall"
    "Show how to uninstall Stack or a Stack-supplied tool. This command does \
    \not itself uninstall Stack or a Stack-supplied tool."
    uninstallCmd
    (pure ())

  unpack = addCommand'
    "unpack"
    "Unpack one or more packages, or one or more package candidates, locally."
    unpackCmd
    unpackOptsParser

  update = addCommand'
    "update"
    "Update the package index."
    updateCmd
    (pure ())

  upgrade = addCommand''
    "upgrade"
    "Upgrade Stack, installing to Stack's local-bin directory and, if \
    \different and permitted, the directory of the current Stack \
    \executable."
    upgradeCmd
    "Warning: if you use GHCup to install Stack, use only GHCup to \
    \upgrade Stack."
    (upgradeOptsParser onlyLocalBins)
   where
    isProgNameStack =
         (lowercase progName == lowercase stackProgName)
      || (  osIsWindows
         && lowercase progName == lowercase (stackProgName <> ".EXE")
         )
    isRelFileNameStack relFile =
         (relFile == relFileStack)
      || (osIsWindows && relFile == relFileStackDotExe )
    isExecutableNameStack =
      let mExecutableName = filename <$> mExecutablePath
      in  maybe False isRelFileNameStack mExecutableName
    onlyLocalBins = not (isProgNameStack && isExecutableNameStack)
    lowercase = map toLower

  upload = addCommand'
    "upload"
    "Upload one or more packages, or documentation for one or more packages, \
    \to Hackage."
    uploadCmd
    uploadOptsParser

  -- addCommand hiding global options
  addCommand' ::
       String
    -> String
    -> (a -> RIO Runner ())
    -> Parser a
    -> AddCommand
  addCommand' cmd title constr =
    addCommand
      cmd
      title
      globalFooter
      constr
      (\_ gom -> gom)
      (globalOpts OtherCmdGlobalOpts)

  -- addCommand with custom footer hiding global options
  addCommand'' ::
       String
    -> String
    -> (a -> RIO Runner ())
    -> String
    -> Parser a
    -> AddCommand
  addCommand'' cmd title constr cmdFooter =
    addCommand
      cmd
      title
      (globalFooter <> " " <> cmdFooter)
      constr
      (\_ gom -> gom)
      (globalOpts OtherCmdGlobalOpts)

  -- addCommand with custom footer about options affecting the local install
  --root and hiding global options
  addCommandWithLocalInstallRootFooter ::
       String
    -> String
    -> (a -> RIO Runner ())
    -> Parser a
    -> AddCommand
  addCommandWithLocalInstallRootFooter cmd title constr =
    addCommand''
      cmd
      title
      constr
      "This command also accepts 'stack build' flags and options that affect \
      \the location of the local project installation root directory."

  addSubCommands' ::
       String
    -> String
    -> AddCommand
    -> AddCommand
  addSubCommands' cmd title =
    addSubCommands
      cmd
      title
      globalFooter
      (globalOpts OtherCmdGlobalOpts)

  -- Additional helper that hides global options and shows build options
  addBuildCommand' ::
       String
    -> String
    -> (a -> RIO Runner ())
    -> Parser a
    -> AddCommand
  addBuildCommand' cmd title constr =
      addCommand
        cmd
        title
        globalFooter
        constr
        (\_ gom -> gom)
        (globalOpts BuildCmdGlobalOpts)

  -- Additional helper that hides global options and shows some ghci options
  addGhciCommand' ::
       String
    -> String
    -> (a -> RIO Runner ())
    -> Parser a
    -> AddCommand
  addGhciCommand' cmd title constr =
      addCommand
        cmd
        title
        globalFooter
        constr
        (\_ gom -> gom)
        (globalOpts GhciCmdGlobalOpts)

  globalOpts :: GlobalOptsContext -> Parser GlobalOptsMonoid
  globalOpts kind =
        extraHelpOption
          hide
          progName
          (dockerCmdName ++ "*")
          dockerHelpOptName
    <*> extraHelpOption
          hide
          progName
          (Nix.nixCmdName ++ "*")
          Nix.nixHelpOptName
    <*> globalOptsParser currentDir kind
   where
    hide = kind /= OuterGlobalOpts

-- | fall-through to external executables in \'git\' style if they exist
-- (i.e. @stack something@ looks for @stack-something@ before failing with
-- "Invalid argument \'something\'".)
secondaryCommandHandler ::
     NonEmpty String
  -> ParserFailure ParserHelp
  -> IO (ParserFailure ParserHelp)
secondaryCommandHandler args f =
  -- don't even try when the argument looks like a path or flag
  if elem pathSeparator cmd || "-" `L.isPrefixOf` NE.head args
     then pure f
  else do
    mExternalExec <- D.findExecutable cmd
    case mExternalExec of
      Just ex -> withProcessContextNoLogging $ do
        -- TODO show the command in verbose mode
        -- hPutStrLn stderr $ unwords $
        --   ["Running", "[" ++ ex, unwords (tail args) ++ "]"]
        _ <- RIO.Process.exec ex (NE.tail args)
        pure f
      Nothing -> pure $ fmap (vcatErrorHelp (noSuchCmd cmd)) f
 where
  -- FIXME this is broken when any options are specified before the command
  -- e.g. stack --verbosity silent cmd
  cmd = stackProgName <> "-" <> NE.head args
  noSuchCmd name = errorHelp $ stringChunk
    ("Auxiliary command not found in path '" ++ name ++ "'.")

interpreterHandler ::
     Monoid t
  => String
     -- ^ The name of the current Stack executable, as it was invoked.
  -> Maybe (Path Abs File)
     -- ^ The path to the current Stack executable, if the operating system
     -- provides a reliable way to determine it and where a result was
     -- available.
  -> FilePath
  -> NonEmpty String
  -> ParserFailure ParserHelp
  -> IO (GlobalOptsMonoid, (RIO Runner (), t))
interpreterHandler progName mExecutablePath currentDir args f = do
  -- args can include top-level config such as --extra-lib-dirs=... (set by
  -- nix-shell) - we need to find the first argument which is a file, everything
  -- afterwards is an argument to the script, everything before is an argument
  -- to Stack
  (stackArgs, fileArgs) <- spanM (fmap not . D.doesFileExist) args
  case fileArgs of
    (file:fileArgs') -> runInterpreterCommand file stackArgs fileArgs'
    [] -> parseResultHandler (errorCombine (noSuchFile firstArg))
 where
  firstArg = NE.head args

  spanM p xs@(x :| rest) = do
    r <- p x
    if r
      then case rest of
        [] -> pure ([x], [])
        (x': rest') -> do
          (ys, zs) <- spanM p (x' :| rest')
          pure (x : ys, zs)
      else
        pure ([], NE.toList xs)

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
    ("File does not exist or is not a regular file '" ++ name ++ "'.")

  runInterpreterCommand path stackArgs fileArgs = do
    iargs <- getInterpreterArgs path
    let parseCmdLine =
          commandLineHandler currentDir progName mExecutablePath True
        -- Implicit file arguments are put before other arguments that
        -- occur after "--". See #3658
        cmdArgs = prependList stackArgs $ case NE.break (== "--") iargs of
          (beforeSep, []) -> prependList beforeSep $ "--" <| path :| fileArgs
          (beforeSep, optSep : afterSep) ->
            prependList beforeSep $ optSep <| path :| fileArgs <> afterSep
     -- TODO show the command in verbose mode
     -- hPutStrLn stderr $ unwords $
     --   ["Running", "[" ++ progName, unwords cmdArgs ++ "]"]
    (a,b) <- withArgs (NE.toList cmdArgs) parseCmdLine
    pure (a,(b,mempty))

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }
