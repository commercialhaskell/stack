{-# LANGUAGE NoImplicitPrelude #-}

module Stack.CLI
  ( commandLineHandler
  ) where

import           BuildInfo ( hpackVersion, versionString' )
import           Data.Attoparsec.Interpreter ( getInterpreterArgs )
import           Data.Char ( toLower )
import qualified Data.List as L
import           Options.Applicative
                   ( Parser, ParserFailure, ParserHelp, ParserResult (..), flag
                   , handleParseResult, help, helpError, idm, long, metavar
                   , overFailure, renderFailure, strArgument, switch )
import           Options.Applicative.Help ( errorHelp, stringChunk, vcatChunks )
import           Options.Applicative.Builder.Extra
                   ( boolFlags, extraHelpOption, textOption )
import           Options.Applicative.Complicated
                   ( addCommand, addSubCommands, complicatedOptions )
import qualified RIO.Process ( exec )
import           RIO.Process ( withProcessContextNoLogging )
import           Stack.Build ( buildCmd )
import           Stack.Clean ( CleanCommand (..), cleanCmd )
import           Stack.ConfigCmd as ConfigCmd
import           Stack.Constants ( globalFooter, osIsWindows, stackProgName )
import           Stack.Coverage ( hpcReportCmd )
import           Stack.Docker
                   ( dockerCmdName, dockerHelpOptName, dockerPullCmdName )
import           Stack.DockerCmd ( dockerPullCmd, dockerResetCmd )
import qualified Stack.Dot ( dot )
import           Stack.Exec ( SpecialExecCmd (..), execCmd )
import           Stack.Eval ( evalCmd )
import           Stack.Ghci ( ghciCmd )
import           Stack.Hoogle ( hoogleCmd )
import           Stack.IDE
                   ( ListPackagesCmd (..), OutputStream (..), idePackagesCmd
                   , ideTargetsCmd
                   )
import           Stack.Init ( initCmd )
import           Stack.List ( listCmd )
import           Stack.Ls ( lsCmd )
import           Stack.New ( newCmd )
import qualified Stack.Nix as Nix
import           Stack.Options.BuildParser ( buildOptsParser )
import           Stack.Options.CleanParser ( cleanOptsParser )
import           Stack.Options.DotParser ( dotOptsParser )
import           Stack.Options.EvalParser ( evalOptsParser )
import           Stack.Options.ExecParser ( execOptsParser )
import           Stack.Options.GhciParser ( ghciOptsParser )
import           Stack.Options.GlobalParser ( globalOptsParser )
import           Stack.Options.HpcReportParser ( hpcReportOptsParser )
import           Stack.Options.InitParser ( initOptsParser )
import           Stack.Options.LsParser ( lsOptsParser )
import           Stack.Options.NewParser ( newOptsParser )
import           Stack.Options.PathParser ( pathParser )
import           Stack.Options.SDistParser ( sdistOptsParser )
import           Stack.Options.ScriptParser ( scriptOptsParser )
import           Stack.Options.SetupParser ( setupOptsParser )
import           Stack.Options.UpgradeParser ( upgradeOptsParser )
import           Stack.Options.UploadParser ( uploadOptsParser )
import           Stack.Options.Utils ( GlobalOptsContext (..) )
import qualified Stack.Path ( path )
import           Stack.Prelude
import           Stack.Query ( queryCmd )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.SDist ( sdistCmd )
import           Stack.Script ( ScriptOpts (..), scriptCmd )
import           Stack.SetupCmd ( setupCmd )
import           Stack.Templates ( templatesCmd )
import           Stack.Types.AddCommand ( AddCommand )
import           Stack.Types.BuildOpts ( BuildCommand (..) )
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid (..) )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.Version ( stackVersion )
import           Stack.Uninstall ( uninstallCmd )
import           Stack.Unpack ( unpackCmd )
import           Stack.Update ( updateCmd )
import           Stack.Upgrade ( upgradeCmd )
import           Stack.Upload ( uploadCmd )
import qualified System.Directory as D
import           System.Environment ( getProgName, withArgs )
import           System.FilePath ( pathSeparator, takeDirectory )

commandLineHandler ::
     FilePath
  -> String
  -> Bool
  -> IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir progName isInterpreter =
  -- Append the relevant default (potentially affecting the LogLevel) *after*
  -- appending the global options of the `stack` command to the global options
  -- of the subcommand - see #5326.
  first (<> defaultGlobalOpts) <$> complicatedOptions
    stackVersion
    (Just versionString')
    hpackVersion
    "stack - The Haskell Tool Stack"
    ""
    "Stack's documentation is available at https://docs.haskellstack.org/. \
    \Command 'stack COMMAND --help' for help about a Stack command. Stack also \
    \supports the Haskell Error Index at https://errors.haskell.org/."
    (globalOpts OuterGlobalOpts)
    (Just failureCallback)
    addCommands
 where
  defaultGlobalOpts = if isInterpreter
    then
      -- Silent except when errors occur - see #2879
      mempty { globalMonoidLogLevel = First (Just LevelError) }
    else mempty
  failureCallback f args =
    case L.stripPrefix "Invalid argument" (fst (renderFailure f "")) of
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
      ConfigCmd.cfgCmdName
        "Subcommands for accessing and modifying configuration values."
        ( do
            addCommand'
              ConfigCmd.cfgCmdSetName
              "Sets a key in YAML configuration file to value."
              (withConfig NoReexec . cfgCmdSet)
              configCmdSetParser
            addCommand'
              ConfigCmd.cfgCmdEnvName
              "Print environment variables for use in a shell."
              (withConfig YesReexec . withDefaultEnvConfig . cfgCmdEnv)
              configCmdEnvParser
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
    Stack.Dot.dot
    (dotOptsParser False) -- Default for --external is False.

  eval = addCommand'
    "eval"
    "Evaluate some Haskell code inline. Shortcut for \
    \'stack exec ghc -- -e CODE'."
    evalCmd
    (evalOptsParser "CODE")

  exec = addCommand'
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
    ( let outputFlag = flag
            OutputLogInfo
            OutputStdout
            (  long "stdout"
            <> help "Send output to the standard output stream instead of the \
                    \default, the standard error stream."
            )
          cabalFileFlag = flag
            ListPackageNames
            ListPackageCabalFiles
            (  long "cabal-files"
            <> help "Print paths to package Cabal files instead of package \
                    \names."
            )
       in  do
             addCommand'
               "packages"
               "List all available local loadable packages."
               idePackagesCmd
               ((,) <$> outputFlag <*> cabalFileFlag)
             addCommand'
               "targets"
               "List all available Stack targets."
               ideTargetsCmd
               outputFlag
    )

  init = addCommand'
    "init"
    "Create Stack project configuration from Cabal or Hpack package \
    \specifications."
    initCmd
    initOptsParser

  install = addBuildCommand'
    "install"
    "Shortcut for 'build --copy-bins'."
    buildCmd
    (buildOptsParser Install)

  list = addCommand'
    "list"
    "List package id's in snapshot (experimental)."
    listCmd
    (many $ strArgument $ metavar "PACKAGE")

  ls = addCommand'
    "ls"
    "List command. (Supports snapshots, dependencies, Stack's styles and \
    \installed tools.)"
    lsCmd
    lsOptsParser

  new = addCommand'
    "new"
    "Create a new project from a template. Run 'stack templates' to see \
    \available templates. Will also initialise if there is no stack.yaml \
    \file. Note: you can also specify a local file or a remote URL as a \
    \template; or force an initialisation."
    newCmd
    newOptsParser

  path = addCommand'
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
    ( \so gom ->
        gom
          { globalMonoidResolverRoot =
              First $ Just $ takeDirectory $ soFile so
          }
    )
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
    "Unpack one or more packages locally."
    unpackCmd
    ( (,)
        <$> some (strArgument $ metavar "PACKAGE")
        <*> optional (textOption
              (  long "to"
              <> help "Optional path to unpack the package into (will \
                      \unpack into subdirectory)."
              ))
    )

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
    onlyLocalBins =
         (lowercase progName /= lowercase stackProgName)
      && not ( osIsWindows
             && lowercase progName == lowercase (stackProgName <> ".EXE")
             )
    lowercase = map toLower

  upload = addCommand'
    "upload"
    "Upload a package to Hackage."
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

-- | fall-through to external executables in `git` style if they exist
-- (i.e. `stack something` looks for `stack-something` before
-- failing with "Invalid argument `something'")
secondaryCommandHandler ::
     [String]
  -> ParserFailure ParserHelp
  -> IO (ParserFailure ParserHelp)
secondaryCommandHandler args f =
  -- don't even try when the argument looks like a path or flag
  if elem pathSeparator cmd || "-" `L.isPrefixOf` L.head args
     then pure f
  else do
    mExternalExec <- D.findExecutable cmd
    case mExternalExec of
      Just ex -> withProcessContextNoLogging $ do
        -- TODO show the command in verbose mode
        -- hPutStrLn stderr $ unwords $
        --   ["Running", "[" ++ ex, unwords (tail args) ++ "]"]
        _ <- RIO.Process.exec ex (L.tail args)
        pure f
      Nothing -> pure $ fmap (vcatErrorHelp (noSuchCmd cmd)) f
 where
  -- FIXME this is broken when any options are specified before the command
  -- e.g. stack --verbosity silent cmd
  cmd = stackProgName ++ "-" ++ L.head args
  noSuchCmd name = errorHelp $ stringChunk
    ("Auxiliary command not found in path '" ++ name ++ "'.")

interpreterHandler ::
     Monoid t
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
  firstArg = L.head args

  spanM _ [] = pure ([], [])
  spanM p xs@(x:xs') = do
    r <- p x
    if r
    then do
      (ys, zs) <- spanM p xs'
      pure (x:ys, zs)
    else
      pure ([], xs)

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
    pure (a,(b,mempty))

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }
