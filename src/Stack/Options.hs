{-# LANGUAGE OverloadedStrings,RecordWildCards #-}

module Stack.Options
    (Command(..)
    ,benchOptsParser
    ,buildOptsParser
    ,cleanOptsParser
    ,configCmdSetParser
    ,configOptsParser
    ,dockerOptsParser
    ,dockerCleanupOptsParser
    ,dotOptsParser
    ,execOptsParser
    ,evalOptsParser
    ,globalOptsParser
    ,initOptsParser
    ,newOptsParser
    ,logLevelOptsParser
    ,ghciOptsParser
    ,solverOptsParser
    ,testOptsParser
    ,hpcReportOptsParser
    ,pvpBoundsOption
    ,globalOptsFromMonoid
    ) where

import           Control.Monad.Logger (LogLevel(..))
import           Data.Char (isSpace, toLower)
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Distribution.Version (anyVersion)
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Types (fromM, oneM, readerAsk)
import           Stack.Clean (CleanOpts(..))
import           Stack.Config (packagesParser)
import           Stack.ConfigCmd
import           Stack.Constants (stackProgName)
import           Stack.Coverage (HpcReportOpts(..))
import           Stack.Docker
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.Ghci (GhciOpts(..))
import           Stack.Init
import           Stack.New
import           Stack.Types
import           Stack.Types.TemplateName

-- | Command sum type for conditional arguments.
data Command
    = Build
    | Test
    | Haddock
    | Bench
    | Install
    deriving (Eq)

-- | Parser for bench arguments.
benchOptsParser :: Parser BenchmarkOpts
benchOptsParser = BenchmarkOpts
        <$> optional (strOption (long "benchmark-arguments" <>
                                 metavar "BENCH_ARGS" <>
                                 help ("Forward BENCH_ARGS to the benchmark suite. " <>
                                       "Supports templates from `cabal bench`")))
        <*> switch (long "no-run-benchmarks" <>
                   help "Disable running of benchmarks. (Benchmarks will still be built.)")

-- | Parser for build arguments.
buildOptsParser :: Command
                -> Parser BuildOpts
buildOptsParser cmd =
            BuildOpts <$> target <*> libProfiling <*> exeProfiling <*>
            haddock <*> haddockDeps <*> dryRun <*> ghcOpts <*>
            flags <*> copyBins <*> preFetch <*> buildSubset <*>
            fileWatch' <*> keepGoing <*> forceDirty <*> tests <*>
            testOptsParser <*> benches <*> benchOptsParser <*>
            many exec <*> onlyConfigure <*> reconfigure <*> cabalVerbose
  where target =
           many (textArgument
                   (metavar "TARGET" <>
                    help "If none specified, use all packages"))
        libProfiling =
          boolFlags False
                    "library-profiling"
                    "library profiling for TARGETs and all its dependencies"
                    idm
        exeProfiling =
          boolFlags False
                    "executable-profiling"
                    "executable profiling for TARGETs and all its dependencies"
                    idm
        haddock =
          boolFlags (cmd == Haddock)
                    "haddock"
                    "generating Haddocks the package(s) in this directory/configuration"
                    idm
        haddockDeps =
             maybeBoolFlags
                       "haddock-deps"
                       "building Haddocks for dependencies"
                       idm
        copyBins = boolFlags (cmd == Install)
            "copy-bins"
            "copying binaries to the local-bin-path (see 'stack path')"
            idm

        dryRun = switch (long "dry-run" <>
                         help "Don't build anything, just prepare to")
        ghcOpts = (\x y z -> concat [x, y, z])
          <$> flag [] ["-Wall", "-Werror"]
              ( long "pedantic"
             <> help "Turn on -Wall and -Werror"
              )
          <*> flag [] ["-O0"]
              ( long "fast"
             <> help "Turn off optimizations (-O0)"
              )
          <*> many (textOption (long "ghc-options" <>
                                metavar "OPTION" <>
                                help "Additional options passed to GHC"))

        flags = Map.unionsWith Map.union <$> many
                  (option readFlag
                      (long "flag" <>
                       metavar "PACKAGE:[-]FLAG" <>
                       help ("Override flags set in stack.yaml " <>
                             "(applies to local packages and extra-deps)")))

        preFetch = switch
            (long "prefetch" <>
             help "Fetch packages necessary for the build immediately, useful with --dry-run")

        buildSubset =
            flag' BSOnlyDependencies
                (long "dependencies-only" <>
                 help "A synonym for --only-dependencies")
            <|> flag' BSOnlySnapshot
                (long "only-snapshot" <>
                 help "Only build packages for the snapshot database, not the local database")
            <|> flag' BSOnlyDependencies
                (long "only-dependencies" <>
                 help "Only build packages that are dependencies of targets on the command line")
            <|> pure BSAll

        fileWatch' =
            flag' FileWatch
                (long "file-watch" <>
                 help "Watch for changes in local files and automatically rebuild. Ignores files in VCS boring/ignore file")
            <|> flag' FileWatchPoll
                (long "file-watch-poll" <>
                 help "Like --file-watch, but polling the filesystem instead of using events")
            <|> pure NoFileWatch

        keepGoing = maybeBoolFlags
            "keep-going"
            "continue running after a step fails (default: false for build, true for test/bench)"
            idm

        forceDirty = switch
            (long "force-dirty" <>
             help "Force treating all local packages as having dirty files (useful for cases where stack can't detect a file change)")

        tests = boolFlags (cmd == Test)
            "test"
            "testing the package(s) in this directory/configuration"
            idm

        benches = boolFlags (cmd == Bench)
            "bench"
            "benchmarking the package(s) in this directory/configuration"
            idm

        exec = cmdOption
            ( long "exec" <>
              metavar "CMD [ARGS]" <>
              help "Command and arguments to run after a successful build" )

        onlyConfigure = switch
            (long "only-configure" <>
             help "Only perform the configure step, not any builds. Intended for tool usage, may break when used on multiple packages at once!")

        reconfigure = switch
            (long "reconfigure" <>
             help "Perform the configure step even if unnecessary. Useful in some corner cases with custom Setup.hs files")

        cabalVerbose = switch
            (long "cabal-verbose" <>
             help "Ask Cabal to be verbose in its output")

-- | Parser for package:[-]flag
readFlag :: ReadM (Map (Maybe PackageName) (Map FlagName Bool))
readFlag = do
    s <- readerAsk
    case break (== ':') s of
        (pn, ':':mflag) -> do
            pn' <-
                case parsePackageNameFromString pn of
                    Nothing
                        | pn == "*" -> return Nothing
                        | otherwise -> readerError $ "Invalid package name: " ++ pn
                    Just x -> return $ Just x
            let (b, flagS) =
                    case mflag of
                        '-':x -> (False, x)
                        _ -> (True, mflag)
            flagN <-
                case parseFlagNameFromString flagS of
                    Nothing -> readerError $ "Invalid flag name: " ++ flagS
                    Just x -> return x
            return $ Map.singleton pn' $ Map.singleton flagN b
        _ -> readerError "Must have a colon"

-- | Command-line parser for the clean command.
cleanOptsParser :: Parser CleanOpts
cleanOptsParser = CleanOpts <$> packages
  where
    packages =
        many
            (packageNameArgument
                 (metavar "PACKAGE" <>
                  help "If none specified, clean all local packages"))

-- | Command-line arguments parser for configuration.
configOptsParser :: Bool -> Parser ConfigMonoid
configOptsParser hide0 =
    (\opts systemGHC installGHC arch os ghcVariant jobs includes libs skipGHCCheck skipMsys localBin modifyCodePage -> mempty
        { configMonoidDockerOpts = opts
        , configMonoidSystemGHC = systemGHC
        , configMonoidInstallGHC = installGHC
        , configMonoidSkipGHCCheck = skipGHCCheck
        , configMonoidArch = arch
        , configMonoidOS = os
        , configMonoidGHCVariant = ghcVariant
        , configMonoidJobs = jobs
        , configMonoidExtraIncludeDirs = includes
        , configMonoidExtraLibDirs = libs
        , configMonoidSkipMsys = skipMsys
        , configMonoidLocalBinPath = localBin
        , configMonoidModifyCodePage = modifyCodePage
        })
    <$> dockerOptsParser True
    <*> maybeBoolFlags
            "system-ghc"
            "using the system installed GHC (on the PATH) if available and a matching version"
            hide
    <*> maybeBoolFlags
            "install-ghc"
            "downloading and installing GHC if necessary (can be done manually with stack setup)"
            hide
    <*> optional (strOption
            ( long "arch"
           <> metavar "ARCH"
           <> help "System architecture, e.g. i386, x86_64"
           <> hide
            ))
    <*> optional (strOption
            ( long "os"
           <> metavar "OS"
           <> help "Operating system, e.g. linux, windows"
           <> hide
            ))
    <*> optional (ghcVariantParser hide0)
    <*> optional (option auto
            ( long "jobs"
           <> short 'j'
           <> metavar "JOBS"
           <> help "Number of concurrent jobs to run"
           <> hide
            ))
    <*> fmap Set.fromList (many (textOption
            ( long "extra-include-dirs"
           <> metavar "DIR"
           <> help "Extra directories to check for C header files"
           <> hide
            )))
    <*> fmap Set.fromList (many (textOption
            ( long "extra-lib-dirs"
           <> metavar "DIR"
           <> help "Extra directories to check for libraries"
           <> hide
            )))
    <*> maybeBoolFlags
            "skip-ghc-check"
            "skipping the GHC version and architecture check"
            hide
    <*> maybeBoolFlags
            "skip-msys"
            "skipping the local MSYS installation (Windows only)"
            hide
    <*> optional (strOption
             ( long "local-bin-path"
            <> metavar "DIR"
            <> help "Install binaries to DIR"
            <> hide
             ))
    <*> maybeBoolFlags
            "modify-code-page"
            "setting the codepage to support UTF-8 (Windows only)"
            hide
  where hide = hideMods hide0

-- | Options parser configuration for Docker.
dockerOptsParser :: Bool -> Parser DockerOptsMonoid
dockerOptsParser hide0 =
    DockerOptsMonoid
    <$> pure False
    <*> maybeBoolFlags dockerCmdName
                       "using a Docker container"
                       hide
    <*> ((Just . DockerMonoidRepo) <$> option str (long (dockerOptName dockerRepoArgName) <>
                                                   hide <>
                                                   metavar "NAME" <>
                                                   help "Docker repository name") <|>
         (Just . DockerMonoidImage) <$> option str (long (dockerOptName dockerImageArgName) <>
                                                    hide <>
                                                    metavar "IMAGE" <>
                                                    help "Exact Docker image ID (overrides docker-repo)") <|>
         pure Nothing)
    <*> maybeBoolFlags (dockerOptName dockerRegistryLoginArgName)
                       "registry requires login"
                       hide
    <*> maybeStrOption (long (dockerOptName dockerRegistryUsernameArgName) <>
                        hide <>
                        metavar "USERNAME" <>
                        help "Docker registry username")
    <*> maybeStrOption (long (dockerOptName dockerRegistryPasswordArgName) <>
                        hide <>
                        metavar "PASSWORD" <>
                        help "Docker registry password")
    <*> maybeBoolFlags (dockerOptName dockerAutoPullArgName)
                       "automatic pulling latest version of image"
                       hide
    <*> maybeBoolFlags (dockerOptName dockerDetachArgName)
                       "running a detached Docker container"
                       hide
    <*> maybeBoolFlags (dockerOptName dockerPersistArgName)
                       "not deleting container after it exits"
                       hide
    <*> maybeStrOption (long (dockerOptName dockerContainerNameArgName) <>
                        hide <>
                        metavar "NAME" <>
                        help "Docker container name")
    <*> argsOption (long (dockerOptName dockerRunArgsArgName) <>
                    hide <>
                    value [] <>
                    metavar "'ARG1 [ARG2 ...]'" <>
                    help "Additional options to pass to 'docker run'")
    <*> many (option auto (long (dockerOptName dockerMountArgName) <>
                           hide <>
                           metavar "(PATH | HOST-PATH:CONTAINER-PATH)" <>
                           help ("Mount volumes from host in container " ++
                                 "(may specify multiple times)")))
    <*> many (option str (long (dockerOptName dockerEnvArgName) <>
                                hide <>
                                metavar "NAME=VALUE" <>
                                help ("Set environment variable in container " ++
                                      "(may specify multiple times)")))
    <*> maybeStrOption (long (dockerOptName dockerDatabasePathArgName) <>
                        hide <>
                        metavar "PATH" <>
                        help "Location of image usage tracking database")
    <*> optional (option str
            (long(dockerOptName dockerStackExeArgName) <>
             hide <>
             metavar (intercalate "|"
                          [ dockerStackExeDownloadVal
                          , dockerStackExeHostVal
                          , dockerStackExeImageVal
                          , "PATH" ]) <>
             help (concat [ "Location of "
                          , stackProgName
                          , " executable used in container" ])))
    <*> maybeBoolFlags (dockerOptName dockerSetUserArgName)
                       "setting user in container to match host"
                       hide
    <*> pure anyVersion
  where
    dockerOptName optName = dockerCmdName ++ "-" ++ T.unpack optName
    maybeStrOption = optional . option str
    hide = hideMods hide0

-- | Parser for docker cleanup arguments.
dockerCleanupOptsParser :: Parser Docker.CleanupOpts
dockerCleanupOptsParser =
  Docker.CleanupOpts <$>
  (flag' Docker.CleanupInteractive
         (short 'i' <>
          long "interactive" <>
          help "Show cleanup plan in editor and allow changes (default)") <|>
   flag' Docker.CleanupImmediate
         (short 'y' <>
          long "immediate" <>
          help "Immediately execute cleanup plan") <|>
   flag' Docker.CleanupDryRun
         (short 'n' <>
          long "dry-run" <>
          help "Display cleanup plan but do not execute") <|>
   pure Docker.CleanupInteractive) <*>
  opt (Just 14) "known-images" "LAST-USED" <*>
  opt Nothing "unknown-images" "CREATED" <*>
  opt (Just 0) "dangling-images" "CREATED" <*>
  opt Nothing "stopped-containers" "CREATED" <*>
  opt Nothing "running-containers" "CREATED"
  where opt def' name mv =
          fmap Just
               (option auto
                       (long name <>
                        metavar (mv ++ "-DAYS-AGO") <>
                        help ("Remove " ++
                              toDescr name ++
                              " " ++
                              map toLower (toDescr mv) ++
                              " N days ago" ++
                              case def' of
                                Just n -> " (default " ++ show n ++ ")"
                                Nothing -> ""))) <|>
          flag' Nothing
                (long ("no-" ++ name) <>
                 help ("Do not remove " ++
                       toDescr name ++
                       case def' of
                         Just _ -> ""
                         Nothing -> " (default)")) <|>
          pure def'
        toDescr = map (\c -> if c == '-' then ' ' else c)

-- | Parser for arguments to `stack dot`
dotOptsParser :: Parser DotOpts
dotOptsParser = DotOpts
            <$> includeExternal
            <*> includeBase
            <*> depthLimit
            <*> fmap (maybe Set.empty Set.fromList . fmap splitNames) prunedPkgs
  where includeExternal = boolFlags False
                                    "external"
                                    "inclusion of external dependencies"
                                    idm
        includeBase = boolFlags True
                                "include-base"
                                "inclusion of dependencies on base"
                                idm
        depthLimit =
            optional (option auto
                             (long "depth" <>
                              metavar "DEPTH" <>
                              help ("Limit the depth of dependency resolution " <>
                                    "(Default: No limit)")))
        prunedPkgs = optional (strOption
                                   (long "prune" <>
                                    metavar "PACKAGES" <>
                                    help ("Prune each package name " <>
                                          "from the comma separated list " <>
                                          "of package names PACKAGES")))

        splitNames :: String -> [String]
        splitNames = map (takeWhile (not . isSpace) . dropWhile isSpace) . splitOn ","

ghciOptsParser :: Parser GhciOpts
ghciOptsParser = GhciOpts
             <$> switch (long "no-build" <> help "Don't build before launching GHCi")
             <*> fmap concat (many (argsOption (long "ghci-options" <>
                                       metavar "OPTION" <>
                                       help "Additional options passed to GHCi")))
             <*> optional
                     (strOption (long "with-ghc" <>
                                 metavar "GHC" <>
                                 help "Use this GHC to run GHCi"))
             <*> (not <$> boolFlags True "load" "load modules on start-up" idm)
             <*> packagesParser
             <*> optional
                     (textOption
                           (long "main-is" <>
                            metavar "TARGET" <>
                            help "Specify which target should contain the main \
                                 \module to load, such as for an executable for \
                                 \test suite or benchmark."))
             <*> buildOptsParser Build

-- | Parser for exec command
execOptsParser :: Maybe SpecialExecCmd -> Parser ExecOpts
execOptsParser mcmd =
    ExecOpts
        <$> maybe eoCmdParser pure mcmd
        <*> eoArgsParser
        <*> execOptsExtraParser
  where
    eoCmdParser = ExecCmd <$> strArgument (metavar "CMD")
    eoArgsParser = many (strArgument (metavar "-- ARGS (e.g. stack ghc -- X.hs -o x)"))

evalOptsParser :: String -- ^ metavar
               -> Parser EvalOpts
evalOptsParser meta =
    EvalOpts
        <$> eoArgsParser
        <*> execOptsExtraParser
  where
    eoArgsParser :: Parser String
    eoArgsParser = strArgument (metavar meta)

-- | Parser for extra options to exec command
execOptsExtraParser :: Parser ExecOptsExtra
execOptsExtraParser = eoPlainParser <|>
                      ExecOptsEmbellished
                         <$> eoEnvSettingsParser
                         <*> eoPackagesParser
  where
    eoEnvSettingsParser :: Parser EnvSettings
    eoEnvSettingsParser = EnvSettings
        <$> pure True
        <*> boolFlags True
                "ghc-package-path"
                "setting the GHC_PACKAGE_PATH variable for the subprocess"
                idm
        <*> boolFlags True
                "stack-exe"
                "setting the STACK_EXE environment variable to the path for the stack executable"
                idm
        <*> pure False

    eoPackagesParser :: Parser [String]
    eoPackagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))

    eoPlainParser :: Parser ExecOptsExtra
    eoPlainParser = flag' ExecOptsPlain
                          (long "plain" <>
                           help "Use an unmodified environment (only useful with Docker)")

-- | Parser for global command-line options.
globalOptsParser :: Bool -> Parser GlobalOptsMonoid
globalOptsParser hide0 =
    GlobalOptsMonoid <$>
    optional (strOption (long Docker.reExecArgName <> hidden <> internal)) <*>
    optional (option auto (long dockerEntrypointArgName <> hidden <> internal)) <*>
    logLevelOptsParser hide0 <*>
    configOptsParser hide0 <*>
    optional (abstractResolverOptsParser hide0) <*>
    optional (compilerOptsParser hide0) <*>
    maybeBoolFlags
        "terminal"
        "overriding terminal detection in the case of running in a false terminal"
        hide <*>
    optional (strOption (long "stack-yaml" <>
                         metavar "STACK-YAML" <>
                         help ("Override project stack.yaml file " <>
                               "(overrides any STACK_YAML environment variable)") <>
                         hide))
  where hide = hideMods hide0

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid :: Bool -> GlobalOptsMonoid -> GlobalOpts
globalOptsFromMonoid defaultTerminal GlobalOptsMonoid{..} = GlobalOpts
    { globalReExecVersion = globalMonoidReExecVersion
    , globalDockerEntrypoint = globalMonoidDockerEntrypoint
    , globalLogLevel = fromMaybe defaultLogLevel (globalMonoidLogLevel)
    , globalConfigMonoid = globalMonoidConfigMonoid
    , globalResolver = globalMonoidResolver
    , globalCompiler = globalMonoidCompiler
    , globalTerminal = fromMaybe defaultTerminal (globalMonoidTerminal)
    , globalStackYaml = globalMonoidStackYaml }

initOptsParser :: Parser InitOpts
initOptsParser =
    InitOpts <$> method <*> overwrite <*> fmap not ignoreSubDirs
  where
    ignoreSubDirs = switch (long "ignore-subdirs" <>
                           help "Do not search for .cabal files in sub directories")
    overwrite = switch (long "force" <>
                       help "Force overwriting of an existing stack.yaml if it exists")
    method = solver
         <|> (MethodResolver <$> resolver)
         <|> (MethodSnapshot <$> snapPref)

    solver =
        flag' MethodSolver
            (long "solver" <>
             help "Use a dependency solver to determine dependencies")

    snapPref =
        flag' PrefLTS
            (long "prefer-lts" <>
             help "Prefer LTS snapshots over Nightly snapshots") <|>
        flag' PrefNightly
            (long "prefer-nightly" <>
             help "Prefer Nightly snapshots over LTS snapshots") <|>
        pure PrefNone

    resolver = option readAbstractResolver
        (long "resolver" <>
         metavar "RESOLVER" <>
         help "Use the given resolver, even if not all dependencies are met")

-- | Parse for a logging level.
logLevelOptsParser :: Bool -> Parser (Maybe LogLevel)
logLevelOptsParser hide =
  fmap (Just . parse)
       (strOption (long "verbosity" <>
                   metavar "VERBOSITY" <>
                   help "Verbosity: silent, error, warn, info, debug" <>
                   hideMods hide)) <|>
  flag' (Just verboseLevel)
       (short 'v' <> long "verbose" <>
        help ("Enable verbose mode: verbosity level \"" <> showLevel verboseLevel <> "\"") <>
        hideMods hide) <|>
  pure Nothing
  where verboseLevel = LevelDebug
        showLevel l =
          case l of
            LevelDebug -> "debug"
            LevelInfo -> "info"
            LevelWarn -> "warn"
            LevelError -> "error"
            LevelOther x -> T.unpack x
        parse s =
          case s of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther (T.pack s)

-- | Parser for the resolver
abstractResolverOptsParser :: Bool -> Parser AbstractResolver
abstractResolverOptsParser hide =
    option readAbstractResolver
        (long "resolver" <>
         metavar "RESOLVER" <>
         help "Override resolver in project file" <>
         hideMods hide)

readAbstractResolver :: ReadM AbstractResolver
readAbstractResolver = do
    s <- readerAsk
    case s of
        "global" -> return ARGlobal
        "nightly" -> return ARLatestNightly
        "lts" -> return ARLatestLTS
        'l':'t':'s':'-':x | Right (x', "") <- decimal $ T.pack x ->
            return $ ARLatestLTSMajor x'
        _ ->
            case parseResolverText $ T.pack s of
                Left e -> readerError $ show e
                Right x -> return $ ARResolver x

compilerOptsParser :: Bool -> Parser CompilerVersion
compilerOptsParser hide =
    option readCompilerVersion
        (long "compiler" <>
         metavar "COMPILER" <>
         help "Use the specified compiler" <>
         hideMods hide)

readCompilerVersion :: ReadM CompilerVersion
readCompilerVersion = do
    s <- readerAsk
    case parseCompilerVersion (T.pack s) of
        Nothing -> readerError $ "Failed to parse compiler: " ++ s
        Just x -> return x

-- | GHC variant parser
ghcVariantParser :: Bool -> Parser GHCVariant
ghcVariantParser hide =
    option
        readGHCVariant
        (long "ghc-variant" <> metavar "VARIANT" <>
         help
             "Specialized GHC variant, e.g. integersimple (implies --no-system-ghc)" <>
         hideMods hide
        )
  where
    readGHCVariant = do
        s <- readerAsk
        case parseGHCVariant s of
            Left e -> readerError (show e)
            Right v -> return v

-- | Parser for @solverCmd@
solverOptsParser :: Parser Bool
solverOptsParser = boolFlags False
    "modify-stack-yaml"
    "Automatically modify stack.yaml with the solver's recommendations"
    idm

-- | Parser for test arguments.
testOptsParser :: Parser TestOpts
testOptsParser = TestOpts
       <$> boolFlags True
                     "rerun-tests"
                     "running already successful tests"
                     idm
       <*> fmap (fromMaybe [])
                (optional (argsOption(long "test-arguments" <>
                                      metavar "TEST_ARGS" <>
                                      help "Arguments passed in to the test suite program")))
      <*> switch (long "coverage" <>
                 help "Generate a code coverage report")
      <*> switch (long "no-run-tests" <>
                 help "Disable running of tests. (Tests will still be built.)")

-- | Parser for @stack new@.
newOptsParser :: Parser (NewOpts,InitOpts)
newOptsParser = (,) <$> newOpts <*> initOptsParser
  where
    newOpts =
        NewOpts <$>
        packageNameArgument
            (metavar "PACKAGE_NAME" <> help "A valid package name.") <*>
        switch
            (long "bare" <>
             help "Do not create a subdirectory for the project") <*>
        templateNameArgument
            (metavar "TEMPLATE_NAME" <>
             help "Name of a template or a local template in a subdirectory,\
                  \ for example: foo or foo.hsfiles" <>
             value defaultTemplateName) <*>
        fmap
            M.fromList
            (many
                 (templateParamArgument
                      (short 'p' <> long "param" <> metavar "KEY:VALUE" <>
                       help
                           "Parameter for the template in the format key:value")))

-- | Parser for @stack hpc report@.
hpcReportOptsParser :: Parser HpcReportOpts
hpcReportOptsParser = HpcReportOpts
    <$> many (textArgument $ metavar "TARGET_OR_TIX")
    <*> switch (long "all" <> help "Use results from all packages and components")
    <*> optional (strOption (long "destdir" <> help "Output directy for HTML report"))

pvpBoundsOption :: Parser PvpBounds
pvpBoundsOption =
    option
        readPvpBounds
        (long "pvp-bounds" <> metavar "PVP-BOUNDS" <>
         help
             "How PVP version bounds should be added to .cabal file: none, lower, upper, both")
  where
    readPvpBounds = do
        s <- readerAsk
        case parsePvpBounds $ T.pack s of
            Left e ->
                readerError e
            Right v ->
                return v

configCmdSetParser :: Parser ConfigCmdSet
configCmdSetParser =
    fromM
        (do field <-
                oneM
                    (strArgument
                         (metavar "FIELD VALUE"))
            oneM (fieldToValParser field))
  where
    fieldToValParser :: String -> Parser ConfigCmdSet
    fieldToValParser s =
        case s of
            "resolver" ->
                ConfigCmdSetResolver <$>
                argument
                    readAbstractResolver
                    idm
            _ ->
                error "parse stack config set field: only set resolver is implemented"

-- | If argument is True, hides the option from usage and help
hideMods :: Bool -> Mod f a
hideMods hide = if hide then internal <> hidden else idm
