{-# LANGUAGE OverloadedStrings #-}

module Stack.Options
    (Command(..)
    ,benchOptsParser
    ,buildOptsParser
    ,configOptsParser
    ,dockerOptsParser
    ,dockerCleanupOptsParser
    ,dotOptsParser
    ,execOptsParser
    ,globalOptsParser
    ,initOptsParser
    ,newOptsParser
    ,logLevelOptsParser
    ,abstractResolverOptsParser
    ,solverOptsParser
    ,testOptsParser
    ) where

import           Control.Monad.Logger (LogLevel(..))
import           Data.Char (isSpace, toLower)
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Options.Applicative.Simple
import           Options.Applicative.Types (readerAsk)
import           Stack.Build.Types
import           Stack.Docker
import qualified Stack.Docker as Docker
import           Stack.Dot
import           Stack.Init
import           Stack.New (NewOpts(..))
import           Stack.Types

-- | Command sum type for conditional arguments.
data Command
    = Build
    | Test
    | Haddock
    | Bench
    deriving (Eq)

-- | Parser for bench arguments.
benchOptsParser :: Parser BenchmarkOpts
benchOptsParser = BenchmarkOpts
        <$> optional (strOption (long "benchmark-arguments" <>
                                 metavar "BENCH_ARGS" <>
                                 help ("Forward BENCH_ARGS to the benchmark suite. " <>
                                       "Supports templates from `cabal bench`")))

-- | Parser for build arguments.
buildOptsParser :: Command
                -> Bool -- ^ default copy-bins value
                -> Parser BuildOpts
buildOptsParser cmd defCopyBins =
            BuildOpts <$> target <*> libProfiling <*> exeProfiling <*>
            optimize <*> enableTests <*> enableBench <*> haddock <*> haddockDeps <*> finalAction <*> dryRun <*> ghcOpts <*>
            flags <*> copyBins <*> preFetch <*>
            ((||) <$> onlySnapshot <*> onlyDependencies) <*>
            fileWatch' <*> keepGoing <*> forceDirty
  where optimize =
          maybeBoolFlags "optimizations" "optimizations for TARGETs and all its dependencies" idm
        enableTests = pure False
        enableBench = pure False
        target =
          fmap (map T.pack)
                (many (strArgument
                         (metavar "TARGET" <>
                          help "If none specified, use all packages")))
        libProfiling =
          boolFlags False
                    "library-profiling"
                    "library profiling for TARGETs and all its dependencies"
                    idm
        exeProfiling =
          boolFlags False
                    "executable-profiling"
                    "library profiling for TARGETs and all its dependencies"
                    idm
        haddock =
          boolFlags (cmd == Haddock)
                    "haddock"
                    "building Haddocks"
                    idm
        haddockDeps =
          if cmd == Haddock
             then maybeBoolFlags
                            "haddock-deps"
                            "building Haddocks for dependencies"
                            idm
             else pure Nothing
        finalAction = pure DoNothing

        copyBins = boolFlags defCopyBins
            "copy-bins"
            "copying binaries to the local-bin-path (see 'stack path')"
            idm

        dryRun = flag False True (long "dry-run" <>
                                  help "Don't build anything, just prepare to")
        ghcOpts = (++)
          <$> flag [] ["-Wall", "-Werror"]
              ( long "pedantic"
             <> help "Turn on -Wall and -Werror (note: option name may change in the future"
              )
          <*> many (fmap T.pack
                     (strOption (long "ghc-options" <>
                                 metavar "OPTION" <>
                                 help "Additional options passed to GHC")))

        flags =
          fmap (Map.unionsWith Map.union) $ many
            (option readFlag
                ( long "flag"
               <> metavar "PACKAGE:[-]FLAG"
               <> help "Override flags set in stack.yaml (applies to local packages and extra-deps)"
                ))

        preFetch = flag False True
            (long "prefetch" <>
             help "Fetch packages necessary for the build immediately, useful with --dry-run")
        onlySnapshot = flag False True
            (long "only-snapshot" <>
             help "Only build packages for the snapshot database, not the local database")
        onlyDependencies = flag False True
            (long "only-dependencies" <>
             help "Currently: a synonym for only-snapshot, see https://github.com/commercialhaskell/stack/issues/387")

        fileWatch' = flag False True
            (long "file-watch" <>
             help "Watch for changes in local files and automatically rebuild")

        keepGoing = maybeBoolFlags
            "keep-going"
            "continue running after a step fails (default: false for build, true for test/bench)"
            idm

        forceDirty = flag False True
            (long "force-dirty" <>
             help "Force treating all local packages as having dirty files (useful for cases where stack can't detect a file change)")

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

-- | Command-line arguments parser for configuration.
configOptsParser :: Bool -> Parser ConfigMonoid
configOptsParser docker =
    (\opts systemGHC installGHC arch os jobs includes libs skipGHCCheck skipMsys localBin -> mempty
        { configMonoidDockerOpts = opts
        , configMonoidSystemGHC = systemGHC
        , configMonoidInstallGHC = installGHC
        , configMonoidSkipGHCCheck = skipGHCCheck
        , configMonoidArch = arch
        , configMonoidOS = os
        , configMonoidJobs = jobs
        , configMonoidExtraIncludeDirs = includes
        , configMonoidExtraLibDirs = libs
        , configMonoidSkipMsys = skipMsys
        , configMonoidLocalBinPath = localBin
        })
    <$> dockerOptsParser docker
    <*> maybeBoolFlags
            "system-ghc"
            "using the system installed GHC (on the PATH) if available and a matching version"
            idm
    <*> maybeBoolFlags
            "install-ghc"
            "downloading and installing GHC if necessary (can be done manually with stack setup)"
            idm
    <*> optional (strOption
            ( long "arch"
           <> metavar "ARCH"
           <> help "System architecture, e.g. i386, x86_64"
            ))
    <*> optional (strOption
            ( long "os"
           <> metavar "OS"
           <> help "Operating system, e.g. linux, windows"
            ))
    <*> optional (option auto
            ( long "jobs"
           <> short 'j'
           <> metavar "JOBS"
           <> help "Number of concurrent jobs to run"
            ))
    <*> fmap (Set.fromList . map T.pack) (many $ strOption
            ( long "extra-include-dirs"
           <> metavar "DIR"
           <> help "Extra directories to check for C header files"
            ))
    <*> fmap (Set.fromList . map T.pack) (many $ strOption
            ( long "extra-lib-dirs"
           <> metavar "DIR"
           <> help "Extra directories to check for libraries"
            ))
    <*> maybeBoolFlags
            "skip-ghc-check"
            "skipping the GHC version and architecture check"
            idm
    <*> maybeBoolFlags
            "skip-msys"
            "skipping the local MSYS installation (Windows only)"
            idm
    <*> optional (strOption
             ( long "local-bin-path"
             <> metavar "DIR"
             <> help "Install binaries to DIR"
              ))

-- | Options parser configuration for Docker.
dockerOptsParser :: Bool -> Parser DockerOptsMonoid
dockerOptsParser showOptions =
    DockerOptsMonoid
    <$> pure Nothing
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
                                 "(may specify mutliple times)")))
    <*> maybeStrOption (long (dockerOptName dockerDatabasePathArgName) <>
                        hide <>
                        metavar "PATH" <>
                        help "Location of image usage tracking database")
  where
    dockerOptName optName = dockerCmdName ++ "-" ++ T.unpack optName
    maybeStrOption = optional . option str
    hide = if showOptions
              then idm
              else internal <> hidden

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

-- | Parser for exec command
execOptsParser :: Maybe String -- ^ command
               -> Parser ExecOpts
execOptsParser mcmd =
    ExecOpts
        <$> maybe eoCmdParser pure mcmd
        <*> eoArgsParser
        <*> (eoPlainParser <|>
             ExecOptsEmbellished
                <$> eoEnvSettingsParser
                <*> eoPackagesParser)
  where
    eoCmdParser :: Parser String
    eoCmdParser = strArgument (metavar "CMD")

    eoArgsParser :: Parser [String]
    eoArgsParser = many (strArgument (metavar "-- ARGS (e.g. stack ghc -- X.hs -o x)"))

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

    eoPackagesParser :: Parser [String]
    eoPackagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))

    eoPlainParser :: Parser ExecOptsExtra
    eoPlainParser = flag' ExecOptsPlain
                          (long "plain" <>
                           help "Use an unmodified environment (only useful with Docker)")


-- | Parser for global command-line options.
globalOptsParser :: Bool -> Parser GlobalOpts
globalOptsParser defaultTerminal =
    GlobalOpts <$>
    switch (long Docker.reExecArgName <>
            hidden <>
            internal) <*>
    logLevelOptsParser <*>
    configOptsParser False <*>
    optional abstractResolverOptsParser <*>
    flag
        defaultTerminal
        False
        (long "no-terminal" <>
         help
             "Override terminal detection in the case of running in a false terminal") <*>
    (optional (strOption
        (long "stack-yaml" <>
         metavar "STACK-YAML" <>
         help "Override project stack.yaml file (overrides any STACK_YAML environment variable)")))

initOptsParser :: Parser InitOpts
initOptsParser =
    InitOpts <$> method <*> overwrite <*> fmap not ignoreSubDirs
  where
    ignoreSubDirs = flag False
                         True
                         (long "ignore-subdirs" <>
                         help "Do not search for .cabal files in sub directories")
    overwrite = flag False
                     True
                     (long "force" <>
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
logLevelOptsParser :: Parser LogLevel
logLevelOptsParser =
  fmap parse
       (strOption (long "verbosity" <>
                   metavar "VERBOSITY" <>
                   help "Verbosity: silent, error, warn, info, debug")) <|>
  flag defaultLogLevel
       verboseLevel
       (short 'v' <> long "verbose" <>
        help ("Enable verbose mode: verbosity level \"" <> showLevel verboseLevel <> "\""))
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
abstractResolverOptsParser :: Parser AbstractResolver
abstractResolverOptsParser =
    option readAbstractResolver
        (long "resolver" <>
         metavar "RESOLVER" <>
         help "Override resolver in project file")

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
      <*> flag False
               True
               (long "coverage" <>
               help "Generate a code coverage report")
      <*> flag False
               True
               (long "no-run-tests" <>
                help "Disable running of tests. (Tests will still be built.)")

newOptsParser :: Parser NewOpts
newOptsParser =
    NewOpts <$> templateRepositoryParser
            <*> optional templateParser
            <*> many templateArgParser
            <*> initOptsParser
  where
    templateRepositoryParser = strOption
         $ long "template-url-base"
        <> metavar "URL"
        <> value "raw.githubusercontent.com/commercialhaskell/stack-templates/master/"
    -- TODO(DanBurton): reject argument if it has a colon.
    templateParser = strArgument $ metavar "TEMPLATE"
    -- TODO(DanBurton): reject argument if it doesn't have a colon.
    templateArgParser = strArgument $ metavar "ARG:VAL"
