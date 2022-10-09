{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.BuildMonoidParser where

import qualified Data.Text as T
import           Distribution.Parsec (eitherParsec)
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Build (splitObjsWarning)
import           Stack.Prelude
import           Stack.Options.BenchParser
import           Stack.Options.TestParser
import           Stack.Options.HaddockParser
import           Stack.Options.Utils
import           Stack.Types.Config.Build

buildOptsMonoidParser :: GlobalOptsContext -> Parser BuildOptsMonoid
buildOptsMonoidParser hide0 =
    BuildOptsMonoid <$> trace' <*> profile <*> noStrip <*>
    libProfiling <*> exeProfiling <*> libStripping <*>
    exeStripping <*> haddock <*> haddockOptsParser hideBool <*>
    openHaddocks <*> haddockDeps <*> haddockInternal <*>
    haddockHyperlinkSource <*> copyBins <*> copyCompilerTool <*>
    preFetch <*> keepGoing <*> keepTmpFiles <*> forceDirty <*>
    tests <*> testOptsParser hideBool <*> benches <*>
    benchOptsParser hideBool <*> reconfigure <*> cabalVerbose <*> splitObjs <*> skipComponents <*>
    interleavedOutput <*> ddumpDir
  where
    hideBool = hide0 /= BuildCmdGlobalOpts
    hide =
        hideMods hideBool
    hideExceptGhci =
        hideMods (hide0 `notElem` [BuildCmdGlobalOpts, GhciCmdGlobalOpts])

    -- These use 'Any' because they are not settable in stack.yaml, so
    -- there is no need for options like --no-profile.
    trace' = Any <$>
        flag
            False
            True
            (long "trace" <>
             help
                 "Enable profiling in libraries, executables, etc. \
                     \for all expressions and generate a backtrace on \
                     \exception" <>
             hideExceptGhci)
    profile = Any <$>
        flag
            False
            True
            (long "profile" <>
             help
                 "profiling in libraries, executables, etc. \
                     \for all expressions and generate a profiling report\
                     \ in tests or benchmarks" <>
             hideExceptGhci)
    noStrip = Any <$>
        flag
             False
             True
             (long "no-strip" <>
              help
                  "Disable DWARF debugging symbol stripping in libraries, \
                      \executables, etc. for all expressions, producing \
                      \larger executables but allowing the use of standard \
                      \debuggers/profiling tools/other utilities that use \
                      \debugging symbols." <>
             hideExceptGhci)
    libProfiling =
        firstBoolFlagsFalse
            "library-profiling"
            "library profiling for TARGETs and all its dependencies"
            hide
    exeProfiling =
        firstBoolFlagsFalse
            "executable-profiling"
            "executable profiling for TARGETs and all its dependencies"
            hide
    libStripping =
        firstBoolFlagsTrue
            "library-stripping"
            "library stripping for TARGETs and all its dependencies"
            hide
    exeStripping =
        firstBoolFlagsTrue
            "executable-stripping"
            "executable stripping for TARGETs and all its dependencies"
            hide
    haddock =
        firstBoolFlagsFalse
            "haddock"
            "generating Haddocks the package(s) in this directory/configuration"
            hide
    openHaddocks =
        firstBoolFlagsFalse
            "open"
            "opening the local Haddock documentation in the browser"
            hide
    haddockDeps =
        firstBoolFlagsNoDefault
            "haddock-deps"
            "building Haddocks for dependencies (default: true if building Haddocks, false otherwise)"
            hide
    haddockInternal =
        firstBoolFlagsFalse
            "haddock-internal"
            "building Haddocks for internal modules (like cabal haddock --internal)"
            hide
    haddockHyperlinkSource =
        firstBoolFlagsTrue
            "haddock-hyperlink-source"
            "building hyperlinked source for Haddock (like haddock --hyperlinked-source)"
            hide
    copyBins =
        firstBoolFlagsFalse
            "copy-bins"
            "copying binaries to local-bin (see 'stack path')"
            hide
    copyCompilerTool =
        firstBoolFlagsFalse
            "copy-compiler-tool"
            "copying binaries of targets to compiler-tools-bin (see 'stack path')"
            hide
    keepGoing =
        firstBoolFlagsNoDefault
            "keep-going"
            "continue running after a step fails (default: false for build, true for test/bench)"
            hide
    keepTmpFiles =
        firstBoolFlagsFalse
            "keep-tmp-files"
            "keep intermediate files and build directories"
            hide
    preFetch =
        firstBoolFlagsFalse
            "prefetch"
             "Fetch packages necessary for the build immediately, useful with --dry-run"
             hide
    forceDirty =
        firstBoolFlagsFalse
            "force-dirty"
            "Force treating all local packages as having dirty files (useful \
            \for cases where Stack can't detect a file change"
            hide
    tests =
        firstBoolFlagsFalse
            "test"
            "testing the package(s) in this directory/configuration"
            hideExceptGhci
    benches =
        firstBoolFlagsFalse
            "bench"
            "benchmarking the package(s) in this directory/configuration"
            hideExceptGhci
    reconfigure =
        firstBoolFlagsFalse
             "reconfigure"
             "Perform the configure step even if unnecessary. Useful in some corner cases with custom Setup.hs files"
            hide
    cabalVerbose = cabalVerbosityOptsParser hideBool
    splitObjs =
        firstBoolFlagsFalse
            "split-objs"
            ("Enable split-objs, to reduce output size (at the cost of build time). " ++ splitObjsWarning)
            hide
    skipComponents = many
        (fmap
            T.pack
            (strOption
                (long "skip" <>
                 help "Skip given component (can be specified multiple times)" <>
                 hide)))
    interleavedOutput =
        firstBoolFlagsTrue
            "interleaved-output"
            "Print concurrent GHC output to the console with a prefix for the package name"
            hide
    ddumpDir =
        optionalFirst
            (strOption
                (long "ddump-dir" <>
                 help "Specify output ddump-files" <>
                 hide))

-- | Parser for Cabal verbosity options
cabalVerbosityOptsParser :: Bool -> Parser (First CabalVerbosity)
cabalVerbosityOptsParser hide =
  cabalVerbosityParser hide <|> cabalVerboseParser hide

-- | Parser for Cabal verbosity option
cabalVerbosityParser :: Bool -> Parser (First CabalVerbosity)
cabalVerbosityParser hide =
  let pCabalVerbosity = option (eitherReader eitherParsec)
         (  long "cabal-verbosity"
         <> metavar "VERBOSITY"
         <> help "Cabal verbosity (accepts Cabal's numerical and extended syntax)"
         <> hideMods hide)
  in  First . Just <$> pCabalVerbosity

-- | Parser for the Cabal verbose flag, retained for backward compatibility
cabalVerboseParser :: Bool -> Parser (First CabalVerbosity)
cabalVerboseParser hide =
  let pVerboseFlag = firstBoolFlagsFalse
                       "cabal-verbose"
                       "Ask Cabal to be verbose in its output"
                       (hideMods hide)
  in  toFirstCabalVerbosity <$> pVerboseFlag
