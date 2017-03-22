module Stack.Options.BuildMonoidParser where

import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Build                       (splitObjsWarning)
import           Stack.Options.BenchParser
import           Stack.Options.TestParser
import           Stack.Options.HaddockParser
import           Stack.Options.Utils
import           Stack.Types.Config.Build

buildOptsMonoidParser :: GlobalOptsContext -> Parser BuildOptsMonoid
buildOptsMonoidParser hide0 =
    BuildOptsMonoid <$> trace <*> profile <*> noStrip <*>
    libProfiling <*> exeProfiling <*> libStripping <*>
    exeStripping <*> haddock <*> haddockOptsParser hideBool <*>
    openHaddocks <*> haddockDeps <*> haddockInternal <*> copyBins <*>
    preFetch <*> keepGoing <*> forceDirty <*> tests <*>
    testOptsParser hideBool <*> benches <*> benchOptsParser hideBool <*>
    reconfigure <*> cabalVerbose <*> splitObjs
  where
    hideBool = hide0 /= BuildCmdGlobalOpts
    hide =
        hideMods hideBool
    hideExceptGhci =
        hideMods (hide0 `notElem` [BuildCmdGlobalOpts, GhciCmdGlobalOpts])

    trace =
        firstBoolFlags
            "trace"
            "Enable profiling in libraries, executables, etc. \
                \for all expressions and generate a backtrace on \
                \exception"
            hideExceptGhci
    profile =
        firstBoolFlags
            "profile"
            "profiling in libraries, executables, etc. \
                \for all expressions and generate a profiling report\
                \ in tests or benchmarks"
            hideExceptGhci
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
        firstBoolFlags
            "library-profiling"
            "library profiling for TARGETs and all its dependencies"
            hide
    exeProfiling =
        firstBoolFlags
            "executable-profiling"
            "executable profiling for TARGETs and all its dependencies"
            hide
    libStripping =
        firstBoolFlags
            "library-stripping"
            "library stripping for TARGETs and all its dependencies"
            hide
    exeStripping =
        firstBoolFlags
            "executable-stripping"
            "executable stripping for TARGETs and all its dependencies"
            hide
    haddock =
        firstBoolFlags
            "haddock"
            "generating Haddocks the package(s) in this directory/configuration"
            hide
    openHaddocks =
        firstBoolFlags
            "open"
            "opening the local Haddock documentation in the browser"
            hide
    haddockDeps =
        firstBoolFlags "haddock-deps" "building Haddocks for dependencies" hide
    haddockInternal =
        firstBoolFlags
            "haddock-internal"
            "building Haddocks for internal modules (like cabal haddock --internal)"
            hide
    copyBins =
        firstBoolFlags
            "copy-bins"
            "copying binaries to the local-bin-path (see 'stack path')"
            hide
    keepGoing =
        firstBoolFlags
            "keep-going"
            "continue running after a step fails (default: false for build, true for test/bench)"
            hide
    preFetch =
        firstBoolFlags
            "prefetch"
             "Fetch packages necessary for the build immediately, useful with --dry-run"
             hide
    forceDirty =
        firstBoolFlags
            "force-dirty"
            "Force treating all local packages as having dirty files (useful for cases where stack can't detect a file change"
            hide
    tests =
        firstBoolFlags
            "test"
            "testing the package(s) in this directory/configuration"
            hideExceptGhci
    benches =
        firstBoolFlags
            "bench"
            "benchmarking the package(s) in this directory/configuration"
            hideExceptGhci
    reconfigure =
        firstBoolFlags
             "reconfigure"
             "Perform the configure step even if unnecessary. Useful in some corner cases with custom Setup.hs files"
            hide
    cabalVerbose =
        firstBoolFlags
            "cabal-verbose"
            "Ask Cabal to be verbose in its output"
            hide
    splitObjs =
        firstBoolFlags
            "split-objs"
            ("Enable split-objs, to reduce output size (at the cost of build time). " ++ splitObjsWarning)
            hide
