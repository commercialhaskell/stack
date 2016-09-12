module Stack.Options.TestParser where

import           Data.Maybe
import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Stack.Options.Utils
import           Stack.Types.Config

-- | Parser for test arguments.
-- FIXME hide args
testOptsParser :: Bool -> Parser TestOptsMonoid
testOptsParser hide0 =
    TestOptsMonoid
        <$> firstBoolFlags
                "rerun-tests"
                "running already successful tests"
                hide
        <*> fmap
                (fromMaybe [])
                (optional
                    (argsOption
                        (long "test-arguments" <>
                         metavar "TEST_ARGS" <>
                         help "Arguments passed in to the test suite program" <>
                         hide)))
        <*> optionalFirst
                (switch
                    (long "coverage" <>
                     help "Generate a code coverage report" <>
                     hide))
        <*> optionalFirst
                (switch
                    (long "no-run-tests" <>
                     help "Disable running of tests. (Tests will still be built.)" <>
                     hide))
   where hide = hideMods hide0
