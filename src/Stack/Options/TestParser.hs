{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.TestParser
  ( testOptsParser
  ) where

import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Stack.Options.Utils
import           Stack.Prelude
import           Stack.Types.Config

-- | Parser for test arguments.
-- FIXME hide args
testOptsParser :: Bool -> Parser TestOptsMonoid
testOptsParser hide0 = TestOptsMonoid
  <$> firstBoolFlagsTrue
        "rerun-tests"
        "running already successful tests."
        hide
  <*> fmap concat (many (argsOption
        (  long "test-arguments"
        <> long "ta"
        <> metavar "TEST_ARGS"
        <> help "Arguments passed in to the test suite program."
        <> hide
        )))
  <*> optionalFirstFalse (flag' True
        (  long "coverage"
        <> help "Generate a code coverage report."
        <> hide
        ))
  <*> optionalFirstFalse (flag' True
        (  long "no-run-tests"
        <> help "Disable running of tests. (Tests will still be built.)"
        <> hide
        ))
  <*> optionalFirst (option (fmap Just auto)
        (  long "test-suite-timeout"
        <> help "Maximum test suite run time in seconds."
        <> hide
        ))
  <*> firstBoolFlagsTrue
        "tests-allow-stdin"
        "allow standard input in test executables."
        hide
 where
  hide = hideMods hide0
