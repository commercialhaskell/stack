{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.TestParser
License     : BSD-3-Clause
-}

module Stack.Options.TestParser
  ( testOptsParser
  ) where

import           Options.Applicative
                   ( Mod, Parser, auto, flag', help, long, metavar, option )
import           Options.Applicative.Args ( argsOption )
import           Options.Applicative.Builder.Extra
                   ( firstBoolFlagsTrue, optionalFirst, optionalFirstFalse )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude
import           Stack.Types.BuildOptsMonoid ( TestOptsMonoid (..) )

-- | Parser for test arguments.
-- FIXME hide args
testOptsParser :: Bool -> Parser TestOptsMonoid
testOptsParser hide0 = TestOptsMonoid
  <$> firstBoolFlagsTrue
        "rerun-tests"
        "running already successful test suites."
        hide
  <*> fmap concat (many (argsOption
        (  long "test-arguments"
        <> long "ta"
        <> metavar "TEST_ARGS"
        <> help "Arguments passed to the test suites."
        <> hide
        )))
  <*> optionalFirstFalse (flag' True
        (  long "coverage"
        <> help "Generate a code coverage report."
        <> hide
        ))
  <*> firstBoolFlagsTrue
        "run-tests"
        "running of targeted test suites."
        hide
  <*> optionalFirst (option (fmap Just auto)
        (  long "test-suite-timeout"
        <> help "Maximum test suite run time in seconds."
        <> hide
        ))
  <*> firstBoolFlagsTrue
        "tests-allow-stdin"
        "allow standard input in test suites."
        hide
 where
  hide :: Mod f a
  hide = hideMods hide0
