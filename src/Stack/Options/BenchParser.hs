{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.BenchParser
 ( benchOptsParser
 ) where

import           Options.Applicative ( Parser, help, long, metavar, strOption )
import           Options.Applicative.Builder.Extra
                   ( firstBoolFlagsTrue, optionalFirst )
import           Stack.Prelude
import           Stack.Options.Utils ( hideMods )
import           Stack.Types.BuildOptsMonoid ( BenchmarkOptsMonoid (..) )

-- | Parser for bench arguments.
-- FIXME hiding options
benchOptsParser :: Bool -> Parser BenchmarkOptsMonoid
benchOptsParser hide0 = BenchmarkOptsMonoid
  <$> optionalFirst (strOption
        (  long "benchmark-arguments"
        <> long "ba"
        <> metavar "BENCH_ARGS"
        <> help "Arguments passed to the benchmarks. Supports path variables \
                \provided by the Cabal build system."
        <> hide
        ))
  <*> firstBoolFlagsTrue
        "run-benchmarks"
        "running of targeted benchmarks."
        hide
 where
  hide = hideMods hide0
