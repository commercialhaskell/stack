{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.BenchParser
 ( benchOptsParser
 ) where

import           Options.Applicative
                   ( Parser, flag', help, long, metavar, strOption )
import           Options.Applicative.Builder.Extra ( optionalFirst )
import           Stack.Prelude
import           Stack.Options.Utils ( hideMods )
import           Stack.Types.BuildOpts ( BenchmarkOptsMonoid (..) )

-- | Parser for bench arguments.
-- FIXME hiding options
benchOptsParser :: Bool -> Parser BenchmarkOptsMonoid
benchOptsParser hide0 = BenchmarkOptsMonoid
  <$> optionalFirst (strOption
        (  long "benchmark-arguments"
        <> long "ba"
        <> metavar "BENCH_ARGS"
        <> help "Forward BENCH_ARGS to the benchmark suite. Supports templates \
                \from 'cabal bench'."
        <> hide
        ))
  <*> optionalFirst (flag' True
        (  long "no-run-benchmarks"
        <> help "Disable running of benchmarks. (Benchmarks will still be \
                \built.)"
        <> hide
        ))
 where
  hide = hideMods hide0
