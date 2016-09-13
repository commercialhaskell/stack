{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.BenchParser where

import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Options.Utils
import           Stack.Types.Config

-- | Parser for bench arguments.
-- FIXME hiding options
benchOptsParser :: Bool -> Parser BenchmarkOptsMonoid
benchOptsParser hide0 = BenchmarkOptsMonoid
        <$> optionalFirst (strOption (long "benchmark-arguments" <>
                                 metavar "BENCH_ARGS" <>
                                 help ("Forward BENCH_ARGS to the benchmark suite. " <>
                                       "Supports templates from `cabal bench`") <>
                                 hide))
        <*> optionalFirst (switch (long "no-run-benchmarks" <>
                          help "Disable running of benchmarks. (Benchmarks will still be built.)" <>
                             hide))
   where hide = hideMods hide0
