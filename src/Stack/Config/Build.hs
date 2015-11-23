{-# LANGUAGE RecordWildCards #-}

-- | Build configuration
module Stack.Config.Build where

import           Data.Maybe          (fromMaybe)
import           Stack.Types

-- | Interprets BuildOptsMonoid options.
buildOptsFromMonoid ::  BuildOptsMonoid -> BuildOpts
buildOptsFromMonoid BuildOptsMonoid{..} =
    defaultBuildOpts
        { boptsLibProfile = fromMaybe
            (boptsLibProfile defaultBuildOpts)
            buildMonoidLibProfile
        , boptsExeProfile = fromMaybe
            (boptsExeProfile defaultBuildOpts)
            buildMonoidExeProfile
        , boptsHaddock = fromMaybe
            (boptsHaddock defaultBuildOpts)
            buildMonoidHaddock
        , boptsInstallExes = fromMaybe
            (boptsInstallExes defaultBuildOpts)
            buildMonoidInstallExes
        , boptsPreFetch = fromMaybe
            (boptsPreFetch defaultBuildOpts)
            buildMonoidPreFetch
        , boptsForceDirty = fromMaybe
            (boptsForceDirty defaultBuildOpts)
            buildMonoidForceDirty
        , boptsTests = fromMaybe
            (boptsTests defaultBuildOpts)
            buildMonoidTests
        , boptsBenchmarks = fromMaybe
            (boptsBenchmarks defaultBuildOpts)
            buildMonoidBenchmarks
        , boptsReconfigure = fromMaybe
            (boptsReconfigure defaultBuildOpts)
            buildMonoidReconfigure
        , boptsCabalVerbose = fromMaybe
            (boptsCabalVerbose defaultBuildOpts)
            buildMonoidCabalVerbose
        }
