{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Types.BuildOptsCLI
Description : Configuration options for building from the command line only.
License     : BSD-3-Clause

Configuration options for building from the command line only.
-}

module Stack.Types.BuildOptsCLI
  ( BuildOptsCLI (..)
  , defaultBuildOptsCLI
  , ApplyCLIFlag (..)
  , BuildSubset (..)
  , FileWatchOpts (..)
  , BuildCommand (..)
  , boptsCLIAllProgOptions
  , boptsCLIFlagsByName
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Stack.Prelude

-- | Build options that are specified from the CLI and not specified as
-- non-project specific configuration options under the build key.
data BuildOptsCLI = BuildOptsCLI
  { targetsCLI :: ![Text]
  , dryrun :: !Bool
  , ghcOptions :: ![Text]
  , progsOptions :: ![(Text, [Text])]
  , flags :: !(Map ApplyCLIFlag (Map FlagName Bool))
  , allowNewer :: !(First Bool)
  , buildSubset :: !BuildSubset
  , fileWatch :: !FileWatchOpts
  , watchAll :: !Bool
  , exec :: ![(String, [String])]
  , onlyConfigure :: !Bool
  , command :: !BuildCommand
  , initialBuildSteps :: !Bool
  }
  deriving Show

defaultBuildOptsCLI ::BuildOptsCLI
defaultBuildOptsCLI = BuildOptsCLI
  { targetsCLI = []
  , dryrun = False
  , flags = Map.empty
  , allowNewer = mempty
  , ghcOptions = []
  , progsOptions = []
  , buildSubset = BSAll
  , fileWatch = NoFileWatch
  , watchAll = False
  , exec = []
  , onlyConfigure = False
  , command = Build
  , initialBuildSteps = False
  }

-- | How to apply a CLI flag
data ApplyCLIFlag
  = ACFAllProjectPackages
    -- ^ Apply to all project packages which have such a flag name available.
  | ACFByName !PackageName
    -- ^ Apply to the specified package only.
  deriving (Eq, Ord, Show)

-- | Which subset of packages to build
data BuildSubset
  = BSAll
  | BSOnlySnapshot
    -- ^ Only install packages in the snapshot database, skipping
    -- packages intended for the local database.
  | BSOnlyDependencies
  | BSOnlyLocals
    -- ^ Refuse to build anything in the snapshot database, see
    -- https://github.com/commercialhaskell/stack/issues/5272
  deriving (Show, Eq)

data FileWatchOpts
  = NoFileWatch
  | FileWatch
  | FileWatchPoll
  deriving (Show, Eq)

-- | Command sum type for conditional arguments.
data BuildCommand
  = Build
  | Test
  | Haddock
  | Bench
  | Install
  deriving (Eq, Show)

-- | Generate a list of @--PROG-option=<argument>@ arguments for all PROGs.

-- At the command line, --PROG-option="<argument>" is received as
-- --PROG-option=<argument> (without quotes). However, with the process library,
-- what is received is --PROG-option="<argument>" (with quotes), which is NOT
-- what is required.
boptsCLIAllProgOptions :: BuildOptsCLI -> [Text]
boptsCLIAllProgOptions boptsCLI =
  concatMap progOptionArgs boptsCLI.progsOptions
 where
  -- Generate a list of --PROG-option=<argument> arguments for a PROG.
  progOptionArgs :: (Text, [Text]) -> [Text]
  progOptionArgs (prog, opts) = map progOptionArg opts
   where
    -- Generate a --PROG-option=<argument> argument for a PROG and option.
    progOptionArg :: Text -> Text
    progOptionArg opt = T.concat
      [ "--"
      , prog
      , "-option="
      , opt
      ]

-- | Only flags set via 'ACFByName'
boptsCLIFlagsByName :: BuildOptsCLI -> Map PackageName (Map FlagName Bool)
boptsCLIFlagsByName = Map.fromList . mapMaybe go . Map.toList . (.flags)
 where
  go (ACFAllProjectPackages, _) = Nothing
  go (ACFByName name, flags) = Just (name, flags)
