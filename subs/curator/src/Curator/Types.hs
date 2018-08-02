{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Curator.Types
  ( Constraints (..)
  , PackageConstraints (..)
  , PackageSource (..)
  , HackageSource (..)
  , Maintainer
  , Revisions (..)
  , ComponentAction (..)
  ) where

import RIO
import Pantry
import Distribution.Types.VersionRange (VersionRange)
import Data.Yaml
import qualified RIO.Map as Map
import qualified RIO.Set as Set

type Maintainer = Text

data Constraints = Constraints
  { consGhcVersion :: !Version
  , consPackages :: !(Map PackageName PackageConstraints)
  }
  deriving Show

instance ToJSON Constraints where
  toJSON c = object
    [ "ghc-version" .= CabalString (consGhcVersion c)
    , "packages" .= toCabalStringMap (consPackages c)
    ]

data PackageConstraints = PackageConstraints
  { pcMaintainers :: !(Set Maintainer)
  , pcSource :: !PackageSource
  , pcFlags :: !(Map FlagName Bool)
  , pcSkipBuild :: !Bool
  , pcTests :: !ComponentAction
  , pcBenchmarks :: !ComponentAction
  , pcHaddock :: !ComponentAction
  , pcNonParallelBuild :: !Bool
  }
  deriving Show

instance ToJSON PackageConstraints where
  toJSON pc = object $ concat
    [ if Set.null (pcMaintainers pc)
        then []
        else ["maintainers" .= pcMaintainers pc]
    , ["source" .= pcSource pc]
    , if Map.null (pcFlags pc)
        then []
        else ["flags" .= toCabalStringMap (pcFlags pc)]
    , if pcSkipBuild pc then ["skip-build" .= True] else []
    , case pcTests pc of
        CAExpectSuccess -> []
        x -> ["tests" .= x]
    , case pcBenchmarks pc of
        CAExpectSuccess -> []
        x -> ["benchmarks" .= x]
    , case pcHaddock pc of
        CAExpectSuccess -> []
        x -> ["haddock" .= x]
    , if pcNonParallelBuild pc
        then ["non-parallel-build" .= True]
        else []
    ]

data PackageSource
  = PSHackage !HackageSource
  deriving Show
instance ToJSON PackageSource where
  toJSON (PSHackage hs) = object $ ("type" .= ("hackage" :: Text)) : hsToPairs hs

data HackageSource = HackageSource
  { hsRange :: !(Maybe VersionRange)
  , hsRequiredLatest :: !(Maybe Version)
  -- ^ required latest version, for tell-me-when-its-released
  , hsRevisions :: !Revisions
  }
  deriving Show

hsToPairs :: HackageSource -> [(Text, Value)]
hsToPairs hs = concat
  [ maybe [] (\range -> ["range" .= CabalString range]) (hsRange hs)
  , maybe [] (\v -> ["required-latest" .= CabalString v]) (hsRequiredLatest hs)
  , case hsRevisions hs of
      NoRevisions -> [] -- the only sane default, of course
      UseRevisions -> ["revisions" .= UseRevisions]
  ]

data ComponentAction
  = CAExpectSuccess
  | CAExpectFailure
  | CASkip
  deriving Show
instance ToJSON ComponentAction where
  toJSON CAExpectSuccess = toJSON ("expect-success" :: Text)
  toJSON CAExpectFailure = toJSON ("expect-failure" :: Text)
  toJSON CASkip = toJSON ("skip" :: Text)

data Revisions
  = UseRevisions
  | NoRevisions
  deriving Show

instance ToJSON Revisions where
  toJSON UseRevisions = toJSON ("use-revisions" :: Text)
  toJSON NoRevisions = toJSON ("no-revisions" :: Text)
