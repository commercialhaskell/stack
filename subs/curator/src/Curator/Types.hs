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
  , Component (..)
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
  , consGithubUsers :: !(Map Text (Set Text))
  }
  deriving Show

instance ToJSON Constraints where
  toJSON c = object
    [ "ghc-version" .= CabalString (consGhcVersion c)
    , "packages" .= toCabalStringMap (consPackages c)
    , "github-users" .= consGithubUsers c
    ]
instance FromJSON Constraints where
  parseJSON = withObject "Constraints" $ \o -> Constraints
    <$> fmap unCabalString (o .: "ghc-version")
    <*> fmap unCabalStringMap (o .: "packages")
    <*> o .: "github-users"

data PackageConstraints = PackageConstraints
  { pcMaintainers :: !(Set Maintainer)
  , pcSource :: !PackageSource
  , pcFlags :: !(Map FlagName Bool)
  , pcSkipBuild :: !Bool
  , pcTests :: !ComponentAction
  , pcBenchmarks :: !ComponentAction
  , pcHaddock :: !ComponentAction
  , pcNonParallelBuild :: !Bool
  , pcHide :: !Bool
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
    , if pcHide pc
        then ["hide" .= True]
        else []
    ]
instance FromJSON PackageConstraints where
  parseJSON = withObject "PackageConstraints" $ \o -> PackageConstraints
    <$> o .:? "maintainers" .!= mempty
    <*> o .: "source"
    <*> fmap unCabalStringMap (o .:? "flags" .!= mempty)
    <*> o .:? "skip-build" .!= False
    <*> o .:? "tests" .!= CAExpectSuccess
    <*> o .:? "benchmarks" .!= CAExpectSuccess
    <*> o .:? "haddock" .!= CAExpectSuccess
    <*> o .:? "non-parallel-build" .!= False
    <*> o .:? "hide" .!= False

data PackageSource
  = PSHackage !HackageSource
  deriving Show
instance ToJSON PackageSource where
  toJSON (PSHackage hs) = object $ ("type" .= ("hackage" :: Text)) : hsToPairs hs
instance FromJSON PackageSource where
  parseJSON = withObject "PackageSource" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "hackage" -> PSHackage <$> hackage o
      _ -> fail $ "Invalid type: " ++ show typ
    where
      hackage o = HackageSource
        <$> fmap (fmap unCabalString) (o .:? "range")
        <*> fmap (fmap unCabalString) (o .:? "required-latest")
        <*> o .:? "revisions" .!= NoRevisions

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
  deriving (Eq, Show)
instance ToJSON ComponentAction where
  toJSON CAExpectSuccess = toJSON ("expect-success" :: Text)
  toJSON CAExpectFailure = toJSON ("expect-failure" :: Text)
  toJSON CASkip = toJSON ("skip" :: Text)
instance FromJSON ComponentAction where
  parseJSON = withText "ComponentAction" $ \t ->
    case t of
      "expect-success" -> pure CAExpectSuccess
      "expect-failure" -> pure CAExpectFailure
      "skip" -> pure CASkip
      _ -> fail $ "Invalid component action: " ++ show t

data Revisions
  = UseRevisions
  | NoRevisions
  deriving Show

instance ToJSON Revisions where
  toJSON UseRevisions = toJSON ("use-revisions" :: Text)
  toJSON NoRevisions = toJSON ("no-revisions" :: Text)
instance FromJSON Revisions where
  parseJSON = withText "Revisions" $ \t ->
    case t of
      "use-revisions" -> pure UseRevisions
      "no-revisions" -> pure NoRevisions
      _ -> fail $ "Invalid revisions: " ++ show t

data Component
  = CompLibrary
  | CompExecutable
  | CompTestSuite
  | CompBenchmark
  deriving Eq
