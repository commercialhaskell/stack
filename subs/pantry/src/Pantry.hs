{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry
  ( -- * Congiruation
    PantryConfig
  , HackageSecurityConfig (..)
  , defaultHackageSecurityConfig
  , HasPantryConfig (..)
  , mkPantryConfig

    -- * Types
  , StaticSHA256
  , CabalHash (..)
  , CabalFileInfo (..)
  , Revision (..)
  , FileSize (..)
  , PackageLocation (..)
  , Archive (..)
  , Repo (..)
  , RepoType (..)
  , PackageIdentifierRevision (..)
  , PackageName
  , Version
  , PackageIdentifier (..)
  , FlagName

    -- ** Cabal helpers
  , parseC
  , displayC
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap

    -- * Cabal files
  , parseCabalFile

    -- * Hackage index
  , updateHackageIndex
  , hackageIndexTarballL
  , getLatestHackageVersion

    -- * FIXME legacy from Stack, to be updated
  , loadFromIndex
  , getPackageVersions
  , fetchPackages
  , unpackPackageIdent
  ) where

import RIO
import RIO.FilePath ((</>))
import qualified RIO.Map as Map
import qualified Data.Map.Strict as Map (mapKeysMonotonic)
import qualified RIO.Set as Set
import qualified RIO.Text as T
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import Pantry.Hackage
import Data.Aeson (ToJSON (..), FromJSON (..), withText, ToJSONKey (..), FromJSONKey (..))
import Data.Aeson.Types (ToJSONKey (..) ,toJSONKeyText)
import qualified Distribution.Text
import Data.List.NonEmpty (NonEmpty)
import Distribution.PackageDescription (GenericPackageDescription, FlagName)
import qualified Data.List.NonEmpty as NE
import Data.Coerce (coerce)

mkPantryConfig
  :: HasLogFunc env
  => FilePath -- ^ pantry root
  -> HackageSecurityConfig
  -> RIO env PantryConfig
mkPantryConfig root hsc = do
  storage <- initStorage $ root </> "pantry.sqlite3"
  ur <- newMVar True
  pure PantryConfig
    { pcHackageSecurity = hsc
    , pcRootDir = root
    , pcStorage = storage
    , pcUpdateRef = ur
    }

defaultHackageSecurityConfig :: HackageSecurityConfig
defaultHackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds =
      [ "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
      , "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
      , "280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833"
      , "2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201"
      , "2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3"
      , "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
      , "772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d"
      , "aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9"
      , "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
      ]
  , hscKeyThreshold = 3
  , hscDownloadPrefix = "https://hackage.haskell.org/"
  }

lookupPackageIdentifierExact
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env (Maybe ByteString)
lookupPackageIdentifierExact name version cfi =
  withStorage $ loadHackageCabalFile name version cfi

loadFromIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env (Either () ByteString)
loadFromIndex name version cfi = do
  mres <- lookupPackageIdentifierExact name version cfi
  case mres of
    Just bs -> return $ Right bs
    -- Update the cache and try again
    Nothing -> do
      updated <- updateHackageIndex $ Just $
                "Didn't see " <>
                display (PackageIdentifierRevision name version cfi) <>
                " in your package indices.\n" <>
                "Updating and trying again."
      if updated
        then loadFromIndex name version cfi
        else do
            pure $ Left ()
            {- FIXME
            fuzzy <- fuzzyLookupCandidates name version cfi
            let suggestions = case fuzzy of
                    FRNameNotFound Nothing -> ""
                    FRNameNotFound (Just cs) ->
                          "Perhaps you meant " <> orSeparated cs <> "?"
                    FRVersionNotFound cs -> "Possible candidates: " <>
                      commaSeparated (NE.map packageIdentifierText cs)
                      <> "."
                    FRRevisionNotFound cs ->
                      "The specified revision was not found.\nPossible candidates: " <>
                      commaSeparated (NE.map (T.pack . packageIdentifierRevisionString) cs)
                      <> "."
            pure (False, Left $ UnknownPackageIdentifiers
                                  (Set.singleton (name, version, cfi))
                                  suggestions)

orSeparated :: NonEmpty Text -> Text
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = T.intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty Text -> Text
commaSeparated = fold . NE.intersperse ", "

data FuzzyResults
  = FRNameNotFound !(Maybe (NonEmpty Text))
  | FRVersionNotFound !(NonEmpty (PackageName, Version))
  | FRRevisionNotFound !(NonEmpty (PackageName, Version, CabalFileInfo))

-- | Given package identifier and package caches, return list of packages
-- with the same name and the same two first version number components found
-- in the caches.
fuzzyLookupCandidates
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env FuzzyResults
fuzzyLookupCandidates name ver _rev =
  case Map.lookup name caches of
    Nothing -> FRNameNotFound $ typoCorrectionCandidates name (PackageCache caches)
    Just m ->
      case Map.lookup ver m of
        Nothing ->
          case NE.nonEmpty $ filter sameMajor $ Map.keys m of
            Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
            Nothing ->
              case NE.nonEmpty $ Map.keys m of
                Nothing -> error "fuzzyLookupCandidates: no versions"
                Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
        Just (_index, _mpd, revisions) ->
          let hashes = concatMap fst $ NE.toList revisions
              pirs = map (PackageIdentifierRevision (PackageIdentifier name ver) . CFIHash Nothing) hashes
           in case NE.nonEmpty pirs of
                Nothing -> error "fuzzyLookupCandidates: no revisions"
                Just pirs' -> FRRevisionNotFound pirs'
  where
    sameMajor v = toMajorVersion v == toMajorVersion ver

-- | Try to come up with typo corrections for given package identifier using
-- package caches. This should be called before giving up, i.e. when
-- 'fuzzyLookupCandidates' cannot return anything.
typoCorrectionCandidates
  :: PackageName
  -> Maybe (NonEmpty Text)
typoCorrectionCandidates name' =
  let name = packageNameText name'
  in  NE.nonEmpty
    . take 10
    . map snd
    . filter (\(distance, _) -> distance < 4)
    . map (\k -> (damerauLevenshtein name (packageNameText k), packageNameText k))
    . Map.keys
    $ cache
-}

-- | Returns the versions of the package available on Hackage.
getPackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Map Version (Map Revision CabalHash))
getPackageVersions = withStorage . loadHackagePackageVersions

-- | Returns the latest version of the given package available from
-- Hackage.
getLatestHackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Maybe (Version, Revision, CabalHash))
getLatestHackageVersion =
  fmap ((fmap fst . Map.maxViewWithKey) >=> go) . getPackageVersions
  where
    go (version, m) = do
      (rev, ch) <- fst <$> Map.maxViewWithKey m
      pure (version, rev, ch)

fetchPackages
  :: (HasPantryConfig env, HasLogFunc env, Foldable f)
  => f PackageIdentifier
  -> RIO env ()
fetchPackages _ = undefined

unpackPackageIdent
  :: (HasPantryConfig env, HasLogFunc env)
  => FilePath -- ^ unpack directory
  -> PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env ()
unpackPackageIdent fp name ver cfi = do
  (_treekey, tree) <- getHackageTarball name ver cfi
  unpackTree fp tree

parseCabalFile
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env GenericPackageDescription
parseCabalFile = undefined

-- | Newtype wrapper for easier JSON integration with Cabal types.
newtype CabalString a = CabalString { unCabalString :: a }
  deriving (Show, Eq, Ord, Typeable)

toCabalStringMap :: Map a v -> Map (CabalString a) v
toCabalStringMap = Map.mapKeysMonotonic CabalString -- FIXME why doesn't coerce work?

unCabalStringMap :: Map (CabalString a) v -> Map a v
unCabalStringMap = Map.mapKeysMonotonic unCabalString -- FIXME why doesn't coerce work?

instance Distribution.Text.Text a => ToJSON (CabalString a) where
  toJSON = toJSON . Distribution.Text.display . unCabalString
instance Distribution.Text.Text a => ToJSONKey (CabalString a) where
  toJSONKey = toJSONKeyText $ displayC . unCabalString

instance forall a. IsCabalString a => FromJSON (CabalString a) where
  parseJSON = withText name $ \t ->
    case Distribution.Text.simpleParse $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
    where
      name = cabalStringName (Nothing :: Maybe a)
instance forall a. IsCabalString a => FromJSONKey (CabalString a)

class Distribution.Text.Text a => IsCabalString a where
  cabalStringName :: proxy a -> String
instance IsCabalString PackageName where
  cabalStringName _ = "package name"
instance IsCabalString Version where
  cabalStringName _ = "version"
instance IsCabalString PackageIdentifier where
  cabalStringName _ = "package identifier"
instance IsCabalString FlagName where
  cabalStringName _ = "flag name"
