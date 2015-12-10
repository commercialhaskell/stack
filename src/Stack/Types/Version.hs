{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Versions for packages.

module Stack.Types.Version
  (Version
  ,Cabal.VersionRange -- TODO in the future should have a newtype wrapper
  ,VersionCheck(..)
  ,versionParser
  ,parseVersion
  ,parseVersionFromString
  ,versionString
  ,versionText
  ,toCabalVersion
  ,fromCabalVersion
  ,mkVersion
  ,versionRangeText
  ,withinRange
  ,Stack.Types.Version.intersectVersionRanges
  ,toMajorVersion
  ,latestApplicableVersion
  ,checkVersion
  ,nextMajorVersion)
  where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Catch
import           Data.Aeson.Extended
import           Data.Attoparsec.Text
import           Data.Binary.VersionTagged (Binary, HasStructuralInfo)
import           Data.Data
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Binary ()
import           Data.Vector.Binary ()
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Distribution.Text (disp)
import qualified Distribution.Version as Cabal
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude -- Fix warning: Word in Prelude from base-4.8.
import           Text.PrettyPrint (render)

-- | A parse fail.
data VersionParseFail =
  VersionParseFail Text
  deriving (Typeable)
instance Exception VersionParseFail
instance Show VersionParseFail where
    show (VersionParseFail bs) = "Invalid version: " ++ show bs

-- | A package version.
newtype Version =
  Version {unVersion :: Vector Word}
  deriving (Eq,Ord,Typeable,Data,Generic,Binary,NFData)
instance HasStructuralInfo Version

instance Hashable Version where
  hashWithSalt i = hashWithSalt i . V.toList . unVersion

instance Lift Version where
  lift (Version n) =
    appE (conE 'Version)
         (appE (varE 'V.fromList)
               (listE (map (litE . IntegerL . fromIntegral)
                           (V.toList n))))

instance Show Version where
  show (Version v) =
    intercalate "."
                (map show (V.toList v))

instance ToJSON Version where
  toJSON = toJSON . versionText
instance FromJSON Version where
  parseJSON j =
    do s <- parseJSON j
       case parseVersionFromString s of
         Nothing ->
           fail ("Couldn't parse package version: " ++ s)
         Just ver -> return ver
instance FromJSON a => FromJSON (Map Version a) where
    parseJSON val = do
        m <- parseJSON val
        fmap Map.fromList $ mapM go $ Map.toList m
      where
        go (k, v) = do
            k' <- either (fail . show) return $ parseVersionFromString k
            return (k', v)

-- | Attoparsec parser for a package version.
versionParser :: Parser Version
versionParser =
  do ls <- ((:) <$> num <*> many num')
     let !v = V.fromList ls
     return (Version v)
  where num = decimal
        num' = point *> num
        point = satisfy (== '.')

-- | Convenient way to parse a package version from a 'Text'.
parseVersion :: MonadThrow m => Text -> m Version
parseVersion x = go x
  where go =
          either (const (throwM (VersionParseFail x))) return .
          parseOnly (versionParser <* endOfInput)

-- | Migration function.
parseVersionFromString :: MonadThrow m => String -> m Version
parseVersionFromString =
  parseVersion . T.pack

-- | Get a string representation of a package version.
versionString :: Version -> String
versionString (Version v) =
  intercalate "."
              (map show (V.toList v))

-- | Get a string representation of a package version.
versionText :: Version -> Text
versionText (Version v) =
  T.intercalate
    "."
    (map (T.pack . show)
         (V.toList v))

-- | Convert to a Cabal version.
toCabalVersion :: Version -> Cabal.Version
toCabalVersion (Version v) =
  Cabal.Version (map fromIntegral (V.toList v)) []

-- | Convert from a Cabal version.
fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion (Cabal.Version vs _) =
  let !v = V.fromList (map fromIntegral vs)
  in Version v

-- | Make a package version.
mkVersion :: String -> Q Exp
mkVersion s =
  case parseVersionFromString s of
    Nothing -> error ("Invalid package version: " ++ show s)
    Just pn -> [|pn|]

-- | Display a version range
versionRangeText :: Cabal.VersionRange -> Text
versionRangeText = T.pack . render . disp

-- | Check if a version is within a version range.
withinRange :: Version -> Cabal.VersionRange -> Bool
withinRange v r = toCabalVersion v `Cabal.withinRange` r

-- | A modified intersection which also simplifies, for better display.
intersectVersionRanges :: Cabal.VersionRange -> Cabal.VersionRange -> Cabal.VersionRange
intersectVersionRanges x y = Cabal.simplifyVersionRange $ Cabal.intersectVersionRanges x y

-- | Returns the first two components, defaulting to 0 if not present
toMajorVersion :: Version -> Version
toMajorVersion  (Version v) =
    case V.length v of
        0 -> Version (V.fromList [0,        0])
        1 -> Version (V.fromList [V.head v, 0])
        _ -> Version (V.fromList [V.head v, v V.! 1])

-- | Given a version range and a set of versions, find the latest version from
-- the set that is within the range.
latestApplicableVersion :: Cabal.VersionRange -> Set Version -> Maybe Version
latestApplicableVersion r = listToMaybe . filter (`withinRange` r) . Set.toDescList

-- | Get the next major version number for the given version
nextMajorVersion :: Version -> Version
nextMajorVersion (Version v) =
  case  V.length v of
    0 -> Version (V.fromList [0,        1])
    1 -> Version (V.fromList [V.head v, 1])
    _ -> Version (V.fromList [V.head v, (v V.! 1) + 1])

data VersionCheck
    = MatchMinor
    | MatchExact
    | NewerMinor
    deriving (Show, Eq, Ord)
instance ToJSON VersionCheck where
    toJSON MatchMinor = String "match-minor"
    toJSON MatchExact = String "match-exact"
    toJSON NewerMinor = String "newer-minor"
instance FromJSON VersionCheck where
    parseJSON = withText expected $ \t ->
        case t of
            "match-minor" -> return MatchMinor
            "match-exact" -> return MatchExact
            "newer-minor" -> return NewerMinor
            _ -> fail ("Expected " ++ expected ++ ", but got " ++ show t)
      where
        expected = "VersionCheck value (match-minor, match-exact, or newer-minor)"

checkVersion :: VersionCheck -> Version -> Version -> Bool
checkVersion check (Version wanted) (Version actual) =
    case check of
        MatchMinor -> V.and (V.take 3 matching)
        MatchExact -> V.length wanted == V.length actual && V.and matching
        NewerMinor -> V.and (V.take 2 matching) && newerMinor
  where
    matching = V.zipWith (==) wanted actual
    newerMinor =
        case (wanted V.!? 2, actual V.!? 2) of
            (Nothing, _) -> True
            (Just _, Nothing) -> False
            (Just w, Just a) -> a >= w
