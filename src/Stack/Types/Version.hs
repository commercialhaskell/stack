{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

-- | Versions for packages.

module Stack.Types.Version
  (Version
  ,Cabal.VersionRange -- TODO in the future should have a newtype wrapper
  ,IntersectingVersionRange(..)
  ,VersionCheck(..)
  ,parseVersion
  ,parseVersionFromString
  ,mkVersion
  ,versionRangeText
  ,withinRange
  ,Stack.Types.Version.intersectVersionRanges
  ,toMajorVersion
  ,latestApplicableVersion
  ,checkVersion
  ,nextMajorVersion
  ,UpgradeTo(..)
  ,minorVersion
  ,stackVersion
  ,stackMinorVersion)
  where

import           Stack.Prelude hiding (Vector)
import           Pantry
import           Data.Aeson.Extended
import           Data.Hashable (Hashable (..))
import           Data.List
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Distribution.Text (disp)
import qualified Distribution.Version as Cabal
import           Distribution.Version (Version, versionNumbers, withinRange)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Paths_stack as Meta
import           Pantry
import           Text.PrettyPrint (render)

-- | A parse fail.
newtype VersionParseFail = VersionParseFail Text
  deriving (Typeable)
instance Exception VersionParseFail
instance Show VersionParseFail where
    show (VersionParseFail bs) = "Invalid version: " ++ show bs

-- | A Package upgrade; Latest or a specific version.
data UpgradeTo = Specific Version | Latest deriving (Show)

{- FIXME
instance ToJSON Version where
  toJSON = toJSON . versionText
instance FromJSON Version where
  parseJSON j =
    do s <- parseJSON j
       case parseVersionFromString s of
         Nothing ->
           fail ("Couldn't parse package version: " ++ s)
         Just ver -> return ver
instance FromJSONKey Version where
  fromJSONKey = FromJSONKeyTextParser $ \k ->
    either (fail . show) return $ parseVersion k
-}

newtype IntersectingVersionRange =
    IntersectingVersionRange { getIntersectingVersionRange :: Cabal.VersionRange }
    deriving Show

instance Semigroup IntersectingVersionRange where
    IntersectingVersionRange l <> IntersectingVersionRange r =
        IntersectingVersionRange (l `Cabal.intersectVersionRanges` r)

instance Monoid IntersectingVersionRange where
    mempty = IntersectingVersionRange Cabal.anyVersion
    mappend = (<>)

-- | Convenient way to parse a package version from a 'Text'.
parseVersion :: MonadThrow m => Text -> m Version
parseVersion = parseVersionFromString . T.unpack

-- | Migration function.
parseVersionFromString :: MonadThrow m => String -> m Version
parseVersionFromString str =
  case parseC str of
    Nothing -> throwM $ VersionParseFail $ T.pack str
    Just v -> pure v

-- | Make a package version.
mkVersion :: String -> Q Exp
mkVersion s =
  case parseVersionFromString s of
    Left e -> qRunIO $ throwIO e
    Right (versionNumbers -> vs) -> [|Cabal.mkVersion vs|]

-- | Display a version range
versionRangeText :: Cabal.VersionRange -> Text
versionRangeText = T.pack . render . disp

-- | A modified intersection which also simplifies, for better display.
intersectVersionRanges :: Cabal.VersionRange -> Cabal.VersionRange -> Cabal.VersionRange
intersectVersionRanges x y = Cabal.simplifyVersionRange $ Cabal.intersectVersionRanges x y

-- | Returns the first two components, defaulting to 0 if not present
toMajorVersion :: Version -> Version
toMajorVersion v =
  case versionNumbers v of
    []    -> Cabal.mkVersion [0, 0]
    [a]   -> Cabal.mkVersion [a, 0]
    a:b:_ -> Cabal.mkVersion [a, b]

-- | Given a version range and a set of versions, find the latest version from
-- the set that is within the range.
latestApplicableVersion :: Cabal.VersionRange -> Set Version -> Maybe Version
latestApplicableVersion r = listToMaybe . filter (`withinRange` r) . Set.toDescList

-- | Get the next major version number for the given version
nextMajorVersion :: Version -> Version
nextMajorVersion v =
  case versionNumbers v of
    []    -> Cabal.mkVersion [0, 1]
    [a]   -> Cabal.mkVersion [a, 1]
    a:b:_ -> Cabal.mkVersion [a, b + 1]

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
checkVersion check (versionNumbers -> wanted) (versionNumbers -> actual) =
    case check of
        MatchMinor -> and (take 3 matching)
        MatchExact -> length wanted == length actual && and matching
        NewerMinor -> and (take 2 matching) && newerMinor
  where
    matching = zipWith (==) wanted actual

    getMinor (_a:_b:c:_) = Just c
    getMinor _ = Nothing

    newerMinor =
        case (getMinor wanted, getMinor actual) of
            (Nothing, _) -> True
            (Just _, Nothing) -> False
            (Just w, Just a) -> a >= w

-- | Get minor version (excludes any patchlevel)
minorVersion :: Version -> Version
minorVersion = Cabal.mkVersion . take 3 . versionNumbers

-- | Current Stack version
stackVersion :: Version
stackVersion = Cabal.mkVersion' Meta.version

-- | Current Stack minor version (excludes patchlevel)
stackMinorVersion :: Version
stackMinorVersion = minorVersion stackVersion
