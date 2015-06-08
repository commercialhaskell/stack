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
  ,MajorVersion (..)
  ,getMajorVersion
  ,fromMajorVersion
  ,parseMajorVersionFromString
  ,versionParser
  ,parseVersion
  ,parseVersionFromString
  ,versionString
  ,versionText
  ,toCabalVersion
  ,fromCabalVersion
  ,mkVersion
  ,versionRangeText
  ,withinRange)
  where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
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
  VersionParseFail ByteString
  | NotAMajorVersion Version
  deriving (Typeable)
instance Exception VersionParseFail
instance Show VersionParseFail where
    show (VersionParseFail bs) = "Invalid version: " ++ show bs
    show (NotAMajorVersion v) = concat
        [ "Not a major version: "
        , versionString v
        , ", expecting exactly two numbers (e.g. 7.10)"
        ]

-- | A package version.
newtype Version =
  Version {unVersion :: Vector Word}
  deriving (Eq,Ord,Typeable,Data,Generic,Binary,NFData)

-- | The first two components of a version.
data MajorVersion = MajorVersion !Word !Word
    deriving (Typeable, Eq, Ord)
instance Show MajorVersion where
    show (MajorVersion x y) = concat [show x, ".", show y]
instance ToJSON MajorVersion where
    toJSON = toJSON . fromMajorVersion

-- | Parse major version from @String@
parseMajorVersionFromString :: MonadThrow m => String -> m MajorVersion
parseMajorVersionFromString s = do
    Version v <- parseVersionFromString s
    if V.length v == 2
        then return $ getMajorVersion (Version v)
        else throwM $ NotAMajorVersion (Version v)

instance FromJSON MajorVersion where
    parseJSON = withText "MajorVersion"
              $ either (fail . show) return
              . parseMajorVersionFromString
              . T.unpack
instance FromJSON a => FromJSON (Map MajorVersion a) where
    parseJSON val = do
        m <- parseJSON val
        fmap Map.fromList $ mapM go $ Map.toList m
      where
        go (k, v) = do
            k' <- either (fail . show) return $ parseMajorVersionFromString k
            return (k', v)

-- | Returns the first two components, defaulting to 0 if not present
getMajorVersion :: Version -> MajorVersion
getMajorVersion (Version v) =
    case V.length v of
        0 -> MajorVersion 0 0
        1 -> MajorVersion (V.head v) 0
        _ -> MajorVersion (V.head v) (v V.! 1)

-- | Convert a two-component version into a @Version@
fromMajorVersion :: MajorVersion -> Version
fromMajorVersion (MajorVersion x y) = Version $ V.fromList [x, y]

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

-- | Attoparsec parser for a package version from bytestring.
versionParser :: Parser Version
versionParser =
  do ls <- ((:) <$> num <*> many num')
     let !v = V.fromList ls
     return (Version v)
  where num = decimal
        num' = point *> num
        point = satisfy (== '.')

-- | Convenient way to parse a package version from a bytestring.
parseVersion :: MonadThrow m => ByteString -> m Version
parseVersion x = go x
  where go =
          either (const (throwM (VersionParseFail x))) return .
          parseOnly (versionParser <* endOfInput)

-- | Migration function.
parseVersionFromString :: MonadThrow m => String -> m Version
parseVersionFromString =
  parseVersion . S8.pack

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
