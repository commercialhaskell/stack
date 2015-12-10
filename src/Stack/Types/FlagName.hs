{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

-- | Names for flags.

module Stack.Types.FlagName
  (FlagName
  ,FlagNameParseFail(..)
  ,flagNameParser
  ,parseFlagName
  ,parseFlagNameFromString
  ,flagNameString
  ,flagNameText
  ,fromCabalFlagName
  ,toCabalFlagName
  ,mkFlagName)
  where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson.Extended
import           Data.Attoparsec.Text
import           Data.Attoparsec.Combinators
import           Data.Binary.VersionTagged
import           Data.Char (isLetter, isDigit, toLower)
import           Data.Data
import           Data.Hashable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Binary ()
import qualified Distribution.PackageDescription as Cabal
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- | A parse fail.
data FlagNameParseFail
  = FlagNameParseFail Text
  deriving (Typeable)
instance Exception FlagNameParseFail
instance Show FlagNameParseFail where
    show (FlagNameParseFail bs) = "Invalid flag name: " ++ show bs

-- | A flag name.
newtype FlagName =
  FlagName Text
  deriving (Typeable,Data,Generic,Hashable,Binary,NFData)
instance HasStructuralInfo FlagName
instance Eq FlagName where
    x == y = compare x y == EQ
instance Ord FlagName where
    compare (FlagName x) (FlagName y) =
        compare (T.map toLower x) (T.map toLower y)

instance Lift FlagName where
  lift (FlagName n) =
    appE (conE 'FlagName)
         (stringE (T.unpack n))

instance Show FlagName where
  show (FlagName n) = T.unpack n

instance FromJSON FlagName where
  parseJSON j =
    do s <- parseJSON j
       case parseFlagNameFromString s of
         Nothing ->
           fail ("Couldn't parse flag name: " ++ s)
         Just ver -> return ver

-- | Attoparsec parser for a flag name
flagNameParser :: Parser FlagName
flagNameParser =
  fmap (FlagName . T.pack)
       (appending (many1 (satisfy isLetter))
                  (concating (many (alternating
                                      (pured (satisfy isAlphaNum))
                                      (appending (pured (satisfy separator))
                                                 (pured (satisfy isAlphaNum)))))))
  where separator c = c == '-' || c == '_'
        isAlphaNum c = isLetter c || isDigit c

-- | Make a flag name.
mkFlagName :: String -> Q Exp
mkFlagName s =
  case parseFlagNameFromString s of
    Nothing -> error ("Invalid flag name: " ++ show s)
    Just pn -> [|pn|]

-- | Convenient way to parse a flag name from a 'Text'.
parseFlagName :: MonadThrow m => Text -> m FlagName
parseFlagName x = go x
  where go =
          either (const (throwM (FlagNameParseFail x))) return .
          parseOnly (flagNameParser <* endOfInput)

-- | Convenience function for parsing from a 'String'
parseFlagNameFromString :: MonadThrow m => String -> m FlagName
parseFlagNameFromString =
  parseFlagName . T.pack

-- | Produce a string representation of a flag name.
flagNameString :: FlagName -> String
flagNameString (FlagName n) = T.unpack n

-- | Produce a string representation of a flag name.
flagNameText :: FlagName -> Text
flagNameText (FlagName n) = n

-- | Convert from a Cabal flag name.
fromCabalFlagName :: Cabal.FlagName -> FlagName
fromCabalFlagName (Cabal.FlagName name) =
  let !x = T.pack name
  in FlagName x

-- | Convert to a Cabal flag name.
toCabalFlagName :: FlagName -> Cabal.FlagName
toCabalFlagName (FlagName name) =
  let !x = T.unpack name
  in Cabal.FlagName x

instance ToJSON a => ToJSON (Map FlagName a) where
  toJSON = toJSON . Map.mapKeysWith const flagNameText
instance FromJSON a => FromJSON (Map FlagName a) where
    parseJSON val = do
        m <- parseJSON val
        fmap Map.fromList $ mapM go $ Map.toList m
      where
        go (k, v) = fmap (, v) $ either (fail . show) return $ parseFlagNameFromString k
