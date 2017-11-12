{-# LANGUAGE NoImplicitPrelude #-}
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

import           Stack.Prelude
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import           Data.Char (isLetter, isDigit, toLower)
import qualified Data.Text as T
import qualified Distribution.PackageDescription as Cabal
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- | A parse fail.
newtype FlagNameParseFail
  = FlagNameParseFail Text
  deriving (Typeable)
instance Exception FlagNameParseFail
instance Show FlagNameParseFail where
    show (FlagNameParseFail bs) = "Invalid flag name: " ++ show bs

-- | A flag name.
newtype FlagName =
  FlagName Text
  deriving (Typeable,Data,Generic,Hashable,Store,NFData,ToJSONKey)
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

instance FromJSONKey FlagName where
  fromJSONKey = FromJSONKeyTextParser $ \k ->
    either (fail . show) return $ parseFlagName k

-- | Attoparsec parser for a flag name
flagNameParser :: Parser FlagName
flagNameParser = fmap FlagName $ do
  t <- A.takeWhile1 (\c -> isAlphaNum c || separator c)
  when (T.head t == '-') $ fail "flag name cannot start with dash"
  return t
  where separator c = c == '-' || c == '_'
        isAlphaNum c = isLetter c || isDigit c

-- | Make a flag name.
mkFlagName :: String -> Q Exp
mkFlagName s =
  case parseFlagNameFromString s of
    Nothing -> qRunIO $ throwString ("Invalid flag name: " ++ show s)
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
fromCabalFlagName name =
  let !x = T.pack $ Cabal.unFlagName name
  in FlagName x

-- | Convert to a Cabal flag name.
toCabalFlagName :: FlagName -> Cabal.FlagName
toCabalFlagName (FlagName name) =
  let !x = T.unpack name
  in Cabal.mkFlagName x
