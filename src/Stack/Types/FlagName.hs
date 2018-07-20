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
  ,parseFlagName
  ,parseFlagNameFromString
  ,mkFlagName)
  where

import           Stack.Prelude
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import           Data.Char (isLetter, isDigit, toLower)
import qualified Data.Text as T
import qualified Distribution.PackageDescription as Cabal
import           Distribution.PackageDescription (FlagName)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Pantry

-- | A parse fail.
newtype FlagNameParseFail = FlagNameParseFail Text
  deriving (Typeable)
instance Exception FlagNameParseFail
instance Show FlagNameParseFail where
    show (FlagNameParseFail bs) = "Invalid flag name: " ++ show bs

    {-
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
    -}

-- | Make a flag name.
mkFlagName :: String -> Q Exp
mkFlagName s =
  case parseFlagNameFromString s of
    Nothing -> qRunIO $ throwString ("Invalid flag name: " ++ show s)
    Just _ -> [|Cabal.mkFlagName s|]

-- | Convenient way to parse a flag name from a 'Text'.
parseFlagName :: MonadThrow m => Text -> m FlagName
parseFlagName = parseFlagNameFromString . T.unpack

-- | Convenience function for parsing from a 'String'
parseFlagNameFromString :: MonadThrow m => String -> m FlagName
parseFlagNameFromString str =
  case parseC str of
    Nothing -> throwM $ FlagNameParseFail $ T.pack str
    Just fn -> pure fn
