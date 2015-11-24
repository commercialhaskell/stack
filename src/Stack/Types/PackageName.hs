{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

-- | Names for packages.

module Stack.Types.PackageName
  (PackageName
  ,PackageNameParseFail(..)
  ,packageNameParser
  ,parsePackageName
  ,parsePackageNameFromString
  ,packageNameByteString
  ,packageNameString
  ,packageNameText
  ,fromCabalPackageName
  ,toCabalPackageName
  ,parsePackageNameFromFilePath
  ,mkPackageName
  ,packageNameArgument)
  where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Catch
import           Data.Aeson.Extended
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.Combinators
import           Data.Binary.VersionTagged (Binary, HasStructuralInfo)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char (isLetter)
import           Data.Data
import           Data.Hashable
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Distribution.Package as Cabal
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Path
import qualified Options.Applicative as O

-- | A parse fail.
data PackageNameParseFail
  = PackageNameParseFail ByteString
  | CabalFileNameParseFail FilePath
  | CabalFileNameInvalidPackageName FilePath
  deriving (Typeable)
instance Exception PackageNameParseFail
instance Show PackageNameParseFail where
    show (PackageNameParseFail bs) = "Invalid package name: " ++ show bs
    show (CabalFileNameParseFail fp) = "Invalid file path for cabal file, must have a .cabal extension: " ++ fp
    show (CabalFileNameInvalidPackageName fp) = "cabal file names must use valid package names followed by a .cabal extension, the following is invalid: " ++ fp

-- | A package name.
newtype PackageName =
  PackageName ByteString
  deriving (Eq,Ord,Typeable,Data,Generic,Hashable,Binary,NFData)

instance Lift PackageName where
  lift (PackageName n) =
    appE (conE 'PackageName)
         (stringE (S8.unpack n))

instance Show PackageName where
  show (PackageName n) = S8.unpack n

instance HasStructuralInfo PackageName

instance ToJSON PackageName where
    toJSON = toJSON . packageNameText
instance FromJSON PackageName where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageNameFromString s of
         Nothing ->
           fail ("Couldn't parse package name: " ++ s)
         Just ver -> return ver

-- | Attoparsec parser for a package name from bytestring.
packageNameParser :: Parser PackageName
packageNameParser =
  fmap (PackageName . S8.pack . intercalate "-")
       (sepBy1 word (char '-'))
  where
    word = concat <$> sequence [many digit,
                                pured letter,
                                many (alternating letter digit)]
    letter = satisfy isLetter

-- | Make a package name.
mkPackageName :: String -> Q Exp
mkPackageName s =
  case parsePackageNameFromString s of
    Nothing -> error ("Invalid package name: " ++ show s)
    Just pn -> [|pn|]

-- | Convenient way to parse a package name from a bytestring.
parsePackageName :: MonadThrow m => ByteString -> m PackageName
parsePackageName x = go x
  where go =
          either (const (throwM (PackageNameParseFail x))) return .
          parseOnly (packageNameParser <* endOfInput)

-- | Migration function.
parsePackageNameFromString :: MonadThrow m => String -> m PackageName
parsePackageNameFromString =
  parsePackageName . S8.pack

-- | Produce a bytestring representation of a package name.
packageNameByteString :: PackageName -> ByteString
packageNameByteString (PackageName n) = n

-- | Produce a string representation of a package name.
packageNameString :: PackageName -> String
packageNameString (PackageName n) = S8.unpack n

-- | Produce a string representation of a package name.
packageNameText :: PackageName -> Text
packageNameText (PackageName n) = T.decodeUtf8 n

-- | Convert from a Cabal package name.
fromCabalPackageName :: Cabal.PackageName -> PackageName
fromCabalPackageName (Cabal.PackageName name) =
  let !x = S8.pack name
  in PackageName x

-- | Convert to a Cabal package name.
toCabalPackageName :: PackageName -> Cabal.PackageName
toCabalPackageName (PackageName name) =
  let !x = S8.unpack name
  in Cabal.PackageName x

-- | Parse a package name from a file path.
parsePackageNameFromFilePath :: MonadThrow m => Path a File -> m PackageName
parsePackageNameFromFilePath fp = do
    base <- clean $ toFilePath $ filename fp
    case parsePackageNameFromString base of
        Nothing -> throwM $ CabalFileNameInvalidPackageName $ toFilePath fp
        Just x -> return x
  where clean = liftM reverse . strip . reverse
        strip ('l':'a':'b':'a':'c':'.':xs) = return xs
        strip _ = throwM (CabalFileNameParseFail (toFilePath fp))

instance ToJSON a => ToJSON (Map PackageName a) where
  toJSON = toJSON . Map.mapKeysWith const packageNameText
instance FromJSON a => FromJSON (Map PackageName a) where
    parseJSON val = do
        m <- parseJSON val
        fmap Map.fromList $ mapM go $ Map.toList m
      where
        go (k, v) = fmap (, v) $ either (fail . show) return $ parsePackageNameFromString k

-- | An argument which accepts a template name of the format
-- @foo.hsfiles@.
packageNameArgument :: O.Mod O.ArgumentFields PackageName
                    -> O.Parser PackageName
packageNameArgument =
    O.argument
        (do s <- O.str
            either O.readerError return (p s))
  where
    p s =
        case parsePackageNameFromString s of
            Just x -> Right x
            Nothing -> Left ("Expected valid package name, but got: " ++ s)
