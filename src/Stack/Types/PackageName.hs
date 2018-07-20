{-# LANGUAGE NoImplicitPrelude #-}
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
  ,parsePackageName
  ,parsePackageNameFromString
  ,parsePackageNameFromFilePath
  ,mkPackageName
  ,packageNameArgument)
  where

import           Stack.Prelude
import           Pantry
import           Data.Aeson.Extended
import           Data.Attoparsec.Combinators
import           Data.Attoparsec.Text
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Distribution.Package as Cabal
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Options.Applicative as O
import           Path

-- | A parse fail.
data PackageNameParseFail
  = PackageNameParseFail Text
  | CabalFileNameParseFail FilePath
  | CabalFileNameInvalidPackageName FilePath
  deriving (Typeable)
instance Exception PackageNameParseFail
instance Show PackageNameParseFail where
    show (PackageNameParseFail bs) = "Invalid package name: " ++ show bs
    show (CabalFileNameParseFail fp) = "Invalid file path for cabal file, must have a .cabal extension: " ++ fp
    show (CabalFileNameInvalidPackageName fp) = "cabal file names must use valid package names followed by a .cabal extension, the following is invalid: " ++ fp

    {- FIXME
instance FromJSON PackageName where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageNameFromString s of
         Nothing ->
           fail ("Couldn't parse package name: " ++ s)
         Just ver -> return ver

instance FromJSONKey PackageName where
  fromJSONKey = FromJSONKeyTextParser $ \k ->
    either (fail . show) return $ parsePackageName k
    -}

-- | Make a package name.
mkPackageName :: String -> Q Exp
mkPackageName s =
  case parsePackageNameFromString s of
    Left e -> qRunIO $ throwIO e
    Right _ -> [|Cabal.mkPackageName s|]

-- | Parse a package name from a 'Text'.
parsePackageName :: MonadThrow m => Text -> m PackageName
parsePackageName = parsePackageNameFromString . T.unpack

-- | Parse a package name from a 'String'.
parsePackageNameFromString :: MonadThrow m => String -> m PackageName
parsePackageNameFromString str =
  case parseC str of
    Nothing -> throwM $ PackageNameParseFail $ T.pack str
    Just pn -> pure pn

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
            Nothing -> Left $ unlines
                [ "Expected valid package name, but got: " ++ s
                , "Package names consist of one or more alphanumeric words separated by hyphens."
                , "To avoid ambiguity with version numbers, each of these words must contain at least one letter."
                ]
