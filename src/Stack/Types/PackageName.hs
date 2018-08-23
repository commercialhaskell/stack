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
  (parsePackageNameThrowing
  ,parsePackageNameFromFilePath
  ,packageNameArgument)
  where

import           Stack.Prelude
import qualified Data.Text as T
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

-- | Parse a package name from a 'String'.
parsePackageNameThrowing :: MonadThrow m => String -> m PackageName
parsePackageNameThrowing str =
  case parsePackageName str of
    Nothing -> throwM $ PackageNameParseFail $ T.pack str
    Just pn -> pure pn

-- | Parse a package name from a file path.
parsePackageNameFromFilePath :: MonadThrow m => Path a File -> m PackageName
parsePackageNameFromFilePath fp = do
    base <- clean $ toFilePath $ filename fp
    case parsePackageName base of
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
        case parsePackageName s of
            Just x -> Right x
            Nothing -> Left $ unlines
                [ "Expected valid package name, but got: " ++ s
                , "Package names consist of one or more alphanumeric words separated by hyphens."
                , "To avoid ambiguity with version numbers, each of these words must contain at least one letter."
                ]
