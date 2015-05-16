{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stackage.GhcPkg
  (getAllPackages
  ,getUserDbPath
  ,findPackageId
  ,getPackageIds)
  where

import           Stackage.GhcPkgId
import           Stackage.PackageIdentifier
import           Stackage.PackageName
import           Stackage.PackageVersion
import           Stackage.Process

import           Control.Applicative
import           Control.Exception hiding (catch)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AttoLazy
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Streaming.Process

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Prelude hiding (FilePath)
import           System.IO hiding (char8)

-- | A ghc-pkg exception.
data GhcPkgException
  = GetAllPackagesFail
  | GetUserDbPathFail
  | FindPackageIdFail PackageName ProcessExitedUnsuccessfully
  deriving (Typeable,Show)
instance Exception GhcPkgException

-- | Get all available packages.
getAllPackages :: (MonadCatch m,MonadIO m,MonadThrow m)
               => m (Map PackageName PackageVersion)
getAllPackages =
  do result <-
       tryProcessStdout "ghc-pkg"
                        ["list"]
     case result of
       Left{} -> throw GetAllPackagesFail
       Right lbs ->
         case AttoLazy.parse pkgsListParser
                             (L.fromStrict lbs) of
           AttoLazy.Fail _ _ _ ->
             throw GetAllPackagesFail
           AttoLazy.Done _ r ->
             liftIO (evaluate r)

-- | Parser for ghc-pkg's list output.
pkgsListParser :: Parser (Map PackageName PackageVersion)
pkgsListParser =
  fmap (M.fromList . concat) sections
  where sections =
          many (heading *>
                (many (pkg <* endOfLine)) <*
                optional endOfLine)
        heading =
          many1 (satisfy (not . (== '\n'))) <*
          endOfLine
        pkg =
          do space
             space
             space
             space
             fmap toTuple packageIdentifierParser

-- | Get the package of the package database.
getUserDbPath :: IO FilePath
getUserDbPath =
  do result <-
       tryProcessStdout "ghc-pkg"
                        ["list","--user"]
     case find (not .
                T.isPrefixOf " ")
               (map T.decodeUtf8 (S8.lines (either (const "") id result))) >>=
          T.stripSuffix ":" of
       Nothing -> throw GetUserDbPathFail
       Just path -> evaluate (T.unpack path)

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findPackageId :: PackageName -> IO (Maybe GhcPkgId)
findPackageId name =
  do result <-
       tryProcessStdout "ghc-pkg"
                        ["describe",packageNameString name]
     case result of
       Left err -> return Nothing
       Right lbs ->
         do let mpid =
                  fmap T.unpack
                       (listToMaybe
                          (mapMaybe (T.stripPrefix "id: ")
                                    (map T.decodeUtf8 (S8.lines lbs))))
            case mpid of
              Just !pid ->
                return (parseGhcPkgIdFromString pid)
              _ -> return Nothing

-- | Get all current package ids.
getPackageIds :: [PackageName] -> IO (Map PackageName GhcPkgId)
getPackageIds pkgs = collect pkgs >>= evaluate
  where collect =
          fmap (M.fromList . catMaybes) .
          mapM getTuple
        getTuple name =
          do mpid <- findPackageId name
             case mpid of
               Nothing -> return Nothing
               Just pid ->
                 return (Just (name,pid))
