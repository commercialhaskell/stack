{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Functions for the GHC package database.

module Stack.GhcPkg
  (getAllPackages
  ,findPackageId
  ,getPackageIds)
  where

import           Stack.Types
import           System.Process.Read

import           Control.Applicative
import           Control.Exception hiding (catch)
import           Control.Monad (liftM, forM_, unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
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
import           Path (Path, Abs, Dir, toFilePath, parent)
import           Prelude hiding (FilePath)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist)

-- | A ghc-pkg exception.
data GhcPkgException
  = GetAllPackagesFail
  | GetUserDbPathFail
  | FindPackageIdFail PackageName ProcessExitedUnsuccessfully
  deriving (Typeable,Show)
instance Exception GhcPkgException

-- | Run the ghc-pkg executable
ghcPkg :: (MonadIO m, MonadReader env m, HasExternalEnv env)
       => [Path Abs Dir]
       -> [String]
       -> m (Either ProcessExitedUnsuccessfully S8.ByteString)
ghcPkg pkgDbs args = do
    eres <- go
    case eres of
        Left _ -> do
            forM_ pkgDbs $ \db -> do
                let db' = toFilePath db
                exists <- liftIO $ doesDirectoryExist db'
                unless exists $ do
                    -- Creating the parent doesn't seem necessary, as ghc-pkg
                    -- seems to be sufficiently smart. But I don't feel like
                    -- finding out it isn't the hard way
                    liftIO $ createDirectoryIfMissing True $ toFilePath $ parent db
                    _ <- tryProcessStdout "ghc-pkg" ["init", db']
                    return ()
            go
        Right _ -> return eres
  where
    go = tryProcessStdout "ghc-pkg"
        $ "--no-user-package-db"
        : map (\x -> ("--package-db=" ++ toFilePath x)) pkgDbs
       ++ args

-- | Get all available packages.
getAllPackages :: (MonadCatch m,MonadIO m,MonadThrow m,MonadReader env m,HasExternalEnv env)
               => [Path Abs Dir] -- ^ package databases
               -> m (Map PackageName Version)
getAllPackages pkgDbs =
  do result <- ghcPkg pkgDbs ["list"]
     case result of
       Left {} -> throw GetAllPackagesFail
       Right lbs ->
         case AttoLazy.parse pkgsListParser
                             (L.fromStrict lbs) of
           AttoLazy.Fail _ _ _ ->
             throw GetAllPackagesFail
           AttoLazy.Done _ r ->
             liftIO (evaluate r)

-- | Parser for ghc-pkg's list output.
pkgsListParser :: Parser (Map PackageName Version)
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

-- | Get the id of the package e.g. @foo-0.0.0-9c293923c0685761dcff6f8c3ad8f8ec@.
findPackageId :: (MonadIO m, MonadReader env m, HasExternalEnv env)
              => [Path Abs Dir] -- ^ package databases
              -> PackageName -> m (Maybe GhcPkgId)
findPackageId pkgDbs name =
  do result <- ghcPkg pkgDbs ["describe", packageNameString name]
     case result of
       Left{} -> return Nothing
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
getPackageIds :: (MonadIO m, MonadReader env m, HasExternalEnv env)
              => [Path Abs Dir] -- ^ package databases
              -> [PackageName]
              -> m (Map PackageName GhcPkgId)
getPackageIds pkgDbs pkgs = collect pkgs >>= liftIO . evaluate
  where collect =
          liftM (M.fromList . catMaybes) .
          mapM getTuple
        getTuple name =
          do mpid <- findPackageId pkgDbs name
             case mpid of
               Nothing -> return Nothing
               Just pid ->
                 return (Just (name,pid))
