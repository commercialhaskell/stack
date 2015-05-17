{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Resolve versions for packages against a stackage version.

module Stackage.Resolve
  (PackageSuggestion(..)
  ,resolvePackageVersions)
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.List
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Prelude hiding (FilePath)
import           Stackage.FlagName
import           Stackage.PackageName
import           Stackage.PackageVersion

data ResolveException =
  FPResolvePackagesError [PackageName]
                         (Response L.ByteString)
 deriving (Show,Typeable)
instance Exception ResolveException

-- | A suggestion for package version/flags from stackage.org
data PackageSuggestion =
  PackageSuggestion {suggestionName :: !PackageName
                    ,suggestionVersion :: !PackageVersion
                    ,suggestionFlags :: !(Map FlagName Bool)}
  deriving (Show)

instance FromJSON PackageSuggestion where
  parseJSON j =
    do o <- parseJSON j
       name <- o .: "name"
       ver <- o .: "version"
       flags <- o .: "flags"
       return (PackageSuggestion name ver flags)

-- | Resolve package versions.
-- TODO: Accept a stackage version (as opposed to just LTS).
-- TODO: Catch http exceptions.
-- TODO: Handle non-existent package case.
-- Example usage:
-- runNoLoggingT (resolvePackageVersions [PackageName "warp",PackageName "snap"])
resolvePackageVersions :: (MonadThrow m,MonadIO m) => [PackageName] -> m [PackageSuggestion]
resolvePackageVersions (null -> True) = return []
resolvePackageVersions names =
  do liftIO (putStrLn "Resolving package versions against Stackage ...")
     req <- parseUrl url
     resp <-
       liftIO (withManager defaultManagerSettings
                           (httpLbs req))
     case responseStatus resp of
       Status 200 _ ->
         case decode (responseBody resp) of
           Nothing ->
             liftIO (throwIO (FPResolvePackagesError names resp))
           Just vers -> return vers
       _ ->
         liftIO (throwIO (FPResolvePackagesError names resp))
  where url = "http://www.stackage.org/lts/build-plan?" ++
              packages ++ "&_accept=application/json"
        packages =
          intercalate
            "&"
            (map (\x ->
                    "package=" ++
                    (packageNameString x))
                 names)
