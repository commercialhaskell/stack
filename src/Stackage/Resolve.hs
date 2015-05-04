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
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Version as V ( parseVersion )
import           Distribution.Version
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Prelude hiding (FilePath)
import           Stackage.PackageName
import qualified Text.ParserCombinators.ReadP as ReadP ( readP_to_S )

data ResolveException =
  FPResolvePackagesError [PackageName]
                         (Response L.ByteString)
 deriving (Show,Typeable)
instance Exception ResolveException

-- | Simple wrapper for orphan instances.
newtype Aeson a = Aeson { unAeson :: a}

instance FromJSON (Aeson Version) where
  parseJSON j =
    do s <- parseJSON j
       case parseVersion s of
         Nothing ->
           fail "Couldn't parse version."
         Just ver -> return (Aeson ver)

instance FromJSON (Aeson PackageName) where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageName (T.encodeUtf8 s) of
         Nothing ->
           fail "Couldn't parse version."
         Just n -> return (Aeson n)

-- | A suggestion for package version/flags from stackage.org
data PackageSuggestion =
  PackageSuggestion {suggestionName :: !PackageName
                    ,suggestionVersion :: !Version
                    ,suggestionFlags :: !(Map Text Bool)}
  deriving (Show)

instance FromJSON PackageSuggestion where
  parseJSON j =
    do o <- parseJSON j
       name <- fmap unAeson (o .: "name")
       ver <- fmap unAeson (o .: "version")
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

-- | Parse a package version.
parseVersion :: String -> Maybe Version
parseVersion s =
  case reverse (ReadP.readP_to_S V.parseVersion s) of
    ((ver,""):_) -> Just ver
    _ -> Nothing
