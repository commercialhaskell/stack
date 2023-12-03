{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.DependencyTree
  ( DependencyTree (..)
  , DotPayload (..)
  , licenseText
  , versionText
  ) where

import           Data.Aeson ( ToJSON (..), Value, (.=), object )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Distribution.License ( License (..), licenseFromSPDX )
import qualified Distribution.SPDX.License as SPDX
import           Distribution.Text ( display )
import           Stack.Prelude hiding ( Display (..), pkgName, loadPackage )

-- | Information about a package in the dependency graph, when available.
data DotPayload = DotPayload
  { payloadVersion :: Maybe Version
    -- ^ The package version.
  , payloadLicense :: Maybe (Either SPDX.License License)
    -- ^ The license the package was released under.
  , payloadLocation :: Maybe PackageLocation
    -- ^ The location of the package.
  }
  deriving (Eq, Show)

data DependencyTree =
  DependencyTree (Set PackageName)
                 (Map PackageName (Set PackageName, DotPayload))

instance ToJSON DependencyTree where
  toJSON (DependencyTree _ dependencyMap) =
    toJSON $ foldToList dependencyToJSON dependencyMap

foldToList :: (k -> a -> b) -> Map k a -> [b]
foldToList f = Map.foldrWithKey (\k a bs -> bs ++ [f k a]) []

dependencyToJSON :: PackageName -> (Set PackageName, DotPayload) -> Value
dependencyToJSON pkg (deps, payload) =
  let fieldsAlwaysPresent = [ "name" .= packageNameString pkg
                            , "license" .= licenseText payload
                            , "version" .= versionText payload
                            , "dependencies" .= Set.map packageNameString deps
                            ]
      loc = catMaybes
              [("location" .=) . pkgLocToJSON <$> payloadLocation payload]
  in  object $ fieldsAlwaysPresent ++ loc

pkgLocToJSON :: PackageLocation -> Value
pkgLocToJSON (PLMutable (ResolvedPath _ dir)) = object
  [ "type" .= ("project package" :: Text)
  , "url" .= ("file://" ++ toFilePath dir)
  ]
pkgLocToJSON (PLImmutable (PLIHackage pkgid _ _)) = object
  [ "type" .= ("hackage" :: Text)
  , "url" .= ("https://hackage.haskell.org/package/" ++ display pkgid)
  ]
pkgLocToJSON (PLImmutable (PLIArchive archive _)) =
  let url = case archiveLocation archive of
              ALUrl u -> u
              ALFilePath (ResolvedPath _ path) ->
                Text.pack $ "file://" ++ toFilePath path
  in  object
        [ "type" .= ("archive" :: Text)
        , "url" .= url
        , "sha256" .= archiveHash archive
        , "size" .= archiveSize archive
        ]
pkgLocToJSON (PLImmutable (PLIRepo repo _)) = object
  [ "type" .= case repoType repo of
                RepoGit -> "git" :: Text
                RepoHg -> "hg" :: Text
  , "url" .= repoUrl repo
  , "commit" .= repoCommit repo
  , "subdir" .= repoSubdir repo
  ]

licenseText :: DotPayload -> Text
licenseText payload =
  maybe "<unknown>" (Text.pack . display . either licenseFromSPDX id)
                    (payloadLicense payload)

versionText :: DotPayload -> Text
versionText payload =
  maybe "<unknown>" (Text.pack . display) (payloadVersion payload)
