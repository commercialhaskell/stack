{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.Project
  ( Project (..)
  ) where

import           Data.Aeson.Types ( ToJSON (..), (.=), object )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Stack.Prelude
import           Stack.Types.Curator ( Curator )

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
  { projectUserMsg :: !(Maybe String)
    -- ^ A warning message to display to the user when the auto generated
    -- config may have issues.
  , projectPackages :: ![RelFilePath]
    -- ^ Packages which are actually part of the project (as opposed
    -- to dependencies).
  , projectDependencies :: ![RawPackageLocation]
    -- ^ Dependencies defined within the stack.yaml file, to be applied on top
    -- of the snapshot.
  , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flags to be applied on top of the snapshot flags.
  , projectResolver :: !RawSnapshotLocation
    -- ^ How we resolve which @Snapshot@ to use
  , projectCompiler :: !(Maybe WantedCompiler)
    -- ^ Override the compiler in 'projectResolver'
  , projectExtraPackageDBs :: ![FilePath]
  , projectCurator :: !(Maybe Curator)
    -- ^ Extra configuration intended exclusively for usage by the curator tool.
    -- In other words, this is /not/ part of the documented and exposed Stack
    -- API. SUBJECT TO CHANGE.
  , projectDropPackages :: !(Set PackageName)
    -- ^ Packages to drop from the 'projectResolver'.
  }
  deriving Show

instance ToJSON Project where
  -- Expanding the constructor fully to ensure we don't miss any fields.
  toJSON (Project userMsg packages extraDeps flags resolver mcompiler extraPackageDBs mcurator drops) = object $ concat
    [ maybe [] (\cv -> ["compiler" .= cv]) mcompiler
    , maybe [] (\msg -> ["user-message" .= msg]) userMsg
    , [ "extra-package-dbs" .= extraPackageDBs | not (null extraPackageDBs) ]
    , [ "extra-deps" .= extraDeps | not (null extraDeps) ]
    , [ "flags" .= fmap toCabalStringMap (toCabalStringMap flags)
      | not (Map.null flags)
      ]
    , ["packages" .= packages]
    , ["resolver" .= resolver]
    , maybe [] (\c -> ["curator" .= c]) mcurator
    , [ "drop-packages" .= Set.map CabalString drops | not (Set.null drops) ]
    ]
