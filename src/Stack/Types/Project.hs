{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

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
  { userMsg :: !(Maybe String)
    -- ^ A warning message to display to the user when the auto generated
    -- config may have issues.
  , packages :: ![RelFilePath]
    -- ^ Packages which are actually part of the project (as opposed
    -- to dependencies).
  , extraDeps :: ![RawPackageLocation]
    -- ^ Dependencies defined within the stack.yaml file, to be applied on top
    -- of the snapshot.
  , flagsByPkg :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flags to be applied on top of the snapshot flags.
  , resolver :: !RawSnapshotLocation
    -- ^ How we resolve which @Snapshot@ to use
  , compiler :: !(Maybe WantedCompiler)
    -- ^ Override the compiler in 'projectResolver'
  , extraPackageDBs :: ![FilePath]
  , curator :: !(Maybe Curator)
    -- ^ Extra configuration intended exclusively for usage by the curator tool.
    -- In other words, this is /not/ part of the documented and exposed Stack
    -- API. SUBJECT TO CHANGE.
  , dropPackages :: !(Set PackageName)
    -- ^ Packages to drop from the 'projectResolver'.
  }
  deriving Show

instance ToJSON Project where
  -- Expanding the constructor fully to ensure we don't miss any fields.
  toJSON project = object $ concat
    [ maybe [] (\cv -> ["compiler" .= cv]) project.compiler
    , maybe [] (\msg -> ["user-message" .= msg]) project.userMsg
    , [ "extra-package-dbs" .= project.extraPackageDBs
      | not (null project.extraPackageDBs)
      ]
    , [ "extra-deps" .= project.extraDeps | not (null project.extraDeps) ]
    , [ "flags" .= fmap toCabalStringMap (toCabalStringMap project.flagsByPkg)
      | not (Map.null project.flagsByPkg)
      ]
    , ["packages" .= project.packages]
    , ["resolver" .= project.resolver]
    , maybe [] (\c -> ["curator" .= c]) project.curator
    , [ "drop-packages" .= Set.map CabalString project.dropPackages
      | not (Set.null project.dropPackages)
      ]
    ]
