{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Stack.Types.DumpPackage
  ( DumpPackage (..)
  , SublibDump (..)
  , dpParentLibIdent
  ) where

import qualified Distribution.License as C
import           Distribution.ModuleName ( ModuleName )
import           Stack.Prelude
import           Stack.Types.Component ( StackUnqualCompName )
import           Stack.Types.GhcPkgId ( GhcPkgId )

-- | Type representing dump information for a single package, as output by the
-- @ghc-pkg describe@ command.
data DumpPackage = DumpPackage
  { ghcPkgId :: !GhcPkgId
    -- ^ The @id@ field.
  , packageIdent :: !PackageIdentifier
    -- ^ The @name@ and @version@ fields. The @name@ field is the munged package
    -- name. If the package is not for a sub library, its munged name is its
    -- name.
  , sublib :: !(Maybe SublibDump)
    -- ^ The sub library information if it's a sub-library.
  , license :: !(Maybe C.License)
  , libDirs :: ![FilePath]
    -- ^ The @library-dirs@ field.
  , libraries :: ![Text]
    -- ^ The @hs-libraries@ field.
  , hasExposedModules :: !Bool
  , exposedModules :: !(Set ModuleName)
  , depends :: ![GhcPkgId]
    -- ^ The @depends@ field (packages on which this package depends).
  , haddockInterfaces :: ![FilePath]
  , haddockHtml :: !(Maybe FilePath)
  , isExposed :: !Bool
  }
  deriving (Eq, Read, Show)

-- | ghc-pkg has a notion of sublibraries when using ghc-pkg dump. We can only
-- know it's different through the fields it shows.
data SublibDump = SublibDump
  { packageName :: PackageName
    -- ^ "package-name" field from ghc-pkg
  , libraryName :: StackUnqualCompName
    -- ^ "lib-name" field from ghc-pkg
  }
  deriving (Eq, Read, Show)

dpParentLibIdent :: DumpPackage -> Maybe PackageIdentifier
dpParentLibIdent dp = case (dp.sublib, dp.packageIdent) of
  (Nothing, _) -> Nothing
  (Just sublibDump, PackageIdentifier _ v) ->
    Just $ PackageIdentifier libParentPackageName v
   where
    SublibDump { packageName = libParentPackageName } = sublibDump
