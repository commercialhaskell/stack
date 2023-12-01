{-# LANGUAGE NoImplicitPrelude #-}

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
  { dpGhcPkgId :: !GhcPkgId
    -- ^ The @id@ field.
  , dpPackageIdent :: !PackageIdentifier
    -- ^ The @name@ and @version@ fields. The @name@ field is the munged package
    -- name. If the package is not for a sub library, its munged name is its
    -- name.
  , dpSublib :: !(Maybe SublibDump)
    -- ^ The sub library information if it's a sub-library.
  , dpLicense :: !(Maybe C.License)
  , dpLibDirs :: ![FilePath]
    -- ^ The @library-dirs@ field.
  , dpLibraries :: ![Text]
    -- ^ The @hs-libraries@ field.
  , dpHasExposedModules :: !Bool
  , dpExposedModules :: !(Set ModuleName)
  , dpDepends :: ![GhcPkgId]
    -- ^ The @depends@ field (packages on which this package depends).
  , dpHaddockInterfaces :: ![FilePath]
  , dpHaddockHtml :: !(Maybe FilePath)
  , dpIsExposed :: !Bool
  }
  deriving (Eq, Read, Show)

-- | ghc-pkg has a notion of sublibraries when using ghc-pkg dump. We can only
-- know it's different through the fields it shows.
data SublibDump = SublibDump
  { sdPackageName :: PackageName
    -- ^ "package-name" field from ghc-pkg
  , sdLibraryName :: StackUnqualCompName
    -- ^ "lib-name" field from ghc-pkg
  }
  deriving (Eq, Read, Show)

dpParentLibIdent :: DumpPackage -> Maybe PackageIdentifier
dpParentLibIdent dp = case (dpSublib dp, dpPackageIdent dp) of
  (Nothing, _) -> Nothing
  (Just sublibDump, PackageIdentifier _ v) ->
    Just $ PackageIdentifier libParentPackageName v
   where
    SublibDump { sdPackageName = libParentPackageName } = sublibDump
