{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.DumpPackage
  ( DumpPackage (..)
  ) where

import qualified Distribution.License as C
import           Distribution.ModuleName ( ModuleName )
import           Stack.Prelude
import           Stack.Types.GhcPkgId

-- | Type representing dump information for a single package, as output by the
-- @ghc-pkg describe@ command.
data DumpPackage = DumpPackage
  { dpGhcPkgId :: !GhcPkgId
    -- ^ The @id@ field.
  , dpPackageIdent :: !PackageIdentifier
    -- ^ The @name@ and @version@ fields. The @name@ field is the munged package
    -- name. If the package is not for a sub library, its munged name is its
    -- name.
  , dpParentLibIdent :: !(Maybe PackageIdentifier)
    -- ^ The @package-name@ and @version@ fields, if @package-name@ is present.
    -- That field is present if the package is for a sub library.
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
