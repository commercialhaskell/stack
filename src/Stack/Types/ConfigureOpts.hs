{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}

{-|
Module      : Stack.Types.ConfigureOpts
License     : BSD-3-Clause
-}

module Stack.Types.ConfigureOpts
  ( ConfigureOpts (..)
  , BaseConfigOpts (..)
  , PackageConfigureOpts (..)
  ) where

import           Stack.Prelude
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI )

-- | Basic information used to calculate what the configure options are
data BaseConfigOpts = BaseConfigOpts
  { snapDB :: !(Path Abs Dir)
  , localDB :: !(Path Abs Dir)
  , snapInstallRoot :: !(Path Abs Dir)
  , localInstallRoot :: !(Path Abs Dir)
  , buildOpts :: !BuildOpts
  , buildOptsCLI :: !BuildOptsCLI
  , extraDBs :: ![Path Abs Dir]
  }
  deriving Show

-- | All these fields come from the v'Package' data type but bringing the
-- whole t'Package' is way too much, hence this datatype.
data PackageConfigureOpts = PackageConfigureOpts
  { pkgCabalConfigOpts :: [Text]
  , pkgGhcOptions :: [Text]
  , pkgFlags :: Map FlagName Bool
  , pkgDefaultFlags :: Map FlagName Bool
  , pkgIdentifier :: PackageIdentifier
  }
  deriving Show

-- | Configure options to be sent to Setup.hs configure.
data ConfigureOpts = ConfigureOpts
  { pathRelated :: ![String]
    -- ^ Options related to various paths. We separate these out since they do
    -- not have an effect on the contents of the compiled binary for checking
    -- if we can use an existing precompiled cache.
  , nonPathRelated :: ![String]
    -- ^ Options other than path-related options.
  }
  deriving (Data, Eq, Generic, Show)

instance NFData ConfigureOpts
