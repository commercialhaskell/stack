{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.EnvSettings
  ( EnvSettings (..)
  , minimalEnvSettings
  , defaultEnvSettings
  , plainEnvSettings
  ) where

import           Stack.Prelude

-- | Controls which version of the environment is used
data EnvSettings = EnvSettings
  { esIncludeLocals :: !Bool
  -- ^ include local project bin directory, GHC_PACKAGE_PATH, etc
  , esIncludeGhcPackagePath :: !Bool
  -- ^ include the GHC_PACKAGE_PATH variable
  , esStackExe :: !Bool
  -- ^ set the STACK_EXE variable to the current executable name
  , esLocaleUtf8 :: !Bool
  -- ^ set the locale to C.UTF-8
  , esKeepGhcRts :: !Bool
  -- ^ if True, keep GHCRTS variable in environment
  }
  deriving (Eq, Ord, Show)

minimalEnvSettings :: EnvSettings
minimalEnvSettings =
  EnvSettings
  { esIncludeLocals = False
  , esIncludeGhcPackagePath = False
  , esStackExe = False
  , esLocaleUtf8 = False
  , esKeepGhcRts = False
  }

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH.
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
  { esIncludeLocals = True
  , esIncludeGhcPackagePath = True
  , esStackExe = True
  , esLocaleUtf8 = False
  , esKeepGhcRts = True
  }

-- | Environment settings which do not embellish the environment
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
plainEnvSettings :: EnvSettings
plainEnvSettings = EnvSettings
  { esIncludeLocals = False
  , esIncludeGhcPackagePath = False
  , esStackExe = False
  , esLocaleUtf8 = False
  , esKeepGhcRts = True
  }
