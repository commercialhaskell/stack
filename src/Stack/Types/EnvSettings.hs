{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

module Stack.Types.EnvSettings
  ( EnvSettings (..)
  , minimalEnvSettings
  , defaultEnvSettings
  , plainEnvSettings
  ) where

import           Stack.Prelude

-- | Controls which version of the environment is used
data EnvSettings = EnvSettings
  { includeLocals :: !Bool
  -- ^ include local project bin directory, GHC_PACKAGE_PATH, etc
  , includeGhcPackagePath :: !Bool
  -- ^ include the GHC_PACKAGE_PATH variable
  , stackExe :: !Bool
  -- ^ set the STACK_EXE variable to the current executable name
  , localeUtf8 :: !Bool
  -- ^ set the locale to C.UTF-8
  , keepGhcRts :: !Bool
  -- ^ if True, keep GHCRTS variable in environment
  }
  deriving (Eq, Ord, Show)

minimalEnvSettings :: EnvSettings
minimalEnvSettings =
  EnvSettings
  { includeLocals = False
  , includeGhcPackagePath = False
  , stackExe = False
  , localeUtf8 = False
  , keepGhcRts = False
  }

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH.
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
  { includeLocals = True
  , includeGhcPackagePath = True
  , stackExe = True
  , localeUtf8 = False
  , keepGhcRts = True
  }

-- | Environment settings which do not embellish the environment
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
plainEnvSettings :: EnvSettings
plainEnvSettings = EnvSettings
  { includeLocals = False
  , includeGhcPackagePath = False
  , stackExe = False
  , localeUtf8 = False
  , keepGhcRts = True
  }
