{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module Stack.Constants.UsrLibDirs
  ( usrLibDirs
  ) where

import          Path ( mkAbsDir )
import          Stack.Prelude

-- | Used in Stack.Setup for detecting libtinfo, see comments at use site
usrLibDirs :: [Path Abs Dir]
usrLibDirs = [$(mkAbsDir "/usr/lib"), $(mkAbsDir "/usr/lib64")]
