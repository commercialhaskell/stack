{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module Stack.Constants.UsrLibDirs
  ( libDirs
  , usrLibDirs
  ) where

import          Path ( mkAbsDir )
import          Stack.Prelude

-- | Used in Stack.Setup for detecting libc.musl-x86_64.so.1, see comments at
-- use site
libDirs :: [Path Abs Dir]
libDirs = [$(mkAbsDir "/lib"), $(mkAbsDir "/lib64")]

-- | Used in Stack.Setup for detecting libtinfo, see comments at use site
usrLibDirs :: [Path Abs Dir]
usrLibDirs = [$(mkAbsDir "/usr/lib"), $(mkAbsDir "/usr/lib64")]
