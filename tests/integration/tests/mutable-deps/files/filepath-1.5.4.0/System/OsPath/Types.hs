{-# LANGUAGE CPP #-}

module System.OsPath.Types
  (
  -- * FilePath types
    OsPath
  , WindowsPath
  , PosixPath
  , PlatformPath

  -- * OsString reexports
  , WindowsString
  , PosixString
  , WindowsChar
  , PosixChar
  , OsString
  , OsChar
  )
where

import System.OsString.Internal.Types


-- | Filepaths are @wchar_t*@ data on windows as passed to syscalls.
type WindowsPath = WindowsString

-- | Filepaths are @char[]@ data on unix as passed to syscalls.
type PosixPath = PosixString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- | Ifdef around current platform (either 'WindowsPath' or 'PosixPath').
type PlatformPath = WindowsPath
#else
-- | Ifdef around current platform (either 'WindowsPath' or 'PosixPath').
type PlatformPath = PosixPath
#endif


-- | Type representing filenames\/pathnames.
--
-- This type doesn't add any guarantees over 'OsString'.
type OsPath = OsString
