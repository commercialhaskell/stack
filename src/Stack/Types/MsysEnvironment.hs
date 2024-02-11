{-# LANGUAGE NoImplicitPrelude #-}

-- | Type representing MSYS2 environments and related functions.
module Stack.Types.MsysEnvironment
  ( MsysEnvironment (..)
  , msysEnvArch
  , relDirMsysEnv
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Distribution.System ( Arch (..) )
import           Stack.Constants
                   ( relDirClang32, relDirClang64, relDirClangArm64
                   , relDirMingw32, relDirMingw64, relDirUcrt64
                   )
import           Stack.Prelude

-- | Type representing MSYS2 environments.
data MsysEnvironment
  = CLANG32
  | CLANG64
  | CLANGARM64
  | MINGW32
    -- ^ Stack's default on architecture i386, and applied if GHC version is
    -- earlier than GHC 9.6.
  | MINGW64
    -- ^ Stack's default on architecture x86_64, and applied if GHC version is
    -- earlier than GHC 9.6.
  | UCRT64
  deriving (Eq, Ord, Show)

-- | MSYS2 environment names are treated as case sensitive.
instance FromJSON MsysEnvironment where
  parseJSON v = do
    s <- parseJSON v
    case s of
      "CLANG32" -> pure CLANG32
      "CLANG64" -> pure CLANG64
      "CLANGARM64" -> pure CLANGARM64
      "MINGW32" -> pure MINGW32
      "MINGW64" -> pure MINGW64
      "UCRT64" -> pure UCRT64
      _ -> fail ("Unknown MSYS2 environment: " <> s)

-- | Function that yields the architecture relevant to an MSYS2 environment,
-- based on https://www.msys2.org/docs/environments/.
msysEnvArch :: MsysEnvironment -> Arch
msysEnvArch env = case env of
  CLANG32 -> I386
  CLANG64 -> X86_64
  CLANGARM64 -> AArch64
  MINGW32 -> I386
  MINGW64 -> X86_64
  UCRT64 -> X86_64

-- | Function that yields the prefix relevant to an MSYS2 environment, based on
-- https://www.msys2.org/docs/environments/.
relDirMsysEnv :: MsysEnvironment -> Path Rel Dir
relDirMsysEnv env = case env of
  CLANG32 -> relDirClang32
  CLANG64 -> relDirClang64
  CLANGARM64 -> relDirClangArm64
  MINGW32 -> relDirMingw32
  MINGW64 -> relDirMingw64
  UCRT64 -> relDirUcrt64
