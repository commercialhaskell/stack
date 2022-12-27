-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module System.Uname
    ( getRelease
    )
    where

#include <sys/utsname.h>

import Foreign
import Foreign.C

getRelease :: IO String
getRelease = do
  alloca $ \ ptr ->
             do throwErrnoIfMinus1_ "uname" $ uname ptr
                peekCString $ release ptr
-- | @'uname' name@ stores nul-terminated strings of information
--   identifying the current system info to the structure referenced
--   by name.
--
--   > import Foreign.C
--   > import Foreign.Marshal
--   >
--   > sysName :: IO String
--   > sysName = alloca $ \ ptr ->
--   >           do throwErrnoIfMinus1_ "uname" $ uname ptr
--   >              peekCString $ sysname ptr
--
foreign import ccall unsafe "haskell_uname"
        uname :: Ptr Utsname -> IO CInt

data Utsname

instance Storable Utsname where
    sizeOf    = const #size struct utsname
    alignment = const #alignment struct utsname
    poke      = error "Storable Utsname: peek: unsupported operation"
    peek      = error "Storable Utsname: poke: unsupported operation"

release :: Ptr Utsname -> CString
release = (#ptr struct utsname, release)
