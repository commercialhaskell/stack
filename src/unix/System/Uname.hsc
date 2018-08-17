module System.Uname
    ( Utsname
    , uname

    , sysname
    , nodename
    , release
    , version
    , machine
    )
    where

#include <sys/utsname.h>

import Foreign
import Foreign.C

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

sysname :: Ptr Utsname -> CString
sysname = (#ptr struct utsname, sysname)

nodename :: Ptr Utsname -> CString
nodename = (#ptr struct utsname, nodename)

release :: Ptr Utsname -> CString
release = (#ptr struct utsname, release)

version :: Ptr Utsname -> CString
version = (#ptr struct utsname, version)

machine :: Ptr Utsname -> CString
machine = (#ptr struct utsname, machine)
