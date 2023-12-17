{- HLINT ignore -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module System.OsString.Internal.Types
  (
    WindowsString(..)
  , pattern WS
  , unWS
  , PosixString(..)
  , unPS
  , pattern PS
  , PlatformString
  , WindowsChar(..)
  , unWW
  , pattern WW
  , PosixChar(..)
  , unPW
  , pattern PW
  , PlatformChar
  , OsString(..)
  , OsChar(..)
  )
where


import Control.DeepSeq
import Data.Data
import Data.Word
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import GHC.Generics (Generic)

import System.OsPath.Encoding.Internal
import qualified System.OsPath.Data.ByteString.Short as BS
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16
#if MIN_VERSION_template_haskell(2,16,0)
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
--
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays

-- | Commonly used windows string as wide character bytes.
newtype WindowsString = WindowsString { getWindowsString :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid, Typeable, Generic, NFData)

-- | Decodes as UCS-2.
instance Show WindowsString where
  -- cWcharsToChars_UCS2 is total
  show = show . cWcharsToChars_UCS2 . BS16.unpack . getWindowsString

-- | Just a short bidirectional synonym for 'WindowsString' constructor.
pattern WS :: BS.ShortByteString -> WindowsString
pattern WS { unWS } <- WindowsString unWS where
  WS a = WindowsString a
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE WS #-}
#endif


instance Lift WindowsString where
  lift (WindowsString bs)
    = [| WindowsString (BS.pack $(lift $ BS.unpack bs)) :: WindowsString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Commonly used Posix string as uninterpreted @char[]@
-- array.
newtype PosixString = PosixString { getPosixString :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid, Typeable, Generic, NFData)

-- | Prints the raw bytes without decoding.
instance Show PosixString where
  show (PosixString ps) = show ps

-- | Just a short bidirectional synonym for 'PosixString' constructor.
pattern PS :: BS.ShortByteString -> PosixString
pattern PS { unPS } <- PosixString unPS where
  PS a = PosixString a
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE PS #-}
#endif

instance Lift PosixString where
  lift (PosixString bs)
    = [| PosixString (BS.pack $(lift $ BS.unpack bs)) :: PosixString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype WindowsChar = WindowsChar { getWindowsChar :: Word16 }
  deriving (Eq, Ord, Typeable, Generic, NFData)

instance Show WindowsChar where
  show (WindowsChar wc) = show wc

newtype PosixChar   = PosixChar { getPosixChar :: Word8 }
  deriving (Eq, Ord, Typeable, Generic, NFData)

instance Show PosixChar where
  show (PosixChar pc) = show pc

-- | Just a short bidirectional synonym for 'WindowsChar' constructor.
pattern WW :: Word16 -> WindowsChar
pattern WW { unWW } <- WindowsChar unWW where
  WW a = WindowsChar a
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE WW #-}
#endif

-- | Just a short bidirectional synonym for 'PosixChar' constructor.
pattern PW :: Word8 -> PosixChar
pattern PW { unPW } <- PosixChar unPW where
  PW a = PosixChar a
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE PW #-}
#endif

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformChar = WindowsChar
#else
type PlatformChar = PosixChar
#endif


-- | Newtype representing short operating system specific strings.
--
-- Internally this is either 'WindowsString' or 'PosixString',
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency.
--
-- The constructor is only exported via "System.OsString.Internal.Types", since
-- dealing with the internals isn't generally recommended, but supported
-- in case you need to write platform specific code.
newtype OsString = OsString { getOsString :: PlatformString }
  deriving (Typeable, Generic, NFData)

-- | On windows, decodes as UCS-2. On unix prints the raw bytes without decoding.
instance Show OsString where
  show (OsString os) = show os

-- | Byte equality of the internal representation.
instance Eq OsString where
  (OsString a) == (OsString b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsString where
  compare (OsString a) (OsString b) = compare a b


-- | \"String-Concatenation\" for 'OsString'. This is __not__ the same
-- as '(</>)'.
instance Monoid OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = OsString (WindowsString BS.empty)
#if MIN_VERSION_base(4,16,0)
    mappend = (<>)
#else
    mappend (OsString (WindowsString a)) (OsString (WindowsString b))
      = OsString (WindowsString (mappend a b))
#endif
#else
    mempty      = OsString (PosixString BS.empty)
#if MIN_VERSION_base(4,16,0)
    mappend = (<>)
#else
    mappend (OsString (PosixString a)) (OsString (PosixString b))
      = OsString (PosixString (mappend a b))
#endif
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup OsString where
#if MIN_VERSION_base(4,16,0)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    (<>) (OsString (WindowsString a)) (OsString (WindowsString b))
      = OsString (WindowsString (mappend a b))
#else
    (<>) (OsString (PosixString a)) (OsString (PosixString b))
      = OsString (PosixString (mappend a b))
#endif
#else
    (<>) = mappend
#endif
#endif


instance Lift OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (OsString (WindowsString bs))
    = [| OsString (WindowsString (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#else
  lift (OsString (PosixString bs))
    = [| OsString (PosixString (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif


-- | Newtype representing a code unit.
--
-- On Windows, this is restricted to two-octet codepoints 'Word16',
-- on POSIX one-octet ('Word8').
newtype OsChar = OsChar { getOsChar :: PlatformChar }
  deriving (Typeable, Generic, NFData)

instance Show OsChar where
  show (OsChar pc) = show pc

-- | Byte equality of the internal representation.
instance Eq OsChar where
  (OsChar a) == (OsChar b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsChar where
  compare (OsChar a) (OsChar b) = compare a b
