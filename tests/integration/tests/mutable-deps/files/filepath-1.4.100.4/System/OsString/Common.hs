{- HLINT ignore -}
{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True
--
#if defined(WINDOWS)
#define WINDOWS_DOC
#else
#define POSIX_DOC
#endif

module System.OsString.MODULE_NAME
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
#else
    PosixString
  , PosixChar
#endif

  -- * String construction
  , encodeUtf
  , encodeWith
  , encodeFS
  , fromBytes
  , pstr
  , pack

  -- * String deconstruction
  , decodeUtf
  , decodeWith
  , decodeFS
  , unpack

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar
  )
where



import System.OsString.Internal.Types (
#ifdef WINDOWS
  WindowsString(..), WindowsChar(..)
#else
  PosixString(..), PosixChar(..)
#endif
  )

import Data.Char
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.ByteString.Internal
    ( ByteString )
import Control.Exception
    ( SomeException, try, displayException )
import Control.DeepSeq ( force )
import Data.Bifunctor ( first )
import GHC.IO
    ( evaluate, unsafePerformIO )
import qualified GHC.Foreign as GHC
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )


import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#ifdef WINDOWS
import System.OsPath.Encoding
import System.IO
    ( TextEncoding, utf16le )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16
import qualified System.OsPath.Data.ByteString.Short as BS8
#else
import System.OsPath.Encoding
import System.IO
    ( TextEncoding, utf8 )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import qualified System.OsPath.Data.ByteString.Short as BS
#endif



#ifdef WINDOWS_DOC
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF16-LE (strictly), which is a pretty good guess.
--
-- Throws an 'EncodingException' if encoding fails.
#else
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF8 (strictly), which is a good guess.
--
-- Throws an 'EncodingException' if encoding fails.
#endif
encodeUtf :: MonadThrow m => String -> m PLATFORM_STRING
#ifdef WINDOWS
encodeUtf = either throwM pure . encodeWith utf16le
#else
encodeUtf = either throwM pure . encodeWith utf8
#endif

-- | Encode a 'String' with the specified encoding.
encodeWith :: TextEncoding
           -> String
           -> Either EncodingException PLATFORM_STRING
encodeWith enc str = unsafePerformIO $ do
#ifdef WINDOWS
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> WindowsString <$> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> PosixString <$> BS.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif

#ifdef WINDOWS_DOC
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. This is safe to 'unsafePerformIO'/'unsafeDupablePerformIO'.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
encodeFS :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
encodeFS = fmap WindowsString . encodeWithBaseWindows
#else
encodeFS = fmap PosixString . encodeWithBasePosix
#endif


#ifdef WINDOWS_DOC
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF16-LE (strictly), which is a pretty good.
--
-- Throws a 'EncodingException' if decoding fails.
#else
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
#endif
decodeUtf :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
decodeUtf = either throwM pure . decodeWith utf16le
#else
decodeUtf = either throwM pure . decodeWith utf8
#endif

#ifdef WINDOWS
-- | Decode a 'WindowsString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding
           -> PLATFORM_STRING
           -> Either EncodingException String
decodeWith winEnc (WindowsString ba) = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen winEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
-- | Decode a 'PosixString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding
       -> PLATFORM_STRING
       -> Either EncodingException String
decodeWith unixEnc (PosixString ba) = unsafePerformIO $ do
  r <- try @SomeException $ BS.useAsCStringLen ba $ \fp -> GHC.peekCStringLen unixEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif


#ifdef WINDOWS_DOC
-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. 'unsafePerformIO'/'unsafeDupablePerformIO' are safe, however.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
decodeFS :: PLATFORM_STRING -> IO String
#ifdef WINDOWS
decodeFS (WindowsString ba) = decodeWithBaseWindows ba
#else
decodeFS (PosixString ba) = decodeWithBasePosix ba
#endif


#ifdef WINDOWS_DOC
-- | Constructs a platform string from a ByteString.
--
-- This ensures valid UCS-2LE.
-- Note that this doesn't expand Word8 to Word16 on windows, so you may get invalid UTF-16.
--
-- Throws 'EncodingException' on invalid UCS-2LE (although unlikely).
#else
-- | Constructs a platform string from a ByteString.
--
-- This is a no-op.
#endif
fromBytes :: MonadThrow m
          => ByteString
          -> m PLATFORM_STRING
#ifdef WINDOWS
fromBytes bs =
  let ws = WindowsString . BS16.toShort $ bs
  in either throwM (const . pure $ ws) $ decodeWith ucs2le ws
#else
fromBytes = pure . PosixString . BS.toShort
#endif


#ifdef WINDOWS_DOC
-- | QuasiQuote a 'WindowsString'. This accepts Unicode characters
-- and encodes as UTF-16LE on windows.
#else
-- | QuasiQuote a 'PosixString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix.
#endif
pstr :: QuasiQuoter
pstr =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF16le ErrorOnCodingFailure) s
      lift ps
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp = \s -> do
      ps <- either (fail . show) pure $ encodeWith (mkUTF8 ErrorOnCodingFailure) s
      lift ps
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#endif


-- | Unpack a platform string to a list of platform words.
unpack :: PLATFORM_STRING -> [PLATFORM_WORD]
#ifdef WINDOWS
unpack (WindowsString ba) = WindowsChar <$> BS16.unpack ba
#else
unpack (PosixString ba) = PosixChar <$> BS.unpack ba
#endif


-- | Pack a list of platform words to a platform string.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to platform string is probably not what
-- you want, because it will truncate unicode code points.
pack :: [PLATFORM_WORD] -> PLATFORM_STRING
#ifdef WINDOWS
pack = WindowsString . BS16.pack . fmap (\(WindowsChar w) -> w)
#else
pack = PosixString . BS.pack . fmap (\(PosixChar w) -> w)
#endif


#ifdef WINDOWS
-- | Truncates to 2 octets.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = WindowsChar . fromIntegral . fromEnum
#else
-- | Truncates to 1 octet.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = PosixChar . fromIntegral . fromEnum
#endif

-- | Converts back to a unicode codepoint (total).
toChar :: PLATFORM_WORD -> Char
#ifdef WINDOWS
toChar (WindowsChar w) = chr $ fromIntegral w
#else
toChar (PosixChar w) = chr $ fromIntegral w
#endif
