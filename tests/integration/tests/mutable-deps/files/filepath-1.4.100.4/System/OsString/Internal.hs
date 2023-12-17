{- HLINT ignore -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

module System.OsString.Internal where

import System.OsString.Internal.Types

import Control.Monad.Catch
    ( MonadThrow )
import Data.ByteString
    ( ByteString )
import Data.Char
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import System.IO
    ( TextEncoding )

import System.OsPath.Encoding ( EncodingException(..) )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import qualified System.OsString.Windows as PF
#else
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import qualified System.OsString.Posix as PF
#endif




-- | Partial unicode friendly encoding.
--
-- On windows this encodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this encodes as UTF8 (strictly), which is a good guess.
--
-- Throws a 'EncodingException' if encoding fails.
encodeUtf :: MonadThrow m => String -> m OsString
encodeUtf = fmap OsString . PF.encodeUtf

-- | Encode an 'OsString' given the platform specific encodings.
encodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> String
           -> Either EncodingException OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
encodeWith _ winEnc str = OsString <$> PF.encodeWith winEnc str
#else
encodeWith unixEnc _ str = OsString <$> PF.encodeWith unixEnc str
#endif

-- | Like 'encodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
encodeFS :: String -> IO OsString
encodeFS = fmap OsString . PF.encodeFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
decodeUtf :: MonadThrow m => OsString -> m String
decodeUtf (OsString x) = PF.decodeUtf x

-- | Decode an 'OsString' with the specified encoding.
--
-- The String is forced into memory to catch all exceptions.
decodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> OsString
           -> Either EncodingException String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
decodeWith _ winEnc (OsString x) = PF.decodeWith winEnc x
#else
decodeWith unixEnc _ (OsString x) = PF.decodeWith unixEnc x
#endif


-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
decodeFS :: OsString -> IO String
decodeFS (OsString x) = PF.decodeFS x


-- | Constructs an @OsString@ from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
fromBytes :: MonadThrow m
          => ByteString
          -> m OsString
fromBytes = fmap OsString . PF.fromBytes


-- | QuasiQuote an 'OsString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows.
osstr :: QuasiQuoter
osstr =
  QuasiQuoter
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  { quoteExp = \s -> do
      osp <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF16le ErrorOnCodingFailure) $ s
      lift osp
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp = \s -> do
      osp <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF8 ErrorOnCodingFailure) $ s
      lift osp
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#endif


-- | Unpack an 'OsString' to a list of 'OsChar'.
unpack :: OsString -> [OsChar]
unpack (OsString x) = OsChar <$> PF.unpack x


-- | Pack a list of 'OsChar' to an 'OsString'
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'OsString' is probably not what
-- you want, because it will truncate unicode code points.
pack :: [OsChar] -> OsString
pack = OsString . PF.pack . fmap (\(OsChar x) -> x)


-- | Truncates on unix to 1 and on Windows to 2 octets.
unsafeFromChar :: Char -> OsChar
unsafeFromChar = OsChar . PF.unsafeFromChar

-- | Converts back to a unicode codepoint (total).
toChar :: OsChar -> Char
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toChar (OsChar (WindowsChar w)) = chr $ fromIntegral w
#else
toChar (OsChar (PosixChar w)) = chr $ fromIntegral w
#endif
