{- HLINT ignore -}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , TypeApplications
           , MultiWayIf
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}


module System.OsPath.Encoding.Internal where

import qualified System.OsPath.Data.ByteString.Short as BS8
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import Data.Bits
import Control.Exception (SomeException, try, Exception (displayException), evaluate)
import qualified GHC.Foreign as GHC
import Data.Either (Either)
import GHC.IO (unsafePerformIO)
import Control.DeepSeq (force, NFData (rnf))
import Data.Bifunctor (first)
import Data.Data (Typeable)
import GHC.Show (Show (show))
import Numeric (showHex)
import Foreign.C (CStringLen)
import Data.Char (chr)
import Foreign
import Prelude (FilePath)
import GHC.IO.Encoding (getFileSystemEncoding)

-- -----------------------------------------------------------------------------
-- UCS-2 LE
--

ucs2le :: TextEncoding
ucs2le = mkUcs2le ErrorOnCodingFailure

mkUcs2le :: CodingFailureMode -> TextEncoding
mkUcs2le cfm = TextEncoding { textEncodingName = "UCS-2LE",
                              mkTextDecoder = ucs2le_DF cfm,
                              mkTextEncoder = ucs2le_EF cfm }

ucs2le_DF :: CodingFailureMode -> IO (TextDecoder ())
ucs2le_DF cfm =
  return (BufferCodec {
             encode   = ucs2le_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

ucs2le_EF :: CodingFailureMode -> IO (TextEncoder ())
ucs2le_EF cfm =
  return (BufferCodec {
             encode   = ucs2le_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


ucs2le_decode :: DecodeBuffer
ucs2le_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
       loop !ir !ow
         | ow >= os     = done OutputUnderflow ir ow
         | ir >= iw     = done InputUnderflow ir ow
         | ir + 1 == iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              let x1 = fromIntegral c1 `shiftL` 8 + fromIntegral c0
              ow' <- writeCharBuf oraw ow (unsafeChr x1)
              loop (ir+2) ow'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0


ucs2le_encode :: EncodeBuffer
ucs2le_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done InputUnderflow ir ow
        | os - ow < 2  =  done OutputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case ord c of
             x | x < 0x10000 -> do
                     writeWord8Buf oraw ow     (fromIntegral x)
                     writeWord8Buf oraw (ow+1) (fromIntegral (x `shiftR` 8))
                     loop ir' (ow+2)
               | otherwise -> done InvalidSequence ir ow
    in
    loop ir0 ow0

-- -----------------------------------------------------------------------------
-- UTF-16b
--

-- | Mimics the base encoding for filesystem operations. This should be total on all inputs (word16 byte arrays).
--
-- Note that this has a subtle difference to 'encodeWithBaseWindows'/'decodeWithBaseWindows': it doesn't care for
-- the @0x0000@ end marker and will as such produce different results. Use @takeWhile (/= '\NUL')@ on the input
-- to recover this behavior.
utf16le_b :: TextEncoding
utf16le_b = mkUTF16le_b ErrorOnCodingFailure

mkUTF16le_b :: CodingFailureMode -> TextEncoding
mkUTF16le_b cfm = TextEncoding { textEncodingName = "UTF-16LE_b",
                                 mkTextDecoder = utf16le_b_DF cfm,
                                 mkTextEncoder = utf16le_b_EF cfm }

utf16le_b_DF :: CodingFailureMode -> IO (TextDecoder ())
utf16le_b_DF cfm =
  return (BufferCodec {
             encode   = utf16le_b_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

utf16le_b_EF :: CodingFailureMode -> IO (TextEncoder ())
utf16le_b_EF cfm =
  return (BufferCodec {
             encode   = utf16le_b_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


utf16le_b_decode :: DecodeBuffer
utf16le_b_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
       loop !ir !ow
         | ow >= os     = done OutputUnderflow ir ow
         | ir >= iw     = done InputUnderflow ir ow
         | ir + 1 == iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              let x1 = fromIntegral c1 `shiftL` 8 + fromIntegral c0
              if | iw - ir >= 4 -> do
                      c2 <- readWord8Buf iraw (ir+2)
                      c3 <- readWord8Buf iraw (ir+3)
                      let x2 = fromIntegral c3 `shiftL` 8 + fromIntegral c2
                      if | 0xd800 <= x1 && x1 <= 0xdbff
                         , 0xdc00 <= x2 && x2 <= 0xdfff -> do
                             ow' <- writeCharBuf oraw ow (unsafeChr ((x1 - 0xd800)*0x400 + (x2 - 0xdc00) + 0x10000))
                             loop (ir+4) ow'
                         | otherwise -> do
                             ow' <- writeCharBuf oraw ow (unsafeChr x1)
                             loop (ir+2) ow'
                 | iw - ir >= 2 -> do
                        ow' <- writeCharBuf oraw ow (unsafeChr x1)
                        loop (ir+2) ow'
                 | otherwise -> done InputUnderflow ir ow

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0


utf16le_b_encode :: EncodeBuffer
utf16le_b_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done InputUnderflow ir ow
        | os - ow < 2  =  done OutputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case ord c of
             x | x < 0x10000 -> do
                     writeWord8Buf oraw ow     (fromIntegral x)
                     writeWord8Buf oraw (ow+1) (fromIntegral (x `shiftR` 8))
                     loop ir' (ow+2)
               | otherwise ->
                     if os - ow < 4 then done OutputUnderflow ir ow else do
                     let x' = x - 0x10000
                         w1 = x' `div` 0x400 + 0xd800
                         w2 = x' `mod` 0x400 + 0xdc00
                     writeWord8Buf oraw ow     (fromIntegral w1)
                     writeWord8Buf oraw (ow+1) (fromIntegral (w1 `shiftR` 8))
                     writeWord8Buf oraw (ow+2) (fromIntegral w2)
                     writeWord8Buf oraw (ow+3) (fromIntegral (w2 `shiftR` 8))
                     loop ir' (ow+4)
    in
    loop ir0 ow0

-- -----------------------------------------------------------------------------
-- Windows encoding (ripped off from base)
--

cWcharsToChars_UCS2 :: [Word16] -> [Char]
cWcharsToChars_UCS2 = map (chr . fromIntegral)


-- On Windows, wchar_t is 16 bits wide and CWString uses the UTF-16 encoding.

-- coding errors generate Chars in the surrogate range
cWcharsToChars :: [Word16] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 :: [Int] -> [Int]
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

charsToCWchars :: [Char] -> [Word16]
charsToCWchars = foldr (utf16Char . ord) []
 where
  utf16Char :: Int -> [Word16] -> [Word16]
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- FFI
--

withFilePathWin :: FilePath -> (Int -> Ptr Word16 -> IO a) -> IO a
withFilePathWin = withArrayLen . charsToCWchars

peekFilePathWin :: (Ptr Word16, Int) -> IO FilePath
peekFilePathWin (cp, l) = do
  cs <- peekArray l cp
  return (cWcharsToChars cs)

withFilePathPosix :: FilePath -> (CStringLen -> IO a) -> IO a
withFilePathPosix fp f = getFileSystemEncoding >>= \enc -> GHC.withCStringLen enc fp f

peekFilePathPosix :: CStringLen -> IO FilePath
peekFilePathPosix fp = getFileSystemEncoding >>= \enc -> GHC.peekCStringLen enc fp

-- | Decode with the given 'TextEncoding'.
decodeWithTE :: TextEncoding -> BS8.ShortByteString -> Either EncodingException String
decodeWithTE enc ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r

-- | Encode with the given 'TextEncoding'.
encodeWithTE :: TextEncoding -> String -> Either EncodingException BS8.ShortByteString
encodeWithTE enc str = unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r

-- -----------------------------------------------------------------------------
-- Encoders / decoders
--

-- | This mimics the filepath decoder base uses on unix,
-- with the small distinction that we're not truncating at NUL bytes (because we're not at
-- the outer FFI layer).
decodeWithBasePosix :: BS8.ShortByteString -> IO String
decodeWithBasePosix ba = BS8.useAsCStringLen ba $ \fp -> peekFilePathPosix fp

-- | This mimics the filepath dencoder base uses on unix,
-- with the small distinction that we're not truncating at NUL bytes (because we're not at
-- the outer FFI layer).
encodeWithBasePosix :: String -> IO BS8.ShortByteString
encodeWithBasePosix str = withFilePathPosix str $ \cstr -> BS8.packCStringLen cstr

-- | This mimics the filepath decoder base uses on windows,
-- with the small distinction that we're not truncating at NUL bytes (because we're not at
-- the outer FFI layer).
decodeWithBaseWindows :: BS16.ShortByteString -> IO String
decodeWithBaseWindows ba = BS16.useAsCWStringLen ba $ \fp -> peekFilePathWin fp

-- | This mimics the filepath dencoder base uses on windows,
-- with the small distinction that we're not truncating at NUL bytes (because we're not at
-- the outer FFI layer).
encodeWithBaseWindows :: String -> IO BS16.ShortByteString
encodeWithBaseWindows str = withFilePathWin str $ \l cstr -> BS16.packCWStringLen (cstr, l)


-- -----------------------------------------------------------------------------
-- Types
--

data EncodingException =
    EncodingError String (Maybe Word8)
    -- ^ Could not decode a byte sequence because it was invalid under
    -- the given encoding, or ran out of input in mid-decode.
    deriving (Eq, Typeable)


showEncodingException :: EncodingException -> String
showEncodingException (EncodingError desc (Just w))
    = "Cannot decode byte '\\x" ++ showHex w ("': " ++ desc)
showEncodingException (EncodingError desc Nothing)
    = "Cannot decode input: " ++ desc

instance Show EncodingException where
    show = showEncodingException

instance Exception EncodingException

instance NFData EncodingException where
    rnf (EncodingError desc w) = rnf desc `seq` rnf w


-- -----------------------------------------------------------------------------
-- Words
--

wNUL :: Word16
wNUL = 0x00
