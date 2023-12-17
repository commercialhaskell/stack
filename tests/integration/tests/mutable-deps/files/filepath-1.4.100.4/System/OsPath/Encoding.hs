{- HLINT ignore -}
module System.OsPath.Encoding
  (
  -- * Types
    EncodingException(..)
  , showEncodingException

  -- * UCS-2
  , ucs2le
  , mkUcs2le
  , ucs2le_DF
  , ucs2le_EF
  , ucs2le_decode
  , ucs2le_encode

  -- * UTF-16LE_b
  , utf16le_b
  , mkUTF16le_b
  , utf16le_b_DF
  , utf16le_b_EF
  , utf16le_b_decode
  , utf16le_b_encode

  -- * base encoding
  , encodeWithBasePosix
  , decodeWithBasePosix
  , encodeWithBaseWindows
  , decodeWithBaseWindows
  )
  where

import System.OsPath.Encoding.Internal
