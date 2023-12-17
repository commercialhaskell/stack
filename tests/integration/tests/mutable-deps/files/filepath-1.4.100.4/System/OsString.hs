{- HLINT ignore -}
-- |
-- Module      :  OsString
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of platform specific short 'OsString', which is:
--
-- 1. on windows wide char bytes (@[Word16]@)
-- 2. on unix char bytes (@[Word8]@)
--
-- It captures the notion of syscall specific encoding (or the lack thereof) to avoid roundtrip issues
-- and memory fragmentation by using unpinned byte arrays. Bytes are not touched or interpreted.
module System.OsString
  (
  -- * String types
    OsString

  -- * OsString construction
  , encodeUtf
  , encodeWith
  , encodeFS
  , osstr
  , pack

  -- * OsString deconstruction
  , decodeUtf
  , decodeWith
  , decodeFS
  , unpack

  -- * Word types
  , OsChar

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar
  )
where

import System.OsString.Internal
    ( unsafeFromChar
    , toChar
    , encodeUtf
    , encodeWith
    , encodeFS
    , osstr
    , pack
    , decodeUtf
    , decodeWith
    , decodeFS
    , unpack
    )
import System.OsString.Internal.Types
    ( OsString, OsChar )
