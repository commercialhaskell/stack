{- HLINT ignore -}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : System.OsPath.Data.ByteString.Short
-- Copyright   : (c) Duncan Coutts 2012-2013, Julian Ospald 2022
-- License     : BSD-style
--
-- Maintainer  : hasufell@posteo.de
-- Stability   : stable
-- Portability : ghc only
--
-- A compact representation suitable for storing short byte strings in memory.
--
-- In typical use cases it can be imported alongside "Data.ByteString", e.g.
--
-- > import qualified Data.ByteString       as B
-- > import qualified Data.ByteString.Short as B
-- >          (ShortByteString, toShort, fromShort)
--
-- Other 'ShortByteString' operations clash with "Data.ByteString" or "Prelude"
-- functions however, so they should be imported @qualified@ with a different
-- alias e.g.
--
-- > import qualified Data.ByteString.Short as B.Short
--
module System.OsPath.Data.ByteString.Short (

    -- * The @ShortByteString@ type

    ShortByteString(..),

    -- ** Memory overhead
    -- | With GHC, the memory overheads are as follows, expressed in words and
    -- in bytes (words are 4 and 8 bytes on 32 or 64bit machines respectively).
    --
    -- * 'B.ByteString' unshared: 8 words; 32 or 64 bytes.
    --
    -- * 'B.ByteString' shared substring: 4 words; 16 or 32 bytes.
    --
    -- * 'ShortByteString': 4 words; 16 or 32 bytes.
    --
    -- For the string data itself, both 'ShortByteString' and 'B.ByteString' use
    -- one byte per element, rounded up to the nearest word. For example,
    -- including the overheads, a length 10 'ShortByteString' would take
    -- @16 + 12 = 28@ bytes on a 32bit platform and @32 + 16 = 48@ bytes on a
    -- 64bit platform.
    --
    -- These overheads can all be reduced by 1 word (4 or 8 bytes) when the
    -- 'ShortByteString' or 'B.ByteString' is unpacked into another constructor.
    --
    -- For example:
    --
    -- > data ThingId = ThingId {-# UNPACK #-} !Int
    -- >                        {-# UNPACK #-} !ShortByteString
    --
    -- This will take @1 + 1 + 3@ words (the @ThingId@ constructor +
    -- unpacked @Int@ + unpacked @ShortByteString@), plus the words for the
    -- string data.

    -- ** Heap fragmentation
    -- | With GHC, the 'B.ByteString' representation uses /pinned/ memory,
    -- meaning it cannot be moved by the GC. This is usually the right thing to
    -- do for larger strings, but for small strings using pinned memory can
    -- lead to heap fragmentation which wastes space. The 'ShortByteString'
    -- type (and the @Text@ type from the @text@ package) use /unpinned/ memory
    -- so they do not contribute to heap fragmentation. In addition, with GHC,
    -- small unpinned strings are allocated in the same way as normal heap
    -- allocations, rather than in a separate pinned area.

    -- * Introducing and eliminating 'ShortByteString's
    empty,
    singleton,
    pack,
    unpack,
    fromShort,
    toShort,

    -- * Basic interface
    snoc,
    cons,
    append,
    last,
    tail,
    uncons,
    uncons2,
    head,
    init,
    unsnoc,
    null,
    length,

    -- * Transforming ShortByteStrings
    map,
    reverse,
    intercalate,

    -- * Reducing 'ShortByteString's (folds)
    foldl,
    foldl',
    foldl1,
    foldl1',

    foldr,
    foldr',
    foldr1,
    foldr1',

    -- ** Special folds
    all,
    any,
    concat,

    -- ** Generating and unfolding ByteStrings
    replicate,
    unfoldr,
    unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    takeWhileEnd,
    takeWhile,
    drop,
    dropEnd,
    dropWhile,
    dropWhileEnd,
    breakEnd,
    break,
    span,
    spanEnd,
    splitAt,
    split,
    splitWith,
    stripSuffix,
    stripPrefix,

    -- * Predicates
    isInfixOf,
    isPrefixOf,
    isSuffixOf,

    -- ** Search for arbitrary substrings
    breakSubstring,

    -- * Searching ShortByteStrings

    -- ** Searching by equality
    elem,

    -- ** Searching with a predicate
    find,
    filter,
    partition,

    -- * Indexing ShortByteStrings
    index,
    indexMaybe,
    (!?),
    elemIndex,
    elemIndices,
    count,
    findIndex,
    findIndices,

    -- * Low level conversions
    -- ** Packing 'Foreign.C.String.CString's and pointers
    packCString,
    packCStringLen,

    -- ** Using ShortByteStrings as 'Foreign.C.String.CString's
    useAsCString,
    useAsCStringLen,
  ) where

import Data.ByteString.Short.Internal
import System.OsPath.Data.ByteString.Short.Internal

import Prelude (Maybe(..), Ord(..), Num(..), ($), otherwise)
import Data.Word (Word8)

uncons2 :: ShortByteString -> Maybe (Word8, Word8, ShortByteString)
uncons2 = \sbs ->
  let l  = length sbs
      nl = l - 2
  in if | l <= 1 -> Nothing
        | otherwise -> let h  = indexWord8Array (asBA sbs) 0
                           h' = indexWord8Array (asBA sbs) 1
                           t  = create nl $ \mba -> copyByteArray (asBA sbs) 1 mba 0 nl
                       in Just (h, h', t)
