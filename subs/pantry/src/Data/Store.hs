-- | This is the main public API of the store package. The functions
-- exported here are more likely to be stable between versions.
--
-- Usually you won't need to write your own 'Store' instances, and
-- instead can rely on either using the 'Generic' deriving approach or
-- "Data.Store.TH" for defining 'Store' instances for your datatypes.
-- There are some tradeoffs here - the generics instances do not require
-- @-XTemplateHaskell@, but they do not optimize as well for sum types
-- that only require a constant number of bytes.
module Data.Store
    (
    -- * Encoding and decoding strict ByteStrings.
      encode,
      decode, decodeWith,
      decodeEx, decodeExWith, decodeExPortionWith,
      decodeIO, decodeIOWith, decodeIOPortionWith
    -- * Store class and related types.
    , Store(..), Size(..), Poke, Peek
    , GStoreSize, GStorePoke, GStorePeek
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException
    ) where

import Data.Store.Internal
