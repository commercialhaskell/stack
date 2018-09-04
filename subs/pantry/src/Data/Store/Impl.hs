{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module is not exposed. The reason that it is split out from
-- "Data.Store.Internal" is to allow "Data.Store.TH" to refer to these
-- identifiers. "Data.Store.Internal" must be separate from
-- "Data.Store.TH" due to Template Haskell's stage restriction.
module Data.Store.Impl where

import           Control.Applicative
import           Control.Exception (try)
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Proxy
import           Data.Store.Core
import           Data.Typeable (Typeable, typeRep)
import           Data.Word
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Generics
import           GHC.TypeLits
import           Prelude
import           System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- Store class

-- TODO: write down more elaborate laws

-- | The 'Store' typeclass provides efficient serialization and
-- deserialization to raw pointer addresses.
--
-- The 'peek' and 'poke' methods should be defined such that
-- @ decodeEx (encode x) == x @.
class Store a where
    -- | Yields the 'Size' of the buffer, in bytes, required to store
    -- the encoded representation of the type.
    --
    -- Note that the correctness of this function is crucial for the
    -- safety of 'poke', as it does not do any bounds checking. It is
    -- the responsibility of the invoker of 'poke' ('encode' and similar
    -- functions) to ensure that there's enough space in the output
    -- buffer. If 'poke' writes beyond, then arbitrary memory can be
    -- overwritten, causing undefined behavior and segmentation faults.
    size :: Size a
    -- | Serializes a value to bytes. It is the responsibility of the
    -- caller to ensure that at least the number of bytes required by
    -- 'size' are available. These details are handled by 'encode' and
    -- similar utilities.
    poke :: a -> Poke ()
    -- | Serialized a value from bytes, throwing exceptions if it
    -- encounters invalid data or runs out of input bytes.
    peek :: Peek a

    default size :: (Generic a, GStoreSize (Rep a)) => Size a
    size = genericSize

    default poke :: (Generic a, GStorePoke (Rep a)) => a -> Poke ()
    poke = genericPoke

    default peek :: (Generic a , GStorePeek (Rep a)) => Peek a
    peek = genericPeek

    -- NB: Do not INLINE the default implementations of size, poke, or peek!
    -- Doing so can lead to enormous memory blowup (a maximum residency of
    -- 5.17 GB with GHC 8.0.2 has been observed). For more information, please
    -- read issue #91.

------------------------------------------------------------------------
-- Utilities for encoding / decoding strict ByteStrings

-- | Serializes a value to a 'BS.ByteString'. In order to do this, it
-- first allocates a 'BS.ByteString' of the correct size (based on
-- 'size'), and then uses 'poke' to fill it.
--
-- Safety of this function depends on correctness of the 'Store'
-- instance. If 'size' returns a. The good news is that this isn't an
-- issue if you use well-tested manual instances (such as those from
-- this package) combined with auomatic definition of instances.
encode :: Store a => a -> BS.ByteString
encode x = unsafeEncodeWith (poke x) (getSize x)

-- | Decodes a value from a 'BS.ByteString'. Returns an exception if
-- there's an error while decoding, or if decoding undershoots /
-- overshoots the end of the buffer.
decode :: Store a => BS.ByteString -> Either PeekException a
decode = unsafePerformIO . try . decodeIO

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions. It is an exception to not consume all input.
decodeEx :: Store a => BS.ByteString -> a
decodeEx = unsafePerformIO . decodeIO

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions. It is an exception to not consume all input.
decodeIO :: Store a => BS.ByteString -> IO a
decodeIO = decodeIOWith peek

------------------------------------------------------------------------
-- Size

-- | Info about a type's serialized length. Either the length is known
-- independently of the value, or the length depends on the value.
data Size a
    = VarSize (a -> Int)
    | ConstSize !Int
    deriving Typeable

instance Contravariant Size where
  contramap f sz = case sz of
    ConstSize n -> ConstSize n
    VarSize g -> VarSize (\x -> g (f x))

-- | Get the number of bytes needed to store the given value. See
-- 'size'.
getSize :: Store a => a -> Int
getSize = getSizeWith size
{-# INLINE getSize #-}

-- | Given a 'Size' value and a value of the type @a@, returns its 'Int'
-- size.
getSizeWith :: Size a -> a -> Int
getSizeWith (VarSize f) x = f x
getSizeWith (ConstSize n) _ = n
{-# INLINE getSizeWith #-}

-- | Create an aggregate 'Size' by providing functions to split the
-- input into two pieces.
--
-- If both of the types are 'ConstSize', the result is 'ConstSize' and
-- the functions will not be used.
combineSize :: forall a b c. (Store a, Store b) => (c -> a) -> (c -> b) -> Size c
combineSize toA toB = combineSizeWith toA toB size size
{-# INLINE combineSize #-}

-- | Create an aggregate 'Size' by providing functions to split the
-- input into two pieces, as well as 'Size' values to use to measure the
-- results.
--
-- If both of the input 'Size' values are 'ConstSize', the result is
-- 'ConstSize' and the functions will not be used.
combineSizeWith :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
combineSizeWith toA toB sizeA sizeB =
    case (sizeA, sizeB) of
        (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
        (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
        (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)
{-# INLINE combineSizeWith #-}

-- | Adds a constant amount to a 'Size' value.
addSize :: Int -> Size a -> Size a
addSize x (ConstSize n) = ConstSize (x + n)
addSize x (VarSize f) = VarSize ((x +) . f)
{-# INLINE addSize #-}

-- | A 'size' implementation based on an instance of 'Storable' and
-- 'Typeable'.
sizeStorable :: forall a. (Storable a, Typeable a) => Size a
sizeStorable = sizeStorableTy (show (typeRep (Proxy :: Proxy a)))
{-# INLINE sizeStorable #-}

-- | A 'size' implementation based on an instance of 'Storable'. Use this
-- if the type is not 'Typeable'.
sizeStorableTy :: forall a. Storable a => String -> Size a
sizeStorableTy ty = ConstSize (sizeOf (error msg :: a))
  where
    msg = "In Data.Store.storableSize: " ++ ty ++ "'s sizeOf evaluated its argument."
{-# INLINE sizeStorableTy #-}

------------------------------------------------------------------------
-- Generics

genericSize :: (Generic a, GStoreSize (Rep a)) => Size a
genericSize = contramap from gsize
{-# INLINE genericSize #-}

genericPoke :: (Generic a, GStorePoke (Rep a)) => a -> Poke ()
genericPoke = gpoke . from
{-# INLINE genericPoke #-}

genericPeek :: (Generic a , GStorePeek (Rep a)) => Peek a
genericPeek = to <$> gpeek
{-# INLINE genericPeek #-}

type family SumArity (a :: * -> *) :: Nat where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

-- This could be just one typeclass, but currently compile times are
-- better with things split up.
-- https://github.com/bos/aeson/pull/335
--

class GStoreSize f where gsize :: Size (f a)
class GStorePoke f where gpoke :: f a -> Poke ()
class GStorePeek f where gpeek :: Peek (f a)

instance GStoreSize f => GStoreSize (M1 i c f) where
    gsize = contramap unM1 gsize
    {-# INLINE gsize #-}
instance GStorePoke f => GStorePoke (M1 i c f) where
    gpoke = gpoke . unM1
    {-# INLINE gpoke #-}
instance GStorePeek f => GStorePeek (M1 i c f) where
    gpeek = fmap M1 gpeek
    {-# INLINE gpeek #-}

instance Store a => GStoreSize (K1 i a) where
    gsize = contramap unK1 size
    {-# INLINE gsize #-}
instance Store a => GStorePoke (K1 i a) where
    gpoke = poke . unK1
    {-# INLINE gpoke #-}
instance Store a => GStorePeek (K1 i a) where
    gpeek = fmap K1 peek
    {-# INLINE gpeek #-}

instance GStoreSize U1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}
instance GStorePoke U1 where
    gpoke _ = return ()
    {-# INLINE gpoke #-}
instance GStorePeek U1 where
    gpeek = return U1
    {-# INLINE gpeek #-}

instance GStoreSize V1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}
instance GStorePoke V1 where
    gpoke x = case x of {}
    {-# INLINE gpoke #-}
instance GStorePeek V1 where
    gpeek = undefined
    {-# INLINE gpeek #-}

instance (GStoreSize a, GStoreSize b) => GStoreSize (a :*: b) where
    gsize = combineSizeWith (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
    {-# INLINE gsize #-}
instance (GStorePoke a, GStorePoke b) => GStorePoke (a :*: b) where
    gpoke (a :*: b) = gpoke a >> gpoke b
    {-# INLINE gpoke #-}
instance (GStorePeek a, GStorePeek b) => GStorePeek (a :*: b) where
    gpeek = (:*:) <$> gpeek <*> gpeek
    {-# INLINE gpeek #-}

-- The machinery for sum types is why UndecidableInstances is necessary.

-- FIXME: check that this type level stuff dosen't get turned into
-- costly runtime computation

instance (SumArity (a :+: b) <= 255, GStoreSizeSum 0 (a :+: b))
         => GStoreSize (a :+: b) where
    gsize = VarSize $ \x -> sizeOf (undefined :: Word8) + gsizeSum x (Proxy :: Proxy 0)
    {-# INLINE gsize #-}
instance (SumArity (a :+: b) <= 255, GStorePokeSum 0 (a :+: b))
         => GStorePoke (a :+: b) where
    gpoke x = gpokeSum x (Proxy :: Proxy 0)
    {-# INLINE gpoke #-}
instance (SumArity (a :+: b) <= 255, GStorePeekSum 0 (a :+: b))
         => GStorePeek (a :+: b) where
    gpeek = do
        tag <- peekStorable
        gpeekSum tag (Proxy :: Proxy 0)
    {-# INLINE gpeek #-}

-- Similarly to splitting up the generic class into multiple classes, we
-- also split up the one for sum types.

class KnownNat n => GStoreSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: f a -> Proxy n -> Int
class KnownNat n => GStorePokeSum (n :: Nat) (f :: * -> *) where gpokeSum :: f p -> Proxy n -> Poke ()
class KnownNat n => GStorePeekSum (n :: Nat) (f :: * -> *) where gpeekSum :: Word8 -> Proxy n -> Peek (f p)

instance (GStoreSizeSum n a, GStoreSizeSum (n + SumArity a) b, KnownNat n)
         => GStoreSizeSum n (a :+: b) where
    gsizeSum (L1 l) _ = gsizeSum l (Proxy :: Proxy n)
    gsizeSum (R1 r) _ = gsizeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gsizeSum #-}
instance (GStorePokeSum n a, GStorePokeSum (n + SumArity a) b, KnownNat n)
         => GStorePokeSum n (a :+: b) where
    gpokeSum (L1 l) _ = gpokeSum l (Proxy :: Proxy n)
    gpokeSum (R1 r) _ = gpokeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gpokeSum #-}
instance (GStorePeekSum n a, GStorePeekSum (n + SumArity a) b, KnownNat n)
         => GStorePeekSum n (a :+: b) where
    gpeekSum tag proxyL
        | tag < sizeL = L1 <$> gpeekSum tag proxyL
        | otherwise = R1 <$> gpeekSum tag (Proxy :: Proxy (n + SumArity a))
      where
        sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))
    {-# INLINE gpeekSum #-}

instance (GStoreSize a, KnownNat n) => GStoreSizeSum n (C1 c a) where
    gsizeSum x _ = getSizeWith gsize x
    {-# INLINE gsizeSum #-}
instance (GStorePoke a, KnownNat n) => GStorePokeSum n (C1 c a) where
    gpokeSum x _ = do
        pokeStorable (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
        gpoke x
    {-# INLINE gpokeSum #-}
instance (GStorePeek a, KnownNat n) => GStorePeekSum n (C1 c a) where
    gpeekSum tag _
        | tag == cur = gpeek
        | tag > cur = peekException "Sum tag invalid"
        | otherwise = peekException "Error in implementation of Store Generics"
      where
        cur = fromInteger (natVal (Proxy :: Proxy n))
    {-# INLINE gpeekSum #-}
