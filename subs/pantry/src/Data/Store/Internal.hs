{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyCase #-}

-- | Internal API for the store package. The functions here which are
-- not re-exported by "Data.Store" are less likely to have stable APIs.
--
-- This module also defines most of the included 'Store' instances, for
-- types from the base package and other commonly used packages
-- (bytestring, containers, text, time, etc).
module Data.Store.Internal
    (
    -- * Encoding and decoding strict ByteStrings.
      encode,
      decode, decodeWith,
      decodeEx, decodeExWith, decodeExPortionWith
    , decodeIO, decodeIOWith, decodeIOPortionWith
    -- * Store class and related types.
    , Store(..), Poke, Peek, runPeek
    -- ** Exceptions thrown by Poke
    , PokeException(..), pokeException
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException, tooManyBytes
    -- ** Size type
    , Size(..)
    , getSize, getSizeWith
    , combineSize, combineSizeWith, addSize
    -- ** Store instances in terms of IsSequence
    , sizeSequence, pokeSequence, peekSequence
    -- ** Store instances in terms of IsSet
    , sizeSet, pokeSet, peekSet
    , markMapPokedInAscendingOrder
    -- ** Store instances in terms of IsMap
    , sizeMap, pokeMap, peekMap
    -- *** Utilities for ordered maps
    , sizeOrdMap, pokeOrdMap, peekOrdMapWith
    -- ** Store instances in terms of IArray
    , sizeArray, pokeArray, peekArray
    -- ** Store instances in terms of Generic
    , GStoreSize, genericSize
    , GStorePoke, genericPoke
    , GStorePeek, genericPeek
    -- ** Peek utilities
    , skip, isolate
    , peekMagic
    -- ** Static Size type
    --
    -- This portion of the library is still work-in-progress.
    -- 'IsStaticSize' is only supported for strict ByteStrings, in order
    -- to support the use case of 'Tagged'.
    , IsStaticSize(..), StaticSize(..), toStaticSizeEx
    ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Containers (IsMap, ContainerKey, MapValue, mapFromList, mapToList, IsSet, setFromList)
import           Data.Data (Data)
import           Data.Fixed (Fixed (..), Pico)
import           Data.Foldable (forM_, foldl')
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable)
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.MonoTraversable
import           Data.Monoid
import           Data.Orphans ()
import           Data.Primitive.ByteArray
import           Data.Proxy (Proxy(..))
import           Data.Sequence (Seq)
import           Data.Sequences (IsSequence, Index, replicateM)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Core
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Foreign as T
import qualified Data.Text.Internal as T
import qualified Data.Time as Time
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import           Data.Word
import           Foreign.C.Types ()
import           Foreign.Ptr (plusPtr, minusPtr)
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Generics (Generic (..), C1, (:+:) (..), (:*:) (..), V1, U1 (..), K1 (..), M1 (..))
import qualified GHC.Integer.GMP.Internals as I
import           GHC.Real (Ratio(..))
import           GHC.TypeLits
import           GHC.Types (Int (I#))
import           Instances.TH.Lift ()
import           Language.Haskell.TH hiding (SumArity)
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Syntax (ModName, NameFlavour, OccName, PkgName)
import           RIO hiding (to)
import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Functor.Contravariant.Divisible (Divisible (..))
import           Data.Typeable (typeRep)
import           System.IO.Unsafe (unsafePerformIO)
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription (FlagName, unFlagName)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import qualified Distribution.Text as DT
import Path (Path, Abs, File, Dir, toFilePath, parseAbsFile, parseAbsDir)
import Path.Internal (Path (..))

-- Conditional import to avoid warning
#if MIN_VERSION_integer_gmp(1,0,0)
import           GHC.Prim (sizeofByteArray#)
#endif

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
instance Divisible Size where
  conquer = ConstSize 0
  divide _ (ConstSize b) (ConstSize c) = ConstSize (b + c)
  divide toBC bSize cSize = VarSize $ \a ->
    let (b, c) = toBC a
     in getSizeWith bSize b + getSizeWith cSize c

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

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'IsSequence'

-- | Implement 'size' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
sizeSequence :: forall t. (IsSequence t, Store (Element t)) => Size t
sizeSequence = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * (olength t) + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSequence #-}

-- | Implement 'poke' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
pokeSequence :: (IsSequence t, Store (Element t)) => t -> Poke ()
pokeSequence t =
  do pokeStorable len
     Poke (\ptr offset ->
             do offset' <-
                  ofoldlM (\offset' a ->
                             do (offset'',_) <- runPoke (poke a) ptr offset'
                                return offset'')
                          offset
                          t
                return (offset',()))
  where len = olength t
{-# INLINE pokeSequence #-}

-- | Implement 'peek' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
peekSequence :: (IsSequence t, Store (Element t), Index t ~ Int) => Peek t
peekSequence = do
    len <- peek
    replicateM len peek
{-# INLINE peekSequence #-}

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'IsSet'

-- | Implement 'size' for an 'IsSet' of 'Store' instances.
sizeSet :: forall t. (IsSet t, Store (Element t)) => Size t
sizeSet = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * (olength t) + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSet #-}

-- | Implement 'poke' for an 'IsSequence' of 'Store' instances.
pokeSet :: (IsSet t, Store (Element t)) => t -> Poke ()
pokeSet t = do
    pokeStorable (olength t)
    omapM_ poke t
{-# INLINE pokeSet #-}

-- | Implement 'peek' for an 'IsSequence' of 'Store' instances.
peekSet :: (IsSet t, Store (Element t)) => Peek t
peekSet = do
    len <- peek
    setFromList <$> replicateM len peek
{-# INLINE peekSet #-}

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of a 'IsMap'

-- | Implement 'size' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
sizeMap
    :: forall t. (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Size t
sizeMap = VarSize $ \t ->
    case (size :: Size (ContainerKey t), size :: Size (MapValue t)) of
        (ConstSize nk, ConstSize na) -> (nk + na) * olength t + sizeOf (undefined :: Int)
        (szk, sza) -> ofoldl' (\acc (k, a) -> acc + getSizeWith szk k + getSizeWith sza a)
                              (sizeOf (undefined :: Int))
                              (mapToList t)
{-# INLINE sizeMap #-}

-- | Implement 'poke' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
pokeMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => t
    -> Poke ()
pokeMap = pokeSequence . mapToList
{-# INLINE pokeMap #-}

-- | Implement 'peek' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
peekMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Peek t
peekMap = mapFromList <$> peek
{-# INLINE peekMap #-}

------------------------------------------------------------------------
-- Utilities for defining 'Store' instances for ordered containers like
-- 'IntMap' and 'Map'

-- | Marker for maps that are encoded in ascending order instead of the
-- descending order mistakenly implemented in 'peekMap' in store versions
-- < 0.4.
--
-- See https://github.com/fpco/store/issues/97.
markMapPokedInAscendingOrder :: Word32
markMapPokedInAscendingOrder = 1217678090

-- | Ensure the presence of a given magic value.
--
-- Throws a 'PeekException' if the value isn't present.
peekMagic
    :: (Eq a, Show a, Store a)
    => String -> a -> Peek ()
peekMagic markedThing x = do
    x' <- peek
    when (x' /= x) $
        fail ("Expected marker for " ++ markedThing ++ ": " ++ show x ++ " but got: " ++ show x')
{-# INLINE peekMagic #-}

-- | Like 'sizeMap' but should only be used for ordered containers where
-- 'Data.Containers.mapToList' returns an ascending list.
sizeOrdMap
    :: forall t.
       (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Size t
sizeOrdMap =
    combineSizeWith (const markMapPokedInAscendingOrder) id size sizeMap
{-# INLINE sizeOrdMap #-}

-- | Like 'pokeMap' but should only be used for ordered containers where
-- 'Data.Containers.mapToList' returns an ascending list.
pokeOrdMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => t -> Poke ()
pokeOrdMap x = poke markMapPokedInAscendingOrder >> pokeMap x
{-# INLINE pokeOrdMap #-}

-- | Decode the results of 'pokeOrdMap' using a given function to construct
-- the map.
peekOrdMapWith
    :: (Ord (ContainerKey t), Store (ContainerKey t), Store (MapValue t))
    => ([(ContainerKey t, MapValue t)] -> t)
       -- ^ A function to construct the map from an ascending list such as
       -- 'Map.fromDistinctAscList'.
    -> Peek t
peekOrdMapWith f = do
    peekMagic "ascending Map / IntMap" markMapPokedInAscendingOrder
    xs <- peek
    remaining <-
        Peek $ \ps sourcePtr ->
            return $
            PeekResult sourcePtr (peekStateEndPtr ps `minusPtr` sourcePtr)
    unless
        (ascending (map fst xs))
        (liftIO
             (throwIO
                  (PeekException
                       remaining
                       "The keys in the input map are not ascending.")))
    pure (f xs)
{-# INLINE peekOrdMapWith #-}

-- | Are all the values in the list ascending?
ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending (c:cs) = go c cs
  where
    go x (y:xs) =
        (x < y) && go y xs
    go _ [] = True
{-# INLINE ascending #-}

{-
------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of Foldable

-- | Implement 'size' for a 'Foldable' of 'Store' instances. Note that
-- this assumes the extra 'Foldable' structure is discardable - this
-- only serializes the elements.
sizeListLikeFoldable :: forall t a. (Foldable t, Store a) => Size (t a)
sizeListLikeFoldable = VarSize $ \t ->
    case size :: Size e of
        ConstSize n ->  n * length x + sizeOf (undefined :: Int)
        VarSize f -> foldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int))
{-# INLINE sizeSequence #-}

pokeListLikeFoldable :: forall t a. Foldable t => t a -> Poke ()
pokeListLikeFoldable x = do
    poke (length x)
-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances for list-like mutable things

-- | Implementation of peek for mutable sequences. The user provides a
-- function for initializing the sequence and a function for mutating an
-- element at a particular index.
peekMutableSequence
    :: forall a r. Store a
    => String -- ^ type
    -> (Int -> IO r)
    -> (r -> Int -> a -> IO ())
    -> Peek r
peekMutableSequence ty new write = do
    n :: Int <- peek
    let minBufferSize :: Integer
        minBufferSize =
          case size :: Size a of
            VarSize _ -> fromIntegral n -- minimum bound, assume 1 byte
            ConstSize x -> fromIntegral n * fromIntegral x
    remaining <-
      Peek $ \ps sourcePtr ->
      return $ PeekResult sourcePtr (peekStateEndPtr ps `minusPtr` sourcePtr)
    when (minBufferSize > fromIntegral remaining) $
      liftIO (tooManyBytes (fromIntegral minBufferSize) remaining ty)
    case (size :: Size a) of
      ConstSize 0 | n > maxNullaryVectorSize ->
                    liftIO (throwIO $ PeekException remaining $ T.pack $
                             "Max nullary vector size.")
      _ -> return ()
    mut <- liftIO (new n)
    forM_ [0..n-1] $ \i -> peek >>= liftIO . write mut i
    return mut
  where maxNullaryVectorSize = 4096
{-# INLINE peekMutableSequence #-}

------------------------------------------------------------------------
-- Useful combinators

-- | Skip n bytes forward.
{-# INLINE skip #-}
skip :: Int -> Peek ()
skip len = Peek $ \ps ptr -> do
    let ptr2 = ptr `plusPtr` len
        remaining = peekStateEndPtr ps `minusPtr` ptr
    when (len > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
        tooManyBytes len remaining "skip"
    return $ PeekResult ptr2 ()

-- | Isolate the input to n bytes, skipping n bytes forward. Fails if @m@
-- advances the offset beyond the isolated region.
{-# INLINE isolate #-}
isolate :: Int -> Peek a -> Peek a
isolate len m = Peek $ \ps ptr -> do
    let end = peekStateEndPtr ps
        ptr2 = ptr `plusPtr` len
        remaining = end `minusPtr` ptr
    when (len > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
        tooManyBytes len remaining "isolate"
    PeekResult ptr' x <- runPeek m ps ptr
    when (ptr' > end) $
        throwIO $ PeekException (ptr' `minusPtr` end) "Overshot end of isolated bytes"
    return $ PeekResult ptr2 x

------------------------------------------------------------------------
-- Instances for types based on flat representations

instance Store a => Store (V.Vector a) where
    size = sizeSequence
    poke = pokeSequence
    peek = V.unsafeFreeze =<< peekMutableSequence "Data.Vector.Vector" MV.new MV.write

instance Storable a => Store (SV.Vector a) where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        sizeOf (undefined :: a) * SV.length x
    poke x = do
        let (fptr, len) = SV.unsafeToForeignPtr0 x
        poke len
        pokeFromForeignPtr fptr 0 (sizeOf (undefined :: a) * len)
    peek = do
        len <- peek
        fp <- peekToPlainForeignPtr "Data.Storable.Vector.Vector" (sizeOf (undefined :: a) * len)
        liftIO $ SV.unsafeFreeze (MSV.MVector len fp)

instance Store BS.ByteString where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        BS.length x
    poke x = do
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        poke sourceLength
        pokeFromForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        len <- peek
        fp <- peekToPlainForeignPtr "Data.ByteString.ByteString" len
        return (BS.PS fp 0 len)

instance Store SBS.ShortByteString where
    size = VarSize $ \x ->
         sizeOf (undefined :: Int) +
         SBS.length x
    poke x@(SBS.SBS arr) = do
        let len = SBS.length x
        poke len
        pokeFromByteArray arr 0 len
    peek = do
        len <- peek
        ByteArray array <- peekToByteArray "Data.ByteString.Short.ShortByteString" len
        return (SBS.SBS array)

instance Store LBS.ByteString where
    -- FIXME: faster conversion? Is this ever going to be a problem?
    --
    -- I think on 64 bit systems, Int will have 64 bits. On 32 bit
    -- systems, we'll never exceed the range of Int by this conversion.
    size = VarSize $ \x ->
         sizeOf (undefined :: Int)  +
         fromIntegral (LBS.length x)
    -- FIXME: more efficient implementation that avoids the double copy
    poke = poke . LBS.toStrict
    peek = fmap LBS.fromStrict peek

instance Store T.Text where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        2 * (T.lengthWord16 x)
    poke x = do
        let !(T.Text (TA.Array array) w16Off w16Len) = x
        poke w16Len
        pokeFromByteArray array (2 * w16Off) (2 * w16Len)
    peek = do
        w16Len <- peek
        ByteArray array <- peekToByteArray "Data.Text.Text" (2 * w16Len)
        return (T.Text (TA.Array array) 0 w16Len)

------------------------------------------------------------------------
-- Known size instances

-- TODO: this doesn't scale nicely to 'Text'. Force it to be byte size?
-- 'StaticByteSize'?

newtype StaticSize (n :: Nat) a = StaticSize { unStaticSize :: a }
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

instance NFData a => NFData (StaticSize n a)

class KnownNat n => IsStaticSize n a where
    toStaticSize :: a -> Maybe (StaticSize n a)

toStaticSizeEx :: IsStaticSize n a => a -> StaticSize n a
toStaticSizeEx x =
    case toStaticSize x of
        Just r -> r
        Nothing -> error "Failed to assert a static size via toStaticSizeEx"

instance KnownNat n => IsStaticSize n BS.ByteString where
    toStaticSize bs
        | BS.length bs == fromInteger (natVal (Proxy :: Proxy n)) = Just (StaticSize bs)
        | otherwise = Nothing

instance KnownNat n => Store (StaticSize n BS.ByteString) where
    size = ConstSize (fromInteger (natVal (Proxy :: Proxy n)))
    poke (StaticSize x) = do
        -- TODO: worth it to put an assert here?
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        pokeFromForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        let len = fromInteger (natVal (Proxy :: Proxy n))
        fp <- peekToPlainForeignPtr ("StaticSize " ++ show len ++ " Data.ByteString.ByteString") len
        return (StaticSize (BS.PS fp 0 len))

------------------------------------------------------------------------
-- containers instances

instance Store a => Store [a] where
    size = sizeSequence
    poke = pokeSequence
    peek = peekSequence

instance Store a => Store (NE.NonEmpty a)

instance Store a => Store (Seq a) where
    size = sizeSequence
    poke = pokeSequence
    peek = peekSequence

instance (Store a, Ord a) => Store (Set a) where
    size =
        VarSize $ \t ->
            sizeOf (undefined :: Int) +
            case size of
                ConstSize n -> n * Set.size t
                VarSize f -> Set.foldl' (\acc a -> acc + f a) 0 t
    poke = pokeSet
    peek = Set.fromDistinctAscList <$> peek

instance Store IntSet where
    size = sizeSet
    poke = pokeSet
    peek = IntSet.fromDistinctAscList <$> peek

instance Store a => Store (IntMap a) where
    size = sizeOrdMap
    poke = pokeOrdMap
    peek = peekOrdMapWith IntMap.fromDistinctAscList

instance (Ord k, Store k, Store a) => Store (Map k a) where
    size =
        VarSize $ \t ->
            sizeOf markMapPokedInAscendingOrder + sizeOf (undefined :: Int) +
            case (size, size) of
                (ConstSize nk, ConstSize na) -> (nk + na) * Map.size t
                (szk, sza) ->
                    Map.foldlWithKey'
                        (\acc k a -> acc + getSizeWith szk k + getSizeWith sza a)
                        0
                        t
    poke = pokeOrdMap
    peek = peekOrdMapWith Map.fromDistinctAscList

instance (Eq k, Hashable k, Store k, Store a) => Store (HashMap k a) where
    size = sizeMap
    poke = pokeMap
    peek = peekMap

instance (Eq a, Hashable a, Store a) => Store (HashSet a) where
    size = sizeSet
    poke = pokeSet
    peek = peekSet

instance (A.Ix i, Store i, Store e) => Store (A.Array i e) where
    -- TODO: Speed up poke and peek
    size = sizeArray
    poke = pokeArray
    peek = peekArray

instance (A.Ix i, A.IArray A.UArray e, Store i, Store e) => Store (A.UArray i e) where
    -- TODO: Speed up poke and peek
    size = sizeArray
    poke = pokeArray
    peek = peekArray

sizeArray :: (A.Ix i, A.IArray a e, Store i, Store e) => Size (a i e)
sizeArray = VarSize $ \arr ->
    let bounds = A.bounds arr
    in  getSize bounds +
        case size of
            ConstSize n ->  n * A.rangeSize bounds
            VarSize f -> foldl' (\acc x -> acc + f x) 0 (A.elems arr)
{-# INLINE sizeArray #-}

pokeArray :: (A.Ix i, A.IArray a e, Store i, Store e) => a i e -> Poke ()
pokeArray arr = do
    poke (A.bounds arr)
    forM_ (A.elems arr) poke
{-# INLINE pokeArray #-}

peekArray :: (A.Ix i, A.IArray a e, Store i, Store e) => Peek (a i e)
peekArray = do
    bounds <- peek
    let len = A.rangeSize bounds
    elems <- replicateM len peek
    return (A.listArray bounds elems)
{-# INLINE peekArray #-}

instance Store Integer where
    size = VarSize $ \ x ->
        sizeOf (undefined :: Word8) + case x of
            I.S# _ -> sizeOf (undefined :: Int)
            I.Jp# (I.BN# arr) -> sizeOf (undefined :: Int) + I# (sizeofByteArray# arr)
            I.Jn# (I.BN# arr) -> sizeOf (undefined :: Int) + I# (sizeofByteArray# arr)
    poke (I.S# x) = poke (0 :: Word8) >> poke (I# x)
    poke (I.Jp# (I.BN# arr)) = do
        let len = I# (sizeofByteArray# arr)
        poke (1 :: Word8)
        poke len
        pokeFromByteArray arr 0 len
    poke (I.Jn# (I.BN# arr)) = do
        let len = I# (sizeofByteArray# arr)
        poke (2 :: Word8)
        poke len
        pokeFromByteArray arr 0 len
    peek = do
        tag <- peek :: Peek Word8
        case tag of
            0 -> fromIntegral <$> (peek :: Peek Int)
            1 -> I.Jp# <$> peekBN
            2 -> I.Jn# <$> peekBN
            _ -> peekException "Invalid Integer tag"
      where
        peekBN = do
          len <- peek :: Peek Int
          ByteArray arr <- peekToByteArray "GHC>Integer" len
          return $ I.BN# arr

-- instance Store GHC.Fingerprint.Types.Fingerprint where

instance Store (Fixed a) where
    size = contramap (\(MkFixed x) -> x) (size :: Size Integer)
    poke (MkFixed x) = poke x
    peek = MkFixed <$> peek

-- instance Store a => Store (Tree a) where

------------------------------------------------------------------------
-- Other instances

-- Manual implementation due to no Generic instance for Ratio. Also due
-- to the instance for Storable erroring when the denominator is 0.
-- Perhaps we should keep the behavior but instead a peekException?
--
-- In that case it should also error on poke.
--
-- I prefer being able to Store these, because they are constructable.

instance Store a => Store (Ratio a) where
    size = combineSize (\(x :% _) -> x) (\(_ :% y) -> y)
    poke (x :% y) = poke (x, y)
    peek = uncurry (:%) <$> peek

instance Store Time.Day where
    size = contramap Time.toModifiedJulianDay (size :: Size Integer)
    poke = poke . Time.toModifiedJulianDay
    peek = Time.ModifiedJulianDay <$> peek

instance Store Time.DiffTime where
    size = contramap (realToFrac :: Time.DiffTime -> Pico) (size :: Size Pico)
    poke = (poke :: Pico -> Poke ()) . realToFrac
    peek = Time.picosecondsToDiffTime <$> peek

instance Store Time.UTCTime where
    size = combineSize Time.utctDay Time.utctDayTime
    poke (Time.UTCTime day time) = poke (day, time)
    peek = uncurry Time.UTCTime <$> peek

instance Store ()
instance Store a => Store (Dual a)
instance Store a => Store (Sum a)
instance Store a => Store (Product a)
instance Store a => Store (First a)
instance Store a => Store (Last a)
instance Store a => Store (Maybe a)
instance (Store a, Store b) => Store (Either a b)


-- FIXME: have TH deriving handle unboxed fields?

------------------------------------------------------------------------
-- Instances generated by TH

instance Store Word8 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Word16 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Word32 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Word64 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Word where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable

instance Store Int8 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Int16 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Int32 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Int64 where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable
instance Store Int where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable

instance Store Char where
  size = sizeStorable
  poke = pokeStorable
  peek = peekStorable

instance Store ModName
instance Store NameFlavour
instance Store OccName
instance Store Name
instance Store NameSpace
instance Store PkgName
instance Store Info
instance Store Dec
instance Store Clause
instance Store Pat
instance Store Lit
instance Store Type
instance Store Body
instance Store Guard
instance Store TyVarBndr
instance Store Exp
instance Store Match
instance Store Stmt
instance Store Range
instance Store TyLit
instance Store Con
instance Store Bang
instance Store DerivClause
instance Store DerivStrategy
instance Store SourceUnpackedness
instance Store FunDep
instance Store SourceStrictness
instance Store Overlap
instance Store Foreign
instance Store Callconv
instance Store Fixity
instance Store FixityDirection
instance Store Pragma
instance Store Safety
instance Store Inline
instance Store TySynEqn
instance Store RuleMatch
instance Store TypeFamilyHead
instance Store FamilyResultSig
instance Store Phases
instance Store RuleBndr
instance Store InjectivityAnn
instance Store AnnTarget
instance Store Role
instance Store PatSynArgs
instance Store PatSynDir
instance Store Bool

instance (Store a, Store b) => Store (a, b)
instance (Store a, Store b, Store c) => Store (a, b, c)

instance Store PackageIdentifier where
  size = divide
    (\(PackageIdentifier name version) -> (name, version))
    size
    size
  peek = PackageIdentifier <$> peek <*> peek
  poke (PackageIdentifier name version) = poke name *> poke version
instance Store PackageName where
  size = contramap unPackageName size
  peek = peek >>= maybe (fail "Invalid package name") pure . DT.simpleParse
  poke = poke . unPackageName
instance Store Version where
  size = contramap versionNumbers size
  peek = mkVersion <$> peek
  poke = poke . versionNumbers
instance Store FlagName where
  size = contramap unFlagName size
  peek = peek >>= maybe (fail "Invalid flag name") pure . DT.simpleParse
  poke = poke . unFlagName
instance Store ModuleName where
  size = contramap ModuleName.components size
  peek = ModuleName.fromComponents <$> peek
  poke = poke . ModuleName.components

instance Store (Path Abs File) where
  size = contramap toFilePath size
  peek = peek >>= either (fail . show) pure . parseAbsFile
  poke = poke . toFilePath

instance Store (Path Abs Dir) where
  size = contramap toFilePath size
  peek = peek >>= either (fail . show) pure . parseAbsDir
  poke = poke . toFilePath

-- Orphans. Should be added upstream to path itself, but this whole module will
-- disappear in the future anyway.
deriving instance Data Abs
deriving instance Data Dir
deriving instance Data File
deriving instance (Data a, Data b) => Data (Path a b)
