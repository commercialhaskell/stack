{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    , IsStaticSize(..), StaticSize(..), toStaticSizeEx, liftStaticSize, staticByteStringExp
    ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Exception (throwIO)
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Containers (IsMap, ContainerKey, MapValue, mapFromList, mapToList, IsSet, setFromList)
import           Data.Data (Data)
import           Data.Fixed (Fixed (..), Pico)
import           Data.Foldable (forM_, foldl')
import           Data.Functor.Contravariant
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
import           Data.Store.Impl
import           Data.Store.Core
import           Data.Store.TH.Internal
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
import           Data.Void
import           Data.Word
import           Foreign.C.Types ()
import           Foreign.Ptr (plusPtr, minusPtr)
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Generics (Generic)
import qualified GHC.Integer.GMP.Internals as I
import           GHC.Real (Ratio(..))
import           GHC.TypeLits
import           GHC.Types (Int (I#))
import           Instances.TH.Lift ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Syntax
import           Prelude
import           TH.Derive

-- Conditional import to avoid warning
#if MIN_VERSION_integer_gmp(1,0,0)
import           GHC.Prim (sizeofByteArray#)
#endif

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

-- NOTE: this could be a 'Lift' instance, but we can't use type holes in
-- TH. Alternatively we'd need a (TypeRep -> Type) function and Typeable
-- constraint.
liftStaticSize :: forall n a. (KnownNat n, Lift a) => TypeQ -> StaticSize n a -> ExpQ
liftStaticSize tyq (StaticSize x) = do
    let numTy = litT $ numTyLit $ natVal (Proxy :: Proxy n)
    [| StaticSize $(lift x) :: StaticSize $(numTy) $(tyq) |]

staticByteStringExp :: BS.ByteString -> ExpQ
staticByteStringExp bs =
    [| StaticSize bs :: StaticSize $(litT (numTyLit (fromIntegral len))) BS.ByteString |]
  where
    len = BS.length bs

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

$($(derive [d|
    -- TODO
    -- instance Deriving (Store ())
    instance Deriving (Store All)
    instance Deriving (Store Any)
    instance Deriving (Store Void)
    instance Deriving (Store Bool)
    |]))

-- TODO: higher arities?  Limited now by Generics instances for tuples
$(return $ map deriveTupleStoreInstance [2..7])

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

{- Temporarily removed, let's reduce compile times
$(deriveManyStoreUnboxVector)

$(deriveManyStoreFromStorable
  -- TODO: Figure out why on GHC-8.2.1 this internal datatype is visible
  -- in the instances of Storable. Here's a gist of an attempt at
  -- debugging the issue:
  --
  -- https://gist.github.com/mgsloan/a7c416b961015949d3b5674ce053bbf6
  --
  -- The mysterious thing is why this is happening despite not having a
  -- direct import of Data.Text.Encoding.
  (\ty ->
    case ty of
      ConT n | nameModule n == Just "Data.Text.Encoding"
            && nameBase n == "DecoderState" -> False
      ConT n | nameModule n == Just "Data.Text.Encoding"
            && nameBase n == "CodePoint" -> False
      ConT n | nameModule n == Just "Network.Socket.Types"
            && nameBase n == "In6Addr" -> False
      -- AddrInfo's Storable instance is lossy, so avoid having a Store
      -- instance for it.
      ConT n | n == ''AddrInfo -> False
      _ -> True
    ))

$(deriveManyStorePrimVector)

$(reifyManyWithoutInstances ''Store [''ModName, ''NameSpace, ''PkgName] (const True) >>=
--   mapM (\name -> deriveStore [] (ConT name) .dtCons =<< reifyDataType name))
   mapM (\name -> return (deriveGenericInstance [] (ConT name))))

$(reifyManyWithoutInstances ''Store [''Info] (const True) >>=
--   mapM (\name -> deriveStore [] (ConT name) .dtCons =<< reifyDataType name))
   mapM (\name -> return (deriveGenericInstance [] (ConT name))))

-}
