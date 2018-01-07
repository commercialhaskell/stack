{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module RIO.Prelude
  ( mapLeft
  , withLazyFile
  , fromFirst
  , mapMaybeA
  , mapMaybeM
  , forMaybeA
  , forMaybeM
  , stripCR
  , RIO (..)
  , runRIO
  , liftRIO
  , tshow
  , readFileBinary
  , writeFileBinary
  , ReadFileUtf8Exception (..)
  , readFileUtf8
  , writeFileUtf8
  , LByteString
  , toStrictBytes
  , fromStrictBytes
  , decodeUtf8Lenient
  , LText
  , view
  , UVector
  , SVector
  , GVector
  , module X
  , DisplayBuilder (..)
  , Display (..)
  , displayShow
  , displayBuilderToText
  , displayBytesUtf8
  , writeFileDisplayBuilder
  , hPutBuilder
  , sappend
  ) where

import           Control.Applicative  as X (Alternative, Applicative (..),
                                            liftA, liftA2, liftA3, many,
                                            optional, some, (<|>))
import           Control.Arrow        as X (first, second, (&&&), (***))
import           Control.DeepSeq      as X (NFData (..), force, ($!!))
import           Control.Monad        as X (Monad (..), MonadPlus (..), filterM,
                                            foldM, foldM_, forever, guard, join,
                                            liftM, liftM2, replicateM_, unless,
                                            when, zipWithM, zipWithM_, (<$!>),
                                            (<=<), (=<<), (>=>))
import           Control.Monad.Catch  as X (MonadThrow (..))
import           Control.Monad.Reader as X (MonadReader, MonadTrans (..),
                                            ReaderT (..), ask, asks, local)
import           Data.Bool            as X (Bool (..), not, otherwise, (&&),
                                            (||))
import           Data.ByteString      as X (ByteString)
import           Data.ByteString.Builder as X (Builder)
import           Data.ByteString.Short as X (ShortByteString, toShort, fromShort)
import           Data.Char            as X (Char)
import           Data.Data            as X (Data (..))
import           Data.Either          as X (Either (..), either, isLeft,
                                            isRight, lefts, partitionEithers,
                                            rights)
import           Data.Eq              as X (Eq (..))
import           Data.Foldable        as X (Foldable, all, and, any, asum,
                                            concat, concatMap, elem, fold,
                                            foldMap, foldl', foldr, forM_, for_,
                                            length, mapM_, msum, notElem, null,
                                            or, product, sequenceA_, sequence_,
                                            sum, toList, traverse_)
import           Data.Function        as X (const, fix, flip, id, on, ($), (&),
                                            (.))
import           Data.Functor         as X (Functor (..), void, ($>), (<$),
                                            (<$>))
import           Data.Hashable        as X (Hashable)
import           Data.HashMap.Strict  as X (HashMap)
import           Data.HashSet         as X (HashSet)
import           Data.Int             as X
import           Data.IntMap.Strict   as X (IntMap)
import           Data.IntSet          as X (IntSet)
import           Data.List            as X (break, drop, dropWhile, filter,
                                            lines, lookup, map, replicate,
                                            reverse, span, take, takeWhile,
                                            unlines, unwords, words, zip, (++))
import           Data.Map.Strict      as X (Map)
import           Data.Maybe           as X (Maybe (..), catMaybes, fromMaybe,
                                            isJust, isNothing, listToMaybe,
                                            mapMaybe, maybe, maybeToList)
import           Data.Monoid          as X (All (..), Any (..), Endo (..),
                                            First (..), Last (..), Monoid (..),
                                            Product (..), Sum (..), (<>))
import           Data.Ord             as X (Ord (..), Ordering (..), comparing)
import           Data.Semigroup       as X (Semigroup)
import           Data.Set             as X (Set)
import           Data.String          as X (IsString (..))
import           Data.Text            as X (Text)
import           Data.Text.Encoding   as X (encodeUtf8, decodeUtf8', decodeUtf8With, encodeUtf8Builder)
import           Data.Text.Encoding.Error as X (lenientDecode, UnicodeException (..))
import           Data.Traversable     as X (Traversable (..), for, forM)
import           Data.Vector          as X (Vector)
import           Data.Void            as X (Void, absurd)
import           Data.Word            as X
import           Foreign.Storable     as X (Storable)
import           GHC.Generics         as X (Generic)
import           GHC.Stack            as X (HasCallStack)
import           Lens.Micro           as X (ASetter, ASetter', sets, over, set, SimpleGetter, Getting, (^.), to, Lens, Lens', lens)
import           Prelude              as X (Bounded (..), Double, Enum,
                                            FilePath, Float, Floating (..),
                                            Fractional (..), IO, Integer,
                                            Integral (..), Num (..), Rational,
                                            Real (..), RealFloat (..),
                                            RealFrac (..), Show, String,
                                            asTypeOf, curry, error, even,
                                            fromIntegral, fst, gcd, lcm, odd,
                                            realToFrac, seq, show, snd,
                                            subtract, uncurry, undefined, ($!),
                                            (^), (^^))
import           System.Exit          as X (ExitCode (..))
import           Text.Read            as X (Read, readMaybe)
import           UnliftIO             as X

import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Storable as SVector
import qualified Data.Vector.Generic as GVector

import qualified Data.ByteString.Builder as BB
import qualified Data.Semigroup

import Control.Applicative (Const (..))
import Lens.Micro.Internal ((#.))

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left a1) = Left (f a1)
mapLeft _ (Right b) = Right b

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

-- | Applicative 'mapMaybe'.
mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeA f = fmap catMaybes . traverse f

-- | @'forMaybeA' '==' 'flip' 'mapMaybeA'@
forMaybeA :: Applicative f => [a] -> (a -> f (Maybe b)) -> f [b]
forMaybeA = flip mapMaybeA

-- | Monadic 'mapMaybe'.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

-- | @'forMaybeM' '==' 'flip' 'mapMaybeM'@
forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

-- | Strip trailing carriage return from Text
stripCR :: T.Text -> T.Text
stripCR t = fromMaybe t (T.stripSuffix "\r" t)

-- | Lazily get the contents of a file. Unlike 'BL.readFile', this
-- ensures that if an exception is thrown, the file handle is closed
-- immediately.
withLazyFile :: MonadUnliftIO m => FilePath -> (BL.ByteString -> m a) -> m a
withLazyFile fp inner = withBinaryFile fp ReadMode $ inner <=< liftIO . BL.hGetContents

-- | The Reader+IO monad. This is different from a 'ReaderT' because:
--
-- * It's not a transformer, it hardcodes IO for simpler usage and
-- error messages.
--
-- * Instances of typeclasses like 'MonadLogger' are implemented using
-- classes defined on the environment, instead of using an
-- underlying monad.
newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow)

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
  env <- ask
  runRIO env rio

instance MonadUnliftIO (RIO env) where
    askUnliftIO = RIO $ ReaderT $ \r ->
                  withUnliftIO $ \u ->
                  return (UnliftIO (unliftIO u . flip runReaderT r . unRIO))

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Same as 'B.readFile', but generalized to 'MonadIO'
readFileBinary :: MonadIO m => FilePath -> m ByteString
readFileBinary = liftIO . B.readFile

-- | Same as 'B.writeFile', but generalized to 'MonadIO'
writeFileBinary :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBinary fp = liftIO . B.writeFile fp

-- | Read a file in UTF8 encoding, throwing an exception on invalid character
-- encoding.
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 fp = do
  bs <- readFileBinary fp
  case decodeUtf8' bs of
    Left e -> throwIO $ ReadFileUtf8Exception fp e
    Right text -> return text

data ReadFileUtf8Exception = ReadFileUtf8Exception !FilePath !UnicodeException
  deriving (Show, Typeable)
instance Exception ReadFileUtf8Exception

-- | Write a file in UTF8 encoding
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp = writeFileBinary fp . encodeUtf8

type LByteString = BL.ByteString

toStrictBytes :: LByteString -> ByteString
toStrictBytes = BL.toStrict

fromStrictBytes :: ByteString -> LByteString
fromStrictBytes = BL.fromStrict

view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst #. l Const)

type UVector = UVector.Vector
type SVector = SVector.Vector
type GVector = GVector.Vector

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

type LText = TL.Text

newtype DisplayBuilder = DisplayBuilder { getUtf8Builder :: Builder }
  deriving (Semigroup, Monoid)

instance IsString DisplayBuilder where
  fromString = DisplayBuilder . BB.stringUtf8

class Display a where
  display :: a -> DisplayBuilder
instance Display Text where
  display = DisplayBuilder . encodeUtf8Builder
instance Display LText where
  display = foldMap display . TL.toChunks
instance Display Int where
  display = DisplayBuilder . BB.intDec

displayShow :: Show a => a -> DisplayBuilder
displayShow = fromString . show

displayBytesUtf8 :: ByteString -> DisplayBuilder
displayBytesUtf8 = DisplayBuilder . BB.byteString

displayBuilderToText :: DisplayBuilder -> Text
displayBuilderToText =
  decodeUtf8With lenientDecode . BL.toStrict . BB.toLazyByteString . getUtf8Builder

sappend :: Semigroup s => s -> s -> s
sappend = (Data.Semigroup.<>)

writeFileDisplayBuilder :: MonadIO m => FilePath -> DisplayBuilder -> m ()
writeFileDisplayBuilder fp (DisplayBuilder builder) =
  liftIO $ withBinaryFile fp WriteMode $ \h -> hPutBuilder h builder

hPutBuilder :: MonadIO m => Handle -> Builder -> m ()
hPutBuilder h = liftIO . BB.hPutBuilder h
{-# INLINE hPutBuilder #-}
