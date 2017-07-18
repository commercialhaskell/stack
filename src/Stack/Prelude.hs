{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stack.Prelude
  ( mapLeft
  , runConduitRes
  , withSystemTempDir
  , fromFirst
  , mapMaybeA
  , mapMaybeM
  , forMaybeA
  , forMaybeM
  , stripCR
  , StackT (..)
  , HasLogFunc (..)
  , module X
  ) where

import Text.Read as X (Read, readMaybe)
import UnliftIO as X
import Control.Monad.Catch as X (MonadThrow (..)) -- future consideration: move to explicit Either
import Data.Conduit as X (runConduit, (.|), ConduitM)
import Data.Void as X (Void, absurd)
import Path as X (Path, Abs, Rel, Dir, File, toFilePath)
import Control.Monad.Logger as X
       (MonadLogger(..), MonadLoggerIO(..), logDebug, logInfo, logWarn, logOther,
        logError, toLogStr, Loc, LogSource, LogLevel (..), LogStr, liftLoc)
import Control.Monad.Reader as X (MonadReader, ask, asks, ReaderT (..), MonadTrans (..))
import Lens.Micro as X (Getting)
import Lens.Micro.Mtl as X (view)
import Data.Char as X (Char)
import Data.Bool as X (Bool (..), (&&), (||), not, otherwise)
import Prelude as X (String, IO, Show, show, Num (..), fst, snd, curry, uncurry, Enum, Bounded (..), Integer, Float, Double, Rational, Real (..), Integral (..), Fractional (..), Floating (..), RealFrac (..), RealFloat (..), subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac, asTypeOf, error, undefined, seq, ($!), FilePath)
import Data.List as X ((++), map, reverse, filter, words, unwords, lines, unlines, lookup, break, span, take, drop, takeWhile, dropWhile, zip, replicate)
import Data.Data as X (Data (..))
import Data.Eq as X (Eq (..))
import Data.Functor as X (Functor (..), (<$), ($>), (<$>), void)
import Data.Either as X (Either (..), partitionEithers, either, lefts, rights, isLeft, isRight)
import Data.Maybe as X (Maybe (..), maybe, isJust, isNothing, fromMaybe, listToMaybe, maybeToList, catMaybes, mapMaybe)
import Data.Monoid as X (Monoid (..), (<>), Endo (..), All (..), Any (..), Sum (..), Product (..), First (..), Last (..))
import Data.Ord as X (Ord (..), Ordering (..), comparing)
import Data.Traversable as X (Traversable (..), for, forM)
import Data.String as X (IsString (..))
import Data.Text as X (Text)
import Data.ByteString as X (ByteString)
import Control.Applicative as X (Applicative (..), Alternative, many, some, (<|>), optional, liftA, liftA2, liftA3)
import Control.Arrow as X (first, second, (***), (&&&))
import Control.Monad as X (Monad (..), join, MonadPlus (..), (=<<), (>=>), (<=<), forever, filterM, zipWithM, zipWithM_, foldM, foldM_, replicateM_, guard, when, unless, liftM, liftM2, (<$!>))
import Data.Int as X
import Data.Word as X
import Data.Foldable as X
       (Foldable, fold, foldMap, foldr, foldl', toList, null, length,
        elem, sum, product, traverse_, for_, sequenceA_, asum, mapM_,
        forM_, sequence_, msum, concat, concatMap, and, or, any, all,
        notElem)
import Data.Function as X (id, const, (.), flip, ($), (&), fix, on)
import Data.Map.Strict as X (Map)
import Data.Set as X (Set)
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.IntMap.Strict as X (IntMap)
import Data.IntSet as X (IntSet)
import GHC.Generics as X (Generic)
import Control.DeepSeq as X (NFData (..), ($!!), force)
import Data.Hashable as X (Hashable)
import Data.Vector as X (Vector)
import Data.Store as X (Store)

import qualified Path.IO
import qualified Data.Text as T

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

runConduitRes :: MonadUnliftIO m => ConduitM () Void (ResourceT m) r -> m r
runConduitRes = runResourceT . runConduit

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run -> Path.IO.withSystemTempDir str $ run . inner

--------------------------------------------------------------------------------
-- Main StackT monad transformer

-- | The monad used for the executable @stack@.
newtype StackT env m a =
  StackT {unStackT :: ReaderT env m a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow,MonadTrans)

class HasLogFunc env where
  logFuncL :: Getting r env (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

instance (MonadIO m, HasLogFunc env) => MonadLogger (StackT env m) where
  monadLoggerLog a b c d = do
    f <- view logFuncL
    liftIO $ f a b c $ toLogStr d

instance (MonadIO m, HasLogFunc env) => MonadLoggerIO (StackT env m) where
  askLoggerIO = view logFuncL

instance MonadUnliftIO m => MonadUnliftIO (StackT config m) where
    askUnliftIO = StackT $ ReaderT $ \r ->
                  withUnliftIO $ \u ->
                  return (UnliftIO (unliftIO u . flip runReaderT r . unStackT))
