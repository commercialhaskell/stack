{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Stack.Prelude
  ( mapLeft
  , ResourceT
  , runConduitRes
  , NoLogging (..)
  , withSystemTempDir
  , fromFirst
  , mapMaybeA
  , mapMaybeM
  , forMaybeA
  , forMaybeM
  , stripCR
  , logSticky
  , logStickyDone
  , RIO (..)
  , runRIO
  , HasLogFunc (..)
  , module X
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
import           Control.Monad.Logger.CallStack
                                      as X (Loc, LogLevel (..), LogSource,
                                            LogStr, MonadLogger (..),
                                            MonadLoggerIO (..), liftLoc,
                                            logDebug, logError, logInfo,
                                            logOther, logWarn, toLogStr)
import           Control.Monad.Reader as X (MonadReader, MonadTrans (..),
                                            ReaderT (..), ask, asks)
import           Data.Bool            as X (Bool (..), not, otherwise, (&&),
                                            (||))
import           Data.ByteString      as X (ByteString)
import           Data.Char            as X (Char)
import           Data.Conduit         as X (ConduitM, runConduit, (.|))
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
import           Data.Set             as X (Set)
import           Data.Store           as X (Store)
import           Data.String          as X (IsString (..))
import           Data.Text            as X (Text)
import           Data.Traversable     as X (Traversable (..), for, forM)
import           Data.Vector          as X (Vector)
import           Data.Void            as X (Void, absurd)
import           Data.Word            as X
import           GHC.Generics         as X (Generic)
import           GHC.Stack            as X (HasCallStack)
import           Lens.Micro           as X (Getting)
import           Lens.Micro.Mtl       as X (view)
import           Path                 as X (Abs, Dir, File, Path, Rel,
                                            toFilePath)
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
import           Text.Read            as X (Read, readMaybe)
import           UnliftIO             as X

import qualified Data.Text            as T
import qualified Path.IO

import qualified Control.Monad.Trans.Resource as Res (runResourceT, transResourceT)
import           Control.Monad.Trans.Resource (ResourceT)

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

runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT r = withRunInIO $ \run -> Res.runResourceT (Res.transResourceT run r)

-- | Avoid orphan messes
newtype NoLogging a = NoLogging { runNoLogging :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadUnliftIO NoLogging where
  askUnliftIO = NoLogging $
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . runNoLogging))
instance MonadLogger NoLogging where
  monadLoggerLog _ _ _ _ = return ()

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run -> Path.IO.withSystemTempDir str $ run . inner

-- | Write a "sticky" line to the terminal. Any subsequent lines will
-- overwrite this one, and that same line will be repeated below
-- again. In other words, the line sticks at the bottom of the output
-- forever. Running this function again will replace the sticky line
-- with a new sticky line. When you want to get rid of the sticky
-- line, run 'logStickyDone'.
--
logSticky :: MonadLogger m => Text -> m ()
logSticky =
    logOther (LevelOther "sticky")

-- | This will print out the given message with a newline and disable
-- any further stickiness of the line until a new call to 'logSticky'
-- happens.
--
-- It might be better at some point to have a 'runSticky' function
-- that encompasses the logSticky->logStickyDone pairing.
logStickyDone :: MonadLogger m => Text -> m ()
logStickyDone =
    logOther (LevelOther "sticky-done")

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

class HasLogFunc env where
  logFuncL :: Getting r env (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog a b c d = do
    f <- view logFuncL
    liftIO $ f a b c $ toLogStr d

instance HasLogFunc env => MonadLoggerIO (RIO env) where
  askLoggerIO = view logFuncL

instance MonadUnliftIO (RIO env) where
    askUnliftIO = RIO $ ReaderT $ \r ->
                  withUnliftIO $ \u ->
                  return (UnliftIO (unliftIO u . flip runReaderT r . unRIO))
