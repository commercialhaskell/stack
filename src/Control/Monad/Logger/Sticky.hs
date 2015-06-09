{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Output a sticky line at the end of output.

module Control.Monad.Logger.Sticky
    (StickyLoggingT
    ,runStickyLoggingT
    ,logSticky)
    where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Monoid
import           Language.Haskell.TH
import           System.IO
import           System.Log.FastLogger

import           Prelude -- avoid AMP warnings

data State = State
    { stateCurrentLine :: !(Maybe ByteString)
    , stateMaxColumns :: !Int
    , stateLastWasSticky :: !Bool
    }

newtype StickyLoggingT m a = StickyLoggingT
    { unStickyLoggingT :: ReaderT (MVar State) m a
    } deriving (Functor,Applicative,Monad,MonadIO,MonadTrans,MonadThrow)

runStickyLoggingT :: MonadIO m => StickyLoggingT m a -> m a
runStickyLoggingT m = do
    state <- liftIO (newMVar (State Nothing 0 False))
    originalMode <- liftIO (hGetBuffering stdout)
    liftIO (hSetBuffering stdout NoBuffering)
    a <- runReaderT (unStickyLoggingT m) state
    state' <- liftIO (takeMVar state)
    liftIO (when (stateLastWasSticky state') (S8.putStr "\n"))
    liftIO (hSetBuffering stdout originalMode)
    return a

logSticky :: Q Exp
logSticky =
    logOther "sticky"

instance (MonadLogger m, MonadIO m) => MonadLogger (StickyLoggingT m) where
    monadLoggerLog loc src level msg = do
        ref <- StickyLoggingT ask
        state <- liftIO (takeMVar ref) -- TODO: make exception-safe.
        case level of
            LevelOther "sticky" ->
                liftIO $
                do S8.putStr
                       ("\r" <>
                        pad (stateMaxColumns state) msgBytes)
                   putMVar
                       ref
                       (state
                        { stateLastWasSticky = True
                        , stateMaxColumns = max
                              (stateMaxColumns state)
                              (S8.length msgBytes)
                        , stateCurrentLine = Just msgBytes
                        })
            _ -> do
                liftIO
                    (S8.putStr
                         ("\r" <>
                          S8.replicate
                              (stateMaxColumns state)
                              ' ' <>
                          "\r"))
                lift (monadLoggerLog loc src level msg)
                liftIO
                    (case stateCurrentLine state of
                         Nothing ->
                             putMVar
                                 ref
                                 (state
                                  { stateLastWasSticky = False
                                  , stateMaxColumns = max
                                        (stateMaxColumns state)
                                        (S8.length msgBytes)
                                  })
                         Just line -> do
                             S8.putStr line
                             putMVar
                                 ref
                                 (state
                                  { stateLastWasSticky = True
                                  , stateMaxColumns = max
                                        (stateMaxColumns state)
                                        (S8.length msgBytes)
                                  }))
      where
        msgBytes =
            fromLogStr
                (toLogStr msg)
        pad width s =
            S8.take
                (max width (S8.length s))
                (s <>
                 S8.replicate width ' ')
