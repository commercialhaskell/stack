{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The monad used for the command-line executable @stack@.

module Stack.Types.StackT
  (StackT
  ,StackLoggingT
  ,runStackT
  ,runStackLoggingT)
  where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Text (Text)
import           Data.Time
import           Language.Haskell.TH.Syntax
import           Network.HTTP.Client.Conduit (HasHttpManager(..))
import           Network.HTTP.Conduit
import           Stack.Types.Config
import           Stack.Types.Internal
import           System.Locale
import           System.Log.FastLogger

--------------------------------------------------------------------------------
-- Main StackT monad transformer

-- | The monad used for the executable @stack@.
newtype StackT m a =
  StackT {unStackT :: ReaderT Env m a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader Env,MonadThrow,MonadCatch,MonadMask,MonadTrans)

deriving instance (MonadBase b m) => MonadBase b (StackT m)

instance MonadBaseControl b m => MonadBaseControl b (StackT m) where
    type StM (StackT m) a = ComposeSt StackT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadTransControl StackT where
    type StT StackT a = StT (ReaderT Env) a
    liftWith = defaultLiftWith StackT unStackT
    restoreT = defaultRestoreT StackT

-- | Takes the configured log level into account.
instance (MonadIO m) => MonadLogger (StackT m) where
  monadLoggerLog = loggerFunc

-- | Run a Stack action.
runStackT :: (MonadIO m,MonadBaseControl IO m)
          => LogLevel -> Config -> StackT m a -> m a
runStackT logLevel config m =
  do manager <-
       liftIO (newManager conduitManagerSettings)
     runReaderT (unStackT m)
                (Env config logLevel manager)

--------------------------------------------------------------------------------
-- Logging only StackLoggingT monad transformer

-- | The monad used for logging in the executable @stack@ before
-- anything has been initialized.
newtype StackLoggingT m a =
  StackLoggingT {unStackLoggingT :: ReaderT (LogLevel,Manager) m a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadThrow,MonadReader (LogLevel,Manager),MonadCatch,MonadMask,MonadTrans)

-- | Takes the configured log level into account.
instance (MonadIO m) => MonadLogger (StackLoggingT m) where
  monadLoggerLog = loggerFunc

instance HasLogLevel (LogLevel,Manager) where
  getLogLevel = fst

instance HasHttpManager (LogLevel,Manager) where
  getHttpManager = snd

-- | Run the logging monad.
runStackLoggingT :: MonadIO m
                 => LogLevel -> StackLoggingT m a -> m a
runStackLoggingT logLevel m =
  do manager <-
       liftIO (newManager conduitManagerSettings)
     runReaderT (unStackLoggingT m)
                (logLevel,manager)

--------------------------------------------------------------------------------
-- Logging functionality

-- | Logging function takes the log level into account.
loggerFunc :: (MonadIO m,ToLogStr msg,MonadReader r m,HasLogLevel r)
           => Loc -> Text -> LogLevel -> msg -> m ()
loggerFunc loc _src level msg =
  do maxLogLevel <- asks getLogLevel
     when (level >= maxLogLevel)
          (liftIO (do out <- getOutput
                      S8.putStrLn (S8.pack out)))
  where getOutput =
          do date <- getDate
             l <- getLevel
             lc <- getLoc
             return (date ++ l ++ S8.unpack (fromLogStr (toLogStr msg)) ++ lc)
          where getDate
                  | maxLogLevel <= LevelDebug =
                    do now <- getCurrentTime
                       return (formatTime defaultTimeLocale "%Y-%m-%d %T%Q" now ++
                               ": ")
                  | otherwise = return ""
                getLevel
                  | maxLogLevel <= LevelDebug =
                    return ("[" ++
                            map toLower (drop 5 (show level)) ++
                            "] ")
                  | otherwise = return ""
                getLoc
                  | maxLogLevel <= LevelDebug =
                    return (" @(" ++ fileLocStr ++ ")")
                  | otherwise = return ""
                fileLocStr =
                  (loc_package loc) ++
                  ':' :
                  (loc_module loc) ++
                  ' ' :
                  (loc_filename loc) ++
                  ':' :
                  (line loc) ++
                  ':' :
                  (char loc)
                  where line = show . fst . loc_start
                        char = show . snd . loc_start
