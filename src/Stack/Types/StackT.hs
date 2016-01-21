{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The monad used for the command-line executable @stack@.

module Stack.Types.StackT
  (StackT
  ,StackLoggingT
  ,runStackT
  ,runStackTGlobal
  ,runStackLoggingT
  ,runStackLoggingTGlobal
  ,runInnerStackT
  ,runInnerStackLoggingT
  ,newTLSManager
  ,logSticky
  ,logStickyDone)
  where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import           Data.Time
import           GHC.Foreign (withCString, peekCString)
import           Language.Haskell.TH
import           Network.HTTP.Client.Conduit (HasHttpManager(..))
import           Network.HTTP.Conduit
import           Prelude -- Fix AMP warning
import           Stack.Types.Internal
import           Stack.Types.Config (GlobalOpts (..))
import           System.IO
import           System.Log.FastLogger

#ifndef MIN_VERSION_time
#define MIN_VERSION_time(x, y, z) 0
#endif
#if !MIN_VERSION_time(1, 5, 0)
import           System.Locale
#endif

--------------------------------------------------------------------------------
-- Main StackT monad transformer

-- | The monad used for the executable @stack@.
newtype StackT config m a =
  StackT {unStackT :: ReaderT (Env config) m a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader (Env config),MonadThrow,MonadCatch,MonadMask,MonadTrans)

deriving instance (MonadBase b m) => MonadBase b (StackT config m)

instance MonadBaseControl b m => MonadBaseControl b (StackT config m) where
    type StM (StackT config m) a = ComposeSt (StackT config) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadTransControl (StackT config) where
    type StT (StackT config) a = StT (ReaderT (Env config)) a
    liftWith = defaultLiftWith StackT unStackT
    restoreT = defaultRestoreT StackT

-- | Takes the configured log level into account.
instance (MonadIO m) => MonadLogger (StackT config m) where
  monadLoggerLog = stickyLoggerFunc

-- | Run a Stack action, using global options.
runStackTGlobal :: (MonadIO m,MonadBaseControl IO m)
                => Manager -> config -> GlobalOpts -> StackT config m a -> m a
runStackTGlobal manager config GlobalOpts{..} =
    runStackT manager globalLogLevel config globalTerminal (isJust globalReExecVersion)

-- | Run a Stack action.
runStackT :: (MonadIO m,MonadBaseControl IO m)
          => Manager -> LogLevel -> config -> Bool -> Bool -> StackT config m a -> m a
runStackT manager logLevel config terminal reExec m = do
    canUseUnicode <- liftIO getCanUseUnicode
    withSticky
        terminal
        (\sticky ->
              runReaderT
                  (unStackT m)
                  (Env config logLevel terminal reExec manager sticky canUseUnicode))

-- | Taken from GHC: determine if we should use Unicode syntax
getCanUseUnicode :: IO Bool
getCanUseUnicode = do
    let enc = localeEncoding
        str = "\x2018\x2019"
        test = withCString enc str $ \cstr -> do
            str' <- peekCString enc cstr
            return (str == str')
    test `catchIOError` \_ -> return False

--------------------------------------------------------------------------------
-- Logging only StackLoggingT monad transformer

-- | Monadic environment for 'StackLoggingT'.
data LoggingEnv = LoggingEnv
    { lenvLogLevel :: !LogLevel
    , lenvTerminal :: !Bool
    , lenvReExec :: !Bool
    , lenvManager :: !Manager
    , lenvSticky :: !Sticky
    , lenvSupportsUnicode :: !Bool
    }

-- | The monad used for logging in the executable @stack@ before
-- anything has been initialized.
newtype StackLoggingT m a = StackLoggingT
    { unStackLoggingT :: ReaderT LoggingEnv m a
    } deriving (Functor,Applicative,Monad,MonadIO,MonadThrow,MonadReader LoggingEnv,MonadCatch,MonadMask,MonadTrans)

deriving instance (MonadBase b m) => MonadBase b (StackLoggingT m)

instance MonadBaseControl b m => MonadBaseControl b (StackLoggingT m) where
    type StM (StackLoggingT m) a = ComposeSt StackLoggingT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadTransControl StackLoggingT where
    type StT StackLoggingT a = StT (ReaderT LoggingEnv) a
    liftWith = defaultLiftWith StackLoggingT unStackLoggingT
    restoreT = defaultRestoreT StackLoggingT

-- | Takes the configured log level into account.
instance (MonadIO m) => MonadLogger (StackLoggingT m) where
    monadLoggerLog = stickyLoggerFunc

instance HasSticky LoggingEnv where
    getSticky = lenvSticky

instance HasLogLevel LoggingEnv where
    getLogLevel = lenvLogLevel

instance HasHttpManager LoggingEnv where
    getHttpManager = lenvManager

instance HasTerminal LoggingEnv where
    getTerminal = lenvTerminal

instance HasReExec LoggingEnv where
    getReExec = lenvReExec

instance HasSupportsUnicode LoggingEnv where
    getSupportsUnicode = lenvSupportsUnicode

runInnerStackT :: (HasHttpManager r, HasLogLevel r, HasTerminal r, HasReExec r, MonadReader r m, MonadIO m)
               => config -> StackT config IO a -> m a
runInnerStackT config inner = do
    manager <- asks getHttpManager
    logLevel <- asks getLogLevel
    terminal <- asks getTerminal
    reExec <- asks getReExec
    liftIO $ runStackT manager logLevel config terminal reExec inner

runInnerStackLoggingT :: (HasHttpManager r, HasLogLevel r, HasTerminal r, HasReExec r, MonadReader r m, MonadIO m)
                      => StackLoggingT IO a -> m a
runInnerStackLoggingT inner = do
    manager <- asks getHttpManager
    logLevel <- asks getLogLevel
    terminal <- asks getTerminal
    reExec <- asks getReExec
    liftIO $ runStackLoggingT manager logLevel terminal reExec inner

-- | Run the logging monad, using global options.
runStackLoggingTGlobal :: MonadIO m
                       => Manager -> GlobalOpts -> StackLoggingT m a -> m a
runStackLoggingTGlobal manager GlobalOpts{..} =
    runStackLoggingT manager globalLogLevel globalTerminal (isJust globalReExecVersion)

-- | Run the logging monad.
runStackLoggingT :: MonadIO m
                 => Manager -> LogLevel -> Bool -> Bool -> StackLoggingT m a -> m a
runStackLoggingT manager logLevel terminal reExec m = do
    canUseUnicode <- liftIO getCanUseUnicode
    withSticky
        terminal
        (\sticky ->
              runReaderT
                  (unStackLoggingT m)
                  LoggingEnv
                  { lenvLogLevel = logLevel
                  , lenvManager = manager
                  , lenvSticky = sticky
                  , lenvTerminal = terminal
                  , lenvReExec = reExec
                  , lenvSupportsUnicode = canUseUnicode
                  })

-- | Convenience for getting a 'Manager'
newTLSManager :: MonadIO m => m Manager
newTLSManager = liftIO $ newManager tlsManagerSettings

--------------------------------------------------------------------------------
-- Logging functionality
stickyLoggerFunc :: (HasSticky r, HasLogLevel r, HasSupportsUnicode r, ToLogStr msg, MonadReader r (t m), MonadTrans t, MonadIO (t m))
                 => Loc -> LogSource -> LogLevel -> msg -> t m ()
stickyLoggerFunc loc src level msg = do
    Sticky mref <- asks getSticky
    case mref of
        Nothing ->
            loggerFunc
                out
                loc
                src
                (case level of
                     LevelOther "sticky-done" -> LevelInfo
                     LevelOther "sticky" -> LevelInfo
                     _ -> level)
                msg
        Just ref -> do
            sticky <- liftIO (takeMVar ref)
            let backSpaceChar =
                    '\8'
                repeating =
                    S8.replicate
                        (maybe 0 T.length sticky)
                clear =
                    liftIO
                        (S8.hPutStr out
                             (repeating backSpaceChar <>
                              repeating ' ' <>
                              repeating backSpaceChar))
            maxLogLevel <- asks getLogLevel

            -- Convert some GHC-generated Unicode characters as necessary
            supportsUnicode <- asks getSupportsUnicode
            let msgText
                    | supportsUnicode = msgTextRaw
                    | otherwise = T.map replaceUnicode msgTextRaw

            newState <-
                case level of
                    LevelOther "sticky-done" -> do
                        clear
                        liftIO (T.hPutStrLn out msgText >> hFlush out)
                        return Nothing
                    LevelOther "sticky" -> do
                        clear
                        liftIO (T.hPutStr out msgText >> hFlush out)
                        return (Just msgText)
                    _
                      | level >= maxLogLevel -> do
                          clear
                          loggerFunc out loc src level $ toLogStr msgText
                          case sticky of
                              Nothing ->
                                  return Nothing
                              Just line -> do
                                  liftIO (T.hPutStr out line >> hFlush out)
                                  return sticky
                      | otherwise ->
                          return sticky
            liftIO (putMVar ref newState)
  where
    out = stderr
    msgTextRaw = T.decodeUtf8With T.lenientDecode msgBytes
    msgBytes = fromLogStr (toLogStr msg)

-- | Replace Unicode characters with non-Unicode equivalents
replaceUnicode :: Char -> Char
replaceUnicode '\x2018' = '`'
replaceUnicode '\x2019' = '\''
replaceUnicode c = c

-- | Logging function takes the log level into account.
loggerFunc :: (MonadIO m,ToLogStr msg,MonadReader r m,HasLogLevel r)
           => Handle -> Loc -> Text -> LogLevel -> msg -> m ()
loggerFunc outputChannel loc _src level msg =
  do maxLogLevel <- asks getLogLevel
     when (level >= maxLogLevel)
          (liftIO (do out <- getOutput maxLogLevel
                      T.hPutStrLn outputChannel out))
  where getOutput maxLogLevel =
          do timestamp <- getTimestamp
             l <- getLevel
             lc <- getLoc
             return (T.pack timestamp <> T.pack l <> T.decodeUtf8 (fromLogStr (toLogStr msg)) <> T.pack lc)
          where getTimestamp
                  | maxLogLevel <= LevelDebug =
                    do now <- getZonedTime
                       return (formatTime' now ++ ": ")
                  | otherwise = return ""
                  where
                    formatTime' =
                        take timestampLength . formatTime defaultTimeLocale "%F %T.%q"
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
                  loc_package loc ++
                  ':' :
                  loc_module loc ++
                  ' ' :
                  loc_filename loc ++
                  ':' :
                  line loc ++
                  ':' :
                  char loc
                  where line = show . fst . loc_start
                        char = show . snd . loc_start

-- | The length of a timestamp in the format "YYYY-MM-DD hh:mm:ss.μμμμμμ".
-- This definition is top-level in order to avoid multiple reevaluation at runtime.
timestampLength :: Int
timestampLength =
  length (formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0))

-- | With a sticky state, do the thing.
withSticky :: (MonadIO m)
           => Bool -> (Sticky -> m b) -> m b
withSticky terminal m =
    if terminal
       then do state <- liftIO (newMVar Nothing)
               originalMode <- liftIO (hGetBuffering stdout)
               liftIO (hSetBuffering stdout NoBuffering)
               a <- m (Sticky (Just state))
               state' <- liftIO (takeMVar state)
               liftIO (when (isJust state') (S8.putStr "\n"))
               liftIO (hSetBuffering stdout originalMode)
               return a
       else m (Sticky Nothing)

-- | Write a "sticky" line to the terminal. Any subsequent lines will
-- overwrite this one, and that same line will be repeated below
-- again. In other words, the line sticks at the bottom of the output
-- forever. Running this function again will replace the sticky line
-- with a new sticky line. When you want to get rid of the sticky
-- line, run 'logStickyDone'.
--
logSticky :: Q Exp
logSticky =
    logOther "sticky"

-- | This will print out the given message with a newline and disable
-- any further stickiness of the line until a new call to 'logSticky'
-- happens.
--
-- It might be better at some point to have a 'runSticky' function
-- that encompasses the logSticky->logStickyDone pairing.
logStickyDone :: Q Exp
logStickyDone =
    logOther "sticky-done"
