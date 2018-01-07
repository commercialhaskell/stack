{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoImplicitPrelude #-}
module RIO.Logger
  ( LogLevel (..)
  , LogSource
  , LogStr
  , LogFunc
  , HasLogFunc (..)
  , logGeneric
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  , logSticky
  , logStickyDone
  , runNoLogging
  , NoLogging (..)
  , withStickyLogger
  , LogOptions (..)
  ) where

import RIO.Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Lens.Micro (to)
import GHC.Stack (HasCallStack, CallStack, SrcLoc (..), getCallStack)
import Data.Time
import qualified Data.Text.IO as TIO
import Data.ByteString.Builder (toLazyByteString, char7)
import           GHC.IO.Handle.Internals         (wantWritableHandle)
import           GHC.IO.Encoding.Types           (textEncodingName)
import           GHC.IO.Handle.Types             (Handle__ (..))
import qualified Data.ByteString as B

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther !Text
    deriving (Eq, Show, Read, Ord)

type LogSource = Text
type LogStr = DisplayBuilder
class HasLogFunc env where
  logFuncL :: SimpleGetter env LogFunc

type LogFunc = CallStack -> LogSource -> LogLevel -> LogStr -> IO ()

logGeneric
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogSource
  -> LogLevel
  -> LogStr
  -> m ()
logGeneric src level str = do
  logFunc <- view logFuncL
  liftIO $ logFunc ?callStack src level str

logDebug
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logDebug = logGeneric "" LevelDebug

logInfo
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logInfo = logGeneric "" LevelInfo

logWarn
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logWarn = logGeneric "" LevelWarn

logError
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => LogStr
  -> m ()
logError = logGeneric "" LevelError

logOther
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack)
  => Text -- ^ level
  -> LogStr
  -> m ()
logOther = logGeneric "" . LevelOther

runNoLogging :: MonadIO m => ReaderT NoLogging m a -> m a
runNoLogging = flip runReaderT NoLogging

data NoLogging = NoLogging
instance HasLogFunc NoLogging where
  logFuncL = to (\_ _ _ _ _ -> return ())

-- | Write a "sticky" line to the terminal. Any subsequent lines will
-- overwrite this one, and that same line will be repeated below
-- again. In other words, the line sticks at the bottom of the output
-- forever. Running this function again will replace the sticky line
-- with a new sticky line. When you want to get rid of the sticky
-- line, run 'logStickyDone'.
--
logSticky :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => LogStr -> m ()
logSticky = logOther "sticky"

-- | This will print out the given message with a newline and disable
-- any further stickiness of the line until a new call to 'logSticky'
-- happens.
--
-- It might be better at some point to have a 'runSticky' function
-- that encompasses the logSticky->logStickyDone pairing.
logStickyDone :: (MonadIO m, HasCallStack, MonadReader env m, HasLogFunc env) => LogStr -> m ()
logStickyDone = logOther "sticky-done"

canUseUtf8 :: MonadIO m => Handle -> m Bool
canUseUtf8 h = liftIO $ wantWritableHandle "canUseUtf8" h $ \h_ -> do
  -- TODO also handle haOutputNL for CRLF
  return $ (textEncodingName <$> haCodec h_) == Just "UTF-8"

withStickyLogger :: MonadIO m => LogOptions -> (LogFunc -> m a) -> m a
withStickyLogger options inner = do
  useUtf8 <- canUseUtf8 stderr
  let printer =
        if useUtf8 && logUseUnicode options
          then \db -> do
            hPutBuilder stderr $ getUtf8Builder db
            hFlush stderr
          else \db -> do
            let lbs = toLazyByteString $ getUtf8Builder db
                bs = toStrictBytes lbs
            text <-
              case decodeUtf8' bs of
                Left e -> error $ "mkStickyLogger: invalid UTF8 sequence: " ++ show (e, bs)
                Right text -> return text
            let text'
                  | logUseUnicode options = text
                  | otherwise = T.map replaceUnicode text
            TIO.hPutStr stderr text'
            hFlush stderr
  if logTerminal options
    then withSticky $ \var ->
           inner $ stickyImpl var options (simpleLogFunc options printer)
    else
      inner $ \cs src level str ->
      simpleLogFunc options printer cs src (noSticky level) str

-- | Replace Unicode characters with non-Unicode equivalents
replaceUnicode :: Char -> Char
replaceUnicode '\x2018' = '`'
replaceUnicode '\x2019' = '\''
replaceUnicode c = c

noSticky :: LogLevel -> LogLevel
noSticky (LevelOther "sticky-done") = LevelInfo
noSticky (LevelOther "sticky") = LevelInfo
noSticky level = level

data LogOptions = LogOptions
  { logMinLevel :: !LogLevel
  , logVerboseFormat :: !Bool
  , logTerminal :: !Bool
  , logUseTime :: !Bool
  , logUseColor :: !Bool
  , logUseUnicode :: !Bool
  }

simpleLogFunc :: LogOptions -> (LogStr -> IO ()) -> LogFunc
simpleLogFunc lo printer cs _src level msg =
    when (level >= logMinLevel lo) $ do
      timestamp <- getTimestamp
      printer $
        timestamp <>
        getLevel <>
        " " <>
        ansi reset <>
        msg <>
        getLoc <>
        ansi reset <>
        "\n"
  where
   reset = "\ESC[0m"
   setBlack = "\ESC[90m"
   setGreen = "\ESC[32m"
   setBlue = "\ESC[34m"
   setYellow = "\ESC[33m"
   setRed = "\ESC[31m"
   setMagenta = "\ESC[35m"

   ansi :: DisplayBuilder -> DisplayBuilder
   ansi xs | logUseColor lo = xs
           | otherwise = mempty

   getTimestamp :: IO DisplayBuilder
   getTimestamp
     | logVerboseFormat lo && logUseTime lo =
       do now <- getZonedTime
          return $ ansi setBlack <> fromString (formatTime' now) <> ": "
     | otherwise = return mempty
     where
       formatTime' =
           take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

   getLevel :: DisplayBuilder
   getLevel
     | logVerboseFormat lo =
         case level of
           LevelDebug -> ansi setGreen <> "[debug]"
           LevelInfo -> ansi setBlue <> "[info]"
           LevelWarn -> ansi setYellow <> "[warn]"
           LevelError -> ansi setRed <> "[error]"
           LevelOther name ->
             ansi setMagenta <>
             "[" <>
             display name <>
             "] "
     | otherwise = mempty

   getLoc :: DisplayBuilder
   getLoc
     | logVerboseFormat lo = ansi setBlack <> "\n@(" <> fileLocStr <> ")"
     | otherwise = mempty

   fileLocStr :: DisplayBuilder
   fileLocStr =
     case reverse $ getCallStack cs of
       [] -> "<no call stack found>"
       (_desc, loc):_ ->
         let file = srcLocFile loc
          in fromString file <>
             ":" <>
             displayShow (srcLocStartLine loc) <>
             ":" <>
             displayShow (srcLocStartCol loc)

-- | The length of a timestamp in the format "YYYY-MM-DD hh:mm:ss.μμμμμμ".
-- This definition is top-level in order to avoid multiple reevaluation at runtime.
timestampLength :: Int
timestampLength =
  length (formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0))

stickyImpl
    :: MVar ByteString -> LogOptions -> LogFunc
    -> (CallStack -> LogSource -> LogLevel -> LogStr -> IO ())
stickyImpl ref lo logFunc loc src level msgOrig = modifyMVar_ ref $ \sticky -> do
  let backSpaceChar = '\8'
      repeating = mconcat . replicate (B.length sticky) . char7
      clear = hPutBuilder stderr
        (repeating backSpaceChar <>
        repeating ' ' <>
        repeating backSpaceChar)

  case level of
    LevelOther "sticky-done" -> do
      clear
      logFunc loc src LevelInfo msgOrig
      hFlush stderr
      return mempty
    LevelOther "sticky" -> do
      clear
      let bs = toStrictBytes $ toLazyByteString $ getUtf8Builder msgOrig
      B.hPut stderr bs
      hFlush stderr
      return bs
    _
      | level >= logMinLevel lo -> do
          clear
          logFunc loc src level msgOrig
          unless (B.null sticky) $ do
            B.hPut stderr sticky
            hFlush stderr
          return sticky
      | otherwise -> return sticky

-- | With a sticky state, do the thing.
withSticky :: (MonadIO m) => (MVar ByteString -> m b) -> m b
withSticky inner = do
  state <- newMVar mempty
  originalMode <- liftIO (hGetBuffering stdout)
  liftIO (hSetBuffering stdout NoBuffering)
  a <- inner state
  state' <- takeMVar state
  liftIO $ do
    unless (B.null state') (B.putStr "\n")
    hSetBuffering stdout originalMode
  return a
