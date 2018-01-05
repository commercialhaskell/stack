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
  , mkStickyLogger
  , LogOptions (..)
  ) where

import RIO.Prelude
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Lens.Micro (to)
import GHC.Stack (HasCallStack, CallStack, SrcLoc (..), getCallStack)
import Data.Time
import qualified Data.Text.IO as TIO
import Data.ByteString.Builder (toLazyByteString)
import           GHC.IO.Handle.Internals         (wantWritableHandle)
import           GHC.IO.Encoding.Types           (textEncodingName)
import           GHC.IO.Handle.Types             (Handle__ (..))

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

mkStickyLogger :: MonadIO m => LogOptions -> m LogFunc
mkStickyLogger options = do
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
    then undefined
    else
      return $ \cs src level str ->
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
        ansi reset <>
        msg <>
        getLoc <>
        ansi reset <>
        "\n"
  where
   reset = undefined
   setBlack = undefined
   setGreen = undefined
   setBlue = undefined
   setYellow = undefined
   setRed = undefined
   setMagenta = undefined

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
           LevelDebug -> ansi setGreen
           LevelInfo -> ansi setBlue
           LevelWarn -> ansi setYellow
           LevelError -> ansi setRed
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
             dirRoot = "" -- FIXME $(lift . T.unpack . fromMaybe undefined . T.stripSuffix (T.pack $ "Stack" </> "Types" </> "Runner.hs") . T.pack . loc_filename =<< location)
          in fromString (fromMaybe file (stripPrefix dirRoot file)) <>
             ":" <>
             displayShow (srcLocStartLine loc) <>
             ":" <>
             displayShow (srcLocStartCol loc)

-- | The length of a timestamp in the format "YYYY-MM-DD hh:mm:ss.μμμμμμ".
-- This definition is top-level in order to avoid multiple reevaluation at runtime.
timestampLength :: Int
timestampLength =
  length (formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0))

{- FIXME
-- FIXME move into RIO.Logger?
stickyLoggerFuncImpl
    :: Sticky -> LogOptions
    -> (CallStack -> LogSource -> LogLevel -> LogStr -> IO ())
stickyLoggerFuncImpl (Sticky mref) lo loc src level msgOrig =
    case mref of
        Nothing ->
            loggerFunc
                lo
                out
                loc
                src
                (case level of
                     LevelOther "sticky-done" -> LevelInfo
                     LevelOther "sticky" -> LevelInfo
                     _ -> level)
                msgOrig
        Just ref -> modifyMVar_ ref $ \sticky -> do
            let backSpaceChar = '\8'
                repeating = S8.replicate (maybe 0 T.length sticky)
                clear = S8.hPutStr out
                    (repeating backSpaceChar <>
                     repeating ' ' <>
                     repeating backSpaceChar)

            -- Convert some GHC-generated Unicode characters as necessary
            let msgText
                    | logUseUnicode lo = msgTextRaw
                    | otherwise = T.map replaceUnicode msgTextRaw

            case level of
                LevelOther "sticky-done" -> do
                    clear
                    T.hPutStrLn out msgText
                    hFlush out
                    return Nothing
                LevelOther "sticky" -> do
                    clear
                    T.hPutStr out msgText
                    hFlush out
                    return (Just msgText)
                _
                    | level >= logMinLevel lo -> do
                        clear
                        loggerFunc lo out loc src level msgText
                        case sticky of
                            Nothing ->
                                return Nothing
                            Just line -> do
                                T.hPutStr out line >> hFlush out
                                return sticky
                    | otherwise ->
                        return sticky
  where
    out = stderr

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

newtype Sticky = Sticky
  { unSticky :: Maybe (MVar (Maybe Text))
  }
-}
