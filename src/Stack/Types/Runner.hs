{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Run environment

module Stack.Types.Runner
    ( Runner (..)
    , HasRunner (..)
    , terminalL
    , reExecL
    , stickyL
    , logOptionsL
    , Sticky (..)
    , LogOptions (..)
    , ColorWhen (..)
    , withRunner
    ) where

import qualified Data.ByteString.Char8      as S8
import           Data.Char
import           Data.List                  (stripPrefix)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.Text.IO               as T
import           Data.Time
import           GHC.Foreign                (peekCString, withCString)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (lift)
import           Lens.Micro
import           Stack.Prelude              hiding (lift)
import           System.Console.ANSI
import           System.FilePath
import           System.IO
import           System.Log.FastLogger

-- | Monadic environment.
data Runner = Runner
  { runnerReExec     :: !Bool
  , runnerLogOptions :: !LogOptions
  , runnerTerminal   :: !Bool
  , runnerSticky     :: !Sticky
  }

class HasLogFunc env => HasRunner env where
  runnerL :: Lens' env Runner
instance HasRunner Runner where
  runnerL = id

terminalL :: HasRunner env => Lens' env Bool
terminalL = runnerL.lens runnerTerminal (\x y -> x { runnerTerminal = y })

reExecL :: HasRunner env => Lens' env Bool
reExecL = runnerL.lens runnerReExec (\x y -> x { runnerReExec = y })

stickyL :: HasRunner env => Lens' env Sticky
stickyL = runnerL.lens runnerSticky (\x y -> x { runnerSticky = y })

logOptionsL :: HasRunner env => Lens' env LogOptions
logOptionsL = runnerL.lens runnerLogOptions (\x y -> x { runnerLogOptions = y })

newtype Sticky = Sticky
  { unSticky :: Maybe (MVar (Maybe Text))
  }

data LogOptions = LogOptions
  { logUseColor      :: Bool
  , logUseUnicode    :: Bool
  , logUseTime       :: Bool
  , logMinLevel      :: LogLevel
  , logVerboseFormat :: Bool
  }

--------------------------------------------------------------------------------
-- Logging functionality

instance HasLogFunc Runner where
  logFuncL = to $ \env -> stickyLoggerFuncImpl (view stickyL env) (view logOptionsL env)

stickyLoggerFuncImpl
    :: ToLogStr msg
    => Sticky -> LogOptions
    -> (Loc -> LogSource -> LogLevel -> msg -> IO ())
stickyLoggerFuncImpl (Sticky mref) lo loc src level msg =
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
                msg
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
                        loggerFunc lo out loc src level $ toLogStr msgText
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
    msgTextRaw = T.decodeUtf8With T.lenientDecode msgBytes
    msgBytes = fromLogStr (toLogStr msg)

-- | Replace Unicode characters with non-Unicode equivalents
replaceUnicode :: Char -> Char
replaceUnicode '\x2018' = '`'
replaceUnicode '\x2019' = '\''
replaceUnicode c = c

-- | Logging function takes the log level into account.
loggerFunc :: ToLogStr msg
           => LogOptions -> Handle -> Loc -> Text -> LogLevel -> msg -> IO ()
loggerFunc lo outputChannel loc _src level msg =
   when (level >= logMinLevel lo)
        (liftIO (do out <- getOutput
                    T.hPutStrLn outputChannel out))
  where
    getOutput = do
      timestamp <- getTimestamp
      l <- getLevel
      lc <- getLoc
      return $ T.concat
        [ T.pack timestamp
        , T.pack l
        , T.pack (ansi [Reset])
        , T.decodeUtf8 (fromLogStr (toLogStr msg))
        , T.pack lc
        , T.pack (ansi [Reset])
        ]
     where
       ansi xs | logUseColor lo = setSGRCode xs
               | otherwise = ""
       getTimestamp
         | logVerboseFormat lo && logUseTime lo =
           do now <- getZonedTime
              return $
                  ansi [SetColor Foreground Vivid Black]
                  ++ formatTime' now ++ ": "
         | otherwise = return ""
         where
           formatTime' =
               take timestampLength . formatTime defaultTimeLocale "%F %T.%q"
       getLevel
         | logVerboseFormat lo =
           return ((case level of
                      LevelDebug -> ansi [SetColor Foreground Dull Green]
                      LevelInfo -> ansi [SetColor Foreground Dull Blue]
                      LevelWarn -> ansi [SetColor Foreground Dull Yellow]
                      LevelError -> ansi [SetColor Foreground Dull Red]
                      LevelOther _ -> ansi [SetColor Foreground Dull Magenta]) ++
                   "[" ++
                   map toLower (drop 5 (show level)) ++
                   "] ")
         | otherwise = return ""
       getLoc
         | logVerboseFormat lo =
           return $
               ansi [SetColor Foreground Vivid Black] ++
               "\n@(" ++ fileLocStr ++ ")"
         | otherwise = return ""
       fileLocStr =
         fromMaybe file (stripPrefix dirRoot file) ++
         ':' :
         line loc ++
         ':' :
         char loc
         where
           file = loc_filename loc
           line = show . fst . loc_start
           char = show . snd . loc_start
       dirRoot = $(lift . T.unpack . fromMaybe undefined . T.stripSuffix (T.pack $ "Stack" </> "Types" </> "Runner.hs") . T.pack . loc_filename =<< location)

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

-- | With a 'Runner', do the thing
withRunner :: MonadIO m
           => LogLevel
           -> Bool -- ^ use time?
           -> Bool -- ^ terminal?
           -> ColorWhen
           -> Bool -- ^ reexec?
           -> (Runner -> m a)
           -> m a
withRunner logLevel useTime terminal colorWhen reExec inner = do
  useColor <- case colorWhen of
    ColorNever -> return False
    ColorAlways -> return True
    ColorAuto -> liftIO $ hSupportsANSI stderr
  canUseUnicode <- liftIO getCanUseUnicode
  withSticky terminal $ \sticky -> inner Runner
    { runnerReExec = reExec
    , runnerLogOptions = LogOptions
        { logUseColor = useColor
        , logUseUnicode = canUseUnicode
        , logUseTime = useTime
        , logMinLevel = logLevel
        , logVerboseFormat = logLevel <= LevelDebug
        }
    , runnerTerminal = terminal
    , runnerSticky = sticky
    }

-- | Taken from GHC: determine if we should use Unicode syntax
getCanUseUnicode :: IO Bool
getCanUseUnicode = do
    let enc = localeEncoding
        str = "\x2018\x2019"
        test = withCString enc str $ \cstr -> do
            str' <- peekCString enc cstr
            return (str == str')
    test `catchIO` \_ -> return False

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Show, Generic)
