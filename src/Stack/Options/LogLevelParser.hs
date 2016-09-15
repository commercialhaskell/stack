{-# LANGUAGE OverloadedStrings #-}

module Stack.Options.LogLevelParser where

import           Control.Monad.Logger              (LogLevel (..))
import           Data.Monoid.Extra
import qualified Data.Text                         as T
import           Options.Applicative
import           Stack.Options.Utils

-- | Parser for a logging level.
logLevelOptsParser :: Bool -> Maybe LogLevel -> Parser (Maybe LogLevel)
logLevelOptsParser hide defLogLevel =
  fmap (Just . parse)
       (strOption (long "verbosity" <>
                   metavar "VERBOSITY" <>
                   help "Verbosity: silent, error, warn, info, debug" <>
                   hideMods hide)) <|>
  flag' (Just verboseLevel)
       (short 'v' <> long "verbose" <>
        help ("Enable verbose mode: verbosity level \"" <> showLevel verboseLevel <> "\"") <>
        hideMods hide) <|>
  flag' (Just silentLevel)
       (long "silent" <>
        help ("Enable silent mode: verbosity level \"" <> showLevel silentLevel <> "\"") <>
        hideMods hide) <|>
  pure defLogLevel
  where verboseLevel = LevelDebug
        silentLevel = LevelOther "silent"
        showLevel l =
          case l of
            LevelDebug -> "debug"
            LevelInfo -> "info"
            LevelWarn -> "warn"
            LevelError -> "error"
            LevelOther x -> T.unpack x
        parse s =
          case s of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther (T.pack s)
