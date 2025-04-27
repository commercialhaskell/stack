{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.LogLevelParser
License     : BSD-3-Clause
-}

module Stack.Options.LogLevelParser
  ( logLevelOptsParser
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, completeWith, flag', help, long, metavar, short
                   , strOption
                   )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude

-- | Parser for a logging level.
logLevelOptsParser :: Bool -> Parser (Maybe LogLevel)
logLevelOptsParser hide = fmap (Just . parse)
      (strOption
        (  long "verbosity"
        <> metavar "VERBOSITY"
        <> completeWith ["silent", "error", "warn", "info", "debug"]
        <> help "Set verbosity level: silent, error, warn, info or debug."
        <> hideMods hide
        ))
  <|> flag' (Just verboseLevel)
        (  short 'v'
        <> long "verbose"
        <> help
             (  "Enable verbose mode: verbosity level \""
             <> showLevel verboseLevel
             <> "\"."
             )
        <> hideMods hide
        )
  <|> flag' (Just silentLevel)
        (  long "silent"
        <> help (  "Enable silent mode: verbosity level \""
                <> showLevel silentLevel
                <> "\"."
                )
        <> hideMods hide
        )
  <|> pure Nothing
 where
  verboseLevel = LevelDebug
  silentLevel = LevelOther "silent"
  showLevel l = case l of
    LevelDebug -> "debug"
    LevelInfo -> "info"
    LevelWarn -> "warn"
    LevelError -> "error"
    LevelOther x -> T.unpack x
  parse s = case s of
    "debug" -> LevelDebug
    "info" -> LevelInfo
    "warn" -> LevelWarn
    "error" -> LevelError
    _ -> LevelOther (T.pack s)
