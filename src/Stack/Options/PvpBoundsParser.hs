{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.PvpBoundsParser
Description : Parser for PVP bounds.
License     : BSD-3-Clause

Parser for PVP bounds.
-}

module Stack.Options.PvpBoundsParser
  ( pvpBoundsParser
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, completeWith, help, long, metavar, option
                   , readerError
                   )
import           Options.Applicative.Types ( readerAsk )
import           Stack.Prelude
import           Stack.Types.PvpBounds ( PvpBounds (..), parsePvpBounds )

-- | Parser for PVP bounds.
pvpBoundsParser ::
     Maybe Text
     -- ^ Optional context for the option's help message.
  -> Parser PvpBounds
pvpBoundsParser context = option readPvpBounds
  (  long "pvp-bounds"
  <> metavar "PVP-BOUNDS"
  <> completeWith ["none", "lower", "upper", "both"]
  <> help (T.unpack helpMsg)
  )
 where
  readPvpBounds = do
    s <- readerAsk
    case parsePvpBounds $ T.pack s of
      Left e -> readerError e
      Right v -> pure v
  helpMsg =
       helpMsgPrefix
    <> " PVP version bounds should be added to Cabal file: none, lower, upper, \
       \both."
  helpMsgPrefix = maybe "How" (<> ", how") context
