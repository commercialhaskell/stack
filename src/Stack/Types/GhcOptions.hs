{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.GhcOptions
License     : BSD-3-Clause
-}

module Stack.Types.GhcOptions
  ( GhcOptions (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), withText )
import           Data.Attoparsec.Args ( EscapingMode (Escaping), parseArgs )
import qualified Data.Text as T
import           Stack.Prelude

newtype GhcOptions = GhcOptions { ghcOptions :: [Text] }

instance FromJSON GhcOptions where
  parseJSON = withText "GhcOptions" $ \t ->
    case parseArgs Escaping t of
      Left e -> fail e
      Right opts -> pure $ GhcOptions $ map T.pack opts
