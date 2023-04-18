{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.GhcOptions
  ( GhcOptions (..)
  ) where

import           Data.Attoparsec.Args ( EscapingMode (Escaping), parseArgs )
import qualified Data.Text as T
import           Pantry.Internal.AesonExtended ( FromJSON (..), withText )
import           Stack.Prelude

newtype GhcOptions = GhcOptions { unGhcOptions :: [Text] }

instance FromJSON GhcOptions where
  parseJSON = withText "GhcOptions" $ \t ->
    case parseArgs Escaping t of
      Left e -> fail e
      Right opts -> pure $ GhcOptions $ map T.pack opts
