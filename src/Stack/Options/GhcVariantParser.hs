{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.GhcVariantParser where

import           Options.Applicative
import           Options.Applicative.Types         (readerAsk)
import           Stack.Prelude
import           Stack.Options.Utils
import           Stack.Types.Config

-- | GHC variant parser
ghcVariantParser :: Bool -> Parser GHCVariant
ghcVariantParser hide = option readGHCVariant
   ( long "ghc-variant"
  <> metavar "VARIANT"
  <> help "Specialized GHC variant, e.g. int-native or integersimple \
          \(incompatible with --system-ghc)"
  <> hideMods hide
   )
 where
  readGHCVariant = do
    s <- readerAsk
    case parseGHCVariant s of
      Left e -> readerError (show e)
      Right v -> pure v
