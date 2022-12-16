{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.GhcVariantParser
  ( ghcVariantParser
  ) where

import           Options.Applicative
                   ( Parser, help, long, metavar, option, readerError )
import           Options.Applicative.Types ( readerAsk )
import           Stack.Prelude
import           Stack.Options.Utils ( hideMods )
import           Stack.Types.Config ( GHCVariant, parseGHCVariant )

-- | GHC variant parser
ghcVariantParser :: Bool -> Parser GHCVariant
ghcVariantParser hide = option readGHCVariant
  (  long "ghc-variant"
  <> metavar "VARIANT"
  <> help "Specialized GHC variant, e.g. int-native or integersimple \
          \(incompatible with --system-ghc)"
  <> hideMods hide
  )
 where
  readGHCVariant = do
    s <- readerAsk
    case parseGHCVariant s of
      Left e -> readerError (displayException e)
      Right v -> pure v
