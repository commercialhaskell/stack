module Stack.Options.GhcVariantParser where

import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Types         (readerAsk)
import           Stack.Options.Utils
import           Stack.Types.Config

-- | GHC variant parser
ghcVariantParser :: Bool -> Parser GHCVariant
ghcVariantParser hide =
    option
        readGHCVariant
        (long "ghc-variant" <> metavar "VARIANT" <>
         help
             "Specialized GHC variant, e.g. integersimple (incompatible with --system-ghc)" <>
         hideMods hide
        )
  where
    readGHCVariant = do
        s <- readerAsk
        case parseGHCVariant s of
            Left e -> readerError (show e)
            Right v -> return v
