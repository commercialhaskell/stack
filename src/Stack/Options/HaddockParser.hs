module Stack.Options.HaddockParser where

import           Data.Maybe
import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Args
import           Stack.Options.Utils
import           Stack.Types.Config

-- | Parser for haddock arguments.
haddockOptsParser :: Bool -> Parser HaddockOptsMonoid
haddockOptsParser hide0 =
  HaddockOptsMonoid <$> fmap (fromMaybe [])
                             (optional
                              (argsOption
                               (long "haddock-arguments" <>
                                metavar "HADDOCK_ARGS" <>
                                help "Arguments passed to the haddock program" <>
                                hide)))
  where hide = hideMods hide0
