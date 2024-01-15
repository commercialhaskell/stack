{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.HaddockParser
  ( haddockOptsParser
  ) where

import           Options.Applicative ( Parser, help, long, metavar )
import           Options.Applicative.Args ( argsOption )
import           Stack.Options.Utils ( hideMods )
import           Stack.Prelude
import           Stack.Types.BuildOptsMonoid ( HaddockOptsMonoid (..) )

-- | Parser for haddock arguments.
haddockOptsParser :: Bool -> Parser HaddockOptsMonoid
haddockOptsParser hide0 = HaddockOptsMonoid
  <$> fmap
        (fromMaybe [])
        ( optional (argsOption
            (  long "haddock-arguments"
            <> metavar "HADDOCK_ARGS"
            <> help "Arguments passed to the Haddock program."
            <> hide
            ))
        )
 where
  hide = hideMods hide0
