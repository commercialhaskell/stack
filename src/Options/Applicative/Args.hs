{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Accepting arguments to be passed through to a sub-process.

module Options.Applicative.Args
  ( argsArgument
  , argsOption
  , cmdOption
  ) where

import           Data.Attoparsec.Args ( EscapingMode (..), parseArgsFromString )
import qualified Options.Applicative as O
import           Stack.Prelude

-- | An argument which accepts a list of arguments
-- e.g. @--ghc-options="-X P.hs \"this\""@.
argsArgument :: O.Mod O.ArgumentFields [String] -> O.Parser [String]
argsArgument =
  O.argument
    (do s <- O.str
        either O.readerError pure (parseArgsFromString Escaping s))

-- | An option which accepts a list of arguments
-- e.g. @--ghc-options="-X P.hs \"this\""@.
argsOption :: O.Mod O.OptionFields [String] -> O.Parser [String]
argsOption =
  O.option
    (do s <- O.str
        either O.readerError pure (parseArgsFromString Escaping s))

-- | An option which accepts a command and a list of arguments
-- e.g. @--exec "echo hello world"@
cmdOption ::
     O.Mod O.OptionFields (String, [String])
  -> O.Parser (String, [String])
cmdOption =
  O.option
    (do s <- O.str
        xs <- either O.readerError pure (parseArgsFromString Escaping s)
        case xs of
          [] -> O.readerError "Must provide a command"
          x:xs' -> pure (x, xs'))
