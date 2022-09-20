{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Accepting arguments to be passed through to a sub-process.

module Options.Applicative.Args
    (argsArgument
    ,argsOption
    ,cmdOption)
    where

import           Data.Attoparsec.Args
import qualified Options.Applicative as O
import           Stack.Prelude

-- | An argument which accepts a list of arguments e.g. @--ghc-options="-X P.hs \"this\""@.
argsArgument :: O.Mod O.ArgumentFields [String] -> O.Parser [String]
argsArgument =
    O.argument
        (do string <- O.str
            either O.readerError pure (parseArgsFromString Escaping string))

-- | An option which accepts a list of arguments e.g. @--ghc-options="-X P.hs \"this\""@.
argsOption :: O.Mod O.OptionFields [String] -> O.Parser [String]
argsOption =
    O.option
        (do string <- O.str
            either O.readerError pure (parseArgsFromString Escaping string))

-- | An option which accepts a command and a list of arguments e.g. @--exec "echo hello world"@
cmdOption :: O.Mod O.OptionFields (String, [String]) -> O.Parser (String, [String])
cmdOption =
    O.option
        (do string <- O.str
            xs <- either O.readerError pure (parseArgsFromString Escaping string)
            case xs of
                [] -> O.readerError "Must provide a command"
                x:xs' -> pure (x, xs'))
