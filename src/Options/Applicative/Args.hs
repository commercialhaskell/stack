{-# LANGUAGE OverloadedStrings #-}

-- | Accepting arguments to be passed through to a sub-process.

module Options.Applicative.Args
    (argsArgument
    ,argsOption
    ,cmdOption
    ,parseArgsFromString)
    where

import           Data.Attoparsec.Args
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Options.Applicative as O

-- | An argument which accepts a list of arguments e.g. @--ghc-options="-X P.hs \"this\""@.
argsArgument :: O.Mod O.ArgumentFields [String] -> O.Parser [String]
argsArgument =
    O.argument
        (do string <- O.str
            either O.readerError return (parseArgsFromString string))

-- | An option which accepts a list of arguments e.g. @--ghc-options="-X P.hs \"this\""@.
argsOption :: O.Mod O.OptionFields [String] -> O.Parser [String]
argsOption =
    O.option
        (do string <- O.str
            either O.readerError return (parseArgsFromString string))

-- | An option which accepts a command and a list of arguments e.g. @--exec "echo hello world"@
cmdOption :: O.Mod O.OptionFields (String, [String]) -> O.Parser (String, [String])
cmdOption =
    O.option
        (do string <- O.str
            xs <- either O.readerError return (parseArgsFromString string)
            case xs of
                [] -> O.readerError "Must provide a command"
                x:xs' -> return (x, xs'))

-- | Parse from a string.
parseArgsFromString :: String -> Either String [String]
parseArgsFromString = P.parseOnly (argsParser Escaping) . T.pack
