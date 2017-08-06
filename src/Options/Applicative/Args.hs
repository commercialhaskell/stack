{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Accepting arguments to be passed through to a sub-process.

module Options.Applicative.Args
    (argsArgument
    ,manyArgsOptions
    ,textArgsOption
    ,argsOption
    ,cmdOption
    ,parseArgsFromString)
    where

import           Data.Attoparsec.Args
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Options.Applicative as O
import           Stack.Prelude

-- | An argument which accepts a list of arguments e.g. @"-X P.hs \"this\""@.
argsArgument :: O.Mod O.ArgumentFields [String] -> O.Parser [String]
argsArgument =
    O.argument
        (do string <- O.str
            either O.readerError return (parseArgsFromString string))

-- | Like 'textArgsOption', but allows the option to be used multiple
-- times. The options get concatenated.
manyArgsOptions :: O.Mod O.OptionFields [String] -> O.Parser [Text]
manyArgsOptions = fmap concat . many . textArgsOption

-- | Like 'argsOption' but yields 'Text'.
textArgsOption :: O.Mod O.OptionFields [String] -> O.Parser [Text]
textArgsOption = fmap (map T.pack) . argsOption

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
