{-# LANGUAGE OverloadedStrings #-}

-- | Accepting arguments to be passed through to a sub-process.

module Options.Applicative.Args
    (argsArgument
    ,argsOption
    ,parseArgsFromString
    ,argsParser)
    where

import           Control.Applicative
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Types (Parser)
import           Data.Text (Text)
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

-- | Parse from a string.
parseArgsFromString :: String -> Either String [String]
parseArgsFromString = P.parseOnly argsParser . T.pack

-- | A basic argument parser. It supports space-separated text, and
-- string quotation with identity escaping: \x -> x.
argsParser :: Parser Text [String]
argsParser = many (P.skipSpace *> (quoted <|> unquoted)) <*
             P.skipSpace <* (P.endOfInput <?> "unterminated string")
  where
    unquoted = P.many1 naked
    quoted = P.char '"' *> string <* P.char '"'
    string = many (escaped <|> nonquote)
    escaped = P.char '\\' *> P.anyChar
    nonquote = P.satisfy (not . (=='"'))
    naked = P.satisfy (not . flip elem "\" ")
