-- | Parsing argument-like things.

module Data.Attoparsec.Args (EscapingMode(..), argsParser) where

import           Control.Applicative
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Types (Parser)
import           Data.Text (Text)

-- | Mode for parsing escape characters.
data EscapingMode
    = Escaping
    | NoEscaping
    deriving (Show,Eq,Enum)

-- | A basic argument parser. It supports space-separated text, and
-- string quotation with identity escaping: \x -> x.
argsParser :: EscapingMode -> Parser Text [String]
argsParser mode = many (P.skipSpace *> (quoted <|> unquoted)) <*
                  P.skipSpace <* (P.endOfInput <?> "unterminated string")
  where
    unquoted = P.many1 naked
    quoted = P.char '"' *> string <* P.char '"'
    string = many (case mode of
                     Escaping -> escaped <|> nonquote
                     NoEscaping -> nonquote)
    escaped = P.char '\\' *> P.anyChar
    nonquote = P.satisfy (not . (=='"'))
    naked = P.satisfy (not . flip elem ("\" " :: String))
