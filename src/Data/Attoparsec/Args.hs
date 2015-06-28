{-# LANGUAGE OverloadedStrings #-}
-- | Parsing argument-like things.

module Data.Attoparsec.Args (EscapingMode(..), argsParser, withInterpreterArgs) where

import           Control.Applicative
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Types (Parser)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import           System.Directory (doesFileExist)
import           System.Environment (getArgs, withArgs)
import           System.IO (IOMode (ReadMode), withBinaryFile)

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

-- | Use 'withArgs' on result of 'getInterpreterArgs'.
withInterpreterArgs :: String -> ([String] -> Bool -> IO a) -> IO a
withInterpreterArgs progName inner = do
    (args, isInterpreter) <- getInterpreterArgs progName
    withArgs args $ inner args isInterpreter

-- | Check if command-line looks like it's being used as a script interpreter,
-- and if so look for a @-- progName ...@ comment that contains additional
-- arguments.
getInterpreterArgs :: String -> IO ([String], Bool)
getInterpreterArgs progName = do
    args0 <- getArgs
    case args0 of
        (x:_) -> do
            isFile <- doesFileExist x
            if isFile
                then do
                    margs <-
                        withBinaryFile x ReadMode $ \h ->
                        CB.sourceHandle h
                            $= CB.lines
                            $= CL.map killCR
                            $$ sinkInterpreterArgs progName
                    return $ case margs of
                        Nothing -> (args0, True)
                        Just args -> (args ++ "--" : args0, True)
                else return (args0, False)
        _ -> return (args0, False)
  where
    killCR bs
        | S.null bs || S.last bs /= 13 = bs
        | otherwise = S.init bs

sinkInterpreterArgs :: Monad m => String -> Sink ByteString m (Maybe [String])
sinkInterpreterArgs progName =
    await >>= maybe (return Nothing) checkShebang
  where
    checkShebang bs
        | "#!" `S.isPrefixOf` bs = fmap (maybe Nothing parseArgs) await
        | otherwise = return (parseArgs bs)

    parseArgs bs =
        case decodeUtf8' bs of
            Left _ -> Nothing
            Right t ->
                case P.parseOnly (argsParser Escaping) t of
                    Right ("--":progName':rest) | progName' == progName -> Just rest
                    _ -> Nothing
