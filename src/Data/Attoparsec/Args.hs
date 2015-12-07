{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- |  This module implements the following:

    * Parsing of command line arguments for the stack command
    * Parsing of additional arguments embedded in a comment when stack is
      invoked as a script interpreter

  ===Specifying arguments in script interpreter mode
  @/stack/@ can execute a Haskell source file using @/runghc/@ and if required
  it can also install and setup the compiler and any package dependencies
  automatically.

  For using a Haskell source file as an executable script on a Unix like OS,
  the first line of the file must specify @stack@ as the interpreter using a
  shebang directive e.g.

  > #!/usr/bin/env stack

  Additional arguments can be specified in a haskell comment following the
  @#!@ line. The contents inside the comment must be a single valid stack
  command line, starting with @stack@ as the command and followed by the
  options to use for executing this file.

  The comment must be on the line immediately following the @#!@ line. The
  comment must start in the first column of the line. When using a block style
  comment the command can be split on multiple lines.

  Here is an example of a single line comment:

  > #!/usr/bin/env stack
  > -- stack --resolver lts-3.14 --install-ghc runghc --package random

  Here is an example of a multi line block comment:

@
  #!\/usr\/bin\/env stack
  {\- stack
    --verbosity silent
    --resolver lts-3.14
    --install-ghc
    runghc
    --package random
    --package system-argv0
  -\}
@

  When the @#!@ line is not present, the file can still be executed
  using @stack \<file name\>@ command if the file starts with a valid stack
  interpreter comment. This can be used to execute the file on Windows for
  example.

  Nested block comments are not supported.
-}

module Data.Attoparsec.Args
    ( EscapingMode(..)
    , argsParser
    , parseArgs
    , withInterpreterArgs
    ) where

import           Control.Applicative
import           Control.Monad.Catch (MonadThrow)
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Text(decodeUtf8)
import           Data.Char (isSpace)
import           Data.Text (Text, pack)
import           System.Directory (doesFileExist)
import           System.Environment (getArgs, withArgs)
import           System.IO (IOMode (ReadMode), withBinaryFile)

-- | Mode for parsing escape characters.
data EscapingMode
    = Escaping
    | NoEscaping
    deriving (Show,Eq,Enum)

-- | Parse arguments using 'argsParser'.
parseArgs :: EscapingMode -> Text -> Either String [String]
parseArgs mode = P.parseOnly (argsParser mode)

-- | A basic argument parser. It supports space-separated text, and
-- string quotation with identity escaping: \x -> x.
argsParser :: EscapingMode -> P.Parser [String]
argsParser mode = many (P.skipSpace *> (quoted <|> unquoted)) <*
                  P.skipSpace <* (P.endOfInput <?> "unterminated string")
  where
    unquoted = P.many1 naked
    quoted = P.char '"' *> string <* P.char '"'
    string = many (case mode of
                     Escaping -> escaped <|> nonquote
                     NoEscaping -> nonquote)
    escaped = P.char '\\' *> P.anyChar
    nonquote = P.satisfy (/= '"')
    naked = P.satisfy (not . flip elem ("\" " :: String))

-- | Parser to extract the stack command line embedded inside a comment
-- after validating the placement and formatting rules for a valid
-- interpreter specification.
interpreterArgsParser :: String -> P.Parser String
interpreterArgsParser progName = P.option "" sheBangLine *> interpreterComment
  where
    sheBangLine =   P.string "#!"
                 *> P.manyTill P.anyChar P.endOfLine

    commentStart str =   P.string str
                      *> P.skipSpace
                      *> P.string (pack progName)
                      *> P.space

    -- Treat newlines as spaces inside the block comment
    anyCharNormalizeSpace = let normalizeSpace c = if isSpace c then ' ' else c
                            in P.satisfyWith normalizeSpace $ const True

    comment start end =   commentStart start
                       *> P.manyTill anyCharNormalizeSpace end

    lineComment =  comment "--" P.endOfLine
    blockComment = comment "{-" (P.string "-}" <?> "unterminated block comment")
    interpreterComment = lineComment <|> blockComment

-- | Use 'withArgs' on result of 'getInterpreterArgs'.
withInterpreterArgs :: String -> ([String] -> Bool -> IO a) -> IO a
withInterpreterArgs progName inner = do
    (args, isInterpreter) <- getInterpreterArgs progName
    withArgs args $ inner args isInterpreter

-- | Extract stack arguments from a correctly placed and correctly formatted
-- comment when it is being used as an interpreter
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
                            =$= decodeUtf8
                            $$ sinkInterpreterArgs progName
                    return $ case margs of
                        Nothing -> (args0, True)
                        Just args -> (args ++ "--" : args0, True)
                else return (args0, False)
        _ -> return (args0, False)

sinkInterpreterArgs :: MonadThrow m => String -> Sink Text m (Maybe [String])
sinkInterpreterArgs progName = do
    eArgs <- sinkParserEither (interpreterArgsParser progName)
    case eArgs of
        Right (P.parseOnly (argsParser Escaping) . pack -> Right args) ->
            return $ Just args
        _ -> return Nothing
