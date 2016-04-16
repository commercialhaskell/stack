{-# LANGUAGE OverloadedStrings #-}
{- |  This module implements parsing of additional arguments embedded in a
      comment when stack is invoked as a script interpreter

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
    --resolver lts-3.14
    --install-ghc
    runghc
    --package random
  -\}
@

  When the @#!@ line is not present, the file can still be executed
  using @stack \<file name\>@ command if the file starts with a valid stack
  interpreter comment. This can be used to execute the file on Windows for
  example.

  Nested block comments are not supported.
-}

module Data.Attoparsec.Interpreter
    ( interpreterArgsParser -- for unit tests
    , getInterpreterArgs
    ) where

import           Control.Applicative
import           Data.Attoparsec.Args
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Char (isSpace)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Text (decodeUtf8)
import           Data.List (intercalate)
import           Data.Text (pack)
import           Stack.Constants
import           System.FilePath (takeExtension)
import           System.IO (IOMode (ReadMode), withBinaryFile, stderr, hPutStrLn)

-- | Parser to extract the stack command line embedded inside a comment
-- after validating the placement and formatting rules for a valid
-- interpreter specification.
interpreterArgsParser :: Bool -> String -> P.Parser String
interpreterArgsParser isLiterate progName = P.option "" sheBangLine *> interpreterComment
  where
    sheBangLine =   P.string "#!"
                 *> P.manyTill P.anyChar P.endOfLine

    commentStart psr =   (psr <?> (progName ++ " options comment"))
                      *> P.skipSpace
                      *> (P.string (pack progName) <?> show progName)

    -- Treat newlines as spaces inside the block comment
    anyCharNormalizeSpace = let normalizeSpace c = if isSpace c then ' ' else c
                            in P.satisfyWith normalizeSpace $ const True

    comment start end = commentStart start
      *> ((end >> return "")
          <|> (P.space *> (P.manyTill anyCharNormalizeSpace end <?> "-}")))

    horizontalSpace = P.satisfy P.isHorizontalSpace

    lineComment =  comment "--" (P.endOfLine <|> P.endOfInput)
    literateLineComment = comment
      (">" *> horizontalSpace *> "--")
      (P.endOfLine <|> P.endOfInput)
    blockComment = comment "{-" (P.string "-}")

    literateBlockComment =
      (">" *> horizontalSpace *> "{-")
      *> P.skipMany (("" <$ horizontalSpace) <|> (P.endOfLine *> ">"))
      *> (P.string (pack progName) <?> progName)
      *> (P.manyTill' (P.satisfy (not . P.isEndOfLine)
                       <|> (' ' <$ (P.endOfLine *> ">" <?> ">"))) "-}")

    interpreterComment = if isLiterate
                            then literateLineComment <|> literateBlockComment
                            else lineComment <|> blockComment

-- | Extract stack arguments from a correctly placed and correctly formatted
-- comment when it is being used as an interpreter
getInterpreterArgs :: String -> IO [String]
getInterpreterArgs file = do
  eArgStr <- withBinaryFile file ReadMode parseFile
  case eArgStr of
    Left err -> handleFailure $ decodeError err
    Right str -> parseArgStr str
  where
    parseFile h =
      CB.sourceHandle h
      =$= decodeUtf8
      $$ sinkParserEither (interpreterArgsParser isLiterate stackProgName)

    isLiterate = takeExtension file == ".lhs"

    -- FIXME We should print anything only when explicit verbose mode is
    -- specified by the user on command line. But currently the
    -- implementation does not accept or parse any command line flags in
    -- interpreter mode. We can only invoke the interpreter as
    -- "stack <file name>" strictly without any options.
    stackWarn s = hPutStrLn stderr $ stackProgName ++ ": WARNING! " ++ s

    handleFailure err = do
      mapM_ stackWarn (lines err)
      stackWarn "Missing or unusable stack options specification"
      stackWarn "Using runghc without any additional stack options"
      return ["runghc"]

    parseArgStr str =
      case P.parseOnly (argsParser Escaping) (pack str) of
        Left err -> handleFailure ("Error parsing command specified in the \
                        \stack options comment: " ++ err)
        Right [] -> handleFailure ("Empty argument list in stack options \
                        \comment")
        Right args -> return args

    decodeError e =
      case e of
        ParseError ctxs _ (Position line col) ->
          if length ctxs == 0
          then "Parse error"
          else ("Expecting " ++ (intercalate " or " ctxs))
          ++ " at line " ++ (show line) ++ ", column " ++ (show col)
        DivergentParser -> "Divergent parser"
