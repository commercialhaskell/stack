{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.PrettyPrint
    (
      -- * Pretty printing functions
      displayPlain, displayWithColor
      -- * Logging based on pretty-print typeclass
    , prettyDebug, prettyInfo, prettyWarn, prettyError
    , prettyDebugL, prettyInfoL, prettyWarnL, prettyErrorL
    , prettyDebugS, prettyInfoS, prettyWarnS, prettyErrorS
    , debugBracket
      -- * Color utils
      -- | These are preferred to colors directly, so that we can
      -- encourage consistency of color meanings.
    , errorColor, goodColor, shellColor, fileColor
    , displayTargetPkgId, displayCurrentPkgId, displayCurrentPkgName, displayErrorPkgId
    , displayMilliseconds
      -- * Formatting utils
    , bulletedList
      -- * Re-exports from "Text.PrettyPrint.Leijen.Extended"
    , Display(..), AnsiDoc, AnsiAnn(..), HasAnsiAnn(..), Doc
    , nest, line, linebreak, group, softline, softbreak
    , align, hang, indent, encloseSep
    , (<+>)
    , hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate
    , fill, fillBreak
    , enclose, squotes, dquotes, parens, angles, braces, brackets
    , indentAfterLabel, wordDoc
    ) where

import           Stack.Prelude
import           Data.List (intersperse)
import qualified Data.Text as T
import           Language.Haskell.TH
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Runner
import           Stack.Types.Version
import qualified System.Clock as Clock
import           Text.PrettyPrint.Leijen.Extended

displayWithColor
    :: (HasRunner env, MonadReader env m, Display a, HasAnsiAnn (Ann a))
    => a -> m T.Text
displayWithColor x = do
    useAnsi <- liftM logUseColor $ view logOptionsL
    return $ if useAnsi then displayAnsi x else displayPlain x

-- TODO: switch to using implicit callstacks once 7.8 support is dropped

prettyDebug :: Q Exp
prettyDebug = do
    loc <- location
    [e| monadLoggerLog loc "" LevelDebug <=< displayWithColor |]

prettyInfo :: Q Exp
prettyInfo = do
    loc <- location
    [e| monadLoggerLog loc "" LevelInfo <=< displayWithColor |]

prettyWarn :: Q Exp
prettyWarn = do
    loc <- location
    [e| monadLoggerLog loc "" LevelWarn <=< displayWithColor . (line <>) . (warningColor "Warning:" <+>) . indentAfterLabel |]

prettyError :: Q Exp
prettyError = do
    loc <- location
    [e| monadLoggerLog loc "" LevelError <=< displayWithColor . (line <>) . (errorColor "Error:" <+>) . indentAfterLabel |]

-- TODO: Figure out how to collapse these to use the same implementation
--       as the above ones!

prettyDebugL :: Q Exp
prettyDebugL = do
    loc <- location
    [e| monadLoggerLog loc "" LevelDebug <=< displayWithColor . fillSep|]

prettyInfoL :: Q Exp
prettyInfoL = do
    loc <- location
    [e| monadLoggerLog loc "" LevelInfo <=< displayWithColor . fillSep|]

prettyWarnL :: Q Exp
prettyWarnL = do
    loc <- location
    [e| monadLoggerLog loc "" LevelWarn <=< displayWithColor . (line <>) . (warningColor "Warning:" <+>) . indentAfterLabel . fillSep|]

prettyErrorL :: Q Exp
prettyErrorL = do
    loc <- location
    [e| monadLoggerLog loc "" LevelError <=< displayWithColor . (line <>) . (errorColor "Error:" <+>) . indentAfterLabel . fillSep|]

prettyDebugS :: Q Exp
prettyDebugS = do
    loc <- location
    [e| monadLoggerLog loc "" LevelDebug <=< displayWithColor . fillSep . wordDoc|]

prettyInfoS :: Q Exp
prettyInfoS = do
    loc <- location
    [e| monadLoggerLog loc "" LevelInfo <=< displayWithColor . fillSep . wordDoc|]

prettyWarnS :: Q Exp
prettyWarnS = do
    loc <- location
    [e| monadLoggerLog loc "" LevelWarn <=< displayWithColor . (line <>) . (warningColor "Warning:" <+>) . indentAfterLabel . fillSep . wordDoc|]

prettyErrorS :: Q Exp
prettyErrorS = do
    loc <- location
    [e| monadLoggerLog loc "" LevelError <=< displayWithColor . (line <>) . (errorColor "Error:" <+>) . indentAfterLabel . fillSep . wordDoc|]

-- End of duplicates

indentAfterLabel :: Doc a -> Doc a
indentAfterLabel = align

wordDoc :: String -> [Doc a]
wordDoc = map fromString . words

debugBracket :: Q Exp
debugBracket = do
    loc <- location
    [e| \msg f -> do
            let output = monadLoggerLog loc "" LevelDebug <=< displayWithColor
            output $ "Start: " <> msg
            start <- liftIO $ Clock.getTime Clock.Monotonic
            x <- f `catch` \ex -> do
                end <- liftIO $ Clock.getTime Clock.Monotonic
                let diff = Clock.diffTimeSpec start end
                output $ "Finished with exception in" <+> displayMilliseconds diff <> ":" <+>
                    msg <> line <>
                    "Exception thrown: " <> fromString (show ex)
                throwIO (ex :: SomeException)
            end <- liftIO $ Clock.getTime Clock.Monotonic
            let diff = Clock.diffTimeSpec start end
            output $ "Finished in" <+> displayMilliseconds diff <> ":" <+> msg
            return x
      |]

errorColor :: AnsiDoc -> AnsiDoc
errorColor = dullred

warningColor :: AnsiDoc -> AnsiDoc
warningColor = yellow

goodColor :: AnsiDoc -> AnsiDoc
goodColor = green

shellColor :: AnsiDoc -> AnsiDoc
shellColor = magenta

fileColor :: AnsiDoc -> AnsiDoc
fileColor = bold . white

dirColor :: AnsiDoc -> AnsiDoc
dirColor = bold . blue

displayTargetPkgId :: PackageIdentifier -> AnsiDoc
displayTargetPkgId = cyan . display

displayCurrentPkgId :: PackageIdentifier -> AnsiDoc
displayCurrentPkgId = yellow . display

displayCurrentPkgName :: PackageName -> AnsiDoc
displayCurrentPkgName = yellow . display

displayErrorPkgId :: PackageIdentifier -> AnsiDoc
displayErrorPkgId = errorColor . display

instance Display PackageName where
    display = fromString . packageNameString

instance Display PackageIdentifier where
    display = fromString . packageIdentifierString

instance Display Version where
    display = fromString . versionString

instance Display (Path b File) where
    display = fileColor . fromString . toFilePath

instance Display (Path b Dir) where
    display = dirColor . fromString . toFilePath

instance Display (PackageName, NamedComponent) where
    display = cyan . fromString . T.unpack . renderPkgComponent

-- Display milliseconds.
displayMilliseconds :: Clock.TimeSpec -> AnsiDoc
displayMilliseconds t = goodColor $
    (fromString . show . (`div` 10^(6 :: Int)) . Clock.toNanoSecs) t <> "ms"

bulletedList :: [AnsiDoc] -> AnsiDoc
bulletedList = mconcat . intersperse line . map ("*" <+>)
