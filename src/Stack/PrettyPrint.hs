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
    , debugBracket
      -- * Color utils
      -- | These are preferred to colors directly, so that we can
      -- encourage consistency of color meanings.
    , errorRed, goodGreen, shellMagenta
    , displayTargetPkgId, displayCurrentPkgId, displayErrorPkgId
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
    ) where

import           Control.Exception.Lifted
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List (intersperse)
import           Data.Monoid
import           Data.String (fromString)
import qualified Data.Text as T
import           Language.Haskell.TH
import           Path
import           Stack.Types.Internal
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import qualified System.Clock as Clock
import           Text.PrettyPrint.Leijen.Extended

displayWithColor
    :: (HasLogOptions env, MonadReader env m, Display a, HasAnsiAnn (Ann a))
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
    [e| monadLoggerLog loc "" LevelWarn <=< displayWithColor . (line <>) . (warningYellow "Warning:" <+>) |]

prettyError :: Q Exp
prettyError = do
    loc <- location
    [e| monadLoggerLog loc "" LevelError <=< displayWithColor . (line <>) . (errorRed "Error:" <+>) |]

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
                throw (ex :: SomeException)
            end <- liftIO $ Clock.getTime Clock.Monotonic
            let diff = Clock.diffTimeSpec start end
            output $ "Finished in" <+> displayMilliseconds diff <> ":" <+> msg
            return x
      |]

errorRed :: AnsiDoc -> AnsiDoc
errorRed = dullred

warningYellow :: AnsiDoc -> AnsiDoc
warningYellow = yellow

goodGreen :: AnsiDoc -> AnsiDoc
goodGreen = green

shellMagenta :: AnsiDoc -> AnsiDoc
shellMagenta = magenta

displayTargetPkgId :: PackageIdentifier -> AnsiDoc
displayTargetPkgId = cyan . display

displayCurrentPkgId :: PackageIdentifier -> AnsiDoc
displayCurrentPkgId = yellow . display

displayErrorPkgId :: PackageIdentifier -> AnsiDoc
displayErrorPkgId = errorRed . display

instance Display PackageName where
    display = fromString . packageNameString

instance Display PackageIdentifier where
    display = fromString . packageIdentifierString

instance Display Version where
    display = fromString . versionString

instance Display (Path b File) where
    display = bold . white . fromString . toFilePath

instance Display (Path b Dir) where
    display = bold . blue . fromString . toFilePath

instance Display (PackageName, NamedComponent) where
    display = cyan . fromString . T.unpack . renderPkgComponent

-- Display milliseconds.
displayMilliseconds :: Clock.TimeSpec -> AnsiDoc
displayMilliseconds t = goodGreen $
    (fromString . show . (`div` 10^(6 :: Int)) . Clock.toNanoSecs) t <> "ms"

bulletedList :: [AnsiDoc] -> AnsiDoc
bulletedList = mconcat . intersperse line . map ("*" <+>)
