{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Stack.PrettyPrint
    (
      -- * Pretty printing functions
      displayPlain, displayAnsiIfPossible
      -- * Logging based on pretty-print typeclass
    , prettyDebug, prettyInfo, prettyWarn, prettyError
      -- * Color utils
      -- | These are preferred to colors directly, so that we can
      -- encourage consistency of color meanings.
    , errorRed, goodGreen
    , displayTargetPkgId, displayCurrentPkgId, displayErrorPkgId
      -- * Re-exports from "Text.PrettyPrint.Leijen.Extended"
    , Display(..), AnsiDoc, AnsiAnn(..), HasAnsiAnn(..), Doc
    , nest, line, linebreak, group, softline, softbreak
    , align, hang, indent, encloseSep
    , (<+>)
    , hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate
    , fill, fillBreak
    , enclose, squotes, dquotes, parens, angles, braces, brackets
    ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.String (fromString)
import qualified Data.Text as T
import           Language.Haskell.TH
import           Path
import           Stack.Types.Internal
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Text.PrettyPrint.Leijen.Extended

displayAnsiIfPossible
    :: (HasTerminal env, MonadReader env m, Display a, HasAnsiAnn (Ann a))
    => a -> m T.Text
displayAnsiIfPossible x = do
    useAnsi <- asks getAnsiTerminal
    return $ if useAnsi then displayAnsi x else displayPlain x

-- TODO: switch to using implicit callstacks once 7.8 support is dropped

prettyDebug :: Q Exp
prettyDebug = do
    loc <- location
    [e| monadLoggerLog loc "" LevelDebug <=< displayAnsiIfPossible |]

prettyInfo :: Q Exp
prettyInfo = do
    loc <- location
    [e| monadLoggerLog loc "" LevelInfo <=< displayAnsiIfPossible |]

prettyWarn :: Q Exp
prettyWarn = do
    loc <- location
    [e| monadLoggerLog loc "" LevelWarn <=< displayAnsiIfPossible |]

prettyError :: Q Exp
prettyError = do
    loc <- location
    [e| monadLoggerLog loc "" LevelError <=< displayAnsiIfPossible |]

errorRed :: AnsiDoc -> AnsiDoc
errorRed = dullred

goodGreen :: AnsiDoc -> AnsiDoc
goodGreen = green

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
