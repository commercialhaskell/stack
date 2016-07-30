{-# LANGUAGE OverloadedStrings #-}

module Stack.Ghci.Script
  ( GhciScript
  , GhciCommand (..)
  , emptyScript

  , appendCommand

  , scriptToText
  , scriptToLazyText
  , scriptToBuilder
  ) where

import           Data.Monoid
import           Data.List
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Path

newtype GhciScript = GhciScript { unGhciScript :: [GhciCommand] }

emptyScript :: GhciScript
emptyScript = GhciScript []

type ModuleName = Text

data GhciCommand
  = Add (Vector ModuleName)
  | CdGhc (Path Abs Dir)
  | Module (Vector ModuleName)
  deriving (Show)

appendCommand :: GhciCommand -> GhciScript -> GhciScript
appendCommand cmd (GhciScript backwardScript) = GhciScript (cmd:backwardScript)

scriptToText :: GhciScript -> Text
scriptToText = LT.toStrict . scriptToLazyText

scriptToLazyText :: GhciScript -> LT.Text
scriptToLazyText = toLazyText . scriptToBuilder

scriptToBuilder :: GhciScript -> Builder
scriptToBuilder backwardScript = mconcat $ fmap commandToBuilder script
  where
    script = reverse $ unGhciScript backwardScript

-- Command conversion

commandToBuilder :: GhciCommand -> Builder

commandToBuilder (Add modules)
  | V.null modules = mempty
  | otherwise      =
       fromText ":add "
    <> (mconcat $ intersperse (singleton ' ') $ V.toList $ fmap fromText modules)
    <> singleton '\n'

commandToBuilder (CdGhc path) =
  fromText ":cd-ghc " <> fromString (toFilePath path)

commandToBuilder (Module modules)
  | V.null modules = fromText ":module +"
  | otherwise      =
       fromText ":module + "
    <> (mconcat $ intersperse (singleton ' ') $ V.toList $ fmap fromText modules)
    <> singleton '\n'
