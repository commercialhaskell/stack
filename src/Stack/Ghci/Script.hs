{-# LANGUAGE OverloadedStrings #-}

module Stack.Ghci.Script
  ( GhciScript
  , ModuleName

  , cmdAdd
  , cmdCdGhc
  , cmdModule

  , scriptToText
  , scriptToLazyText
  , scriptToBuilder
  ) where

import           Data.Monoid
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT hiding (singleton)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as LT
import           Path

import           Distribution.ModuleName hiding (toFilePath)

newtype GhciScript = GhciScript { unGhciScript :: [GhciCommand] }

instance Monoid GhciScript where
  mempty = GhciScript []
  (GhciScript xs) `mappend` (GhciScript ys) = GhciScript (ys <> xs)

data GhciCommand
  = Add (Set ModuleName)
  | CdGhc (Path Abs Dir)
  | Module (Set ModuleName)
  deriving (Show)

cmdAdd :: Set ModuleName -> GhciScript
cmdAdd = GhciScript . (:[]) . Add

cmdCdGhc :: Path Abs Dir -> GhciScript
cmdCdGhc = GhciScript . (:[]) . CdGhc

cmdModule :: Set ModuleName -> GhciScript
cmdModule = GhciScript . (:[]) . Module

scriptToText :: GhciScript -> Text
scriptToText = LT.toStrict . scriptToLazyText

scriptToLazyText :: GhciScript -> LT.Text
scriptToLazyText = LT.toLazyText . scriptToBuilder

scriptToBuilder :: GhciScript -> Builder
scriptToBuilder backwardScript = mconcat $ fmap commandToBuilder script
  where
    script = reverse $ unGhciScript backwardScript

-- Command conversion

commandToBuilder :: GhciCommand -> Builder

commandToBuilder (Add modules)
  | S.null modules = mempty
  | otherwise      =
       LT.fromText ":add "
    <> (mconcat $ intersperse (LT.singleton ' ')
        $ fmap (LT.fromString . mconcat . intersperse "." . components)
        $ S.toAscList modules)
    <> LT.singleton '\n'

commandToBuilder (CdGhc path) =
  LT.fromText ":cd-ghc " <> LT.fromString (toFilePath path) <> LT.singleton '\n'

commandToBuilder (Module modules)
  | S.null modules = LT.fromText ":module +\n"
  | otherwise      =
       LT.fromText ":module + "
    <> (mconcat $ intersperse (LT.singleton ' ')
        $ fmap (LT.fromString . mconcat . intersperse "." . components)
        $ S.toAscList modules)
    <> LT.singleton '\n'
