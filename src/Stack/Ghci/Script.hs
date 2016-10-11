{-# LANGUAGE OverloadedStrings #-}

module Stack.Ghci.Script
  ( GhciScript
  , ModuleName

  , cmdAdd
  , cmdAddFile
  , cmdCdGhc
  , cmdModule

  , scriptToLazyByteString
  , scriptToBuilder
  , scriptToFile
  ) where

import           Control.Applicative
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8Builder)
import           Path
import           Prelude -- Fix redundant imports warnings
import           System.IO

import           Distribution.ModuleName hiding (toFilePath)

newtype GhciScript = GhciScript { unGhciScript :: [GhciCommand] }

instance Monoid GhciScript where
  mempty = GhciScript []
  (GhciScript xs) `mappend` (GhciScript ys) = GhciScript (ys <> xs)

data GhciCommand
  = Add (Set ModuleName)
  | AddFile (Path Abs File)
  | CdGhc (Path Abs Dir)
  | Module (Set ModuleName)
  deriving (Show)

cmdAdd :: Set ModuleName -> GhciScript
cmdAdd = GhciScript . (:[]) . Add

cmdAddFile :: Path Abs File -> GhciScript
cmdAddFile = GhciScript . (:[]) . AddFile

cmdCdGhc :: Path Abs Dir -> GhciScript
cmdCdGhc = GhciScript . (:[]) . CdGhc

cmdModule :: Set ModuleName -> GhciScript
cmdModule = GhciScript . (:[]) . Module

scriptToLazyByteString :: GhciScript -> ByteString
scriptToLazyByteString = toLazyByteString . scriptToBuilder

scriptToBuilder :: GhciScript -> Builder
scriptToBuilder backwardScript = mconcat $ fmap commandToBuilder script
  where
    script = reverse $ unGhciScript backwardScript

scriptToFile :: Path Abs File -> GhciScript -> IO ()
scriptToFile path script =
  withFile filepath WriteMode
    $ \hdl -> do hSetBuffering hdl (BlockBuffering Nothing)
                 hSetBinaryMode hdl True
                 hPutBuilder hdl (scriptToBuilder script)
  where
    filepath = toFilePath path

-- Command conversion

fromText :: Text -> Builder
fromText = encodeUtf8Builder

commandToBuilder :: GhciCommand -> Builder

commandToBuilder (Add modules)
  | S.null modules = mempty
  | otherwise      =
       fromText ":add "
    <> mconcat (intersperse (fromText " ")
        $ (stringUtf8 . quoteFileName . mconcat . intersperse "." . components) <$> S.toAscList modules)
    <> fromText "\n"

commandToBuilder (AddFile path) =
  fromText ":add " <> stringUtf8 (quoteFileName (toFilePath path)) <> fromText "\n"

commandToBuilder (CdGhc path) =
  fromText ":cd-ghc " <> stringUtf8 (quoteFileName (toFilePath path)) <> fromText "\n"

commandToBuilder (Module modules)
  | S.null modules = fromText ":module +\n"
  | otherwise      =
       fromText ":module + "
    <> mconcat (intersperse (fromText " ")
        $ (stringUtf8 . quoteFileName . mconcat . intersperse "." . components) <$> S.toAscList modules)
    <> fromText "\n"

-- | Make sure that a filename with spaces in it gets the proper quotes.
quoteFileName :: String -> String
quoteFileName x = if ' ' `elem` x then show x else x
