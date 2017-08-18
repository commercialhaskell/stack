{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Ghci.Script
  ( GhciScript
  , ModuleName

  , cmdAdd
  , cmdCdGhc
  , cmdModule

  , scriptToLazyByteString
  , scriptToBuilder
  , scriptToFile
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Builder
import           Data.List
import qualified Data.Set as S
import           Data.Text.Encoding (encodeUtf8Builder)
import           Path
import           Stack.Prelude hiding (ByteString)
import           System.IO (hSetBuffering, BufferMode (..), hSetBinaryMode)

import           Distribution.ModuleName hiding (toFilePath)

newtype GhciScript = GhciScript { unGhciScript :: [GhciCommand] }

instance Monoid GhciScript where
  mempty = GhciScript []
  (GhciScript xs) `mappend` (GhciScript ys) = GhciScript (ys <> xs)

data GhciCommand
  = Add (Set (Either ModuleName (Path Abs File)))
  | CdGhc (Path Abs Dir)
  | Module (Set ModuleName)
  deriving (Show)

cmdAdd :: Set (Either ModuleName (Path Abs File)) -> GhciScript
cmdAdd = GhciScript . (:[]) . Add

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
    <> mconcat (intersperse (fromText " ") $
         fmap (stringUtf8 . quoteFileName . either (mconcat . intersperse "." . components) toFilePath)
              (S.toAscList modules))
    <> fromText "\n"

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
