{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Ghci.Script
License     : BSD-3-Clause
-}

module Stack.Ghci.Script
  ( GhciScript
  , ModuleName
  , cmdAdd
  , cmdModule
  , scriptToLazyByteString
  , scriptToBuilder
  , scriptToFile
  ) where

import           Data.ByteString.Builder ( toLazyByteString )
import qualified Data.List as L
import qualified Data.Set as S
import           Distribution.ModuleName ( ModuleName, components )
import           Stack.Prelude
import           System.IO ( hSetBinaryMode )

newtype GhciScript = GhciScript { ghciScript :: [GhciCommand] }

instance Semigroup GhciScript where
  GhciScript xs <> GhciScript ys = GhciScript (ys <> xs)

instance Monoid GhciScript where
  mempty = GhciScript []
  mappend = (<>)

data GhciCommand
  = AddCmd (Set (Either ModuleName (Path Abs File)))
  | ModuleCmd (Set ModuleName)
  deriving Show

cmdAdd :: Set (Either ModuleName (Path Abs File)) -> GhciScript
cmdAdd = GhciScript . (:[]) . AddCmd

cmdModule :: Set ModuleName -> GhciScript
cmdModule = GhciScript . (:[]) . ModuleCmd

scriptToLazyByteString :: GhciScript -> LByteString
scriptToLazyByteString = toLazyByteString . scriptToBuilder

scriptToBuilder :: GhciScript -> Builder
scriptToBuilder backwardScript = mconcat $ fmap commandToBuilder script
 where
  script = reverse backwardScript.ghciScript

scriptToFile :: Path Abs File -> GhciScript -> IO ()
scriptToFile path script =
  withFile filepath WriteMode
    $ \hdl -> do hSetBuffering hdl (BlockBuffering Nothing)
                 hSetBinaryMode hdl True
                 hPutBuilder hdl (scriptToBuilder script)
 where
  filepath = toFilePath path

-- Command conversion

commandToBuilder :: GhciCommand -> Builder

commandToBuilder (AddCmd modules)
  | S.null modules = mempty
  | otherwise      =
       ":add "
    <> mconcat
         ( L.intersperse " "
             $ fmap
                 ( fromString
                 . quoteFileName
                 . either (mconcat . L.intersperse "." . components) toFilePath
                 )
                 (S.toAscList modules)
         )
    <> "\n"

commandToBuilder (ModuleCmd modules)
  | S.null modules = ":module +\n"
  | otherwise      =
       ":module + "
    <> mconcat
         ( L.intersperse " "
             $ fromString
             . quoteFileName
             . mconcat
             . L.intersperse "."
             . components <$> S.toAscList modules
         )
    <> "\n"

-- | Make sure that a filename with spaces in it gets the proper quotes.
quoteFileName :: String -> String
quoteFileName x = if ' ' `elem` x then show x else x
