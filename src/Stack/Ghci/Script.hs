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
    -- ^ Add the specified modules (specified by module name or source file) to
    -- the current target set and perform a reload (that is, load the target set
    -- of modules and the all the modules they depend on in dependency order).
    -- The modules specified by name are added first (in ascending order), then
    -- the modules specified by source file (in ascending order). The context is
    -- set to the most recently successfully loaded module.
  | ModuleCmd (Set ModuleName)
    -- ^ Add the specified modules to the context. The modules are added in
    -- ascending order.
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
