{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.SourceMapSpec (spec) where

import Distribution.Types.PackageName (mkPackageName)
import Distribution.Version (mkVersion)
import Stack.Prelude
import Stack.SourceMap (loadGlobalHints)
import Stack.Types.Compiler (ActualCompiler(..))
import Stack.Types.Runner (withRunner, ColorWhen (ColorNever))
import Test.Hspec
import qualified RIO.Map as Map
import RIO.ByteString (hPut)
import Path.IO (resolveFile')

spec :: Spec
spec = do
  describe "loadGlobalHints" $ do
    let it' name inner = it name $ withSystemTempFile "global-hints.yaml" $ \fp h -> do
          hPut h "this should be ignored"
          hClose h :: IO ()
          abs' <- resolveFile' fp
          withRunner LevelError False False ColorNever mempty Nothing False $ \runner ->
            runRIO runner $ inner abs'
    it' "unknown compiler" $ \fp -> do
      mmap <- loadGlobalHints fp $ ACGhc (mkVersion [0, 0, 0, 0, 0, 0, 0])
      liftIO $ mmap `shouldBe` Nothing
    it' "known compiler" $ \fp -> do
      mmap <- loadGlobalHints fp $ ACGhc (mkVersion [8, 4, 3])
      case mmap of
        Nothing -> error "not found"
        Just m -> liftIO $ do
          Map.lookup (mkPackageName "ghc") m `shouldBe` Just (mkVersion [8, 4, 3])
          Map.lookup (mkPackageName "base") m `shouldBe` Just (mkVersion [4, 11, 1, 0])
          Map.lookup (mkPackageName "bytestring") m `shouldBe` Just (mkVersion [0, 10, 8, 2])
          Map.lookup (mkPackageName "acme-missiles") m `shouldBe` Nothing
    it' "older known compiler" $ \fp -> do
      mmap <- loadGlobalHints fp $ ACGhc (mkVersion [7, 8, 4])
      case mmap of
        Nothing -> error "not found"
        Just m -> liftIO $ do
          Map.lookup (mkPackageName "ghc") m `shouldBe` Just (mkVersion [7, 8, 4])
          Map.lookup (mkPackageName "base") m `shouldBe` Just (mkVersion [4, 7, 0, 2])
          Map.lookup (mkPackageName "Cabal") m `shouldBe` Just (mkVersion [1, 18, 1, 5])
          Map.lookup (mkPackageName "acme-missiles") m `shouldBe` Nothing
