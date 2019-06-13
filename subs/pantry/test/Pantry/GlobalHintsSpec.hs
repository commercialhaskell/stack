{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.GlobalHintsSpec (spec) where

import Distribution.Types.PackageName (mkPackageName)
import Distribution.Version (mkVersion)
import RIO
import Pantry (loadGlobalHints, WantedCompiler (..), runPantryAppClean)
import Pantry.Internal
import Test.Hspec
import qualified RIO.Map as Map
import Path (toFilePath)

spec :: Spec
spec = do
    let it' name inner = it name $ example $ runPantryAppClean $ do
          file <- getGlobalHintsFile
          writeFileBinary (toFilePath file) "this should be ignored"
          inner
    it' "unknown compiler" $ do
      mmap <- loadGlobalHints $ WCGhc (mkVersion [0, 0, 0, 0, 0, 0, 0])
      liftIO $ mmap `shouldBe` Nothing
    it' "known compiler" $ do
      mmap <- loadGlobalHints $ WCGhc (mkVersion [8, 4, 3])
      case mmap of
        Nothing -> error "not found"
        Just m -> liftIO $ do
          Map.lookup (mkPackageName "ghc") m `shouldBe` Just (mkVersion [8, 4, 3])
          Map.lookup (mkPackageName "base") m `shouldBe` Just (mkVersion [4, 11, 1, 0])
          Map.lookup (mkPackageName "bytestring") m `shouldBe` Just (mkVersion [0, 10, 8, 2])
          Map.lookup (mkPackageName "acme-missiles") m `shouldBe` Nothing
    it' "older known compiler" $ do
      mmap <- loadGlobalHints $ WCGhc (mkVersion [7, 8, 4])
      case mmap of
        Nothing -> error "not found"
        Just m -> liftIO $ do
          Map.lookup (mkPackageName "ghc") m `shouldBe` Just (mkVersion [7, 8, 4])
          Map.lookup (mkPackageName "base") m `shouldBe` Just (mkVersion [4, 7, 0, 2])
          Map.lookup (mkPackageName "Cabal") m `shouldBe` Just (mkVersion [1, 18, 1, 5])
          Map.lookup (mkPackageName "acme-missiles") m `shouldBe` Nothing
