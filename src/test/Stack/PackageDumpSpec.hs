{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Stack.PackageDumpSpec where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource (runResourceT)
import Stack.PackageDump
import Stack.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import System.Process.Read
import Control.Monad.Logger
import Distribution.System (buildPlatform)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "eachSection" $ do
        let test name content expected = it name $ do
                actual <- yield content $$ eachSection CL.consume =$ CL.consume
                actual `shouldBe` expected
        test
            "unix line endings"
            "foo\nbar\n---\nbaz---\nbin\n---\n"
            [ ["foo", "bar"]
            , ["baz---", "bin"]
            ]
        test
            "windows line endings"
            "foo\r\nbar\r\n---\r\nbaz---\r\nbin\r\n---\r\n"
            [ ["foo", "bar"]
            , ["baz---", "bin"]
            ]

    it "eachPair" $ do
        let bss =
                [ "key1: val1"
                , "key2: val2a"
                , "      val2b"
                , "key3:"
                , "key4:"
                , "   val4a"
                , "   val4b"
                ]
            sink k = fmap (k, ) CL.consume
        actual <- mapM_ yield bss $$ eachPair sink =$ CL.consume
        actual `shouldBe`
            [ ("key1", ["val1"])
            , ("key2", ["val2a", "val2b"])
            , ("key3", [])
            , ("key4", ["val4a", "val4b"])
            ]

    describe "conduitDumpPackage" $ do
        it "ghc 7.8" $ do
            haskell2010:_ <- runResourceT
                $ CB.sourceFile "test/package-dump/ghc-7.8.txt"
               $$ conduitDumpPackage
               =$ CL.consume
            ghcPkgId <- parseGhcPkgId "haskell2010-1.1.2.0-05c8dd51009e08c6371c82972d40f55a"
            depends <- mapM parseGhcPkgId
                [ "array-0.5.0.0-470385a50d2b78598af85cfe9d988e1b"
                , "base-4.7.0.2-bfd89587617e381ae01b8dd7b6c7f1c1"
                , "ghc-prim-0.3.1.0-a24f9c14c632d75b683d0f93283aea37"
                ]
            haskell2010 `shouldBe` DumpPackage
                { dpGhcPkgId = ghcPkgId
                , dpLibDirs = ["/opt/ghc/7.8.4/lib/ghc-7.8.4/haskell2010-1.1.2.0"]
                , dpDepends = depends
                , dpLibraries = ["HShaskell2010-1.1.2.0"]
                , dpProfiling = ()
                }

        it "ghc 7.10" $ do
            haskell2010:_ <- runResourceT
                $ CB.sourceFile "test/package-dump/ghc-7.10.txt"
               $$ conduitDumpPackage
               =$ CL.consume
            ghcPkgId <- parseGhcPkgId "ghc-7.10.1-325809317787a897b7a97d646ceaa3a3"
            depends <- mapM parseGhcPkgId
                [ "array-0.5.1.0-e29cdbe82692341ebb7ce6e2798294f9"
                , "base-4.8.0.0-1b689eb8d72c4d4cc88f445839c1f01a"
                , "bin-package-db-0.0.0.0-708fc7d634a370b311371a5bcde40b62"
                , "bytestring-0.10.6.0-0909f8f31271f3d75749190bf2ee35db"
                , "containers-0.5.6.2-2114032c163425cc264e6e1169dc2f6d"
                , "directory-1.2.2.0-b4959b472d9eee380c6b32291ade29e0"
                , "filepath-1.4.0.0-40d643aa87258c186441a1f8f3e13ca6"
                , "hoopl-3.10.0.2-8c8dfc4c3140e5f7c982da224c3cb1f0"
                , "hpc-0.6.0.2-ac9064885aa8cb08a93314222939ead4"
                , "process-1.2.3.0-3b1e9bca6ac38225806ff7bbf3f845b1"
                , "template-haskell-2.10.0.0-e895139a0ffff267d412e3d0191ce93b"
                , "time-1.5.0.1-e17a9220d438435579d2914e90774246"
                , "transformers-0.4.2.0-c1a7bb855a176fe475d7b665301cd48f"
                , "unix-2.7.1.0-e5915eb989e568b732bc7286b0d0817f"
                ]
            haskell2010 `shouldBe` DumpPackage
                { dpGhcPkgId = ghcPkgId
                , dpLibDirs = ["/opt/ghc/7.10.1/lib/ghc-7.10.1/ghc_EMlWrQ42XY0BNVbSrKixqY"]
                , dpDepends = depends
                , dpLibraries = ["HSghc-7.10.1-EMlWrQ42XY0BNVbSrKixqY"]
                , dpProfiling = ()
                }

    it "ghcPkgDump + addProfiling" $ (id :: IO () -> IO ()) $ runNoLoggingT $ do
        menv' <- getEnvOverride buildPlatform
        menv <- mkEnvOverride buildPlatform $ Map.delete "GHC_PACKAGE_PATH" $ unEnvOverride menv'
        pcache <- newProfilingCache
        ghcPkgDump menv Nothing
            $  conduitDumpPackage
            =$ addProfiling pcache
            =$ CL.sinkNull

    it "sinkMatching" $ do
        menv' <- getEnvOverride buildPlatform
        menv <- mkEnvOverride buildPlatform $ Map.delete "GHC_PACKAGE_PATH" $ unEnvOverride menv'
        pcache <- newProfilingCache
        m <- runNoLoggingT $ ghcPkgDump menv Nothing
            $  conduitDumpPackage
            =$ addProfiling pcache
            =$ sinkMatching False (Map.singleton $(mkPackageName "transformers") $(mkVersion "0.0.0.0.0.0.1"))
        case Map.lookup $(mkPackageName "base") m of
            Nothing -> error "base not present"
            Just _ -> return ()
        Map.lookup $(mkPackageName "transformers") m `shouldBe` Nothing
        Map.lookup $(mkPackageName "ghc") m `shouldBe` Nothing

    describe "pruneDeps" $ do
        it "sanity check" $ do
            let prunes =
                    [ ((1, 'a'), [])
                    , ((1, 'b'), [])
                    , ((2, 'a'), [(1, 'b')])
                    , ((2, 'b'), [(1, 'a')])
                    , ((3, 'a'), [(1, 'c')])
                    , ((4, 'a'), [(2, 'a')])
                    ]
                actual = fmap fst $ pruneDeps fst fst snd bestPrune prunes
            actual `shouldBe` Map.fromList
                [ (1, (1, 'b'))
                , (2, (2, 'a'))
                , (4, (4, 'a'))
                ]

        prop "invariant holds" $ \prunes ->
            checkDepsPresent prunes $ fmap fst $ pruneDeps fst fst snd bestPrune prunes

type PruneCheck = ((Int, Char), [(Int, Char)])

bestPrune :: PruneCheck -> PruneCheck -> PruneCheck
bestPrune x y
    | fst x > fst y = x
    | otherwise = y

checkDepsPresent :: [PruneCheck] -> Map Int (Int, Char) -> Bool
checkDepsPresent prunes selected =
    all hasDeps $ Set.toList allIds
  where
    depMap = Map.fromList prunes
    allIds = Set.fromList $ Map.elems selected

    hasDeps ident =
        case Map.lookup ident depMap of
            Nothing -> error "checkDepsPresent: missing in depMap"
            Just deps -> Set.null $ Set.difference (Set.fromList deps) allIds
