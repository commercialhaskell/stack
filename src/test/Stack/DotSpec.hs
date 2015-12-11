{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Test suite for Stack.Dot
module Stack.DotSpec where

import           Control.Monad (filterM)
import           Data.Foldable as F
import           Data.Functor.Identity
import           Data.List ((\\))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Stack.Types
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (forAll,choose,Gen)

import           Stack.Dot

dummyVersion :: Version
dummyVersion = fromMaybe (error "dotspec: parser error") (parseVersionFromString "0.0.0.0")

spec :: Spec
spec = do
  let graph =
         Map.mapKeys pkgName
       . fmap (\p -> (Set.map pkgName p, Just dummyVersion))
       . Map.fromList $ [("one",Set.fromList ["base","free"])
                        ,("two",Set.fromList ["base","free","mtl","transformers","one"])
                        ]
  describe "Stack.Dot" $ do
    it "does nothing if depth is 0" $
      resolveDependencies (Just 0) graph stubLoader `shouldBe` return graph

    it "with depth 1, more dependencies are resolved" $ do
      let graph' = Map.insert (pkgName "cycle")
                              (Set.singleton (pkgName "cycle"), Just dummyVersion)
                              graph
          resultGraph = runIdentity (resolveDependencies (Just 0) graph stubLoader)
          resultGraph' = runIdentity (resolveDependencies (Just 1) graph' stubLoader)
      Map.size resultGraph < Map.size resultGraph' `shouldBe` True

    it "cycles are ignored" $ do
       let graph' = Map.insert (pkgName "cycle")
                               (Set.singleton (pkgName "cycle"), Just dummyVersion)
                                graph
           resultGraph = resolveDependencies Nothing graph stubLoader
           resultGraph' = resolveDependencies Nothing graph' stubLoader
       fmap Map.size resultGraph' `shouldBe` fmap ((+1) . Map.size) resultGraph

    let graphElem e = Set.member e . Set.unions . Map.elems

    prop "requested packages are pruned" $ do
      let resolvedGraph = runIdentity (resolveDependencies Nothing graph stubLoader)
          allPackages g = Set.map show (Map.keysSet g `Set.union`  F.fold (fmap fst g))
      forAll (sublistOf (Set.toList (allPackages resolvedGraph))) $ \toPrune ->
        let pruned = pruneGraph [pkgName "one", pkgName "two"] toPrune resolvedGraph
        in Set.null (allPackages pruned `Set.intersection` Set.fromList toPrune)

    prop "pruning removes orhpans" $ do
      let resolvedGraph = runIdentity (resolveDependencies Nothing graph stubLoader)
          allPackages g = Set.map show (Map.keysSet g `Set.union` F.fold (fmap fst g))
          orphans g = Map.filterWithKey (\k _ -> not (graphElem k g)) g
      forAll (sublistOf (Set.toList (allPackages resolvedGraph))) $ \toPrune ->
        let pruned = pruneGraph [pkgName "one", pkgName "two"] toPrune resolvedGraph
        in null (Map.keys (orphans (fmap fst pruned)) \\ [pkgName "one", pkgName "two"])

{- Helper functions below -}
-- Backport from QuickCheck 2.8 to 2.7.6
sublistOf :: [a] -> Gen [a]
sublistOf = filterM (\_ -> choose (False, True))

-- Unsafe internal helper to create a package name
pkgName :: Text -> PackageName
pkgName = fromMaybe failure . parsePackageName
  where
   failure = error "Internal error during package name creation in DotSpec.pkgName"

-- Stub, simulates the function to load package dependecies
stubLoader :: PackageName -> Identity (Set PackageName, Maybe Version)
stubLoader name = return . (, Just dummyVersion) . Set.fromList . map pkgName $ case show name of
  "StateVar" -> ["stm","transformers"]
  "array" -> []
  "bifunctors" -> ["semigroupoids","semigroups","tagged"]
  "binary" -> ["array","bytestring","containers"]
  "bytestring" -> ["deepseq","ghc-prim","integer-gmp"]
  "comonad" -> ["containers","contravariant","distributive"
               ,"semigroups","tagged","transformers","transformers-compat"
               ]
  "cont" -> ["StateVar","semigroups","transformers","transformers-compat","void"]
  "containers" -> ["array","deepseq","ghc-prim"]
  "deepseq" -> ["array"]
  "distributive" -> ["ghc-prim","tagged","transformers","transformers-compat"]
  "free" -> ["bifunctors","comonad","distributive","mtl"
            ,"prelude-extras","profunctors","semigroupoids"
            ,"semigroups","template-haskell","transformers"
            ]
  "ghc" -> []
  "hashable" -> ["bytestring","ghc-prim","integer-gmp","text"]
  "integer" -> []
  "mtl" -> ["transformers"]
  "nats" -> []
  "one" -> ["free"]
  "prelude" -> []
  "profunctors" -> ["comonad","distributive","semigroupoids","tagged","transformers"]
  "semigroupoids" -> ["comonad","containers","contravariant","distributive"
                     ,"semigroups","transformers","transformers-compat"
                     ]
  "semigroups" -> ["bytestring","containers","deepseq","hashable"
                  ,"nats","text","unordered-containers"
                  ]
  "stm" -> ["array"]
  "tagged" -> ["template-haskell"]
  "template" -> []
  "text" -> ["array","binary","bytestring","deepseq","ghc-prim","integer-gmp"]
  "transformers" -> []
  "two" -> ["free","mtl","one","transformers"]
  "unordered" -> ["deepseq","hashable"]
  "void" -> ["ghc-prim","hashable","semigroups"]
  _ -> []
