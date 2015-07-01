{-# LANGUAGE OverloadedStrings #-}
-- | Test suite for Stack.Dot
module Stack.DotSpec where

import           Data.ByteString.Char8 (ByteString)
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Options.Applicative (execParserPure,idm,prefs,info,getParseResult)
import           Stack.Types
import           Test.Hspec

import           Stack.Dot

spec :: Spec
spec = do
  let graph =
         Map.mapKeys pkgName
       . fmap (Set.map pkgName)
       . Map.fromList $ [("one",Set.fromList ["base","free"])
                        ,("two",Set.fromList ["base","free","mtl","transformers","one"])
                        ]
  describe "Stack.Dot" $ do
    it "does nothing if depth is 0" $
      resolveDependencies (Just 0) graph stubLoader `shouldBe` return graph

    it "with depth 1, more dependencies are resolved" $ do
      let graph' = Map.insert (pkgName "cycle") (Set.singleton (pkgName "cycle")) graph
          resultGraph = runIdentity (resolveDependencies (Just 0) graph stubLoader)
          resultGraph' = runIdentity (resolveDependencies (Just 1) graph' stubLoader)
      Map.size resultGraph < Map.size resultGraph' `shouldBe` True

    it "cycles are ignored" $ do
       let graph' = Map.insert (pkgName "cycle") (Set.singleton (pkgName "cycle")) graph
           resultGraph = resolveDependencies Nothing graph stubLoader
           resultGraph' = resolveDependencies Nothing graph' stubLoader
       fmap Map.size resultGraph' `shouldBe` fmap ((+1) . Map.size) resultGraph

  where graphElem e graph = Set.member e . Set.unions . Map.elems $ graph

{- Helper functions below -}

-- Unsafe internal helper to create a package name
pkgName :: ByteString -> PackageName
pkgName = fromMaybe failure . parsePackageName
  where
   failure = (error "Internal error during package name creation in DotSpec.pkgName")

-- Stub, simulates the function to load package dependecies
stubLoader :: PackageName -> Identity (Set PackageName)
stubLoader name = return . Set.fromList . map pkgName $ case show name of
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
