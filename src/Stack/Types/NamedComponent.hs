{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.NamedComponent
  ( NamedComponent (..)
  , renderComponent
  , renderPkgComponents
  , renderPkgComponent
  , exeComponents
  , testComponents
  , benchComponents
  , internalLibComponents
  , isCLib
  , isCInternalLib
  , isCExe
  , isCTest
  , isCBench
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import           Pantry
import           Stack.Prelude

-- | A single, fully resolved component of a package
data NamedComponent
  = CLib
  | CInternalLib !Text
  | CExe !Text
  | CTest !Text
  | CBench !Text
  deriving (Eq, Ord, Show)

renderComponent :: NamedComponent -> Text
renderComponent CLib = "lib"
renderComponent (CInternalLib x) = "internal-lib:" <> x
renderComponent (CExe x) = "exe:" <> x
renderComponent (CTest x) = "test:" <> x
renderComponent (CBench x) = "bench:" <> x

renderPkgComponents :: [(PackageName, NamedComponent)] -> Text
renderPkgComponents = T.intercalate " " . map renderPkgComponent

renderPkgComponent :: (PackageName, NamedComponent) -> Text
renderPkgComponent (pkg, comp) =
  fromString (packageNameString pkg) <> ":" <> renderComponent comp

exeComponents :: Set NamedComponent -> Set Text
exeComponents = Set.fromList . mapMaybe mExeName . Set.toList
 where
  mExeName (CExe name) = Just name
  mExeName _ = Nothing

testComponents :: Set NamedComponent -> Set Text
testComponents = Set.fromList . mapMaybe mTestName . Set.toList
 where
  mTestName (CTest name) = Just name
  mTestName _ = Nothing

benchComponents :: Set NamedComponent -> Set Text
benchComponents = Set.fromList . mapMaybe mBenchName . Set.toList
 where
  mBenchName (CBench name) = Just name
  mBenchName _ = Nothing

internalLibComponents :: Set NamedComponent -> Set Text
internalLibComponents = Set.fromList . mapMaybe mInternalName . Set.toList
 where
  mInternalName (CInternalLib name) = Just name
  mInternalName _ = Nothing

isCLib :: NamedComponent -> Bool
isCLib CLib{} = True
isCLib _ = False

isCInternalLib :: NamedComponent -> Bool
isCInternalLib CInternalLib{} = True
isCInternalLib _ = False

isCExe :: NamedComponent -> Bool
isCExe CExe{} = True
isCExe _ = False

isCTest :: NamedComponent -> Bool
isCTest CTest{} = True
isCTest _ = False

isCBench :: NamedComponent -> Bool
isCBench CBench{} = True
isCBench _ = False
