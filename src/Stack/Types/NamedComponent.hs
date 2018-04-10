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
  , isCLib
  , isCExe
  , isCTest
  , isCBench
  ) where

import Stack.Prelude
import Stack.Types.PackageName
import qualified Data.Set as Set
import qualified Data.Text as T

-- | A single, fully resolved component of a package
data NamedComponent
    = CLib
    | CExe !Text
    | CTest !Text
    | CBench !Text
    deriving (Show, Eq, Ord)

renderComponent :: NamedComponent -> Text
renderComponent CLib = "lib"
renderComponent (CExe x) = "exe:" <> x
renderComponent (CTest x) = "test:" <> x
renderComponent (CBench x) = "bench:" <> x

renderPkgComponents :: [(PackageName, NamedComponent)] -> Text
renderPkgComponents = T.intercalate " " . map renderPkgComponent

renderPkgComponent :: (PackageName, NamedComponent) -> Text
renderPkgComponent (pkg, comp) = packageNameText pkg <> ":" <> renderComponent comp

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

isCLib :: NamedComponent -> Bool
isCLib CLib{} = True
isCLib _ = False

isCExe :: NamedComponent -> Bool
isCExe CExe{} = True
isCExe _ = False

isCTest :: NamedComponent -> Bool
isCTest CTest{} = True
isCTest _ = False

isCBench :: NamedComponent -> Bool
isCBench CBench{} = True
isCBench _ = False
