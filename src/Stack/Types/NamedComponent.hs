{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.Types.NamedComponent
  ( NamedComponent (..)
  , ComponentMap(..)
  , ComponentMapName
  , ComponentBuildInfo(..)
  , fromComponentList
  , toComponentNameList
  , isNullComponentMap
  , intersectComponentMap
  , intersectComponentMapFull
  , excludeComponentMap
  , componentMapAndPackagePPrint
  , fromNamedComponent
  , toNamedComponent
  , toComponentName
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
  , naiveExecutionOrdering
  , configureComponentFlag
  ) where

import Pantry
import Stack.Prelude
import qualified Data.Set as Set
import qualified Data.Text as T
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName, UnqualComponentName)
import Distribution.Types.LibraryName (LibraryName(..))
import Distribution.Simple.LocalBuildInfo (componentBuildInfo, Component, ComponentName(..))
import qualified Data.Map.Strict as Map
import Distribution.PackageDescription
    ( Library(libName),
      Executable(exeName),
      TestSuite(testName),
      Benchmark(benchmarkName),
      BuildInfo(targetBuildDepends) )
import Distribution.Types.ExeDependency (ExeDependency)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Mixin (Mixin)
import qualified Distribution.Types.Component as CabalComp
import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.Types.ForeignLib (foreignLibBuildInfo, ForeignLib(foreignLibName))

-- | A single, fully resolved component of a package
data NamedComponent
    = CLib
    | CInternalLib !Text
    | CExe !Text
    | CTest !Text
    | CBench !Text
    deriving (Show, Eq, Ord)

renderComponent :: NamedComponent -> Text
renderComponent CLib = "lib"
renderComponent (CInternalLib x) = "internal-lib:" <> x
renderComponent (CExe x) = "exe:" <> x
renderComponent (CTest x) = "test:" <> x
renderComponent (CBench x) = "bench:" <> x

configureComponentFlag :: PackageName -> Maybe NamedComponent -> [String]
configureComponentFlag _ Nothing = mempty
configureComponentFlag pn (Just np) = case np of
    CLib -> ["lib:" <> (show . unPackageName $ pn)]
    (CInternalLib x) -> ["lib:" <> show x]
    (CExe x) -> ["exe:" <> show x]
    (CTest x) -> ["test:" <> show x]
    (CBench x) -> ["bench:" <> show x]

naiveExecutionOrdering :: NamedComponent -> Int
naiveExecutionOrdering CLib = 1
naiveExecutionOrdering (CInternalLib _) = 2
naiveExecutionOrdering (CExe _) = 3
naiveExecutionOrdering _ = 4        

renderPkgComponents :: [(PackageName, NamedComponent)] -> Text
renderPkgComponents = T.intercalate " " . map renderPkgComponent

renderPkgComponent :: (PackageName, NamedComponent) -> Text
renderPkgComponent (pkg, comp) = fromString (packageNameString pkg) <> ":" <> renderComponent comp

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
textNameToCabalName :: Text -> UnqualComponentName
textNameToCabalName = mkUnqualComponentName . show

-- | This is a simplified equivalent of the BuildInfo Cabal type.
data ComponentBuildInfo = ComponentBuildInfo {
  -- | This is from Cabal's BuildInfo type, targetBuildDepends field.
  cbiDependencyList :: [Dependency],
  -- | This is from Cabal's BuildInfo type, buildToolDepends field.
  -- THough this is not directly derived (see the cabal haddock comment).
  cbiExeDependencyList :: [ExeDependency]
} deriving Show

-- | This is used to replace @Set NamedComponent@ in certain situations
-- to simplify target shrinking (that is, reducing the component to build
-- to the ones specified by the user, explicitely or implicitely).
-- All the keys here are component names.
data ComponentMap contentType = ComponentMap {
  exeComp :: Map UnqualComponentName contentType,
  libComp :: Map LibraryName contentType,
  foreignLibComp :: Map UnqualComponentName contentType,
  benchComp :: Map UnqualComponentName contentType,
  testComp :: Map UnqualComponentName contentType
} deriving (Show, Eq, Ord)
type ComponentMapName = ComponentMap ()
instance Semigroup (ComponentMap contentType) where
  (<>) a b = ComponentMap {
    exeComp = exeComp a <> exeComp b,
    libComp = libComp a <> libComp b,
    foreignLibComp = foreignLibComp a <> foreignLibComp b,
    benchComp = benchComp a <> benchComp b,
    testComp = testComp a <> testComp b
    }
instance Monoid (ComponentMap contentType) where
  mempty = ComponentMap {
    exeComp = mempty,
    libComp = mempty,
    foreignLibComp = mempty,
    benchComp = mempty,
    testComp = mempty
    }

isNullComponentMap :: ComponentMap contentType -> Bool 
isNullComponentMap compMap = null (exeComp compMap)
  && null (libComp compMap)
  && null (foreignLibComp compMap)
  && null (benchComp compMap)
  && null (testComp compMap)

-- | Only keep the components where names (keys) are present in both arguments (a and b here).
intersectComponentMap :: Bool -> ComponentMap contentA -> ComponentMap contentB -> ComponentMap contentA
intersectComponentMap intersectForeign a b = ComponentMap {
    exeComp = Map.intersection (exeComp a) (exeComp b),
    libComp = Map.intersection (libComp a) (libComp b),
    foreignLibComp = if intersectForeign then
      Map.intersection (foreignLibComp a) (foreignLibComp b)
      else foreignLibComp a,
    benchComp = Map.intersection (benchComp a) (benchComp b),
    testComp = Map.intersection (testComp a) (testComp b)
    }
-- | Remove the components in a where names (keys) are present in both arguments (a and b here).
excludeComponentMap :: ComponentMap contentA -> ComponentMap contentB -> ComponentMap contentA
excludeComponentMap a b = ComponentMap {
    exeComp = Map.difference (exeComp a) (exeComp b),
    libComp = Map.difference (libComp a) (libComp b),
    foreignLibComp =
      Map.difference (foreignLibComp a) (foreignLibComp b),
    benchComp = Map.difference (benchComp a) (benchComp b),
    testComp = Map.difference (testComp a) (testComp b)
    }

intersectComponentMapFull :: ComponentMap contentA -> ComponentMap contentB -> ComponentMap contentA
intersectComponentMapFull = intersectComponentMap True

-- | This is the main way to get the component map from Cabal.
fromComponentList :: (BuildInfo -> [ExeDependency]) -> [Component] -> ComponentMap ComponentBuildInfo
fromComponentList trans compList = foldMap insertComponent compList
  where
    insertComponent c = case c of
      CabalComp.CLib lib -> mempty{libComp = Map.singleton (libName lib) value}
      CabalComp.CFLib flib -> mempty{foreignLibComp = Map.singleton (foreignLibName flib) value}
      CabalComp.CExe exe -> mempty{foreignLibComp = Map.singleton (exeName exe) value}
      CabalComp.CTest test -> mempty{foreignLibComp = Map.singleton (testName test) value}
      CabalComp.CBench bench -> mempty{foreignLibComp = Map.singleton (benchmarkName bench) value}
      where
        value = cabalBuildInfoToCBI cbuildInfo
        cbuildInfo = componentBuildInfo c
    cabalBuildInfoToCBI bi = ComponentBuildInfo {
        cbiDependencyList = targetBuildDepends bi,
        cbiExeDependencyList = trans bi
      }
    

fromNamedComponent :: Set NamedComponent -> ComponentMapName
fromNamedComponent input = Set.foldr' (insertNamedComponent ()) mempty input

insertNamedComponent :: contentType -> NamedComponent -> (ComponentMap contentType) -> (ComponentMap contentType)
insertNamedComponent content namedComp compSet = case namedComp of
  CExe n -> compSet{exeComp = Map.insert (textNameToCabalName n) content (exeComp compSet)}
  CLib -> compSet{libComp = Map.insert LMainLibName content (libComp compSet)}
  CInternalLib n -> compSet{libComp = Map.insert (LSubLibName $ textNameToCabalName n) content (libComp compSet)}
  CBench n -> compSet{benchComp = Map.insert (textNameToCabalName n) content (benchComp compSet)}
  CTest n -> compSet{testComp = Map.insert (textNameToCabalName n) content (testComp compSet)}

toNamedComponent :: (ComponentMap contentType) -> Set NamedComponent
toNamedComponent input = exe <> bench <> test <> lib
  where
    exe = Set.map (CExe . T.pack . show) $ Map.keysSet $ exeComp input
    bench = Set.map (CBench . T.pack . show) $ Map.keysSet $ benchComp input
    test = Set.map (CTest . T.pack . show) $ Map.keysSet $ testComp input
    lib = Set.map handleLib $ Map.keysSet $ libComp input
    handleLib lName = case lName of
      LMainLibName -> CLib
      LSubLibName name -> CInternalLib . T.pack . show $ name

toComponentName :: (ComponentMap contentType) -> Set ComponentName
toComponentName input = exe <> bench <> test <> lib <> flib
  where
    exe = getter exeComp CExeName
    bench = getter benchComp CBenchName
    test = getter testComp CTestName
    lib = getter libComp CLibName
    flib = getter foreignLibComp CFLibName
    getter accessor cstruct = Set.map cstruct $ (Map.keysSet $ accessor input)

toComponentNameList :: (ComponentMap contentType) -> [(ComponentName, contentType)]
toComponentNameList input = exe <> bench <> test <> lib <> flib
  where
    exe = getter exeComp CExeName
    bench = getter benchComp CBenchName
    test = getter testComp CTestName
    lib = getter libComp CLibName
    flib = getter foreignLibComp CFLibName
    getter accessor cstruct = fmap (first cstruct) (Map.assocs $ accessor input)

componentMapPrettyPrint :: ComponentMap contentType -> String
componentMapPrettyPrint input = mconcat ["{", exe, ",", bench, ",", test, ",", lib, ",", flib, "}"]
  where
    exe = getter exeComp
    bench = getter benchComp
    test = getter testComp
    lib = getter libComp
    flib = getter foreignLibComp
    getter accessor = show <$> Map.keys $ accessor input
  
componentMapAndPackagePPrint :: (PackageName, ComponentMap contentType) -> String
componentMapAndPackagePPrint (pName, compName) = packageNameString pName <> componentMapPrettyPrint compName