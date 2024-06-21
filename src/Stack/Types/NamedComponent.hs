{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module exporting the 'NamedComponent' type and related functions.
module Stack.Types.NamedComponent
  ( NamedComponent (..)
  , componentCachePath
  , renderComponent
  , renderComponentTo
  , renderPkgComponents
  , renderPkgComponent
  , exeComponents
  , testComponents
  , benchComponents
  , subLibComponents
  , isCLib
  , isCSubLib
  , isCExe
  , isCTest
  , isCBench
  , isPotentialDependency
  , splitComponents
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Prelude
import           Stack.Types.ComponentUtils (StackUnqualCompName, unqualCompToText, unqualCompToString)

-- | Type representing components of a fully-resolved Cabal package.
data NamedComponent
  = CLib
    -- The \'main\' unnamed library component.
  | CSubLib !StackUnqualCompName
    -- A named \'subsidiary\' or \'ancillary\` library component (sub-library).
  | CFlib !StackUnqualCompName
    -- A foreign library.
  | CExe !StackUnqualCompName
    -- A named executable component.
  | CTest !StackUnqualCompName
    -- A named test-suite component.
  | CBench !StackUnqualCompName
    -- A named benchmark component.
  deriving (Eq, Ord, Show)

-- | Render a component to anything with an "IsString" instance. For 'Text'
-- prefer 'renderComponent'.
renderComponentTo :: IsString a => NamedComponent -> a
renderComponentTo = fromString . T.unpack . renderComponent

renderComponent :: NamedComponent -> Text
renderComponent CLib = "lib"
renderComponent (CSubLib x) = "sub-lib:" <> unqualCompToText x
renderComponent (CFlib x) = "flib:" <> unqualCompToText x
renderComponent (CExe x) = "exe:" <> unqualCompToText x
renderComponent (CTest x) = "test:" <> unqualCompToText x
renderComponent (CBench x) = "bench:" <> unqualCompToText x

componentCachePath :: NamedComponent -> String
componentCachePath CLib = "lib"
componentCachePath (CSubLib x) = "sub-lib-" <> unqualCompToString x
componentCachePath (CFlib x) = "flib-" <> unqualCompToString x
componentCachePath (CExe x) = "exe-" <> unqualCompToString x
componentCachePath (CTest x) = "test-" <> unqualCompToString x
componentCachePath (CBench x) = "bench-" <> unqualCompToString x

renderPkgComponents :: [(PackageName, NamedComponent)] -> Text
renderPkgComponents = T.intercalate " " . map renderPkgComponent

renderPkgComponent :: (PackageName, NamedComponent) -> Text
renderPkgComponent (pkg, comp) =
  fromPackageName pkg <> ":" <> renderComponent comp

exeComponents :: Set NamedComponent -> Set StackUnqualCompName
exeComponents = Set.fromList . mapMaybe mExeName . Set.toList
 where
  mExeName (CExe name) = Just name
  mExeName _ = Nothing

testComponents :: Set NamedComponent -> Set StackUnqualCompName
testComponents = Set.fromList . mapMaybe mTestName . Set.toList
 where
  mTestName (CTest name) = Just name
  mTestName _ = Nothing

benchComponents :: Set NamedComponent -> Set StackUnqualCompName
benchComponents = Set.fromList . mapMaybe mBenchName . Set.toList
 where
  mBenchName (CBench name) = Just name
  mBenchName _ = Nothing

subLibComponents :: Set NamedComponent -> Set StackUnqualCompName
subLibComponents = Set.fromList . mapMaybe mSubLibName . Set.toList
 where
  mSubLibName (CSubLib name) = Just name
  mSubLibName _ = Nothing

isCLib :: NamedComponent -> Bool
isCLib CLib{} = True
isCLib _ = False

isCSubLib :: NamedComponent -> Bool
isCSubLib CSubLib{} = True
isCSubLib _ = False

isCExe :: NamedComponent -> Bool
isCExe CExe{} = True
isCExe _ = False

isCTest :: NamedComponent -> Bool
isCTest CTest{} = True
isCTest _ = False

isCBench :: NamedComponent -> Bool
isCBench CBench{} = True
isCBench _ = False

isPotentialDependency :: NamedComponent -> Bool
isPotentialDependency v = isCLib v || isCSubLib v || isCExe v

-- | A function to split the given list of components into sets of the names of
-- the named components by the type of component (sub-libraries, executables,
-- test-suites, benchmarks), ignoring any 'main' unnamed library component or
-- foreign library component. This function should be used very sparingly; more
-- often than not, you can keep/parse the components split from the start.
splitComponents ::
     [NamedComponent]
  -> ( Set StackUnqualCompName
       -- ^ Sub-libraries.
     , Set StackUnqualCompName
       -- ^ Executables.
     , Set StackUnqualCompName
       -- ^ Test-suites.
     , Set StackUnqualCompName
       -- ^ Benchmarks.
     )
splitComponents =
  go id id id id
 where
  run c = Set.fromList $ c []
  go s e t b [] = (run s, run e, run t, run b)
  go s e t b (CLib : xs) = go s e t b xs
  go s e t b (CSubLib x : xs) = go (s . (x:)) e t b xs
  -- Ignore foreign libraries, for now.
  go s e t b (CFlib _ : xs) = go s e t b xs
  go s e t b (CExe x : xs) = go s (e . (x:)) t b xs
  go s e t b (CTest x : xs) = go s e (t . (x:)) b xs
  go s e t b (CBench x : xs) = go s e t (b . (x:)) xs
