{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions for IDEs.
module Stack.IDE
    ( ListPackagesCmd(..)
    , listPackages
    , listTargets
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Prelude
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.SourceMap

data ListPackagesCmd = ListPackageNames
                     | ListPackageCabalFiles

-- | List the packages inside the current project.
listPackages :: HasBuildConfig env => ListPackagesCmd -> RIO env ()
listPackages flag = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  let strs = case flag of
        ListPackageNames ->
          map packageNameString (Map.keys packages)
        ListPackageCabalFiles ->
          map (toFilePath . ppCabalFP) (Map.elems packages)
  mapM_ (logInfo . fromString) strs

-- | List the targets in the current project.
listTargets :: forall env. HasBuildConfig env => RIO env ()
listTargets = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  pairs <- concat <$> Map.traverseWithKey toNameAndComponent packages
  logInfo $ display $ T.intercalate "\n" $
    map renderPkgComponent pairs
  where
    toNameAndComponent
      :: PackageName
      -> ProjectPackage
      -> RIO env [(PackageName, NamedComponent)]
    toNameAndComponent pkgName' =
        fmap (map (pkgName', ) . Set.toList) . ppComponents
