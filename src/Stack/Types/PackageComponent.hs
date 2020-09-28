{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Stack.Types.PackageComponent where

import Stack.Prelude
import Stack.Types.NamedComponent (NamedComponent(CLib))

-- | Required for component-addressed build plans.
-- Before introducing this, most packages were 'PackageName' addressed.
-- See <https://github.com/commercialhaskell/stack/issues/4745 this issue>
-- for more details.
data PackageComponentName = PackageComponentName {
  packageName :: !PackageName,
  componentName :: !NamedComponent
} deriving (Eq, Show, Ord)

-- | This is needed for computing the largest packageName length in a BuildPlan.
-- See <../Build/Execute.hs#504 this file>
getPackageNameLength :: PackageComponentName -> Int
getPackageNameLength PackageComponentName{packageName=pn} = length . packageNameString $ pn

-- | This is the main case for most packages, you only depend on their default library.
libraryPackage :: PackageName -> PackageComponentName
libraryPackage pckName = PackageComponentName {
    packageName = pckName,
    componentName = CLib
  }

forgetComponentName :: PackageComponentName -> PackageName
forgetComponentName = packageName
