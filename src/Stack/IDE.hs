{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for IDEs.
module Stack.IDE
    ( listPackages
    , listTargets
    ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Build.Source (getLocalPackageViews)
import           Stack.Build.Target (LocalPackageView(..))
import           Stack.Config (getLocalPackages)
import           Stack.Package (findOrGenerateCabalFile)
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.PackageName
import           Stack.Types.StackT

-- | List the packages inside the current project.
listPackages :: (StackM env m, HasEnvConfig env) => m ()
listPackages = do
    -- TODO: Instead of setting up an entire EnvConfig only to look up the package directories,
    -- make do with a Config (and the Project inside) and use resolvePackageEntry to get
    -- the directory.
    packageDirs <- liftM Map.keys getLocalPackages
    forM_ packageDirs $ \dir -> do
        cabalfp <- findOrGenerateCabalFile dir
        pkgName <- parsePackageNameFromFilePath cabalfp
        ($logInfo . packageNameText) pkgName

-- | List the targets in the current project.
listTargets :: (StackM env m, HasEnvConfig env) => m ()
listTargets =
    do rawLocals <- getLocalPackageViews
       $logInfo
           (T.intercalate
                "\n"
                (map
                     renderPkgComponent
                     (concatMap
                          toNameAndComponent
                          (Map.toList (Map.map fst rawLocals)))))
  where
    toNameAndComponent (pkgName,view') =
        map (pkgName, ) (Set.toList (lpvComponents view'))
