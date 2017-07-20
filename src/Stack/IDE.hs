{-# LANGUAGE NoImplicitPrelude #-}
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

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Config (getLocalPackages)
import           Stack.Package (findOrGenerateCabalFile)
import           Stack.Prelude
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.PackageName

-- | List the packages inside the current project.
listPackages :: HasEnvConfig env => RIO env ()
listPackages = do
    -- TODO: Instead of setting up an entire EnvConfig only to look up the package directories,
    -- make do with a Config (and the Project inside) and use resolvePackageEntry to get
    -- the directory.
    packageDirs <- liftM (map lpvRoot . Map.elems . lpProject) getLocalPackages
    forM_ packageDirs $ \dir -> do
        cabalfp <- findOrGenerateCabalFile dir
        pkgName <- parsePackageNameFromFilePath cabalfp
        ($logInfo . packageNameText) pkgName

-- | List the targets in the current project.
listTargets :: HasEnvConfig env => RIO env ()
listTargets =
    do rawLocals <- lpProject <$> getLocalPackages
       $logInfo
           (T.intercalate
                "\n"
                (map
                     renderPkgComponent
                     (concatMap
                          toNameAndComponent
                          (Map.toList rawLocals))))
  where
    toNameAndComponent (pkgName,view') =
        map (pkgName, ) (Set.toList (lpvComponents view'))
