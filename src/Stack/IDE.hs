{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for IDEs.
module Stack.IDE
    ( ListPackagesCmd(..)
    , listPackages
    , listTargets
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           System.IO (putStrLn)
import           Stack.Config (getLocalPackages)
import           Stack.Package (readPackageUnresolvedDir, gpdPackageName)
import           Stack.Prelude
import           Stack.Types.Config
import           Stack.Types.NamedComponent

data ListPackagesCmd = ListPackageNames
                     | ListPackageCabalFiles

-- | List the packages inside the current project.
listPackages :: HasEnvConfig env => ListPackagesCmd -> RIO env ()
listPackages flag = do
    -- TODO: Instead of setting up an entire EnvConfig only to look up the package directories,
    -- make do with a Config (and the Project inside) and use resolvePackageEntry to get
    -- the directory.
    packages <- liftM (Map.elems . lpProject) getLocalPackages
    forM_ packages $ \pkg -> do
        let dir = lpvRoot pkg
            cabal_file = toFilePath $ lpvCabalFP pkg
            cabal_file :: FilePath
        case flag of
          ListPackageNames -> do
            (gpd, _) <- readPackageUnresolvedDir dir False
            logInfo $ display $ gpdPackageName gpd
          ListPackageCabalFiles ->
            liftIO $ putStrLn $ fromString cabal_file

-- | List the targets in the current project.
listTargets :: HasEnvConfig env => RIO env ()
listTargets =
    do rawLocals <- lpProject <$> getLocalPackages
       logInfo $ display
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
