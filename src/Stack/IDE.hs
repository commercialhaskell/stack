{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for IDEs.
module Stack.IDE
    ( OutputStream(..)
    , ListPackagesCmd(..)
    , listPackages
    , listTargets
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Stack.Config (getLocalPackages)
import           Stack.Package (readPackageUnresolvedDir, gpdPackageName)
import           Stack.Prelude
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.PackageName (packageNameString)
import           System.IO (putStrLn)

data OutputStream = OutputLogInfo
                  | OutputStdout

data ListPackagesCmd = ListPackageNames
                     | ListPackageCabalFiles

outputFunc :: HasLogFunc env => OutputStream -> String -> RIO env ()
outputFunc OutputLogInfo = logInfo . fromString
outputFunc OutputStdout  = liftIO . putStrLn

-- | List the packages inside the current project.
listPackages :: HasEnvConfig env => OutputStream -> ListPackagesCmd -> RIO env ()
listPackages stream flag = do
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
            outputFunc stream $ packageNameString $ gpdPackageName gpd
          ListPackageCabalFiles ->
            outputFunc stream cabal_file

-- | List the targets in the current project.
listTargets :: HasEnvConfig env => OutputStream -> RIO env ()
listTargets stream =
    do rawLocals <- lpProject <$> getLocalPackages
       outputFunc stream $ T.unpack
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
