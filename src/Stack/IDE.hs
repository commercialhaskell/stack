{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions for IDEs.
module Stack.IDE
    ( OutputStream(..)
    , ListPackagesCmd(..)
    , listPackages
    , listTargets
    , bios
    ) where

import           Data.Aeson hiding (pairs)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.ModuleName (ModuleName, components)
import           Distribution.Types.PackageName (PackageName, unPackageName)
import           Stack.Build.Source
import           Stack.Prelude
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.SourceMap
import           System.IO (putStrLn)

data OutputStream = OutputLogInfo
                  | OutputStdout

data ListPackagesCmd = ListPackageNames
                     | ListPackageCabalFiles

outputFunc :: HasLogFunc env => OutputStream -> String -> RIO env ()
outputFunc OutputLogInfo = logInfo . fromString
outputFunc OutputStdout  = liftIO . putStrLn

-- | List the packages inside the current project.
listPackages :: HasBuildConfig env => OutputStream -> ListPackagesCmd -> RIO env ()
listPackages stream flag = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  let strs = case flag of
        ListPackageNames ->
          map packageNameString (Map.keys packages)
        ListPackageCabalFiles ->
          map (toFilePath . ppCabalFP) (Map.elems packages)
  mapM_ (outputFunc stream) strs

-- | List the targets in the current project.
listTargets :: forall env. HasBuildConfig env => OutputStream -> RIO env ()
listTargets stream = do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  pairs <- concat <$> Map.traverseWithKey toNameAndComponent packages
  outputFunc stream $ T.unpack $ T.intercalate "\n" $
    map renderPkgComponent pairs
  where toNameAndComponent
          :: PackageName
          -> ProjectPackage
          -> RIO env [(PackageName, NamedComponent)]
        toNameAndComponent pkgName' =
            fmap (map (pkgName', ) . Set.toList) . ppComponents

-- | List the packages inside the current project.
bios :: (HasBuildConfig env, HasSourceMap env, HasEnvConfig env) => () -> RIO env ()
bios () = do
    packages <-
        fmap M.toList $ view $ buildConfigL . to (smwProject . bcSMWanted)
    packagesWithTheirComponents <-
        traverse
            (\(name, projectPackage) -> do
                 components' <- ppComponents projectPackage
                 package <- loadCommonPackage (ppCommon projectPackage)
                 (modules, files, buildFilesAndDataFiles, warnings) <-
                     getPackageFiles
                         (packageFiles package)
                         (ppCabalFP projectPackage)
                 pure (name, components', Files {..}))
            packages
    let packageJsons = fmap toBiosJson packagesWithTheirComponents
    liftIO (L8.putStrLn $ encodePretty (object ["packages" .= packageJsons]))

data Files = Files
    { modules :: Map NamedComponent (Map ModuleName (Path Abs File))
    , files :: Map NamedComponent [DotCabalPath]
    , buildFilesAndDataFiles :: Set (Path Abs File)
    , warnings :: [PackageWarning]
    }

-- | Produce JSON representation of local package set.
toBiosJson ::
       ( PackageName
       , Set NamedComponent
       , Files)
    -> Value
toBiosJson (packageName', components', Files { modules
                                             , files
                                             , buildFilesAndDataFiles
                                             }) =
    object
        [ "name" .= unPackageName packageName'
        , "components" .= map componnentToJson (Set.toList components')
        ]
  where
    componnentToJson component =
        object
            [ "name" .= renderComponent component
            , "type" .=
              case component of
                  CLib {} -> "library" :: Text
                  CInternalLib {} -> "internal-library"
                  CExe {} -> "executable"
                  CTest {} -> "test"
                  CBench {} -> "benchmark"
            , "modules" .=
              fmap
                  (\(moduleName, path) ->
                       object
                           [ "name" .= intercalate "." (components moduleName)
                           , "path" .= path
                           ])
                  (M.toList (fromMaybe mempty (M.lookup component modules)))
            , "files" .=
              (buildFilesAndDataFiles <>
               Set.fromList
                   (fmap
                        (\case
                             DotCabalModulePath path -> path
                             DotCabalMainPath path -> path
                             DotCabalFilePath path -> path
                             DotCabalCFilePath path -> path)
                        (fromMaybe mempty (M.lookup component files))))
            ]
