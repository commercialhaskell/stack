{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Stack.SourceMap
    ( mkProjectPackage
    , mkDepPackage
    , snapToDepPackage
    , getPLIVersion
    , toActual
    ) where

import qualified Data.Conduit.List as CL
import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Pantry
import qualified RIO.Map as Map
import RIO.Process
import Stack.PackageDump
import Stack.Prelude
import Stack.Types.Compiler
import Stack.Types.SourceMap

-- | Create a 'ProjectPackage' from a directory containing a package.
mkProjectPackage ::
       forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => PrintWarnings
    -> ResolvedPath Dir
    -> Bool
    -> RIO env ProjectPackage
mkProjectPackage printWarnings dir buildHaddocks = do
   (gpd, name, cabalfp) <- loadCabalFilePath (resolvedAbsolute dir)
   return ProjectPackage
     { ppCabalFP = cabalfp
     , ppResolvedDir = dir
     , ppCommon = CommonPackage
                  { cpGPD = gpd printWarnings
                  , cpName = name
                  , cpFlags = mempty
                  , cpGhcOptions = mempty
                  , cpHaddocks = buildHaddocks
                  }
     }

-- | Create a 'DepPackage' from a 'PackageLocation'
mkDepPackage
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Bool
  -> PackageLocation
  -> RIO env DepPackage
mkDepPackage buildHaddocks pl = do
  (name, gpdio) <-
    case pl of
      PLMutable dir -> do
        (gpdio, name, _cabalfp) <- loadCabalFilePath (resolvedAbsolute dir)
        pure (name, gpdio NoPrintWarnings)
      PLImmutable pli -> do
        PackageIdentifier name _ <- getPackageLocationIdent pli
        run <- askRunInIO
        pure (name, run $ loadCabalFileImmutable pli)
  return DepPackage
    { dpLocation = pl
    , dpHidden = False
    , dpCommon = CommonPackage
                  { cpGPD = gpdio
                  , cpName = name
                  , cpFlags = mempty
                  , cpGhcOptions = mempty
                  , cpHaddocks = buildHaddocks
                  }
    }

snapToDepPackage ::
       forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => Bool
    -> PackageName
    -> SnapshotPackage
    -> RIO env DepPackage
snapToDepPackage buildHaddocks name SnapshotPackage{..} = do
  run <- askRunInIO
  return DepPackage
    { dpLocation = PLImmutable spLocation
    , dpHidden = spHidden
    , dpCommon = CommonPackage
                  { cpGPD = run $ loadCabalFileImmutable spLocation
                  , cpName = name
                  , cpFlags = spFlags
                  , cpGhcOptions = spGhcOptions
                  , cpHaddocks = buildHaddocks
                  }
    }

getPLIVersion ::
       MonadIO m
    => PackageLocationImmutable
    -> IO GenericPackageDescription
    -> m Version
getPLIVersion (PLIHackage (PackageIdentifierRevision _ v _) _) _ = pure v
getPLIVersion (PLIArchive _ pm) loadGPD = versionMaybeFromPM pm loadGPD
getPLIVersion (PLIRepo _ pm) loadGPD = versionMaybeFromPM pm loadGPD

versionMaybeFromPM ::
       MonadIO m => PackageMetadata -> IO GenericPackageDescription -> m Version
versionMaybeFromPM pm _ | Just v <- pmVersion pm = pure v
versionMaybeFromPM _ loadGPD = do
    gpd <- liftIO $ loadGPD
    return $ pkgVersion $ PD.package $ PD.packageDescription gpd

toActual ::
       (HasProcessContext env, HasLogFunc env)
    => SMWanted
    -> ActualCompiler
    -> RIO env SMActual
toActual smw compiler = do
    let pkgConduit =
            conduitDumpPackage .|
            CL.foldMap (\dp -> Map.singleton (dpGhcPkgId dp) dp)
        toGlobals ds = Map.fromList $ map toGlobal $ Map.elems ds
        toGlobal d =
            ( pkgName $ dpPackageIdent d
            , GlobalPackage (pkgVersion $ dpPackageIdent d))
    dumped <- toGlobals <$> ghcPkgDump (whichCompiler compiler) [] pkgConduit
    let globals =
            dumped `Map.difference` smwProject smw `Map.difference` smwDeps smw
    return
        SMActual
        { smaCompiler = compiler
        , smaProject = smwProject smw
        , smaDeps = smwDeps smw
        , smaGlobal = globals
        }
