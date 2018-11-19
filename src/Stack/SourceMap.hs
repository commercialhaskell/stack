{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Stack.SourceMap
    ( mkProjectPackage
    , snapToDepPackage
    , additionalDepPackage
    , getPLIVersion
    , toActual
    , checkFlagsUsedThrowing
    ) where

import qualified Data.Conduit.List as CL
import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Pantry
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import RIO.Process
import Stack.PackageDump
import Stack.Prelude
import Stack.Types.Build
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

-- | Create a 'DepPackage' from a 'PackageLocation', from some additional
-- to a snapshot setting (extra-deps or command line)
additionalDepPackage
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Bool
  -> PackageLocation
  -> RIO env DepPackage
additionalDepPackage buildHaddocks pl = do
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
    , dpFromSnapshot = NotFromSnapshot
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
    , dpFromSnapshot = FromSnapshot
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
    gpd <- liftIO loadGPD
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

checkFlagsUsedThrowing ::
       (MonadIO m, MonadThrow m)
    => Map PackageName (Map FlagName Bool)
    -> FlagSource
    -> Map PackageName ProjectPackage
    -> Map PackageName DepPackage
    -> m ()
checkFlagsUsedThrowing packageFlags source prjPackages deps = do
    unusedFlags <-
        forMaybeM (Map.toList packageFlags) $ \(pname, flags) ->
            checkFlagUsed (pname, flags) source prjPackages deps
    unless (null unusedFlags) $
        throwM $ InvalidFlagSpecification $ Set.fromList unusedFlags

checkFlagUsed ::
       MonadIO m
    => (PackageName, Map FlagName Bool)
    -> FlagSource
    -> Map PackageName ProjectPackage
    -> Map PackageName DepPackage
    -> m (Maybe UnusedFlags)
checkFlagUsed (name, userFlags) source prj deps =
    let maybeCommon =
          fmap ppCommon (Map.lookup name prj) <|>
          fmap dpCommon (Map.lookup name deps)
    in case maybeCommon  of
        -- Package is not available as project or dependency
        Nothing ->
            pure $ Just $ UFNoPackage source name
        -- Package exists, let's check if the flags are defined
        Just common -> do
            gpd <- liftIO $ cpGPD common
            let pname = pkgName $ PD.package $ PD.packageDescription gpd
                pkgFlags = Set.fromList $ map PD.flagName $ PD.genPackageFlags gpd
                unused = Set.difference (Map.keysSet userFlags)
                         pkgFlags
            if Set.null unused
                    -- All flags are defined, nothing to do
                    then pure Nothing
                    -- Error about the undefined flags
                    else pure $ Just $ UFFlagsNotDefined source pname pkgFlags unused
