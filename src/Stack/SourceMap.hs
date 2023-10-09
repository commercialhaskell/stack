{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack.SourceMap
  ( mkProjectPackage
  , snapToDepPackage
  , additionalDepPackage
  , loadVersion
  , getPLIVersion
  , loadGlobalHints
  , DumpedGlobalPackage
  , actualFromGhc
  , actualFromHints
  , checkFlagsUsedThrowing
  , globalCondCheck
  , pruneGlobals
  , globalsFromHints
  , getCompilerInfo
  , immutableLocSha
  , loadProjectSnapshotCandidate
  , SnapshotCandidate
  , globalsFromDump
  ) where

import           Data.ByteString.Builder ( byteString )
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Distribution.PackageDescription as PD
import           Distribution.System ( Platform (..) )
import qualified Pantry.SHA256 as SHA256
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import qualified RIO.Set as Set
import           Stack.Constants ( stackProgName' )
import           Stack.PackageDump ( conduitDumpPackage, ghcPkgDump )
import           Stack.Prelude
import           Stack.Types.Build.Exception ( BuildPrettyException (..) )
import           Stack.Types.Compiler
                   ( ActualCompiler, actualToWanted, wantedToActual )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe, HasCompiler (..) )
import           Stack.Types.Config ( HasConfig )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.UnusedFlags ( FlagSource, UnusedFlags (..) )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( rslInLogL )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), FromSnapshot (..)
                   , GlobalPackage (..), GlobalPackageVersion (..)
                   , ProjectPackage (..), SMActual (..), SMWanted (..)
                   )

-- | Create a 'ProjectPackage' from a directory containing a package.
mkProjectPackage ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PrintWarnings
  -> ResolvedPath Dir
  -> Bool
     -- ^ Should Haddock documentation be built for the package?
  -> RIO env ProjectPackage
mkProjectPackage printWarnings dir buildHaddocks = do
  (gpd, name, cabalfp) <-
    loadCabalFilePath (Just stackProgName') (resolvedAbsolute dir)
  pure ProjectPackage
    { ppCabalFP = cabalfp
    , ppResolvedDir = dir
    , ppCommon =
        CommonPackage
          { cpGPD = gpd printWarnings
          , cpName = name
          , cpFlags = mempty
          , cpGhcOptions = mempty
          , cpCabalConfigOpts = mempty
          , cpHaddocks = buildHaddocks
          }
    }

-- | Create a 'DepPackage' from a 'PackageLocation', from some additional
-- to a snapshot setting (extra-deps or command line)
additionalDepPackage ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Bool
     -- ^ Should Haddock documentation be built for the package?
  -> PackageLocation
  -> RIO env DepPackage
additionalDepPackage buildHaddocks pl = do
  (name, gpdio) <-
    case pl of
      PLMutable dir -> do
        (gpdio, name, _cabalfp) <-
          loadCabalFilePath (Just stackProgName') (resolvedAbsolute dir)
        pure (name, gpdio NoPrintWarnings)
      PLImmutable pli -> do
        let PackageIdentifier name _ = packageLocationIdent pli
        run <- askRunInIO
        pure (name, run $ loadCabalFileImmutable pli)
  pure DepPackage
    { dpLocation = pl
    , dpHidden = False
    , dpFromSnapshot = NotFromSnapshot
    , dpCommon =
        CommonPackage
          { cpGPD = gpdio
          , cpName = name
          , cpFlags = mempty
          , cpGhcOptions = mempty
          , cpCabalConfigOpts = mempty
          , cpHaddocks = buildHaddocks
          }
    }

snapToDepPackage ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Bool
     -- ^ Should Haddock documentation be built for the package?
  -> PackageName
  -> SnapshotPackage
  -> RIO env DepPackage
snapToDepPackage buildHaddocks name SnapshotPackage{..} = do
  run <- askRunInIO
  pure DepPackage
    { dpLocation = PLImmutable spLocation
    , dpHidden = spHidden
    , dpFromSnapshot = FromSnapshot
    , dpCommon =
        CommonPackage
          { cpGPD = run $ loadCabalFileImmutable spLocation
          , cpName = name
          , cpFlags = spFlags
          , cpGhcOptions = spGhcOptions
          , cpCabalConfigOpts = [] -- No spCabalConfigOpts, not present in snapshots
          , cpHaddocks = buildHaddocks
          }
    }

loadVersion :: MonadIO m => CommonPackage -> m Version
loadVersion common = do
  gpd <- liftIO $ cpGPD common
  pure (pkgVersion $ PD.package $ PD.packageDescription gpd)

getPLIVersion :: PackageLocationImmutable -> Version
getPLIVersion (PLIHackage (PackageIdentifier _ v) _ _) = v
getPLIVersion (PLIArchive _ pm) = pkgVersion $ pmIdent pm
getPLIVersion (PLIRepo _ pm) = pkgVersion $ pmIdent pm

globalsFromDump ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> RIO env (Map PackageName DumpedGlobalPackage)
globalsFromDump pkgexe = do
  let pkgConduit =    conduitDumpPackage
                   .| CL.foldMap (\dp -> Map.singleton (dpGhcPkgId dp) dp)
      toGlobals ds =
        Map.fromList $ map (pkgName . dpPackageIdent &&& id) $ Map.elems ds
  toGlobals <$> ghcPkgDump pkgexe [] pkgConduit

globalsFromHints ::
     HasConfig env
  => WantedCompiler
  -> RIO env (Map PackageName Version)
globalsFromHints compiler = do
  mglobalHints <- loadGlobalHints compiler
  case mglobalHints of
    Just hints -> pure hints
    Nothing -> do
      prettyWarnL
        [ flow "Unable to load global hints for"
        , fromString $ T.unpack $ textDisplay compiler
        ]
      pure mempty

type DumpedGlobalPackage = DumpPackage

actualFromGhc ::
     (HasConfig env, HasCompiler env)
  => SMWanted
  -> ActualCompiler
  -> RIO env (SMActual DumpedGlobalPackage)
actualFromGhc smw ac = do
  globals <- view $ compilerPathsL.to cpGlobalDump
  pure
    SMActual
      { smaCompiler = ac
      , smaProject = smwProject smw
      , smaDeps = smwDeps smw
      , smaGlobal = globals
      }

actualFromHints ::
     (HasConfig env)
  => SMWanted
  -> ActualCompiler
  -> RIO env (SMActual GlobalPackageVersion)
actualFromHints smw ac = do
  globals <- globalsFromHints (actualToWanted ac)
  pure
    SMActual
      { smaCompiler = ac
      , smaProject = smwProject smw
      , smaDeps = smwDeps smw
      , smaGlobal = Map.map GlobalPackageVersion globals
      }

-- | Simple cond check for boot packages - checks only OS and Arch
globalCondCheck ::
     (HasConfig env)
  => RIO env (PD.ConfVar
  -> Either PD.ConfVar Bool)
globalCondCheck = do
  Platform arch os <- view platformL
  let condCheck (PD.OS os') = pure $ os' == os
      condCheck (PD.Arch arch') = pure $ arch' == arch
      condCheck c = Left c
  pure condCheck

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
      getUnusedPackageFlags (pname, flags) source prjPackages deps
  unless (null unusedFlags) $
    prettyThrowM $ InvalidFlagSpecification $ Set.fromList unusedFlags

getUnusedPackageFlags ::
     MonadIO m
  => (PackageName, Map FlagName Bool)
  -> FlagSource
  -> Map PackageName ProjectPackage
  -> Map PackageName DepPackage
  -> m (Maybe UnusedFlags)
getUnusedPackageFlags (name, userFlags) source prj deps =
  let maybeCommon =     fmap ppCommon (Map.lookup name prj)
                    <|> fmap dpCommon (Map.lookup name deps)
  in  case maybeCommon of
        -- Package is not available as project or dependency
        Nothing ->
          pure $ Just $ UFNoPackage source name
        -- Package exists, let's check if the flags are defined
        Just common -> do
          gpd <- liftIO $ cpGPD common
          let pname = pkgName $ PD.package $ PD.packageDescription gpd
              pkgFlags = Set.fromList $ map PD.flagName $ PD.genPackageFlags gpd
              unused = Map.keysSet $ Map.withoutKeys userFlags pkgFlags
          if Set.null unused
            -- All flags are defined, nothing to do
            then pure Nothing
            -- Error about the undefined flags
            else pure $ Just $ UFFlagsNotDefined source pname pkgFlags unused

pruneGlobals ::
     Map PackageName DumpedGlobalPackage
  -> Set PackageName
  -> Map PackageName GlobalPackage
pruneGlobals globals deps =
  let (prunedGlobals, keptGlobals) =
        partitionReplacedDependencies globals (pkgName . dpPackageIdent)
          dpGhcPkgId dpDepends deps
  in  Map.map (GlobalPackage . pkgVersion . dpPackageIdent) keptGlobals <>
      Map.map ReplacedGlobalPackage prunedGlobals

getCompilerInfo :: (HasConfig env, HasCompiler env) => RIO env Builder
getCompilerInfo = view $ compilerPathsL.to (byteString . cpGhcInfo)

immutableLocSha :: PackageLocationImmutable -> Builder
immutableLocSha = byteString . treeKeyToBs . locationTreeKey
 where
  locationTreeKey (PLIHackage _ _ tk) = tk
  locationTreeKey (PLIArchive _ pm) = pmTreeKey pm
  locationTreeKey (PLIRepo _ pm) = pmTreeKey pm
  treeKeyToBs (TreeKey (BlobKey sha _)) = SHA256.toHexBytes sha

type SnapshotCandidate env
  = [ResolvedPath Dir] -> RIO env (SMActual GlobalPackageVersion)

loadProjectSnapshotCandidate ::
     (HasConfig env)
  => RawSnapshotLocation
  -> PrintWarnings
  -> Bool
     -- ^ Should Haddock documentation be build for the package?
  -> RIO env (SnapshotCandidate env)
loadProjectSnapshotCandidate loc printWarnings buildHaddocks = do
  debugRSL <- view rslInLogL
  (snapshot, _, _) <- loadAndCompleteSnapshotRaw' debugRSL loc Map.empty Map.empty
  deps <- Map.traverseWithKey (snapToDepPackage False) (snapshotPackages snapshot)
  let wc = snapshotCompiler snapshot
  globals <- Map.map GlobalPackageVersion <$> globalsFromHints wc
  pure $ \projectPackages -> do
    prjPkgs <- fmap Map.fromList . for projectPackages $ \resolved -> do
      pp <- mkProjectPackage printWarnings resolved buildHaddocks
      pure (cpName $ ppCommon pp, pp)
    compiler <- either throwIO pure $ wantedToActual $ snapshotCompiler snapshot
    pure
      SMActual
        { smaCompiler = compiler
        , smaProject = prjPkgs
        , smaDeps = Map.difference deps prjPkgs
        , smaGlobal = globals
        }
