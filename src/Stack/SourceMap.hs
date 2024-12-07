{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Stack.SourceMap
  ( mkProjectPackage
  , snapToDepPackage
  , additionalDepPackage
  , loadVersion
  , getPLIVersion
  , loadGlobalHints
  , actualFromGhc
  , actualFromHints
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
import           Stack.Constants ( stackProgName' )
import           Stack.PackageDump ( conduitDumpPackage, ghcPkgDump )
import           Stack.Prelude
import           Stack.Types.Compiler
                   ( ActualCompiler, actualToWanted, wantedToActual )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe, HasCompiler (..) )
import           Stack.Types.Config ( HasConfig )
import           Stack.Types.DumpPackage
                   ( DumpPackage (..), DumpedGlobalPackage )
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
mkProjectPackage printWarnings resolvedDir buildHaddocks = do
  (gpd, name, cabalFP) <-
    loadCabalFilePath (Just stackProgName') (resolvedAbsolute resolvedDir)
  pure ProjectPackage
    { cabalFP
    , resolvedDir
    , projectCommon =
        CommonPackage
          { gpd = gpd printWarnings
          , name
          , flags = mempty
          , ghcOptions = mempty
          , cabalConfigOpts = mempty
          , buildHaddocks
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
additionalDepPackage buildHaddocks location = do
  (name, gpd) <-
    case location of
      PLMutable dir -> do
        (gpd, name, _cabalfp) <-
          loadCabalFilePath (Just stackProgName') (resolvedAbsolute dir)
        pure (name, gpd NoPrintWarnings)
      PLImmutable pli -> do
        let PackageIdentifier name _ = packageLocationIdent pli
        run <- askRunInIO
        pure (name, run $ loadCabalFileImmutable pli)
  pure DepPackage
    { location
    , hidden = False
    , fromSnapshot = NotFromSnapshot
    , depCommon =
        CommonPackage
          { gpd
          , name
          , flags = mempty
          , ghcOptions = mempty
          , cabalConfigOpts = mempty
          , buildHaddocks
          }
    }

snapToDepPackage ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Bool
     -- ^ Should Haddock documentation be built for the package?
  -> PackageName
  -> SnapshotPackage
  -> RIO env DepPackage
snapToDepPackage buildHaddocks name sp = do
  run <- askRunInIO
  pure DepPackage
    { location = PLImmutable sp.spLocation
    , hidden = sp.spHidden
    , fromSnapshot = FromSnapshot
    , depCommon =
        CommonPackage
          { gpd = run $ loadCabalFileImmutable sp.spLocation
          , name
          , flags = sp.spFlags
          , ghcOptions = sp.spGhcOptions
          , cabalConfigOpts = [] -- No spCabalConfigOpts, not present in snapshots
          , buildHaddocks
          }
    }

loadVersion :: MonadIO m => CommonPackage -> m Version
loadVersion common = do
  gpd <- liftIO common.gpd
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
                   .| CL.foldMap (\dp -> Map.singleton dp.ghcPkgId dp)
      toGlobals ds =
        Map.fromList $ map (pkgName . (.packageIdent) &&& id) $ Map.elems ds
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

actualFromGhc ::
     (HasConfig env, HasCompiler env)
  => SMWanted
  -> ActualCompiler
  -> RIO env (SMActual DumpedGlobalPackage)
actualFromGhc smw compiler = do
  globals <- view $ compilerPathsL . to (.globalDump)
  pure
    SMActual
      { compiler
      , project = smw.project
      , deps = smw.deps
      , globals
      }

actualFromHints ::
     (HasConfig env)
  => SMWanted
  -> ActualCompiler
  -> RIO env (SMActual GlobalPackageVersion)
actualFromHints smw compiler = do
  globals <- globalsFromHints (actualToWanted compiler)
  pure
    SMActual
      { compiler
      , project = smw.project
      , deps = smw.deps
      , globals = Map.map GlobalPackageVersion globals
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

pruneGlobals ::
     Map PackageName DumpedGlobalPackage
  -> Set PackageName
  -> Map PackageName GlobalPackage
pruneGlobals globals deps =
  let (prunedGlobals, keptGlobals) =
        partitionReplacedDependencies globals (pkgName . (.packageIdent))
          (.ghcPkgId) (.depends) deps
  in  Map.map (GlobalPackage . pkgVersion . (.packageIdent)) keptGlobals <>
      Map.map ReplacedGlobalPackage prunedGlobals

getCompilerInfo :: (HasConfig env, HasCompiler env) => RIO env Builder
getCompilerInfo = view $ compilerPathsL . to (byteString . (.ghcInfo))

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
  (snapshot, _, _) <-
    loadAndCompleteSnapshotRaw' debugRSL loc Map.empty Map.empty
  deps <-
    Map.traverseWithKey (snapToDepPackage False) (snapshotPackages snapshot)
  let wc = snapshotCompiler snapshot
  globals <- Map.map GlobalPackageVersion <$> globalsFromHints wc
  pure $ \projectPackages -> do
    project <- fmap Map.fromList . for projectPackages $ \resolved -> do
      pp <- mkProjectPackage printWarnings resolved buildHaddocks
      pure (pp.projectCommon.name, pp)
    compiler <- either throwIO pure $ wantedToActual $ snapshotCompiler snapshot
    pure
      SMActual
        { compiler
        , project
        , deps = Map.difference deps project
        , globals
        }
