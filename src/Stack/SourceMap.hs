{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Stack.SourceMap
    ( mkProjectPackage
    , snapToDepPackage
    , additionalDepPackage
    , getPLIVersion
    , loadGlobalHints
    , toActual
    , checkFlagsUsedThrowing
    ) where

import qualified Data.Conduit.List as CL
import Data.Yaml (decodeFileThrow)
import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Network.HTTP.Download (download, redownload)
import Network.HTTP.StackClient (parseRequest)
import Pantry
import qualified RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import RIO.Process
import Stack.PackageDump
import Stack.Prelude
import Stack.Types.Build
import Stack.Types.Compiler
import Stack.Types.Config
import Stack.Types.Runner (HasRunner)
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


-- | Load the global hints from Github.
loadGlobalHints
  :: HasRunner env
  => Path Abs File -- ^ local cached file location
  -> ActualCompiler
  -> RIO env (Maybe (Map PackageName Version))
loadGlobalHints dest ac =
    inner False
  where
    inner alreadyDownloaded = do
      req <- parseRequest "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/global-hints.yaml"
      downloaded <- download req dest
      eres <- tryAny inner2
      mres <-
        case eres of
          Left e -> Nothing <$ logError ("Error when parsing global hints: " <> displayShow e)
          Right x -> pure x
      case mres of
        Nothing | not alreadyDownloaded && not downloaded -> do
          logInfo $
            "Could not find local global hints for " <>
            RIO.display ac <>
            ", forcing a redownload"
          x <- redownload req dest
          if x
            then inner True
            else do
              logInfo "Redownload didn't happen"
              pure Nothing
        _ -> pure mres

    inner2 = liftIO
           $ Map.lookup ac . fmap (fmap unCabalString . unCabalStringMap)
         <$> decodeFileThrow (toFilePath dest)

globalsFromDump ::
       (HasLogFunc env, HasProcessContext env)
    => ActualCompiler
    -> RIO env (Map PackageName GlobalPackage)
globalsFromDump compiler = do
    let pkgConduit =
            conduitDumpPackage .|
            CL.foldMap (\dp -> Map.singleton (dpGhcPkgId dp) dp)
        toGlobals ds = Map.fromList $ map toGlobal $ Map.elems ds
        toGlobal d =
            ( pkgName $ dpPackageIdent d
            , GlobalPackage (pkgVersion $ dpPackageIdent d))
    toGlobals <$> ghcPkgDump (whichCompiler compiler) [] pkgConduit

globalsFromHints ::
       HasConfig env
    => ActualCompiler
    -> RIO env (Map PackageName GlobalPackage)
globalsFromHints compiler = do
    ghfp <- globalHintsFile
    mglobalHints <- loadGlobalHints ghfp compiler
    case mglobalHints of
        Just hints -> pure $ Map.map GlobalPackage hints
        Nothing -> do
            logWarn $ "Unable to load global hints for " <> RIO.display compiler
            pure mempty

toActual ::
       (HasConfig env)
    => SMWanted
    -> WithDownloadCompiler
    -> ActualCompiler
    -> RIO env SMActual
toActual smw downloadCompiler compiler = do
    allGlobals <-
        case downloadCompiler of
            WithDownloadCompiler -> globalsFromDump compiler
            SkipDownloadCompiler -> globalsFromHints compiler
    let globals =
            allGlobals `Map.difference` smwProject smw `Map.difference` smwDeps smw
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
            getUnusedPackageFlags (pname, flags) source prjPackages deps
    unless (null unusedFlags) $
        throwM $ InvalidFlagSpecification $ Set.fromList unusedFlags

getUnusedPackageFlags ::
       MonadIO m
    => (PackageName, Map FlagName Bool)
    -> FlagSource
    -> Map PackageName ProjectPackage
    -> Map PackageName DepPackage
    -> m (Maybe UnusedFlags)
getUnusedPackageFlags (name, userFlags) source prj deps =
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
                unused = Map.keysSet $ Map.withoutKeys userFlags pkgFlags
            if Set.null unused
                    -- All flags are defined, nothing to do
                    then pure Nothing
                    -- Error about the undefined flags
                    else pure $ Just $ UFFlagsNotDefined source pname pkgFlags unused
