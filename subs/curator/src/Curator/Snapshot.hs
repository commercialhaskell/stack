{-# LANGUAGE OverloadedStrings #-}
module Curator.Snapshot
  ( makeSnapshot
  , checkDependencyGraph
  ) where

import Curator.Types
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text as DT
import Distribution.Types.Dependency (depPkgName, depVerRange, Dependency)
import Distribution.Types.UnqualComponentName (unqualComponentNameToPackageName)
import Distribution.Types.VersionRange (withinRange)
import Pantry
import Path.IO (resolveFile')
import RIO
import qualified RIO.Map as Map
import RIO.PrettyPrint
import RIO.Process
import qualified RIO.Set as Set

makeSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Constraints
  -> Text -- ^ name
  -> RIO env SnapshotLayer
makeSnapshot cons name = do
    locs <-
        traverseValidate (uncurry toLoc) $
        Map.toList $ consPackages cons
    pure
        SnapshotLayer
        { slParent = SLCompiler $ WCGhc $ consGhcVersion cons
        , slCompiler = Nothing
        , slName = name
        , slLocations = catMaybes locs
        , slDropPackages = mempty
        , slFlags = Map.mapMaybe getFlags (consPackages cons)
        , slHidden = Map.filter id (pcHide <$> consPackages cons)
        , slGhcOptions = mempty
        }

getFlags :: PackageConstraints -> Maybe (Map FlagName Bool)
getFlags pc
  | Map.null (pcFlags pc) = Nothing
  | otherwise = Just (pcFlags pc)

toLoc
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> PackageConstraints
  -> RIO env (Maybe PackageLocationImmutable)
toLoc name pc =
  case pcSource pc of
    PSHackage (HackageSource mrange mrequiredLatest revisions) -> do
      versions <- getHackagePackageVersions IgnorePreferredVersions name -- don't follow the preferred versions on Hackage, give curators more control
      when (Map.null versions) $ error $ "Package not found on Hackage: " ++ packageNameString name
      for_ mrequiredLatest $ \required ->
        case Map.maxViewWithKey versions of
          Nothing -> error $ "No versions found for " ++ packageNameString name
          Just ((version, _), _)
            | version == required -> pure ()
            | otherwise -> error $ concat
                [ "For package "
                , fromString (packageNameString name)
                , ", required latest version to be "
                , fromString (versionString required)
                , ", but actual latest is "
                , fromString (versionString version)
                ]
      let versions' =
            case mrange of
              Nothing -> versions
              Just range -> Map.filterWithKey (\v _ -> v `withinRange` range) versions
      case Map.maxViewWithKey versions' of
        Nothing -> pure Nothing -- argument could be made for erroring out, but currently used by curators to mean "don't include this"...
        Just ((version, revs), _) -> do
          let viewer =
                case revisions of
                  NoRevisions -> Map.minView
                  UseRevisions -> Map.maxView
          cfi <-
            case viewer revs of
              Nothing -> error $ "Impossible! No revisions found for " ++ show (name, version)
              Just (BlobKey sha size, _) -> pure $ CFIHash sha $ Just size
          pure $ Just $ PLIHackage (PackageIdentifierRevision name version cfi) Nothing

traverseValidate
  :: (MonadUnliftIO m, Traversable t)
  => (a -> m b)
  -> t a
  -> m (t b)
traverseValidate f t = do
  errsRef <- newIORef id
  let f' a = f a `catchAny` \e -> do
        modifyIORef' errsRef $ (. (e:))
        pure $ impureThrow e -- should never be called
  res <- traverse f' t
  errs <- ($ []) <$> readIORef errsRef
  case errs of
    [] -> pure res
    [x] -> throwIO x
    _ -> throwIO $ TraverseValidateExceptions errs
  
newtype TraverseValidateExceptions = TraverseValidateExceptions [SomeException]
  deriving (Show, Typeable)
instance Exception TraverseValidateExceptions

checkDependencyGraph ::
       (HasTerm env, HasProcessContext env, HasPantryConfig env)
    => Constraints
    -> Snapshot
    -> RIO env ()
checkDependencyGraph constraints snapshot = do
    globalHintsYaml <- resolveFile' "global-hints.yaml"
    let compiler = snapshotCompiler snapshot
        compilerVer = case compiler of
          WCGhc v -> v
          WCGhcjs _ _ -> error "GHCJS is not supported"
    mhints <- loadGlobalHints globalHintsYaml compiler
    ghcBootPackages <- case mhints of
      Nothing ->
        error $ "Cannot load global hints for GHC " <> DT.display compilerVer
      Just hints ->
        return $ Map.map Just hints
    let snapshotVersion (PLIHackage (PackageIdentifierRevision _ v _) _) = Just v
        snapshotVersion _ = Nothing
        declared =
            Map.fromList
                [ (pn, snapshotVersion (spLocation sp))
                | (pn, sp) <- Map.toList (snapshotPackages snapshot)
                ] <>
            ghcBootPackages
    errors <- Map.filter (not . null) <$> Map.traverseWithKey
              (validateDeps constraints declared)
              (snapshotPackages snapshot)
    unless (Map.null errors) $ do
      logWarn "Errors in snapshot:"
      void $ flip Map.traverseWithKey errors $ \pname perrors -> do
        logWarn $ "Package " <> fromString (packageNameString pname) <> ":"
        forM_ perrors $ \(ErrorMessage err) ->
          logWarn $ " - " <> fromString err

newtype ErrorMessage = ErrorMessage String

validateDeps ::
       (HasProcessContext env, HasLogFunc env, HasPantryConfig env)
    => Constraints
    -> Map PackageName (Maybe Version)
    -> PackageName
    -> SnapshotPackage
    -> RIO env [ErrorMessage]
validateDeps constraints declared pname sp = do
    let mpc = Map.lookup pname (consPackages constraints)
    gpd <- loadCabalFileImmutable (spLocation sp)
    let skipBuild = maybe False pcSkipBuild mpc
        skipTest = skipBuild || (maybe False ((== CASkip) . pcTests) mpc)
        skipBench = skipBuild || (maybe False ((== CASkip) . pcBenchmarks) mpc)
        toCheck skip condTree = (skip, C.condTreeConstraints condTree)
        sublibraries = Set.fromList $
          map (unqualComponentNameToPackageName. fst) (C.condSubLibraries gpd)
        checks = maybe [] (\ltree -> [toCheck skipBuild ltree]) (C.condLibrary gpd) ++
                 map (toCheck skipBuild . snd) (C.condExecutables gpd) ++
                 map (toCheck skipTest . snd) (C.condTestSuites gpd) ++
                 map (toCheck skipBench . snd) (C.condBenchmarks gpd)
    return $ catMaybes [ checkDependency sublibraries dep
                       | (skip, deps) <- checks, not skip
                       , dep <- deps]
  where
    checkDependency :: Set PackageName -> Dependency -> Maybe ErrorMessage
    checkDependency sublibraries dep =
      let depName = depPkgName dep
          depRange = depVerRange dep
          errMsg = Just . ErrorMessage
      in if Set.member depName sublibraries
         then Nothing
         else case Map.lookup depName declared of
          Nothing ->
            errMsg $ "Dependency '" ++ packageNameString depName ++
                "'  was not found in constraints.yaml"
          Just Nothing ->
            Nothing -- version unknown, can't say anything about it
          Just (Just version) ->
            if withinRange version depRange
            then Nothing
            else errMsg $ "Snapshot version " ++ DT.display version ++ " for dependency '" ++
                          packageNameString depName ++ "' doesn't appear within bounds range " ++
                          DT.display depRange
