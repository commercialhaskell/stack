module Curator.Snapshot
  ( makeSnapshot
  ) where

import RIO
import RIO.Process
import Curator.Types
import Pantry
import qualified RIO.Map as Map
import Distribution.Types.VersionRange (withinRange)

makeSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Constraints
  -> Text -- ^ name
  -> RIO env SnapshotLayer
makeSnapshot cons name = do
  locs <- traverseValidate (uncurry toLoc) $ Map.toList $ consPackages cons
  pure SnapshotLayer
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
