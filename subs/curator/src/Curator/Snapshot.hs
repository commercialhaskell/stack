module Curator.Snapshot
  ( makeSnapshot
  ) where

import RIO
import RIO.Process
import Curator.Types
import Pantry
import qualified RIO.Map as Map
import Distribution.Types.VersionRange (withinRange)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T

makeSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Constraints
  -> Text -- ^ name
  -> RIO env Snapshot
makeSnapshot cons name = do
  hints <- getGlobalHints $ consGhcVersion cons
  locs <- traverseValidate (uncurry toLoc) $ Map.toList $ consPackages cons
  pure Snapshot
    { snapshotParent = SLCompiler $ WCGhc $ consGhcVersion cons
    , snapshotName = name
    , snapshotLocations = catMaybes locs
    , snapshotDropPackages = mempty
    , snapshotFlags = Map.mapMaybe getFlags (consPackages cons)
    , snapshotHidden = Map.filter id (pcHide <$> consPackages cons)
    , snapshotGhcOptions = mempty
    , snapshotGlobalHints = hints
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
      versions <- getPackageVersions NoPreferredVersions name -- don't follow the preferred versions on Hackage, give curators more control
      when (Map.null versions) $ error $ "Package not found on Hackage: " ++ displayC name
      for_ mrequiredLatest $ \required ->
        case Map.maxViewWithKey versions of
          Nothing -> error $ "No versions found for " ++ displayC name
          Just ((version, _), _)
            | version == required -> pure ()
            | otherwise -> error $ concat
                [ "For package "
                , displayC name
                , ", required latest version to be "
                , displayC required
                , ", but actual latest is "
                , displayC version
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

getGlobalHints
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Version -- ^ GHC version
  -> RIO env (Map PackageName (Maybe Version))
getGlobalHints version = do
  let cmd = "ghc-pkg-" ++ displayC version
  lbs <- proc cmd ["list", "--global", "--simple-output"] readProcessStdout_
  text <- either throwIO pure $ decodeUtf8' $ BL.toStrict lbs
  Map.fromList <$> for (T.words text) (\t ->
    case parsePackageIdentifier $ T.unpack t of
      Just (PackageIdentifier n v) -> pure (n, Just v)
      Nothing -> error $ "Invalid package identifier for global hints: " ++ show t)

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
