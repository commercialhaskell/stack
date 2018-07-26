{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stack.Unpack
  ( unpackPackages
  ) where

import Stack.Prelude
import Stack.Types.BuildPlan
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import RIO.Directory (doesDirectoryExist)
import RIO.List (intercalate)
import RIO.FilePath ((</>))

data UnpackException
  = UnpackDirectoryAlreadyExists (Set FilePath)
  | CouldNotParsePackageSelectors [String]
    deriving Typeable
instance Exception UnpackException
instance Show UnpackException where
    show (UnpackDirectoryAlreadyExists dirs) = unlines
        $ "Unable to unpack due to already present directories:"
        : map ("    " ++) (Set.toList dirs)
    show (CouldNotParsePackageSelectors strs) =
        "The following package selectors are not valid package names or identifiers: " ++
        intercalate ", " strs

-- | Intended to work for the command line command.
unpackPackages
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
  -> FilePath -- ^ destination
  -> [String] -- ^ names or identifiers
  -> RIO env ()
unpackPackages mSnapshotDef dest input = do
    let (errs1, (names, pirs1)) =
          fmap partitionEithers $ partitionEithers $ map parse input
    (errs2, locs2) <- fmap partitionEithers $ traverse toLoc names
    case errs1 ++ errs2 of
      [] -> pure ()
      errs -> throwM $ CouldNotParsePackageSelectors errs
    let locs = Map.fromList $ map
          (\(pir, PackageIdentifier name version) ->
               ( pir
               , dest </> displayC (PackageIdentifier name version)
               )
          )
          (map (\pir@(PackageIdentifierRevision name ver _) ->
                  (PLHackage pir, PackageIdentifier name ver)) pirs1 ++
           locs2)

    alreadyUnpacked <- filterM doesDirectoryExist $ Map.elems locs

    unless (null alreadyUnpacked) $
        throwM $ UnpackDirectoryAlreadyExists $ Set.fromList alreadyUnpacked

    forM_ (Map.toList locs) $ \(loc, dest') -> do
      unpackPackageLocation dest' loc
      logInfo $
        "Unpacked " <>
        display loc <>
        " to " <>
        fromString dest'
  where
    toLoc = maybe toLocNoSnapshot toLocSnapshot mSnapshotDef

    toLocNoSnapshot :: PackageName -> RIO env (Either String (PackageLocation, PackageIdentifier))
    toLocNoSnapshot name = do
      mver1 <- getLatestHackageVersion name
      mver <-
        case mver1 of
          Just _ -> pure mver1
          Nothing -> do
            updated <- updateHackageIndex $ Just $ "Could not find package " <> displayC name <> ", updating"
            if updated
              then getLatestHackageVersion name
              else pure Nothing
      pure $
        case mver of
          -- consider updating the index
          Nothing -> Left $ "Could not find package " ++ displayC name
          Just (ver, _rev, cabalHash) -> Right
            ( PLHackage $ PackageIdentifierRevision name ver (CFIHash cabalHash)
            , PackageIdentifier name ver
            )

    toLocSnapshot :: SnapshotDef -> PackageName -> RIO env (Either String (PackageLocation, PackageIdentifier))
    toLocSnapshot sd name =
        go $ sdLocations sd
      where
        go [] = pure $ Left $ "Package does not appear in snapshot: " ++ displayC name
        go (loc:locs) = do
          ident@(PackageIdentifier name' _) <- getPackageLocationIdent loc
          if name == name'
            then pure $ Right (loc, ident)
            else go locs

    -- Possible future enhancement: parse names as name + version range
    parse s =
        case parsePackageName (T.unpack t) of
            Just x -> Right $ Left x
            Nothing ->
                case parsePackageIdentifierRevision t of
                    Right x -> Right $ Right x
                    Left _ -> Left s
      where
        t = T.pack s

{- FIXME
-- | Resolve a set of package names and identifiers into @FetchPackage@ values.
resolvePackages :: HasCabalLoader env
                => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
                -> [PackageIdentifierRevision]
                -> Set PackageName
                -> RIO env [ResolvedPackage]
resolvePackages mSnapshotDef idents0 names0 = do
    eres <- go
    case eres of
        Left _ -> do
            updateAllIndices
            go >>= either throwM return
        Right x -> return x
  where
    go = r <$> getUses00Index <*> resolvePackagesAllowMissing mSnapshotDef idents0 names0
    r uses00Index (missingNames, missingIdents, idents)
      | not $ Set.null missingNames  = Left $ UnknownPackageNames       missingNames
      | not $ HashSet.null missingIdents = Left $ UnknownPackageIdentifiers missingIdents "" uses00Index
      | otherwise                    = Right idents
-}
