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
import RIO.List (intercalate)
import RIO.Process (HasProcessContext)
import Path ((</>), parseRelDir)
import Path.IO (doesDirExist)

data UnpackException
  = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
  | CouldNotParsePackageSelectors [String]
    deriving Typeable
instance Exception UnpackException
instance Show UnpackException where
    show (UnpackDirectoryAlreadyExists dirs) = unlines
        $ "Unable to unpack due to already present directories:"
        : map (("    " ++) . toFilePath) (Set.toList dirs)
    show (CouldNotParsePackageSelectors strs) = unlines
      $ "The following package selectors are not valid package names or identifiers:"
      : map ("- " ++) strs

-- | Intended to work for the command line command.
unpackPackages
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
  -> Path Abs Dir -- ^ destination
  -> [String] -- ^ names or identifiers
  -> RIO env ()
unpackPackages mSnapshotDef dest input = do
    let (errs1, (names, pirs1)) =
          fmap partitionEithers $ partitionEithers $ map parse input
    (errs2, locs2) <- partitionEithers <$> traverse toLoc names
    case errs1 ++ errs2 of
      [] -> pure ()
      errs -> throwM $ CouldNotParsePackageSelectors errs
    locs <- Map.fromList <$> mapM
          (\(pir, ident) -> do
              suffix <- parseRelDir $ packageIdentifierString ident
              pure (pir, dest </> suffix)
          )
          (map (\pir@(PackageIdentifierRevision name ver _) ->
                  (PLIHackage pir Nothing, PackageIdentifier name ver)) pirs1 ++
           locs2)

    alreadyUnpacked <- filterM doesDirExist $ Map.elems locs

    unless (null alreadyUnpacked) $
        throwM $ UnpackDirectoryAlreadyExists $ Set.fromList alreadyUnpacked

    forM_ (Map.toList locs) $ \(loc, dest') -> do
      unpackPackageLocation dest' loc
      logInfo $
        "Unpacked " <>
        display loc <>
        " to " <>
        fromString (toFilePath dest')
  where
    toLoc = maybe toLocNoSnapshot toLocSnapshot mSnapshotDef

    toLocNoSnapshot :: PackageName -> RIO env (Either String (PackageLocationImmutable, PackageIdentifier))
    toLocNoSnapshot name = do
      mver1 <- getLatestHackageVersion name UsePreferredVersions
      mver <-
        case mver1 of
          Just _ -> pure mver1
          Nothing -> do
            updated <- updateHackageIndex $ Just $ "Could not find package " <> fromString (packageNameString name) <> ", updating"
            case updated of
              UpdateOccurred -> getLatestHackageVersion name UsePreferredVersions
              NoUpdateOccurred -> pure Nothing
      case mver of
        Nothing -> do
          candidates <- getHackageTypoCorrections name
          pure $ Left $ concat
            [ "Could not find package "
            , packageNameString name
            , " on Hackage"
            , if null candidates
                then ""
                else ". Perhaps you meant: " ++ intercalate ", " (map packageNameString candidates)
            ]
        Just pir@(PackageIdentifierRevision _ ver _) -> pure $ Right
          ( PLIHackage pir Nothing
          , PackageIdentifier name ver
          )

    toLocSnapshot :: SnapshotDef -> PackageName -> RIO env (Either String (PackageLocationImmutable, PackageIdentifier))
    toLocSnapshot sd name =
        go $ concatMap snapshotLocations $ sdSnapshots sd
      where
        go [] = pure $ Left $ "Package does not appear in snapshot: " ++ packageNameString name
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
                    Left _ -> Left $ "Could not parse as package name or identifier: " ++ s
      where
        t = T.pack s
