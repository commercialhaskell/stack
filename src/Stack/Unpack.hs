{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  :: (HasPantryConfig env, HasLogFunc env)
  => Maybe SnapshotDef -- ^ when looking up by name, take from this build plan
  -> FilePath -- ^ destination
  -> [String] -- ^ names or identifiers
  -> RIO env ()
unpackPackages mSnapshotDef dest input = do
    let (errs1, (names, pirs1)) =
          fmap partitionEithers $ partitionEithers $ map parse input
    (errs2, pirs2) <- fmap partitionEithers $ traverse toPIR names
    case errs1 ++ errs2 of
      [] -> pure ()
      errs -> throwM $ CouldNotParsePackageSelectors errs
    let pirs = Map.fromList $ map
          (\pir@(PackageIdentifierRevision name version _) ->
               ( pir
               , dest </> displayC (PackageIdentifier name version)
               )
          )
          (pirs1 ++ pirs2)

    alreadyUnpacked <- filterM doesDirectoryExist $ Map.elems pirs

    unless (null alreadyUnpacked) $
        throwM $ UnpackDirectoryAlreadyExists $ Set.fromList alreadyUnpacked

    forM_ (Map.toList pirs) $ \(pir, dest') -> do
      unpackPackageLocation dest' (PLHackage pir)
      logInfo $
        "Unpacked " <>
        display pir <>
        " to " <>
        fromString dest'
  where
    toPIR = maybe toPIRNoSnapshot toPIRSnapshot mSnapshotDef

    toPIRNoSnapshot name = do
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
          Just (ver, _rev, cabalHash) -> Right $ PackageIdentifierRevision
            name
            ver
            (CFIHash cabalHash)

    toPIRSnapshot :: Monad m => SnapshotDef -> PackageName -> m (Either String PackageIdentifierRevision)
    toPIRSnapshot sd name =
        pure $
          case mapMaybe go $ sdLocations sd of
            [] -> Left $ "Package does not appear in snapshot: " ++ displayC name
            pir:_ -> Right pir
      where
        -- FIXME should work for things besides PLHackage
        go (PLHackage pir@(PackageIdentifierRevision name' _ _))
          | name' == name = Just pir
        go _ = Nothing

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
