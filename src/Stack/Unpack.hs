{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Unpack
  ( unpackPackages
  ) where

import           Path ( (</>), parseRelDir )
import           Path.IO ( doesDirExist )
import           RIO.List ( intercalate )
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Stack.Prelude

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Unpack" module.
data UnpackException
  = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
  | CouldNotParsePackageSelectors [String]
  deriving (Show, Typeable)

instance Exception UnpackException where
    displayException (UnpackDirectoryAlreadyExists dirs) = unlines
        $ "Error: [S-3515]"
        : "Unable to unpack due to already present directories:"
        : map (("    " ++) . toFilePath) (Set.toList dirs)
    displayException (CouldNotParsePackageSelectors strs) = unlines
        $ "Error: [S-2628]"
        : "The following package selectors are not valid package names or \
          \identifiers:"
        : map ("- " ++) strs

-- | Intended to work for the command line command.
unpackPackages
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Maybe RawSnapshot -- ^ when looking up by name, take from this build plan
  -> Path Abs Dir -- ^ destination
  -> [String] -- ^ names or identifiers
  -> RIO env ()
unpackPackages mSnapshot dest input = do
    let (errs1, (names, pirs1)) =
          fmap partitionEithers $ partitionEithers $ map parse input
    locs1 <- forM pirs1 $ \pir -> do
      loc <- fmap cplComplete $ completePackageLocation $ RPLIHackage pir Nothing
      pure (loc, packageLocationIdent loc)
    (errs2, locs2) <- partitionEithers <$> traverse toLoc names
    case errs1 ++ errs2 of
      [] -> pure ()
      errs -> throwM $ CouldNotParsePackageSelectors errs
    locs <- Map.fromList <$> mapM
          (\(pir, ident) -> do
              suffix <- parseRelDir $ packageIdentifierString ident
              pure (pir, dest </> suffix)
          )
          (locs1 ++ locs2)

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
    toLoc | Just snapshot <- mSnapshot = toLocSnapshot snapshot
          | otherwise = toLocNoSnapshot

    toLocNoSnapshot :: PackageName -> RIO env (Either String (PackageLocationImmutable, PackageIdentifier))
    toLocNoSnapshot name = do
      mloc1 <- getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
      mloc <-
        case mloc1 of
          Just _ -> pure mloc1
          Nothing -> do
            updated <- updateHackageIndex $ Just $ "Could not find package " <> fromString (packageNameString name) <> ", updating"
            case updated of
              UpdateOccurred -> getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
              NoUpdateOccurred -> pure Nothing
      case mloc of
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
        Just loc -> pure $ Right (loc, packageLocationIdent loc)

    toLocSnapshot :: RawSnapshot -> PackageName -> RIO env (Either String (PackageLocationImmutable, PackageIdentifier))
    toLocSnapshot snapshot name =
        case Map.lookup name (rsPackages snapshot) of
          Nothing ->
            pure $ Left $ "Package does not appear in snapshot: " ++ packageNameString name
          Just sp -> do
            loc <- cplComplete <$> completePackageLocation (rspLocation sp)
            pure $ Right (loc, packageLocationIdent loc)

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
