{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions related to Stack's @unpack@ command.
module Stack.Unpack
  ( unpackCmd
  , unpackPackages
  ) where

import           Path ( (</>), parseRelDir )
import           Path.IO ( doesDirExist, resolveDir' )
import           Pantry ( loadSnapshot )
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Stack.Config ( makeConcreteResolver )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Unpack" module.
data UnpackPrettyException
  = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
  | CouldNotParsePackageSelectors [StyleDoc]
  deriving (Show, Typeable)

instance Pretty UnpackPrettyException where
  pretty (UnpackDirectoryAlreadyExists dirs) =
    "[S-3515]"
    <> line
    <> flow "Stack was unable to unpack due to directories already being \
            \present:"
    <> line
    <> bulletedList (map pretty $ Set.toList dirs)
  pretty (CouldNotParsePackageSelectors errs) =
    "[S-2628]"
    <> line
    <> flow "The following package selectors are not valid package names or \
            \identifiers:"
    <> line
    <> bulletedList errs

instance Exception UnpackPrettyException

-- | Function underlying the @stack unpack@ command. Unpack packages to the
-- filesystem.
unpackCmd ::
     ([String], Maybe Text)
     -- ^ A pair of a list of names or identifiers and an optional destination
     -- path.
  -> RIO Runner ()
unpackCmd (names, Nothing) = unpackCmd (names, Just ".")
unpackCmd (names, Just dstPath) = withConfig NoReexec $ do
  mresolver <- view $ globalOptsL.to globalResolver
  mSnapshot <- forM mresolver $ \resolver -> do
    concrete <- makeConcreteResolver resolver
    loc <- completeSnapshotLocation concrete
    loadSnapshot loc
  dstPath' <- resolveDir' $ T.unpack dstPath
  unpackPackages mSnapshot dstPath' names

-- | Intended to work for the command line command.
unpackPackages ::
     forall env. (HasPantryConfig env, HasProcessContext env, HasTerm env)
  => Maybe RawSnapshot -- ^ When looking up by name, take from this build plan.
  -> Path Abs Dir -- ^ Destination.
  -> [String] -- ^ Names or identifiers.
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
    errs -> prettyThrowM $ CouldNotParsePackageSelectors errs
  locs <- Map.fromList <$> mapM
    (\(pir, ident) -> do
        suffix <- parseRelDir $ packageIdentifierString ident
        pure (pir, dest </> suffix)
    )
    (locs1 ++ locs2)

  alreadyUnpacked <- filterM doesDirExist $ Map.elems locs

  unless (null alreadyUnpacked) $
    prettyThrowM $ UnpackDirectoryAlreadyExists $ Set.fromList alreadyUnpacked

  forM_ (Map.toList locs) $ \(loc, dest') -> do
    unpackPackageLocation dest' loc
    prettyInfoL
      [ "Unpacked"
      , fromString $ T.unpack $ textDisplay loc
      , "to"
      , pretty dest' <> "."
      ]
 where
  toLoc | Just snapshot <- mSnapshot = toLocSnapshot snapshot
        | otherwise = toLocNoSnapshot

  toLocNoSnapshot ::
       PackageName
    -> RIO env (Either StyleDoc (PackageLocationImmutable, PackageIdentifier))
  toLocNoSnapshot name = do
    mloc1 <- getLatestHackageLocation
      YesRequireHackageIndex
      name
      UsePreferredVersions
    mloc <-
      case mloc1 of
        Just _ -> pure mloc1
        Nothing -> do
          updated <- updateHackageIndex
            $ Just
            $    "Could not find package "
              <> fromString (packageNameString name)
              <> ", updating"
          case updated of
            UpdateOccurred ->
              getLatestHackageLocation
                YesRequireHackageIndex
                name
                UsePreferredVersions
            NoUpdateOccurred -> pure Nothing
    case mloc of
      Nothing -> do
        candidates <- getHackageTypoCorrections name
        pure $ Left $ fillSep
          [ flow "Could not find package"
          , style Current (fromPackageName name)
          , flow "on Hackage."
          , if null candidates
              then mempty
              else fillSep $
                  flow "Perhaps you meant one of:"
                : mkNarrativeList (Just Good) False
                    (map fromPackageName candidates :: [StyleDoc])
          ]
      Just loc -> pure $ Right (loc, packageLocationIdent loc)

  toLocSnapshot ::
       RawSnapshot
    -> PackageName
    -> RIO env (Either StyleDoc (PackageLocationImmutable, PackageIdentifier))
  toLocSnapshot snapshot name =
    case Map.lookup name (rsPackages snapshot) of
      Nothing ->
        pure $ Left $ fillSep
          [ flow "Package does not appear in snapshot:"
          , style Current (fromPackageName name) <> "."
          ]
      Just sp -> do
        loc <- cplComplete <$> completePackageLocation (rspLocation sp)
        pure $ Right (loc, packageLocationIdent loc)

  -- Possible future enhancement: parse names as name + version range
  parse s =
    case parsePackageName s of
      Just x -> Right $ Left x
      Nothing ->
        case parsePackageIdentifierRevision (T.pack s) of
          Right x -> Right $ Right x
          Left _ -> Left $ fillSep
            [ flow "Could not parse as package name or identifier:"
            , style Current (fromString s) <> "."
            ]
