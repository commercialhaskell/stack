{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions related to Stack's @unpack@ command.
module Stack.Unpack
  ( unpackCmd
  , unpackPackages
  ) where

import qualified Data.List as L
import           Path ( (</>), parseRelDir )
import           Path.IO ( doesDirExist, resolveDir' )
import           Pantry ( loadSnapshot )
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Stack.Config ( loadProjectConfig, makeConcreteResolver )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           Distribution.Types.PackageName ( unPackageName )

data Unpackable
  = UnpackName PackageName
  | UnpackIdent PackageIdentifierRevision
  | UnpackRepoUrl (PackageName, RawPackageLocationImmutable)

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
  mStackYaml <- view $ globalOptsL.to globalStackYaml
  mresolver <- view $ globalOptsL.to globalResolver
  mSnapshot <- forM mresolver $ \resolver -> do
    concrete <- makeConcreteResolver resolver
    loc <- completeSnapshotLocation concrete
    loadSnapshot loc
  dstPath' <- resolveDir' $ T.unpack dstPath
  unpackPackages mStackYaml mSnapshot dstPath' names

-- | Intended to work for the command line command.
unpackPackages ::
     forall env. (HasPantryConfig env, HasProcessContext env, HasTerm env)
  => StackYamlLoc
  -> Maybe RawSnapshot -- ^ When looking up by name, take from this build plan.
  -> Path Abs Dir -- ^ Destination.
  -> [String] -- ^ Names or identifiers.
  -> RIO env ()
unpackPackages mStackYaml mSnapshot dest input = do
  parsed <- mapM (parse mStackYaml) input
  let (errs1, unpackables) = partitionEithers parsed
  let (names, pirs1, raws) = splitUnpackable unpackables 

  repos <- catMaybes <$> mapM
    (\case
        (name, x@RPLIRepo{}) -> do
            suffix <- parseRelDir $ unPackageName name
            pure $ Just (x, dest </> suffix)
        (_, RPLIHackage{}) -> pure Nothing
        (_, RPLIArchive{}) -> pure Nothing)
    (longestUnique raws)

  forM_ repos $ \(loc, dest') -> do
    unpackPackageLocationRaw dest' loc
    prettyInfoL $ unpackMessage loc dest'

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
    prettyInfoL $ unpackMessage loc dest'
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
          , style Current (fromString $ packageNameString name)
          , flow "on Hackage."
          , if null candidates
              then mempty
              else fillSep $
                  flow "Perhaps you meant one of:"
                : mkNarrativeList (Just Good) False
                    (map (fromString . packageNameString) candidates :: [StyleDoc])
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
          , style Current (fromString $ packageNameString name) <> "."
          ]
      Just sp -> do
        loc <- cplComplete <$> completePackageLocation (rspLocation sp)
        pure $ Right (loc, packageLocationIdent loc)

-- Possible future enhancement: parse names as name + version range
parse ::
     (HasPantryConfig env, HasTerm env)
  => StackYamlLoc -> String -> RIO env (Either StyleDoc Unpackable)
parse mStackYaml s = do
  extra <- toLocExtraDep mStackYaml (fromString s)
  pure $ case extra of
    Just x -> Right $ UnpackRepoUrl x
    Nothing -> case parsePackageName s of
      Just x -> Right $ UnpackName x
      Nothing ->
        case parsePackageIdentifierRevision (T.pack s) of
          Right x -> Right $ UnpackIdent x
          Left _ -> Left $ fillSep
            [ flow "Could not parse as package name or identifier:"
            , style Current (fromString s) <> "."
            ]

toLocExtraDep ::
     (HasPantryConfig env, HasTerm env)
  => StackYamlLoc
  -> PackageName
  -> RIO env (Maybe (PackageName, RawPackageLocationImmutable))
toLocExtraDep mstackYaml name = do
  pc <- loadProjectConfig mstackYaml
  case pc of
    PCGlobalProject -> pure Nothing
    PCNoProject{} -> pure Nothing
    PCProject (Project{projectDependencies}, _, _) -> do
      let hits = mapMaybe (\case
            RPLImmutable (RPLIRepo repo meta@RawPackageMetadata{rpmName = Just n}) -> do
              if n == name then Just (name, (repo, meta)) else Nothing
            RPLImmutable (RPLIRepo repo@Repo{repoUrl} meta) -> do
              if T.isSuffixOf (T.pack $ unPackageName name) repoUrl then Just (name, (repo, meta)) else Nothing
            RPLMutable{} -> Nothing
            RPLImmutable{} -> Nothing) projectDependencies

      case hits of
        [] -> pure Nothing
        [(n, (repo, meta))] -> pure $ Just (n, RPLIRepo repo meta)
        _ -> do
          prettyWarnL
            [ flow "Multiple matches for"
            , style Current (fromString $ packageNameString name) <> ":"
            ]
          forM_ hits $ \case
            (_, (repo, RawPackageMetadata{rpmName})) -> do
              prettyWarnL
                [ style Current (fromString . T.unpack $ repoUrl repo)
                , style Current (fromString $ maybe "" unPackageName rpmName)
                ]
          pure Nothing

splitUnpackable ::
     [Unpackable]
  -> ([PackageName], [PackageIdentifierRevision], [(PackageName, RawPackageLocationImmutable)])
splitUnpackable = foldl' go ([], [], [])
  where
    go (names, pirs, raws) = \case
      UnpackName name -> (name : names, pirs, raws)
      UnpackIdent pir -> (names, pir : pirs, raws)
      UnpackRepoUrl raw -> (names, pirs, raw : raws)

longestUnique ::
     [(PackageName, RawPackageLocationImmutable)]
  -> [(PackageName, RawPackageLocationImmutable)]
longestUnique xs =
  L.concat $ L.groupBy (\(_, p1) (_, p2) -> p1 == p2) (L.take 1 $ L.sortBy (flip (comparing fst)) xs)

unpackMessage :: Display a => a -> Path Abs Dir -> [StyleDoc]
unpackMessage loc dest =
  [ "Unpacked"
  , fromString $ T.unpack $ textDisplay loc
  , "to"
  , pretty dest <> "."
  ]