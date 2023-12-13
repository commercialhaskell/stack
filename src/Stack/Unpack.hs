{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions related to Stack's @unpack@ command.
module Stack.Unpack
  ( UnpackOpts (..)
  , UnpackTarget
  , unpackCmd
  , unpackPackages
  ) where

import           Path ( SomeBase (..), (</>), parseRelDir )
import           Path.IO ( doesDirExist, getCurrentDir )
import           Pantry ( loadSnapshot )
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import qualified RIO.Set as Set
import qualified RIO.Text as T
import           Stack.Config ( makeConcreteResolver )
import           Stack.Constants ( relDirRoot )
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

-- | Type synonymn representing packages to be unpacked by the @stack unpack@
-- command, identified either by name only or by an identifier (including
-- Hackage revision).
type UnpackTarget = Either PackageName PackageIdentifierRevision

-- | Type representing options for the @stack unpack@ command.
data UnpackOpts = UnpackOpts
  { upoptsTargets :: [UnpackTarget]
    -- ^ The packages to be unpacked.
  , upOptsDest :: Maybe (SomeBase Dir)
    -- ^ The optional directory into which a package will be unpacked into a
    -- subdirectory.
  }

-- | Function underlying the @stack unpack@ command. Unpack packages to the
-- filesystem.
unpackCmd ::
     UnpackOpts
  -> RIO Runner ()
unpackCmd (UnpackOpts names Nothing) =
  unpackCmd (UnpackOpts names (Just $ Rel relDirRoot))
unpackCmd (UnpackOpts names (Just dstPath)) = withConfig NoReexec $ do
  mresolver <- view $ globalOptsL.to globalResolver
  mSnapshot <- forM mresolver $ \resolver -> do
    concrete <- makeConcreteResolver resolver
    loc <- completeSnapshotLocation concrete
    loadSnapshot loc
  dstPath' <- case dstPath of
    Abs path -> pure path
    Rel path -> do
      wd <- getCurrentDir
      pure $ wd </> path
  unpackPackages mSnapshot dstPath' names

-- | Intended to work for the command line command.
unpackPackages ::
     forall env. (HasPantryConfig env, HasProcessContext env, HasTerm env)
  => Maybe RawSnapshot -- ^ When looking up by name, take from this build plan.
  -> Path Abs Dir -- ^ Destination.
  -> [UnpackTarget]
  -> RIO env ()
unpackPackages mSnapshot dest input = do
  let (names, pirs) = partitionEithers input
  locs1 <- forM pirs $ \pir -> do
    loc <- fmap cplComplete $ completePackageLocation $ RPLIHackage pir Nothing
    pure (loc, packageLocationIdent loc)
  (errs, locs2) <- partitionEithers <$> traverse toLoc names
  unless (null errs) $ prettyThrowM $ CouldNotParsePackageSelectors errs
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
