{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Functions related to Stack's @unpack@ command.
module Stack.Unpack
  ( UnpackOpts (..)
  , UnpackTarget
  , unpackCmd
  , unpackPackages
  ) where

import           Data.List.Extra ( notNull )
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
import           Stack.Types.Config ( Config (..), HasConfig, configL )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Unpack" module.
data UnpackPrettyException
  = UnpackDirectoryAlreadyExists (Set (Path Abs Dir))
  | CouldNotParsePackageSelectors [StyleDoc]
  | PackageCandidatesRequireVersions [PackageName]
  | PackageLocationInvalid PackageIdentifierRevision
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
  pretty (PackageCandidatesRequireVersions names) =
    "[S-6114]"
    <> line
    <> flow "Package candidates to unpack cannot be identified by name only. \
            \The following do not specify a version:"
    <> line
    <> bulletedList (map fromPackageName names)
  pretty (PackageLocationInvalid pir) =
    "[S-5170]"
    <> line
    <> fillSep
         [ flow "While trying to unpack"
         , style Target (fromString $ T.unpack $ textDisplay pir) <> ","
         , flow "Stack encountered an error."
         ]

instance Exception UnpackPrettyException

-- | Type synonymn representing packages to be unpacked by the @stack unpack@
-- command, identified either by name only or by an identifier (including
-- Hackage revision).
type UnpackTarget = Either PackageName PackageIdentifierRevision

-- | Type representing options for the @stack unpack@ command.
data UnpackOpts = UnpackOpts
  { upoptsTargets :: [UnpackTarget]
    -- ^ The packages or package candidates to be unpacked.
  , upoptsAreCandidates :: Bool
    -- ^ Whether the targets are Hackage package candidates.
  , upoptsDest :: Maybe (SomeBase Dir)
    -- ^ The optional directory into which a target will be unpacked into a
    -- subdirectory.
  }

-- | Function underlying the @stack unpack@ command. Unpack packages or package
-- candidates to the filesystem.
unpackCmd ::
     UnpackOpts
  -> RIO Runner ()
unpackCmd (UnpackOpts targets areCandidates Nothing) =
  unpackCmd (UnpackOpts targets areCandidates (Just $ Rel relDirRoot))
unpackCmd (UnpackOpts targets areCandidates (Just dstPath)) =
  withConfig NoReexec $ do
    mresolver <- view $ globalOptsL . to (.resolver)
    mSnapshot <- forM mresolver $ \resolver -> do
      concrete <- makeConcreteResolver resolver
      loc <- completeSnapshotLocation concrete
      loadSnapshot loc
    dstPath' <- case dstPath of
      Abs path -> pure path
      Rel path -> do
        wd <- getCurrentDir
        pure $ wd </> path
    unpackPackages mSnapshot dstPath' targets areCandidates

-- | Intended to work for the command line command.
unpackPackages ::
     forall env.
       (HasConfig env, HasPantryConfig env, HasProcessContext env, HasTerm env)
  => Maybe RawSnapshot -- ^ When looking up by name, take from this build plan.
  -> Path Abs Dir -- ^ Destination.
  -> [UnpackTarget]
  -> Bool
     -- ^ Whether the targets are package candidates.
  -> RIO env ()
unpackPackages mSnapshot dest targets areCandidates = do
  let (names, pirs) = partitionEithers targets
      pisWithRevisions = any hasRevision pirs
      hasRevision (PackageIdentifierRevision _ _ CFILatest) = False
      hasRevision _ = True
  when (areCandidates && notNull names) $
    prettyThrowIO $ PackageCandidatesRequireVersions names
  when (areCandidates && pisWithRevisions) $
    prettyWarn $
         flow "Package revisions are not meaningful for package candidates and \
              \will be ignored."
      <> line
  locs1 <- forM pirs $ \pir -> do
    hackageBaseUrl <- view $ configL . to (.hackageBaseUrl)
    let rpli = if areCandidates
          then
            let -- Ignoring revisions for package candidates.
                PackageIdentifierRevision candidateName candidateVersion _ = pir
                candidatePkgId =
                  PackageIdentifier candidateName candidateVersion
                candidatePkgIdText =
                  T.pack $ packageIdentifierString candidatePkgId
                candidateUrl =
                     hackageBaseUrl
                  <> "package/"
                  <> candidatePkgIdText
                  <> "/candidate/"
                  <> candidatePkgIdText
                  <> ".tar.gz"
                candidateLoc = ALUrl candidateUrl
                candidateArchive = RawArchive candidateLoc Nothing Nothing ""
                candidateMetadata = RawPackageMetadata Nothing Nothing Nothing
            in RPLIArchive candidateArchive candidateMetadata
          else RPLIHackage pir Nothing
    loc <- cplComplete <$> completePackageLocation rpli
      `catch` \(_ :: SomeException) -> prettyThrowIO $ PackageLocationInvalid pir
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
  toLoc name | Just snapshot <- mSnapshot = toLocSnapshot snapshot name
             | otherwise = do
                 void $ updateHackageIndex $ Just "Updating the package index."
                 toLocNoSnapshot name

  toLocNoSnapshot ::
       PackageName
    -> RIO env (Either StyleDoc (PackageLocationImmutable, PackageIdentifier))
  toLocNoSnapshot name = do
    mLoc <- getLatestHackageLocation
      YesRequireHackageIndex
      name
      UsePreferredVersions
    case mLoc of
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
