{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Pantry
  ( -- * Configuration
    PantryConfig
  , HackageSecurityConfig (..)
  , defaultHackageSecurityConfig
  , HasPantryConfig (..)
  , withPantryConfig

    -- ** Lenses
  , hpackExecutableL

    -- * Types
  , StaticSHA256
  , CabalFileInfo (..)
  , Revision (..)
  , FileSize (..)
  , PackageLocation (..)
  , Archive (..)
  , ArchiveLocation (..)
  , Repo (..)
  , RepoType (..)
  , RelFilePath (..)
  , PackageLocationImmutable (..)
  , ResolvedPath (..)
  , PackageIdentifierRevision (..)
  , PackageName
  , Version
  , PackageIdentifier (..)
  , FlagName
  , TreeKey (..)
  , BlobKey (..)
  , HpackExecutable (..)
  , PackageMetadata (..)
  , PantryException (..)

    -- ** Unresolved package locations
  , UnresolvedPackageLocation
  , UnresolvedPackageLocationImmutable (..)
  , resolvePackageLocation
  , resolvePackageLocationImmutable
  , mkUnresolvedPackageLocation
  , mkUnresolvedPackageLocationImmutable
  , completePackageLocation
  , loadPackageLocation

    -- ** Snapshots
  , UnresolvedSnapshotLocation
  , resolveSnapshotLocation
  , unresolveSnapshotLocation
  , SnapshotLocation (..)
  , Snapshot (..)
  , WantedCompiler (..)
  , parseWantedCompiler
  , completeSnapshot
  , completeSnapshotLocation
  , loadPantrySnapshot
  , parseSnapshotLocation
  , ltsSnapshotLocation
  , nightlySnapshotLocation

    -- ** Cabal helpers
  , parsePackageIdentifier
  , parsePackageName
  , parseFlagName
  , parseVersion
  , displayC
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap
  , gpdPackageIdentifier
  , gpdPackageName
  , gpdVersion

    -- ** Parsers
  , parsePackageIdentifierRevision

    -- * Package location
  , parseCabalFile
  , parseCabalFileImmutable
  , parseCabalFilePath
  , getPackageLocationIdent
  , getPackageLocationTreeKey

    -- * Hackage index
  , updateHackageIndex
  , hackageIndexTarballL
  , getLatestHackageVersion

    -- * Convenience
  , PantryApp
  , runPantryApp
  , runPantryAppClean

    -- * FIXME legacy from Stack, to be updated
  , loadFromIndex
  , getPackageVersions
  , UsePreferredVersions (..)
  , fetchPackages
  , unpackPackageLocation
  ) where

import RIO
import Conduit
import qualified RIO.Map as Map
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import Pantry.Archive
import Pantry.Repo
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import Pantry.Hackage
import Path (Path, Abs, File, toFilePath, Dir, mkRelFile, (</>), filename, parseAbsDir, parent)
import Path.IO (resolveDir, doesFileExist, resolveDir', listDir)
import Distribution.PackageDescription (GenericPackageDescription, FlagName)
import qualified Distribution.PackageDescription as D
import Distribution.Parsec.Common (PWarning (..), showPos)
import qualified Hpack
import qualified Hpack.Config as Hpack
import RIO.Process
import RIO.Directory (getAppUserDataDirectory)
import qualified Data.Yaml as Yaml
import Data.Aeson.Extended (WithJSONWarnings (..), Value)
import Data.Aeson.Types (parseEither)
import Data.Monoid (Endo (..))
import Pantry.HTTP
import qualified Distribution.Text
import Distribution.Types.VersionRange (withinRange)
import qualified RIO.FilePath

withPantryConfig
  :: HasLogFunc env
  => Path Abs Dir -- ^ pantry root
  -> HackageSecurityConfig
  -> HpackExecutable
  -> Int -- ^ connection count
  -> (PantryConfig -> RIO env a)
  -> RIO env a
withPantryConfig root hsc he count inner = do
  env <- ask
  -- Silence persistent's logging output, which is really noisy
  runRIO (mempty :: LogFunc) $ initStorage (root </> $(mkRelFile "pantry.sqlite3")) $ \storage -> runRIO env $ do
    ur <- newMVar True
    inner PantryConfig
      { pcHackageSecurity = hsc
      , pcHpackExecutable = he
      , pcRootDir = root
      , pcStorage = storage
      , pcUpdateRef = ur
      , pcConnectionCount = count
      }

defaultHackageSecurityConfig :: HackageSecurityConfig
defaultHackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds =
      [ "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
      , "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
      , "280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833"
      , "2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201"
      , "2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3"
      , "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
      , "772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d"
      , "aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9"
      , "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
      ]
  , hscKeyThreshold = 3
  , hscDownloadPrefix = "https://hackage.haskell.org/"
  }

lookupPackageIdentifierExact
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env (Maybe ByteString)
lookupPackageIdentifierExact name version cfi =
  withStorage $ loadHackageCabalFile name version cfi

loadFromIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env (Either () ByteString)
loadFromIndex name version cfi = do
  mres <- lookupPackageIdentifierExact name version cfi
  case mres of
    Just bs -> return $ Right bs
    -- Update the cache and try again
    Nothing -> do
      updated <- updateHackageIndex $ Just $
                "Didn't see " <>
                display (PackageIdentifierRevision name version cfi) <>
                " in your package indices.\n" <>
                "Updating and trying again."
      if updated
        then loadFromIndex name version cfi
        else do
            pure $ Left ()
            {- FIXME
            fuzzy <- fuzzyLookupCandidates name version cfi
            let suggestions = case fuzzy of
                    FRNameNotFound Nothing -> ""
                    FRNameNotFound (Just cs) ->
                          "Perhaps you meant " <> orSeparated cs <> "?"
                    FRVersionNotFound cs -> "Possible candidates: " <>
                      commaSeparated (NE.map packageIdentifierText cs)
                      <> "."
                    FRRevisionNotFound cs ->
                      "The specified revision was not found.\nPossible candidates: " <>
                      commaSeparated (NE.map (T.pack . packageIdentifierRevisionString) cs)
                      <> "."
            pure (False, Left $ UnknownPackageIdentifiers
                                  (Set.singleton (name, version, cfi))
                                  suggestions)

orSeparated :: NonEmpty Text -> Text
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = T.intercalate ", " (NE.init xs) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty Text -> Text
commaSeparated = fold . NE.intersperse ", "

data FuzzyResults
  = FRNameNotFound !(Maybe (NonEmpty Text))
  | FRVersionNotFound !(NonEmpty (PackageName, Version))
  | FRRevisionNotFound !(NonEmpty (PackageName, Version, CabalFileInfo))

-- | Given package identifier and package caches, return list of packages
-- with the same name and the same two first version number components found
-- in the caches.
fuzzyLookupCandidates
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> RIO env FuzzyResults
fuzzyLookupCandidates name ver _rev =
  case Map.lookup name caches of
    Nothing -> FRNameNotFound $ typoCorrectionCandidates name (PackageCache caches)
    Just m ->
      case Map.lookup ver m of
        Nothing ->
          case NE.nonEmpty $ filter sameMajor $ Map.keys m of
            Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
            Nothing ->
              case NE.nonEmpty $ Map.keys m of
                Nothing -> error "fuzzyLookupCandidates: no versions"
                Just vers -> FRVersionNotFound $ NE.map (PackageIdentifier name) vers
        Just (_index, _mpd, revisions) ->
          let hashes = concatMap fst $ NE.toList revisions
              pirs = map (PackageIdentifierRevision (PackageIdentifier name ver) . CFIHash Nothing) hashes
           in case NE.nonEmpty pirs of
                Nothing -> error "fuzzyLookupCandidates: no revisions"
                Just pirs' -> FRRevisionNotFound pirs'
  where
    sameMajor v = toMajorVersion v == toMajorVersion ver

-- | Try to come up with typo corrections for given package identifier using
-- package caches. This should be called before giving up, i.e. when
-- 'fuzzyLookupCandidates' cannot return anything.
typoCorrectionCandidates
  :: PackageName
  -> Maybe (NonEmpty Text)
typoCorrectionCandidates name' =
  let name = packageNameText name'
  in  NE.nonEmpty
    . take 10
    . map snd
    . filter (\(distance, _) -> distance < 4)
    . map (\k -> (damerauLevenshtein name (packageNameText k), packageNameText k))
    . Map.keys
    $ cache
-}

-- | Should we pay attention to Hackage's preferred versions?
data UsePreferredVersions = YesPreferredVersions | NoPreferredVersions
  deriving Show

-- | Returns the versions of the package available on Hackage.
getPackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => UsePreferredVersions
  -> PackageName -- ^ package name
  -> RIO env (Map Version (Map Revision BlobKey))
getPackageVersions usePreferred name = withStorage $ do
  mpreferred <-
    case usePreferred of
      YesPreferredVersions -> loadPreferredVersion name
      NoPreferredVersions -> pure Nothing
  let predicate :: Version -> Map Revision BlobKey -> Bool
      predicate = fromMaybe (\_ _ -> True) $ do
        preferredT1 <- mpreferred
        preferredT2 <- T.stripPrefix (displayC name) preferredT1
        vr <- Distribution.Text.simpleParse $ T.unpack preferredT2
        Just $ \v _ -> withinRange v vr
  Map.filterWithKey predicate <$> loadHackagePackageVersions name

-- | Returns the latest version of the given package available from
-- Hackage. Uses preferred versions to ignore packages.
getLatestHackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Maybe PackageIdentifierRevision)
getLatestHackageVersion name =
  ((fmap fst . Map.maxViewWithKey) >=> go) <$> getPackageVersions YesPreferredVersions name
  where
    go (version, m) = do
      (_rev, BlobKey sha size) <- fst <$> Map.maxViewWithKey m
      pure $ PackageIdentifierRevision name version $ CFIHash sha $ Just size

fetchTreeKeys
  :: (HasPantryConfig env, HasLogFunc env, Foldable f)
  => f TreeKey
  -> RIO env ()
fetchTreeKeys _ =
  logWarn "Network caching not yet implemented!" -- FIXME

fetchPackages
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env, Foldable f)
  => f PackageLocationImmutable
  -> RIO env ()
fetchPackages pls = do
    fetchTreeKeys $ mapMaybe getTreeKey $ toList pls
    traverseConcurrently_ (void . uncurry getHackageTarball) hackages
    -- FIXME in the future, be concurrent in these as well
    fetchArchives archives
    fetchRepos repos
  where
    s x = Endo (x:)
    run (Endo f) = f []
    (hackagesE, archivesE, reposE) = foldMap go pls
    hackages = run hackagesE
    archives = run archivesE
    repos = run reposE

    go (PLIHackage pir mtree) = (s (pir, mtree), mempty, mempty)
    go (PLIArchive archive pm) = (mempty, s (archive, pm), mempty)
    go (PLIRepo repo pm) = (mempty, mempty, s (repo, pm))

unpackPackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ unpack directory
  -> PackageLocationImmutable
  -> RIO env ()
unpackPackageLocation fp loc = do
  (_, tree) <- loadPackageLocation loc
  unpackTree fp tree

-- | Ignores all warnings
--
-- FIXME! Something to support hpack
parseCabalFileImmutable
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env GenericPackageDescription
parseCabalFileImmutable loc = do
  logDebug $ "Parsing cabal file for " <> display loc
  bs <- loadCabalFile loc
  let foundCabalKey = BlobKey (mkStaticSHA256FromBytes bs) (FileSize (fromIntegral (B.length bs)))
  (_warnings, gpd) <- rawParseGPD (Left loc) bs
  let pm =
        case loc of
          PLIHackage (PackageIdentifierRevision name version cfi) mtree -> PackageMetadata
            { pmName = Just name
            , pmVersion = Just version
            , pmSubdir = ""
            , pmTree = mtree
            , pmCabal =
                case cfi of
                  CFIHash sha (Just size) -> Just $ BlobKey sha size
                  _ -> Nothing
            }
          PLIArchive _ pm' -> pm'
          PLIRepo _ pm' -> pm'
  let exc = MismatchedPackageMetadata loc pm foundCabalKey (gpdPackageIdentifier gpd)
  maybe (throwIO exc) pure $ do
    guard $ maybe True (== gpdPackageName gpd) (pmName pm)
    guard $ maybe True (== gpdVersion gpd) (pmVersion pm)
    guard $ maybe True (== foundCabalKey) (pmCabal pm)
    pure gpd

    {- FIXME
  , runnerParsedCabalFiles :: !(IORef -- FIXME remove
      ( Map PackageIdentifierRevision GenericPackageDescription
      , Map (Path Abs Dir)            (GenericPackageDescription, Path Abs File)
      ))
  -- ^ Cache of previously parsed cabal files.
  --
  -- TODO: This is really an ugly hack to avoid spamming the user with
  -- warnings when we parse cabal files multiple times and bypass
  -- performance issues. Ideally: we would just design the system such
  -- that it only ever parses a cabal file once. But for now, this is
  -- a decent workaround. See:
  -- <https://github.com/commercialhaskell/stack/issues/3591>.

-- | Read the 'GenericPackageDescription' from the given
-- 'PackageIdentifierRevision'.
readPackageUnresolvedIndex
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasRunner env)
  => PackageIdentifierRevision
  -> RIO env GenericPackageDescription
readPackageUnresolvedIndex pir@(PackageIdentifierRevision pn v cfi) = do -- FIXME move to pantry
  ref <- view $ runnerL.to runnerParsedCabalFiles
  (m, _) <- readIORef ref
  case M.lookup pir m of
    Just gpd -> return gpd
    Nothing -> do
      ebs <- loadFromIndex pn v cfi
      bs <-
        case ebs of
          Right bs -> pure bs
      (_warnings, gpd) <- rawParseGPD (Left pir) bs
      let foundPI = D.package $ D.packageDescription gpd
          pi' = D.PackageIdentifier pn v
      unless (pi' == foundPI) $ throwM $ MismatchedCabalIdentifier pir foundPI
      atomicModifyIORef' ref $ \(m1, m2) ->
        ((M.insert pir gpd m1, m2), gpd)
    -}

-- | Same as 'parseCabalFileRemote', but takes a
-- 'PackageLocation'. Never prints warnings, see
-- 'parseCabalFilePath' for that.
parseCabalFile
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocation
  -> RIO env GenericPackageDescription
parseCabalFile (PLImmutable loc) = parseCabalFileImmutable loc
parseCabalFile (PLMutable rfp) = fst <$> parseCabalFilePath (resolvedAbsolute rfp) False

-- | Read the raw, unresolved package information from a file.
parseCabalFilePath
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ project directory, with a cabal file or hpack file
  -> Bool -- ^ print warnings?
  -> RIO env (GenericPackageDescription, Path Abs File)
parseCabalFilePath dir printWarnings = do
    {- FIXME caching
  ref <- view $ runnerL.to runnerParsedCabalFiles
  (_, m) <- readIORef ref
  case Map.lookup dir m of
    Just x -> return x
    Nothing -> do
    -}
      cabalfp <- findOrGenerateCabalFile dir
      bs <- liftIO $ B.readFile $ toFilePath cabalfp
      (warnings, gpd) <- rawParseGPD (Right cabalfp) bs
      when printWarnings
        $ mapM_ (logWarn . toPretty cabalfp) warnings
      checkCabalFileName (gpdPackageName gpd) cabalfp
      let ret = (gpd, cabalfp)
      pure ret
    {- FIXME caching
      atomicModifyIORef' ref $ \(m1, m2) ->
        ((m1, M.insert dir ret m2), ret)
    -}
  where
    toPretty :: Path Abs File -> PWarning -> Utf8Builder
    toPretty src (PWarning _type pos msg) =
      "Cabal file warning in" <>
      fromString (toFilePath src) <> "@" <>
      fromString (showPos pos) <> ": " <>
      fromString msg

    -- | Check if the given name in the @Package@ matches the name of the .cabal file
    checkCabalFileName :: MonadThrow m => PackageName -> Path Abs File -> m ()
    checkCabalFileName name cabalfp = do
        -- Previously, we just use parsePackageNameFromFilePath. However, that can
        -- lead to confusing error messages. See:
        -- https://github.com/commercialhaskell/stack/issues/895
        let expected = displayC name ++ ".cabal"
        when (expected /= toFilePath (filename cabalfp))
            $ throwM $ MismatchedCabalName cabalfp name

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
--
-- If the directory contains a file named package.yaml, hpack is used to
-- generate a .cabal file from it.
findOrGenerateCabalFile
    :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => Path Abs Dir -- ^ package directory
    -> RIO env (Path Abs File)
findOrGenerateCabalFile pkgDir = do
    hpack pkgDir
    findCabalFile1
  where
    findCabalFile1 :: RIO env (Path Abs File)
    findCabalFile1 = findCabalFile2 >>= either throwIO return

    findCabalFile2 :: RIO env (Either PantryException (Path Abs File))
    findCabalFile2 = do
        files <- filter (flip hasExtension "cabal" . toFilePath) . snd
             <$> listDir pkgDir
        return $ case files of
            [] -> Left $ NoCabalFileFound pkgDir
            [x] -> Right x
            -- If there are multiple files, ignore files that start with
            -- ".". On unixlike environments these are hidden, and this
            -- character is not valid in package names. The main goal is
            -- to ignore emacs lock files - see
            -- https://github.com/commercialhaskell/stack/issues/1897.
            (filter (not . ("." `List.isPrefixOf`) . toFilePath . filename) -> [x]) -> Right x
            _:_ -> Left $ MultipleCabalFilesFound pkgDir files
      where hasExtension fp x = FilePath.takeExtension fp == "." ++ x

-- | Generate .cabal file from package.yaml, if necessary.
hpack
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir
  -> RIO env ()
hpack pkgDir = do
    let hpackFile = pkgDir </> $(mkRelFile Hpack.packageConfig)
    exists <- liftIO $ doesFileExist hpackFile
    when exists $ do
        logDebug $ "Running hpack on " <> fromString (toFilePath hpackFile)

        he <- view $ pantryConfigL.to pcHpackExecutable
        case he of
            HpackBundled -> do
                r <- liftIO $ Hpack.hpackResult $ Hpack.setProgramName "stack" $ Hpack.setTarget (toFilePath hpackFile) Hpack.defaultOptions
                forM_ (Hpack.resultWarnings r) (logWarn . fromString)
                let cabalFile = fromString . Hpack.resultCabalFile $ r
                case Hpack.resultStatus r of
                    Hpack.Generated -> logDebug $ "hpack generated a modified version of " <> cabalFile
                    Hpack.OutputUnchanged -> logDebug $ "hpack output unchanged in " <> cabalFile
                    Hpack.AlreadyGeneratedByNewerHpack -> logWarn $
                        cabalFile <>
                        " was generated with a newer version of hpack,\n" <>
                        "please upgrade and try again."
                    Hpack.ExistingCabalFileWasModifiedManually -> logWarn $
                        cabalFile <>
                        " was modified manually. Ignoring " <>
                        fromString (toFilePath hpackFile) <>
                        " in favor of the cabal file.\nIf you want to use the " <>
                        fromString (toFilePath (filename hpackFile)) <>
                        " file instead of the cabal file,\n" <>
                        "then please delete the cabal file."
            HpackCommand command ->
                withWorkingDir (toFilePath pkgDir) $
                proc command [] runProcess_

gpdPackageIdentifier :: GenericPackageDescription -> PackageIdentifier
gpdPackageIdentifier = D.package . D.packageDescription

gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = pkgName . gpdPackageIdentifier

gpdVersion :: GenericPackageDescription -> Version
gpdVersion = pkgVersion . gpdPackageIdentifier

loadCabalFile
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env ByteString

-- Just ignore the mtree for this. Safe assumption: someone who filled
-- in the TreeKey also filled in the cabal file hash, and that's a
-- more efficient lookup mechanism.
loadCabalFile (PLIHackage pir _mtree) = getHackageCabalFile pir

loadCabalFile pl = do
  (_, tree) <- loadPackageLocation pl
  (_sfp, TreeEntry cabalBlobKey _ft) <- findCabalFile pl tree
  mbs <- withStorage $ loadBlob cabalBlobKey
  case mbs of
    Nothing -> error $ "loadCabalFile, blob not found. FIXME In the future: maybe try downloading the archive again."
    Just bs -> pure bs

loadPackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env (TreeKey, Tree)
loadPackageLocation (PLIHackage pir mtree) = getHackageTarball pir mtree
loadPackageLocation (PLIArchive archive pm) = getArchive archive pm
loadPackageLocation (PLIRepo repo pm) = getRepo repo pm

-- | Convert a 'PackageLocation' into a 'UnresolvedPackageLocation'.
mkUnresolvedPackageLocation :: PackageLocation -> UnresolvedPackageLocation
mkUnresolvedPackageLocation (PLImmutable loc) = UPLImmutable (mkUnresolvedPackageLocationImmutable loc)
mkUnresolvedPackageLocation (PLMutable fp) = UPLMutable $ resolvedRelative fp

-- | Convert an 'UnresolvedPackageLocation' into a list of 'PackageLocation's.
resolvePackageLocation
  :: MonadIO m
  => Path Abs Dir -- ^ directory containing configuration file, to be used for resolving relative file paths
  -> UnresolvedPackageLocation
  -> m [PackageLocation]
resolvePackageLocation dir (UPLImmutable rpl) =
  map PLImmutable <$> resolvePackageLocationImmutable (Just dir) rpl
resolvePackageLocation dir (UPLMutable rel@(RelFilePath fp)) = do
  absolute <- resolveDir dir $ T.unpack fp
  pure [PLMutable $ ResolvedPath rel absolute]

-- | Fill in optional fields in a 'PackageLocationImmutable' for more reproducible builds.
completePackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env PackageLocationImmutable
completePackageLocation orig@(PLIHackage (PackageIdentifierRevision _ _ CFIHash{}) (Just _)) = pure orig
completePackageLocation (PLIHackage pir0@(PackageIdentifierRevision name version cfi0) _) = do
  logDebug $ "Completing package location information from " <> display pir0
  pir <-
    case cfi0 of
      CFIHash{} -> pure pir0
      _ -> do
        bs <- getHackageCabalFile pir0
        let cfi = CFIHash (mkStaticSHA256FromBytes bs) (Just (FileSize (fromIntegral (B.length bs))))
            pir = PackageIdentifierRevision name version cfi
        logDebug $ "Added in cabal file hash: " <> display pir
        pure pir
  treeKey <- getHackageTarballKey pir
  pure $ PLIHackage pir (Just treeKey)
completePackageLocation pl@(PLIArchive archive pm) =
  PLIArchive <$> completeArchive archive <*> completePM pl pm
completePackageLocation pl@(PLIRepo repo pm) =
  PLIRepo repo <$> completePM pl pm

completeArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => Archive
  -> RIO env Archive
completeArchive a@(Archive _ (Just _) (Just _)) = pure a
completeArchive a@(Archive loc _ _) =
  withArchiveLoc a $ \_fp sha size ->
  pure $ Archive loc (Just sha) (Just size)

completePM
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> PackageMetadata
  -> RIO env PackageMetadata
completePM plOrig pm
  | isCompletePM pm = pure pm
  | otherwise = do
      (treeKey, tree) <- loadPackageLocation plOrig
      (cabalBlobKey, PackageIdentifier name version) <- loadPackageIdentFromTree plOrig tree
      -- FIXME confirm that no values _changed_
      pure PackageMetadata
        { pmName = Just name
        , pmVersion = Just version
        , pmTree = Just treeKey
        , pmCabal = Just cabalBlobKey
        , pmSubdir = pmSubdir pm
        }
  where
    isCompletePM (PackageMetadata (Just _) (Just _) (Just _) (Just _) _) = True
    isCompletePM _ = False

completeSnapshotLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotLocation
  -> RIO env SnapshotLocation
completeSnapshotLocation sl@SLCompiler{} = pure sl
completeSnapshotLocation sl@SLFilePath{} = pure sl
completeSnapshotLocation sl@(SLUrl _ (Just _) _) = pure sl
completeSnapshotLocation (SLUrl url Nothing mcompiler) = do
  bs <- loadFromURL url Nothing
  let blobKey = BlobKey (mkStaticSHA256FromBytes bs) (FileSize $ fromIntegral $ B.length bs)
  pure $ SLUrl url (Just blobKey) mcompiler

-- | Fill in optional fields in a 'Snapshot' for more reproducible builds.
completeSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Snapshot
  -> RIO env Snapshot
completeSnapshot snapshot = do
  parent' <- completeSnapshotLocation $ snapshotParent snapshot
  pls <- traverseConcurrently completePackageLocation $ snapshotLocations snapshot
  pure snapshot
    { snapshotParent = parent'
    , snapshotLocations = pls
    }

traverseConcurrently_
  :: (Foldable f, HasPantryConfig env)
  => (a -> RIO env ()) -- ^ action to perform
  -> f a -- ^ input values
  -> RIO env ()
traverseConcurrently_ f t0 = do
  cnt <- view $ pantryConfigL.to pcConnectionCount
  traverseConcurrentlyWith_ cnt f t0

traverseConcurrentlyWith_
  :: (MonadUnliftIO m, Foldable f)
  => Int -- ^ concurrent workers
  -> (a -> m ()) -- ^ action to perform
  -> f a -- ^ input values
  -> m ()
traverseConcurrentlyWith_ count f t0 = do
  queue <- newTVarIO $ toList t0

  replicateConcurrently_ count $
    fix $ \loop -> join $ atomically $ do
      toProcess <- readTVar queue
      case toProcess of
        [] -> pure (pure ())
        (x:rest) -> do
          writeTVar queue rest
          pure $ do
            f x
            loop

traverseConcurrently
  :: (HasPantryConfig env, Traversable t)
  => (a -> RIO env b) -- ^ action to perform
  -> t a -- ^ input values
  -> RIO env (t b)
traverseConcurrently f t0 = do
  cnt <- view $ pantryConfigL.to pcConnectionCount
  traverseConcurrentlyWith cnt f t0

-- | Like 'traverse', but does things on
-- up to N separate threads at once.
traverseConcurrentlyWith
  :: (MonadUnliftIO m, Traversable t)
  => Int -- ^ concurrent workers
  -> (a -> m b) -- ^ action to perform
  -> t a -- ^ input values
  -> m (t b)
traverseConcurrentlyWith count f t0 = do
  (queue, t1) <- atomically $ do
    queueDList <- newTVar id
    t1 <- for t0 $ \x -> do
      res <- newEmptyTMVar
      modifyTVar queueDList (. ((x, res):))
      pure $ atomically $ takeTMVar res
    dlist <- readTVar queueDList
    queue <- newTVar $ dlist []
    pure (queue, t1)

  replicateConcurrently_ count $
    fix $ \loop -> join $ atomically $ do
      toProcess <- readTVar queue
      case toProcess of
        [] -> pure (pure ())
        ((x, res):rest) -> do
          writeTVar queue rest
          pure $ do
            y <- f x
            atomically $ putTMVar res y
            loop
  sequence t1

loadPantrySnapshot
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotLocation
  -> RIO env (Either WantedCompiler (Snapshot, Maybe WantedCompiler, StaticSHA256))
loadPantrySnapshot (SLCompiler compiler) = pure $ Left compiler
loadPantrySnapshot sl@(SLUrl url mblob mcompiler) =
  handleAny (throwIO . InvalidSnapshot sl) $ do
    bs <- loadFromURL url mblob
    value <- Yaml.decodeThrow bs
    snapshot <- warningsParserHelper sl value (parseSnapshot Nothing)
    pure $ Right (snapshot, mcompiler, mkStaticSHA256FromBytes bs)
loadPantrySnapshot sl@(SLFilePath fp mcompiler) =
  handleAny (throwIO . InvalidSnapshot sl) $ do
    value <- Yaml.decodeFileThrow $ toFilePath $ resolvedAbsolute fp
    sha <- mkStaticSHA256FromFile $ toFilePath $ resolvedAbsolute fp
    snapshot <- warningsParserHelper sl value $ parseSnapshot $ Just $ parent $ resolvedAbsolute fp
    pure $ Right (snapshot, mcompiler, sha)

loadFromURL
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> Maybe BlobKey
  -> RIO env ByteString
loadFromURL url Nothing = do
  mcached <- withStorage $ loadURLBlob url
  case mcached of
    Just bs -> return bs
    Nothing -> loadWithCheck url Nothing
loadFromURL url (Just bkey) = do
  mcached <- withStorage $ loadBlob bkey
  case mcached of
    Just bs -> return bs
    Nothing -> loadWithCheck url (Just bkey)

loadWithCheck
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> Maybe BlobKey
  -> RIO env ByteString
loadWithCheck url mblobkey = do
  let (msha, msize) =
        case mblobkey of
          Nothing -> (Nothing, Nothing)
          Just (BlobKey sha size) -> (Just sha, Just size)
  (_, _, bss) <- httpSinkChecked url msha msize sinkList
  let bs = B.concat bss
  withStorage $ storeURLBlob url bs
  return bs

warningsParserHelper
  :: HasLogFunc env
  => SnapshotLocation
  -> Value
  -> (Value -> Yaml.Parser (WithJSONWarnings (IO a)))
  -> RIO env a
warningsParserHelper sl val f =
  case parseEither f val of
    Left e -> throwIO $ Couldn'tParseSnapshot sl e
    Right (WithJSONWarnings x ws) -> do
      unless (null ws) $ do
        logWarn $ "Warnings when parsing snapshot " <> display sl
        for_ ws $ logWarn . display
      liftIO x

-- | Get the name of the package at the given location.
getPackageLocationIdent
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env PackageIdentifier
getPackageLocationIdent (PLIHackage (PackageIdentifierRevision name version _) _) = pure $ PackageIdentifier name version
getPackageLocationIdent pli = do
  (_, tree) <- loadPackageLocation pli
  snd <$> loadPackageIdentFromTree pli tree

getPackageLocationTreeKey
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env TreeKey
getPackageLocationTreeKey pl =
  case getTreeKey pl of
    Just treeKey -> pure treeKey
    Nothing ->
      case pl of
        PLIHackage pir _ -> getHackageTarballKey pir
        PLIArchive archive pm -> getArchiveKey archive pm
        PLIRepo repo pm -> getRepoKey repo pm

hpackExecutableL :: HasPantryConfig env => SimpleGetter env HpackExecutable
hpackExecutableL = pantryConfigL.to pcHpackExecutable

getTreeKey :: PackageLocationImmutable -> Maybe TreeKey
getTreeKey (PLIHackage _ mtree) = mtree
getTreeKey (PLIArchive _ pm) = pmTree pm
getTreeKey (PLIRepo _ pm) = pmTree pm

data PantryApp = PantryApp
  { paSimpleApp :: !SimpleApp
  , paPantryConfig :: !PantryConfig
  }

simpleAppL :: Lens' PantryApp SimpleApp
simpleAppL = lens paSimpleApp (\x y -> x { paSimpleApp = y })

instance HasLogFunc PantryApp where
  logFuncL = simpleAppL.logFuncL
instance HasPantryConfig PantryApp where
  pantryConfigL = lens paPantryConfig (\x y -> x { paPantryConfig = y })
instance HasProcessContext PantryApp where
  processContextL = simpleAppL.processContextL

runPantryApp :: MonadIO m => RIO PantryApp a -> m a
runPantryApp f = runSimpleApp $ do
  sa <- ask
  stack <- getAppUserDataDirectory "stack"
  root <- parseAbsDir $ stack RIO.FilePath.</> "pantry"
  withPantryConfig
    root
    defaultHackageSecurityConfig
    HpackBundled
    8
    $ \pc ->
      runRIO
        PantryApp
          { paSimpleApp = sa
          , paPantryConfig = pc
          }
        f

runPantryAppClean :: MonadIO m => RIO PantryApp a -> m a
runPantryAppClean f = liftIO $ withSystemTempDirectory "pantry-clean" $ \dir -> runSimpleApp $ do
  sa <- ask
  root <- resolveDir' dir
  withPantryConfig
    root
    defaultHackageSecurityConfig
    HpackBundled
    8
    $ \pc ->
      runRIO
        PantryApp
          { paSimpleApp = sa
          , paPantryConfig = pc
          }
        f
