{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- | Content addressable Haskell package management, providing for
-- secure, reproducible acquisition of Haskell package contents and
-- metadata.
--
-- @since 0.1.0.0
module Pantry
  ( -- * Running
    PantryConfig
  , HackageSecurityConfig (..)
  , defaultHackageSecurityConfig
  , HasPantryConfig (..)
  , withPantryConfig
  , HpackExecutable (..)

    -- ** Convenience
  , PantryApp
  , runPantryApp
  , runPantryAppClean
  , hpackExecutableL

    -- * Types

    -- ** Exceptions
  , PantryException (..)

    -- ** Cabal types
  , PackageName
  , Version
  , FlagName
  , PackageIdentifier (..)

    -- ** Files
  , FileSize (..)
  , RelFilePath (..)
  , ResolvedPath (..)
  , Unresolved

    -- ** Cryptography
  , SHA256
  , TreeKey (..)
  , BlobKey (..)

    -- ** Packages
  , PackageMetadata (..)
  , Package (..)

    -- ** Hackage
  , CabalFileInfo (..)
  , Revision (..)
  , PackageIdentifierRevision (..)
  , UsePreferredVersions (..)

    -- ** Archives
  , Archive (..)
  , ArchiveLocation (..)

    -- ** Repos
  , Repo (..)
  , RepoType (..)

    -- ** Package location
  , PackageLocation (..)
  , PackageLocationImmutable (..)

    -- ** Snapshots
  , SnapshotLocation (..)
  , Snapshot (..)
  , SnapshotPackage (..)
  , SnapshotLayer (..)
  , WantedCompiler (..)

    -- * Loading values
  , resolvePaths
  , loadPackage
  , loadSnapshotLayer
  , loadSnapshot
  , addPackagesToSnapshot
  , AddPackagesConfig (..)

    -- * Completion functions
  , completePackageLocation
  , completeSnapshotLayer
  , completeSnapshotLocation

    -- * Parsers
  , parseWantedCompiler
  , parseSnapshotLocation
  , parsePackageIdentifierRevision

    -- ** Cabal values
  , parsePackageIdentifier
  , parsePackageName
  , parsePackageNameThrowing
  , parseFlagName
  , parseVersion
  , parseVersionThrowing

    -- * Stackage snapshots
  , ltsSnapshotLocation
  , nightlySnapshotLocation

    -- * Cabal helpers
  , packageIdentifierString
  , packageNameString
  , flagNameString
  , versionString
  , moduleNameString
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap
  , gpdPackageIdentifier
  , gpdPackageName
  , gpdVersion

    -- * Package location
  , fetchPackages
  , unpackPackageLocation
  , getPackageLocationName
  , getPackageLocationIdent
  , getPackageLocationTreeKey

    -- * Cabal files
  , loadCabalFile
  , loadCabalFileImmutable
  , loadCabalFilePath
  , findOrGenerateCabalFile
  , PrintWarnings (..)

    -- * Hackage index
  , updateHackageIndex
  , DidUpdateOccur (..)
  , hackageIndexTarballL
  , getHackagePackageVersions
  , getLatestHackageVersion
  , getHackageTypoCorrections
  ) where

import RIO
import Conduit
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import Pantry.Archive
import Pantry.Repo
import qualified Pantry.SHA256 as SHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import Pantry.Hackage
import Path (Path, Abs, File, toFilePath, Dir, (</>), filename, parseAbsDir, parent, parseRelFile)
import Path.IO (doesFileExist, resolveDir', listDir)
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
import Data.Char (isHexDigit)

-- | Create a new 'PantryConfig' with the given settings.
--
-- For something easier to use in simple cases, see 'runPantryApp'.
--
-- @since 0.1.0.0
withPantryConfig
  :: HasLogFunc env
  => Path Abs Dir
  -- ^ pantry root directory, where the SQLite database and Hackage
  -- downloads are kept.
  -> HackageSecurityConfig
  -- ^ Hackage configuration. You probably want
  -- 'defaultHackageSecurityConfig'.
  -> HpackExecutable
  -- ^ When converting an hpack @package.yaml@ file to a cabal file,
  -- what version of hpack should we use?
  -> Int
  -- ^ Maximum connection count
  -> (PantryConfig -> RIO env a)
  -- ^ What to do with the config
  -> RIO env a
withPantryConfig root hsc he count inner = do
  env <- ask
  pantryRelFile <- parseRelFile "pantry.sqlite3"
  -- Silence persistent's logging output, which is really noisy
  runRIO (mempty :: LogFunc) $ initStorage (root </> pantryRelFile) $ \storage -> runRIO env $ do
    ur <- newMVar True
    ref1 <- newIORef mempty
    ref2 <- newIORef mempty
    inner PantryConfig
      { pcHackageSecurity = hsc
      , pcHpackExecutable = he
      , pcRootDir = root
      , pcStorage = storage
      , pcUpdateRef = ur
      , pcConnectionCount = count
      , pcParsedCabalFilesImmutable = ref1
      , pcParsedCabalFilesMutable = ref2
      }

-- | Default 'HackageSecurityConfig' value using the official Hackage server.
--
-- @since 0.1.0.0
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
  , hscDownloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/"
  }

-- | Returns the latest version of the given package available from
-- Hackage.
--
-- @since 0.1.0.0
getLatestHackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> UsePreferredVersions
  -> RIO env (Maybe PackageIdentifierRevision)
getLatestHackageVersion name preferred =
  ((fmap fst . Map.maxViewWithKey) >=> go) <$> getHackagePackageVersions preferred name
  where
    go (version, m) = do
      (_rev, BlobKey sha size) <- fst <$> Map.maxViewWithKey m
      pure $ PackageIdentifierRevision name version $ CFIHash sha $ Just size

fetchTreeKeys
  :: (HasPantryConfig env, HasLogFunc env, Foldable f)
  => f TreeKey
  -> RIO env ()
fetchTreeKeys _ =
  logWarn "Network caching not yet implemented!" -- TODO pantry wire

-- | Download all of the packages provided into the local cache
-- without performing any unpacking. Can be useful for build tools
-- wanting to prefetch or provide an offline mode.
--
-- @since 0.1.0.0
fetchPackages
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env, Foldable f)
  => f PackageLocationImmutable
  -> RIO env ()
fetchPackages pls = do
    fetchTreeKeys $ mapMaybe getTreeKey $ toList pls
    traverseConcurrently_ (void . uncurry getHackageTarball) hackages
    -- TODO in the future, be concurrent in these as well
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

-- | Unpack a given 'PackageLocationImmutable' into the given
-- directory. Does not generate any extra subdirectories.
--
-- @since 0.1.0.0
unpackPackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ unpack directory
  -> PackageLocationImmutable
  -> RIO env ()
unpackPackageLocation fp loc = loadPackage loc >>= unpackTree loc fp . packageTree

-- | Load the cabal file for the given 'PackageLocationImmutable'.
--
-- This function ignores all warnings.
--
-- Note that, for now, this will not allow support for hpack files in
-- these package locations. Instead, all @PackageLocationImmutable@s
-- will require a .cabal file. This may be relaxed in the future.
--
-- @since 0.1.0.0
loadCabalFileImmutable
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env GenericPackageDescription
loadCabalFileImmutable loc = withCache $ do
  logDebug $ "Parsing cabal file for " <> display loc
  bs <- loadCabalFileBytes loc
  let foundCabalKey = BlobKey (SHA256.hashBytes bs) (FileSize (fromIntegral (B.length bs)))
  (_warnings, gpd) <- rawParseGPD (Left loc) bs
  let pm =
        case loc of
          PLIHackage (PackageIdentifierRevision name version cfi) mtree -> PackageMetadata
            { pmName = Just name
            , pmVersion = Just version
            , pmTreeKey = mtree
            , pmCabal =
                case cfi of
                  CFIHash sha (Just size) -> Just $ BlobKey sha size
                  _ -> Nothing
            }
          PLIArchive _ pm' -> pm'
          PLIRepo _ pm' -> pm'
  let exc = MismatchedPackageMetadata loc pm Nothing foundCabalKey (gpdPackageIdentifier gpd)
  maybe (throwIO exc) pure $ do
    guard $ maybe True (== gpdPackageName gpd) (pmName pm)
    guard $ maybe True (== gpdVersion gpd) (pmVersion pm)
    guard $ maybe True (== foundCabalKey) (pmCabal pm)
    pure gpd
  where
    withCache inner = do
      ref <- view $ pantryConfigL.to pcParsedCabalFilesImmutable
      m0 <- readIORef ref
      case Map.lookup loc m0 of
        Just x -> pure x
        Nothing -> do
          x <- inner
          atomicModifyIORef' ref $ \m -> (Map.insert loc x m, x)

-- | Same as 'loadCabalFileImmutable', but takes a
-- 'PackageLocation'. Never prints warnings, see 'loadCabalFilePath'
-- for that.
--
-- @since 0.1.0.0
loadCabalFile
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocation
  -> RIO env GenericPackageDescription
loadCabalFile (PLImmutable loc) = loadCabalFileImmutable loc
loadCabalFile (PLMutable rfp) = do
  (gpdio, _, _) <- loadCabalFilePath (resolvedAbsolute rfp)
  liftIO $ gpdio NoPrintWarnings

-- | Parse the cabal file for the package inside the given
-- directory. Performs various sanity checks, such as the file name
-- being correct and having only a single cabal file.
--
-- @since 0.1.0.0
loadCabalFilePath
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ project directory, with a cabal file or hpack file
  -> RIO env
       ( PrintWarnings -> IO GenericPackageDescription
       , PackageName
       , Path Abs File
       )
loadCabalFilePath dir = do
  ref <- view $ pantryConfigL.to pcParsedCabalFilesMutable
  mcached <- Map.lookup dir <$> readIORef ref
  case mcached of
    Just triple -> pure triple
    Nothing -> do
      (name, cabalfp) <- findOrGenerateCabalFile dir
      gpdRef <- newIORef Nothing
      run <- askRunInIO
      let gpdio = run . getGPD cabalfp gpdRef
          triple = (gpdio, name, cabalfp)
      atomicModifyIORef' ref $ \m -> (Map.insert dir triple m, triple)
  where
    getGPD cabalfp gpdRef printWarnings = do
      mpair <- readIORef gpdRef
      (warnings0, gpd) <-
        case mpair of
          Just pair -> pure pair
          Nothing -> do
            bs <- liftIO $ B.readFile $ toFilePath cabalfp
            (warnings0, gpd) <- rawParseGPD (Right cabalfp) bs
            checkCabalFileName (gpdPackageName gpd) cabalfp
            pure (warnings0, gpd)
      warnings <-
        case printWarnings of
          YesPrintWarnings -> mapM_ (logWarn . toPretty cabalfp) warnings0 $> []
          NoPrintWarnings -> pure warnings0
      writeIORef gpdRef $ Just (warnings, gpd)
      pure gpd

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
        let expected = T.unpack $ unSafeFilePath $ cabalFileName name
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
    -> RIO env (PackageName, Path Abs File)
findOrGenerateCabalFile pkgDir = do
    hpack pkgDir
    files <- filter (flip hasExtension "cabal" . toFilePath) . snd
         <$> listDir pkgDir
    -- If there are multiple files, ignore files that start with
    -- ".". On unixlike environments these are hidden, and this
    -- character is not valid in package names. The main goal is
    -- to ignore emacs lock files - see
    -- https://github.com/commercialhaskell/stack/issues/1897.
    let isHidden ('.':_) = True
        isHidden _ = False
    case filter (not . isHidden . toFilePath . filename) files of
        [] -> throwIO $ NoCabalFileFound pkgDir
        [x] -> maybe
          (throwIO $ InvalidCabalFilePath x)
          (\pn -> pure $ (pn, x)) $
            List.stripSuffix ".cabal" (toFilePath (filename x)) >>=
            parsePackageName
        _:_ -> throwIO $ MultipleCabalFilesFound pkgDir files
      where hasExtension fp x = FilePath.takeExtension fp == "." ++ x

-- | Generate .cabal file from package.yaml, if necessary.
hpack
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir
  -> RIO env ()
hpack pkgDir = do
    packageConfigRelFile <- parseRelFile Hpack.packageConfig
    let hpackFile = pkgDir </> packageConfigRelFile
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

-- | Get the 'PackageIdentifier' from a 'GenericPackageDescription'.
--
-- @since 0.1.0.0
gpdPackageIdentifier :: GenericPackageDescription -> PackageIdentifier
gpdPackageIdentifier = D.package . D.packageDescription

-- | Get the 'PackageName' from a 'GenericPackageDescription'.
--
-- @since 0.1.0.0
gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = pkgName . gpdPackageIdentifier

-- | Get the 'Version' from a 'GenericPackageDescription'.
--
-- @since 0.1.0.0
gpdVersion :: GenericPackageDescription -> Version
gpdVersion = pkgVersion . gpdPackageIdentifier

loadCabalFileBytes
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env ByteString

-- Just ignore the mtree for this. Safe assumption: someone who filled
-- in the TreeKey also filled in the cabal file hash, and that's a
-- more efficient lookup mechanism.
loadCabalFileBytes (PLIHackage pir _mtree) = getHackageCabalFile pir

loadCabalFileBytes pl = do
  package <- loadPackage pl
  let sfp = cabalFileName $ pkgName $ packageIdent package
  cabalBlobKey <- case (packageCabalEntry package) of
                       PCHpack pcHpack -> pure $ teBlob . phGenerated $ pcHpack
                       PCCabalFile (TreeEntry blobKey _) -> pure blobKey
  mbs <- withStorage $ loadBlob cabalBlobKey
  case mbs of
    Nothing -> do
      -- TODO when we have pantry wire, try downloading
      throwIO $ TreeReferencesMissingBlob pl sfp cabalBlobKey
    Just bs -> pure bs

-- | Load a 'Package' from a 'PackageLocationImmutable'.
--
-- @since 0.1.0.0
loadPackage
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env Package
loadPackage (PLIHackage pir mtree) = getHackageTarball pir mtree
loadPackage pli@(PLIArchive archive pm) = getArchive pli archive pm
loadPackage (PLIRepo repo pm) = getRepo repo pm

-- | Fill in optional fields in a 'PackageLocationImmutable' for more reproducible builds.
--
-- @since 0.1.0.0
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
        let cfi = CFIHash (SHA256.hashBytes bs) (Just (FileSize (fromIntegral (B.length bs))))
            pir = PackageIdentifierRevision name version cfi
        logDebug $ "Added in cabal file hash: " <> display pir
        pure pir
  treeKey <- getHackageTarballKey pir
  pure $ PLIHackage pir (Just treeKey)
completePackageLocation pl@(PLIArchive archive pm) =
  PLIArchive <$> completeArchive archive <*> completePM pl pm
completePackageLocation pl@(PLIRepo repo pm) = do
  unless (isSHA1 (repoCommit repo)) $ throwIO $ CannotCompleteRepoNonSHA1 repo
  PLIRepo repo <$> completePM pl pm
  where
    isSHA1 t = T.length t == 40 && T.all isHexDigit t

completeArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => Archive
  -> RIO env Archive
completeArchive a@(Archive _ (Just _) (Just _) _) = pure a
completeArchive a@(Archive loc _ _ subdir) =
  withArchiveLoc a $ \_fp sha size ->
  pure $ Archive loc (Just sha) (Just size) subdir

completePM
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> PackageMetadata
  -> RIO env PackageMetadata
completePM plOrig pm
  | isCompletePM pm = pure pm
  | otherwise = do
      package <- loadPackage plOrig
      let pkgCabal = case packageCabalEntry package of
                       PCCabalFile tentry -> tentry
                       PCHpack phpack -> phGenerated phpack
          pmNew = PackageMetadata
            { pmName = Just $ pkgName $ packageIdent package
            , pmVersion = Just $ pkgVersion $ packageIdent package
            , pmTreeKey = Just $ packageTreeKey package
            , pmCabal = Just $ teBlob pkgCabal
            }

          isSame (Just x) (Just y) = x == y
          isSame _ _ = True

          allSame =
            isSame (pmName pmNew) (pmName pm) &&
            isSame (pmVersion pmNew) (pmVersion pm) &&
            isSame (pmTreeKey pmNew) (pmTreeKey pm) &&
            isSame (pmCabal pmNew) (pmCabal pm)
      if allSame
        then pure pmNew
        else throwIO $ CompletePackageMetadataMismatch plOrig pmNew
  where
    isCompletePM (PackageMetadata (Just _) (Just _) (Just _) (Just _)) = True
    isCompletePM _ = False

-- | Add in hashes to make a 'SnapshotLocation' reproducible.
--
-- @since 0.1.0.0
completeSnapshotLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotLocation
  -> RIO env SnapshotLocation
completeSnapshotLocation sl@SLCompiler{} = pure sl
completeSnapshotLocation sl@SLFilePath{} = pure sl
completeSnapshotLocation sl@(SLUrl _ (Just _)) = pure sl
completeSnapshotLocation (SLUrl url Nothing) = do
  bs <- loadFromURL url Nothing
  let blobKey = BlobKey (SHA256.hashBytes bs) (FileSize $ fromIntegral $ B.length bs)
  pure $ SLUrl url (Just blobKey)

-- | Fill in optional fields in a 'SnapshotLayer' for more reproducible builds.
--
-- @since 0.1.0.0
completeSnapshotLayer
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => SnapshotLayer
  -> RIO env SnapshotLayer
completeSnapshotLayer snapshot = do
  parent' <- completeSnapshotLocation $ slParent snapshot
  pls <- traverseConcurrently completePackageLocation $ slLocations snapshot
  pure snapshot
    { slParent = parent'
    , slLocations = pls
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

-- | Parse a 'Snapshot' (all layers) from a 'SnapshotLocation.
--
-- @since 0.1.0.0
loadSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => SnapshotLocation
  -> RIO env Snapshot
loadSnapshot loc = do
  eres <- loadSnapshotLayer loc
  case eres of
    Left wc ->
      pure Snapshot
        { snapshotCompiler = wc
        , snapshotName = utf8BuilderToText $ display wc
        , snapshotPackages = mempty
        , snapshotDrop = mempty
        }
    Right (sl, _sha) -> do
      snap0 <- loadSnapshot $ slParent sl
      (packages, unused) <-
        addPackagesToSnapshot
          (display loc)
          (slLocations sl)
          AddPackagesConfig
            { apcDrop = slDropPackages sl
            , apcFlags = slFlags sl
            , apcHiddens = slHidden sl
            , apcGhcOptions = slGhcOptions sl
            }
          (snapshotPackages snap0)
      warnUnusedAddPackagesConfig (display loc) unused
      pure Snapshot
        { snapshotCompiler = fromMaybe (snapshotCompiler snap0) (slCompiler sl)
        , snapshotName = slName sl
        , snapshotPackages = packages
        , snapshotDrop = apcDrop unused
        }

data SingleOrNot a
  = Single !a
  | Multiple !a !a !([a] -> [a])
instance Semigroup (SingleOrNot a) where
  Single a <> Single b = Multiple a b id
  Single a <> Multiple b c d = Multiple a b ((c:) . d)
  Multiple a b c <> Single d = Multiple a b (c . (d:))
  Multiple a b c <> Multiple d e f =
    Multiple a b (c . (d:) . (e:) . f)

sonToEither :: (k, SingleOrNot a) -> Either (k, a) (k, [a])
sonToEither (k, Single a) = Left (k, a)
sonToEither (k, Multiple a b c) = Right (k, (a : b : c []))

-- | Package settings to be passed to 'addPackagesToSnapshot'.
--
-- @since 0.1.0.0
data AddPackagesConfig = AddPackagesConfig
  { apcDrop :: !(Set PackageName)
  , apcFlags :: !(Map PackageName (Map FlagName Bool))
  , apcHiddens :: !(Map PackageName Bool)
  , apcGhcOptions :: !(Map PackageName [Text])
  }

-- | Does not warn about drops, those are allowed in order to ignore global
-- packages.
warnUnusedAddPackagesConfig
  :: HasLogFunc env
  => Utf8Builder -- ^ source
  -> AddPackagesConfig
  -> RIO env ()
warnUnusedAddPackagesConfig source (AddPackagesConfig _drops flags hiddens options) = do
  unless (null ls) $ do
    logWarn $ "Some warnings discovered when adding packages to snapshot (" <> source <> ")"
    traverse_ logWarn ls
  where
    ls = concat [flags', hiddens', options']

    flags' =
      map
        (\pn ->
          "Setting flags for non-existent package: " <>
          fromString (packageNameString pn))
        (Map.keys flags)

    hiddens' =
      map
        (\pn ->
          "Hiding non-existent package: " <>
          fromString (packageNameString pn))
        (Map.keys hiddens)

    options' =
      map
        (\pn ->
          "Setting options for non-existent package: " <>
          fromString (packageNameString pn))
        (Map.keys options)

-- | Add more packages to a snapshot
--
-- Note that any settings on a parent flag which is being replaced will be
-- ignored. For example, if package @foo@ is in the parent and has flag @bar@
-- set, and @foo@ also appears in new packages, then @bar@ will no longer be
-- set.
--
-- Returns any of the 'AddPackagesConfig' values not used.
--
-- @since 0.1.0.0
addPackagesToSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Utf8Builder
  -- ^ Text description of where these new packages are coming from, for error
  -- messages only
  -> [PackageLocationImmutable] -- ^ new packages
  -> AddPackagesConfig
  -> Map PackageName SnapshotPackage -- ^ packages from parent
  -> RIO env (Map PackageName SnapshotPackage, AddPackagesConfig)
addPackagesToSnapshot source newPackages (AddPackagesConfig drops flags hiddens options) old = do
  new' <- for newPackages $ \loc -> do
    name <- getPackageLocationName loc
    pure (name, SnapshotPackage
      { spLocation = loc
      , spFlags = Map.findWithDefault mempty name flags
      , spHidden = Map.findWithDefault False name hiddens
      , spGhcOptions = Map.findWithDefault [] name options
      })
  let (newSingles, newMultiples)
        = partitionEithers
        $ map sonToEither
        $ Map.toList
        $ Map.fromListWith (<>)
        $ map (second Single) new'
  unless (null $ newMultiples) $ throwIO $
    DuplicatePackageNames source $ map (second (map spLocation)) newMultiples
  let new = Map.fromList newSingles
      allPackages0 = new `Map.union` (old `Map.difference` Map.fromSet (const ()) drops)
      allPackages = flip Map.mapWithKey allPackages0 $ \name sp ->
        sp
          { spFlags = Map.findWithDefault (spFlags sp) name flags
          , spHidden = Map.findWithDefault (spHidden sp) name hiddens
          , spGhcOptions = Map.findWithDefault (spGhcOptions sp) name options
          }

      unused = AddPackagesConfig
        (drops `Set.difference` Map.keysSet old)
        (flags `Map.difference` allPackages)
        (hiddens `Map.difference` allPackages)
        (options `Map.difference` allPackages)

  pure (allPackages, unused)

-- | Parse a 'SnapshotLayer' value from a 'SnapshotLocation'.
--
-- Returns a 'Left' value if provided an 'SLCompiler'
-- constructor. Otherwise, returns a 'Right' value providing both the
-- 'Snapshot' and a hash of the input configuration file.
--
-- @since 0.1.0.0
loadSnapshotLayer
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotLocation
  -> RIO env (Either WantedCompiler (SnapshotLayer, SHA256)) -- FIXME remove SHA? Be smart?
loadSnapshotLayer (SLCompiler compiler) = pure $ Left compiler
loadSnapshotLayer sl@(SLUrl url mblob) =
  handleAny (throwIO . InvalidSnapshot sl) $ do
    bs <- loadFromURL url mblob
    value <- Yaml.decodeThrow bs
    snapshot <- warningsParserHelper sl value Nothing
    pure $ Right (snapshot, SHA256.hashBytes bs)
loadSnapshotLayer sl@(SLFilePath fp) =
  handleAny (throwIO . InvalidSnapshot sl) $ do
    value <- Yaml.decodeFileThrow $ toFilePath $ resolvedAbsolute fp
    sha <- SHA256.hashFile $ toFilePath $ resolvedAbsolute fp
    snapshot <- warningsParserHelper sl value $ Just $ parent $ resolvedAbsolute fp
    pure $ Right (snapshot, sha)

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
  -> Maybe (Path Abs Dir)
  -> RIO env SnapshotLayer
warningsParserHelper sl val mdir =
  case parseEither Yaml.parseJSON val of
    Left e -> throwIO $ Couldn'tParseSnapshot sl e
    Right (WithJSONWarnings x ws) -> do
      unless (null ws) $ do
        logWarn $ "Warnings when parsing snapshot " <> display sl
        for_ ws $ logWarn . display
      resolvePaths mdir x

-- | Get the 'PackageName' of the package at the given location.
--
-- @since 0.1.0.0
getPackageLocationName
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env PackageName
getPackageLocationName = fmap pkgName . getPackageLocationIdent

-- | Get the 'PackageIdentifier' of the package at the given location.
--
-- @since 0.1.0.0
getPackageLocationIdent
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env PackageIdentifier
getPackageLocationIdent (PLIHackage (PackageIdentifierRevision name version _) _) = pure $ PackageIdentifier name version
getPackageLocationIdent (PLIRepo _ PackageMetadata { pmName = Just name, pmVersion = Just version }) = pure $ PackageIdentifier name version
getPackageLocationIdent (PLIArchive _ PackageMetadata { pmName = Just name, pmVersion = Just version }) = pure $ PackageIdentifier name version
getPackageLocationIdent pli = packageIdent <$> loadPackage pli

-- | Get the 'TreeKey' of the package at the given location.
--
-- @since 0.1.0.0
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
        PLIArchive archive pm -> getArchiveKey pl archive pm
        PLIRepo repo pm -> getRepoKey repo pm

getTreeKey :: PackageLocationImmutable -> Maybe TreeKey
getTreeKey (PLIHackage _ mtree) = mtree
getTreeKey (PLIArchive _ pm) = pmTreeKey pm
getTreeKey (PLIRepo _ pm) = pmTreeKey pm

-- | Convenient data type that allows you to work with pantry more
-- easily than using 'withPantryConfig' directly. Uses basically sane
-- settings, like sharing a pantry directory with Stack.
--
-- You can use 'runPantryApp' to use this.
--
-- @since 0.1.0.0
data PantryApp = PantryApp
  { paSimpleApp :: !SimpleApp
  , paPantryConfig :: !PantryConfig
  }

simpleAppL :: Lens' PantryApp SimpleApp
simpleAppL = lens paSimpleApp (\x y -> x { paSimpleApp = y })

hpackExecutableL :: Lens' PantryConfig HpackExecutable
hpackExecutableL k pconfig = fmap (\hpExe -> pconfig { pcHpackExecutable = hpExe }) (k (pcHpackExecutable pconfig))

instance HasLogFunc PantryApp where
  logFuncL = simpleAppL.logFuncL
instance HasPantryConfig PantryApp where
  pantryConfigL = lens paPantryConfig (\x y -> x { paPantryConfig = y })
instance HasProcessContext PantryApp where
  processContextL = simpleAppL.processContextL

-- | Run some code against pantry using basic sane settings.
--
-- For testing, see 'runPantryAppClean'.
--
-- @since 0.1.0.0
runPantryApp :: MonadIO m => RIO PantryApp a -> m a
runPantryApp f = runSimpleApp $ do
  sa <- ask
  stack <- getAppUserDataDirectory "stack"
  root <- parseAbsDir $ stack FilePath.</> "pantry"
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

-- | Like 'runPantryApp', but uses an empty pantry directory instead
-- of sharing with Stack. Useful for testing.
--
-- @since 0.1.0.0
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
