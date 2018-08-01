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
  , PackageLocationOrPath (..)
  , ResolvedPath (..)
  , resolvedAbsolute
  , PackageIdentifierRevision (..)
  , PackageName
  , Version
  , PackageIdentifier (..)
  , FlagName
  , TreeKey (..)
  , BlobKey (..)
  , HpackExecutable (..)

    -- ** Raw package locations
  , RawPackageLocation
  , RawPackageLocationOrPath (..)
  , unRawPackageLocation
  , unRawPackageLocationOrPath
  , mkRawPackageLocation
  , mkRawPackageLocationOrPath
  , completePackageLocation
  , resolveDirWithRel

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
  , parseCabalFileRemote
  , parseCabalFilePath
  , getPackageLocationIdent
  , getPackageLocationTreeKey

    -- * Hackage index
  , updateHackageIndex
  , hackageIndexTarballL
  , getLatestHackageVersion

    -- * FIXME legacy from Stack, to be updated
  , loadFromIndex
  , getPackageVersions
  , fetchPackages
  , unpackPackageLocation
  ) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as LB
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Tree
import Pantry.Types
import Pantry.Hackage
import Path (Path, Abs, File, toFilePath, Dir, mkRelFile, (</>), filename)
import Path.Find (findFiles)
import Path.IO (resolveDir, doesFileExist)
import Distribution.PackageDescription (GenericPackageDescription, FlagName)
import qualified Distribution.PackageDescription as D
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.Common (PWarning (..), showPos)
import qualified Hpack
import qualified Hpack.Config as Hpack
import RIO.Process
import qualified Data.Yaml as Yaml
import Data.Aeson.Extended (WithJSONWarnings (..), Value)
import Network.HTTP.StackClient
import Network.HTTP.Types (ok200)

withPantryConfig
  :: HasLogFunc env
  => Path Abs Dir -- ^ pantry root
  -> HackageSecurityConfig
  -> HpackExecutable
  -> (PantryConfig -> RIO env a)
  -> RIO env a
withPantryConfig root hsc he inner = do
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

-- | Returns the versions of the package available on Hackage.
getPackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Map Version (Map Revision BlobKey))
getPackageVersions = withStorage . loadHackagePackageVersions

-- | Returns the latest version of the given package available from
-- Hackage.
getLatestHackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName -- ^ package name
  -> RIO env (Maybe PackageIdentifierRevision)
getLatestHackageVersion name =
  ((fmap fst . Map.maxViewWithKey) >=> go) <$> getPackageVersions name
  where
    go (version, m) = do
      (_rev, BlobKey sha size) <- fst <$> Map.maxViewWithKey m
      pure $ PackageIdentifierRevision name version $ CFIHash sha $ Just size

fetchPackages
  :: (HasPantryConfig env, HasLogFunc env, Foldable f)
  => f PackageLocation
  -> RIO env ()
fetchPackages _ = undefined

unpackPackageLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => Path Abs Dir -- ^ unpack directory
  -> PackageLocation
  -> RIO env ()
unpackPackageLocation fp loc = do
  tree <- loadPackageLocation loc
  unpackTree fp tree

-- | Ignores all warnings
--
-- FIXME! Something to support hpack
parseCabalFileRemote
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env GenericPackageDescription
parseCabalFileRemote loc = do
  logDebug $ "Parsing cabal file for " <> display loc
  bs <- loadCabalFile loc
  (_warnings, gpd) <- rawParseGPD (Left loc) bs
  pure gpd

    {- FIXME
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

-- | A helper function that performs the basic character encoding
-- necessary.
rawParseGPD
  :: MonadThrow m
  => Either PackageLocation (Path Abs File)
  -> ByteString
  -> m ([PWarning], GenericPackageDescription)
rawParseGPD loc bs =
    case eres of
      Left (mversion, errs) -> throwM $ InvalidCabalFile loc mversion errs warnings
      Right gpkg -> return (warnings, gpkg)
  where
    (warnings, eres) = runParseResult $ parseGenericPackageDescription bs

-- | Same as 'parseCabalFileRemote', but takes a
-- 'PackageLocationOrPath'. Never prints warnings, see
-- 'parseCabalFilePath' for that.
parseCabalFile
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationOrPath
  -> RIO env GenericPackageDescription
parseCabalFile (PLRemote loc) = parseCabalFileRemote loc
parseCabalFile (PLFilePath rfp) = fst <$> parseCabalFilePath (resolvedAbsolute rfp) False

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
        files <- liftIO $ findFiles
            pkgDir
            (flip hasExtension "cabal" . toFilePath)
            (const False)
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
                r <- liftIO $ Hpack.hpackResult $ Hpack.setTarget (toFilePath hpackFile) Hpack.defaultOptions
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
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env ByteString
loadCabalFile (PLHackage pir mtree) = getHackageCabalFile pir
{- FIXME this is relatively inefficient
loadCabalFile loc = do
  tree <- loadPackageLocation loc
  mbs <- withStorage $ do
    (_sfp, TreeEntry key _ft) <- findCabalFile loc tree
    loadBlob key
  case mbs of
    Just bs -> pure bs
    -- FIXME what to do on Nothing? perhaps download the PackageLocation again?
-}

loadPackageLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env Tree
loadPackageLocation (PLHackage pir mtree) =
  case mtree of
    Nothing -> snd <$> getHackageTarball pir

-- | Convert a 'PackageLocationOrPath' into a 'RawPackageLocationOrPath'.
mkRawPackageLocationOrPath :: PackageLocationOrPath -> RawPackageLocationOrPath
mkRawPackageLocationOrPath (PLRemote loc) = RPLRemote (mkRawPackageLocation loc)
mkRawPackageLocationOrPath (PLFilePath fp) = RPLFilePath $ resolvedRelative fp

-- | Convert a 'RawPackageLocationOrPath' into a list of 'PackageLocationOrPath's.
unRawPackageLocationOrPath
  :: MonadIO m
  => Path Abs Dir -- ^ directory containing configuration file, to be used for resolving relative file paths
  -> RawPackageLocationOrPath
  -> m [PackageLocationOrPath]
unRawPackageLocationOrPath dir (RPLRemote rpl) =
  map PLRemote <$> unRawPackageLocation (Just dir) rpl
unRawPackageLocationOrPath dir (RPLFilePath fp) = do
  rfp <- resolveDirWithRel dir fp
  pure [PLFilePath rfp]

resolveDirWithRel
  :: MonadIO m
  => Path Abs Dir -- ^ root directory to be relative to
  -> RelFilePath
  -> m (ResolvedPath Dir)
resolveDirWithRel dir (RelFilePath fp) = do
  absolute <- resolveDir dir (T.unpack fp)
  pure ResolvedPath
    { resolvedRelative = RelFilePath fp
    , resolvedAbsoluteHack = toFilePath absolute
    }

-- | Fill in optional fields in a 'PackageLocation' for more reproducible builds.
completePackageLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env PackageLocation
completePackageLocation orig@(PLHackage _ (Just _)) = pure orig
completePackageLocation (PLHackage pir Nothing) = do
  logDebug $ "Completing package location information from " <> display pir
  treeKey <- getHackageTarballKey pir
  pure $ PLHackage pir (Just treeKey)

completeSnapshotLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotLocation
  -> RIO env SnapshotLocation
completeSnapshotLocation (SLCompiler wc) = pure $ SLCompiler wc

-- | Fill in optional fields in a 'Snapshot' for more reproducible builds.
completeSnapshot
  :: (HasPantryConfig env, HasLogFunc env)
  => Maybe (Path Abs Dir) -- ^ directory to resolve relative paths from, if local
  -> Snapshot
  -> RIO env Snapshot
completeSnapshot mdir snapshot = do
  parent' <- completeSnapshotLocation $ snapshotParent snapshot
  pls <- traverseConcurrentlyWith 16 completePackageLocation $ snapshotLocations snapshot
  pure snapshot
    { snapshotParent = parent'
    , snapshotLocations = pls
    }

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
  -> RIO env (Either WantedCompiler (Snapshot, Maybe WantedCompiler))
loadPantrySnapshot (SLCompiler compiler) = pure $ Left compiler
loadPantrySnapshot sl@(SLUrl url mblob mcompiler) =
  handleAny (throwIO . InvalidSnapshot sl) $ do
    bs <- loadFromURL url mblob
    value <- Yaml.decodeThrow bs
    snapshot <- warningsParserHelper value (parseSnapshot Nothing)
    pure $ Right (snapshot, mcompiler)
loadPantrySnapshot sl@(SLFilePath fp mcompiler) =
  handleAny (throwIO . InvalidSnapshot sl) $ do
    value <- Yaml.decodeFileThrow $ toFilePath $ resolvedAbsolute fp
    snapshot <- warningsParserHelper value (parseSnapshot Nothing)
    pure $ Right (snapshot, mcompiler)

loadFromURL
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> Maybe BlobKey
  -> RIO env ByteString
loadFromURL url Nothing = do
  mcached <- withStorage $ loadURLBlob url
  case mcached of
    Just bs -> return bs
    Nothing -> loadWithCheck url $ \_ -> return ()
loadFromURL url (Just bkey@(BlobKey sha size)) = do
  mcached <- withStorage $ loadBlob bkey
  case mcached of
    Just bs -> return bs
    Nothing -> loadWithCheck url $ \bs -> do
      let blobSha = mkStaticSHA256FromBytes bs
          blobSize = FileSize $ fromIntegral $ B.length bs
      when (blobSha /= sha || blobSize /= size) $
        throwIO $ InvalidBlobKey Mismatch
          { mismatchExpected = bkey
          , mismatchActual = BlobKey blobSha blobSize
          }

loadWithCheck
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> (ByteString -> RIO env ()) -- ^ function to check downloaded blob
  -> RIO env ByteString
loadWithCheck url checkResponseBody = do
  req <- parseRequest $ T.unpack url
  res <- httpLbs req
  let statusCode = responseStatus res
  when (statusCode /= ok200) $ throwIO (Non200ResponseStatus statusCode)
  let bs = LB.toStrict $ getResponseBody res
  checkResponseBody bs
  withStorage $ storeURLBlob url bs
  return bs

warningsParserHelper
  :: HasLogFunc env
  => Value
  -> (Value -> Yaml.Parser (WithJSONWarnings (IO a)))
  -> RIO env a
warningsParserHelper = undefined

-- | Get the name of the package at the given location.
getPackageLocationIdent
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env PackageIdentifier
getPackageLocationIdent (PLHackage (PackageIdentifierRevision name version _) _) = pure $ PackageIdentifier name version

getPackageLocationTreeKey
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> RIO env TreeKey
getPackageLocationTreeKey (PLHackage _ (Just treeKey)) = pure treeKey
getPackageLocationTreeKey (PLHackage pir Nothing) = getHackageTarballKey pir

hpackExecutableL :: HasPantryConfig env => SimpleGetter env HpackExecutable
hpackExecutableL = pantryConfigL.to pcHpackExecutable
