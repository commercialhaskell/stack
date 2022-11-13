{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

-- Create a source distribution tarball
module Stack.SDist
    ( getSDistTarball
    , checkSDistTarball
    , checkSDistTarball'
    , SDistOpts (..)
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Concurrent.Execute
                   ( ActionContext(..), Concurrency(..) )
import           Stack.Prelude hiding ( Display (..) )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char ( toLower )
import           Data.Data ( cast )
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock.POSIX
import           Distribution.Package ( Dependency (..) )
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Check as Check
import qualified Distribution.PackageDescription.Parsec as Cabal
import           Distribution.PackageDescription.PrettyPrint
                   ( showGenericPackageDescription )
import           Distribution.Version
                   ( simplifyVersionRange, orLaterVersion, earlierVersion
                   , hasUpperBound, hasLowerBound
                   )
import           Path
import           Path.IO
                   hiding
                     ( getModificationTime, getPermissions, withSystemTempDir )
import           RIO.PrettyPrint
import           Stack.Build ( mkBaseConfigOpts, build, buildLocalTargets )
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source ( projectLocalPackages )
import           Stack.Types.GhcPkgId
import           Stack.Package
import           Stack.SourceMap
import           Stack.Types.Build
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.SourceMap
import           Stack.Types.Version
import           System.Directory ( getModificationTime, getPermissions )
import qualified System.FilePath as FP

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.SDist" module.
data SDistException
  = CheckException (NonEmpty Check.PackageCheck)
  | CabalFilePathsInconsistentBug (Path Abs File) (Path Abs File)
  | ToTarPathException String
  deriving (Typeable)

instance Show SDistException where
  show (CheckException xs) = unlines $
    [ "Error: [S-6439]"
    , "Package check reported the following errors:"
    ] <> fmap show (NE.toList xs)
  show (CabalFilePathsInconsistentBug cabalfp cabalfp') = concat
    [ "Error: [S-9595]\n"
    , "The impossible happened! Two Cabal file paths are inconsistent: "
    , show (cabalfp, cabalfp')
    ]
  show (ToTarPathException e) =
    "Error: [S-7875\n"
    ++ e

instance Exception SDistException

data SDistOpts = SDistOpts
  { sdoptsDirsToWorkWith :: [String]
  -- ^ Directories to package
  , sdoptsPvpBounds :: Maybe PvpBounds
  -- ^ PVP Bounds overrides
  , sdoptsIgnoreCheck :: Bool
  -- ^ Whether to ignore check of the package for common errors
  , sdoptsBuildTarball :: Bool
  -- ^ Whether to build the tarball
  , sdoptsTarPath :: Maybe FilePath
  -- ^ Where to copy the tarball
  }

-- | Given the path to a local package, creates its source
-- distribution tarball.
--
-- While this yields a 'FilePath', the name of the tarball, this
-- tarball is not written to the disk and instead yielded as a lazy
-- bytestring.
getSDistTarball
  :: HasEnvConfig env
  => Maybe PvpBounds            -- ^ Override Config value
  -> Path Abs Dir               -- ^ Path to local package
  -> RIO env (FilePath, L.ByteString, Maybe (PackageIdentifier, L.ByteString))
  -- ^ Filename, tarball contents, and option Cabal file revision to upload
getSDistTarball mpvpBounds pkgDir = do
    config <- view configL
    let PvpBounds pvpBounds asRevision = fromMaybe (configPvpBounds config) mpvpBounds
        tweakCabal = pvpBounds /= PvpBoundsNone
        pkgFp = toFilePath pkgDir
    lp <- readLocalPackage pkgDir
    forM_ (packageSetupDeps (lpPackage lp)) $ \customSetupDeps ->
        case NE.nonEmpty (map (T.pack . packageNameString) (Map.keys customSetupDeps)) of
          Just nonEmptyDepTargets -> do
            eres <- buildLocalTargets nonEmptyDepTargets
            case eres of
              Left err ->
                logError $
                  "Error: [S-8399]\n" <>
                  "Error building custom-setup dependencies: " <>
                  displayShow err
              Right _ ->
                pure ()
          Nothing ->
            logWarn "unexpected empty custom-setup dependencies"
    sourceMap <- view $ envConfigL.to envConfigSourceMap

    installMap <- toInstallMap sourceMap
    (installedMap, _globalDumpPkgs, _snapshotDumpPkgs, _localDumpPkgs) <-
        getInstalled installMap
    let deps = Map.fromList [ (pid, ghcPkgId)
                            | (_, Library pid ghcPkgId _) <- Map.elems installedMap]

    logInfo $ "Getting file list for " <> fromString pkgFp
    (fileList, cabalfp) <- getSDistFileList lp deps
    logInfo $ "Building sdist tarball for " <> fromString pkgFp
    files <- normalizeTarballPaths (map (T.unpack . stripCR . T.pack) (lines fileList))

    -- We're going to loop below and eventually find the cabal
    -- file. When we do, we'll upload this reference, if the
    -- mpvpBounds value indicates that we should be uploading a cabal
    -- file revision.
    cabalFileRevisionRef <- liftIO (newIORef Nothing)

    -- NOTE: Could make this use lazy I/O to only read files as needed
    -- for upload (both GZip.compress and Tar.write are lazy).
    -- However, it seems less error prone and more predictable to read
    -- everything in at once, so that's what we're doing for now:
    let tarPath isDir fp =
            case Tar.toTarPath isDir (forceUtf8Enc (pkgId FP.</> fp)) of
                Left e -> throwIO $ ToTarPathException e
                Right tp -> pure tp
        -- convert a String of proper characters to a String of bytes
        -- in UTF8 encoding masquerading as characters. This is
        -- necessary for tricking the tar package into proper
        -- character encoding.
        forceUtf8Enc = S8.unpack . T.encodeUtf8 . T.pack
        packWith f isDir fp = liftIO $ f (pkgFp FP.</> fp) =<< tarPath isDir fp
        packDir = packWith Tar.packDirectoryEntry True
        packFile fp
            -- This is a Cabal file, we're going to tweak it, but only
            -- tweak it as a revision.
            | tweakCabal && isCabalFp fp && asRevision = do
                lbsIdent <- getCabalLbs pvpBounds (Just 1) cabalfp sourceMap
                liftIO (writeIORef cabalFileRevisionRef (Just lbsIdent))
                packWith packFileEntry False fp
            -- Same, except we'll include the Cabal file in the
            -- original tarball upload.
            | tweakCabal && isCabalFp fp = do
                (_ident, lbs) <- getCabalLbs pvpBounds Nothing cabalfp sourceMap
                currTime <- liftIO getPOSIXTime -- Seconds from UNIX epoch
                tp <- liftIO $ tarPath False fp
                pure $ (Tar.fileEntry tp lbs) { Tar.entryTime = floor currTime }
            | otherwise = packWith packFileEntry False fp
        isCabalFp fp = toFilePath pkgDir FP.</> fp == toFilePath cabalfp
        tarName = pkgId FP.<.> "tar.gz"
        pkgId = packageIdentifierString (packageIdentifier (lpPackage lp))
    dirEntries <- mapM packDir (dirsFromFiles files)
    fileEntries <- mapM packFile files
    mcabalFileRevision <- liftIO (readIORef cabalFileRevisionRef)
    pure (tarName, GZip.compress (Tar.write (dirEntries ++ fileEntries)), mcabalFileRevision)

-- | Get the PVP bounds-enabled version of the given Cabal file
getCabalLbs :: HasEnvConfig env
            => PvpBoundsType
            -> Maybe Int -- ^ optional revision
            -> Path Abs File -- ^ Cabal file
            -> SourceMap
            -> RIO env (PackageIdentifier, L.ByteString)
getCabalLbs pvpBounds mrev cabalfp sourceMap = do
    (gpdio, _name, cabalfp') <- loadCabalFilePath (parent cabalfp)
    gpd <- liftIO $ gpdio NoPrintWarnings
    unless (cabalfp == cabalfp') $
      throwIO $ CabalFilePathsInconsistentBug cabalfp cabalfp'
    installMap <- toInstallMap sourceMap
    (installedMap, _, _, _) <- getInstalled installMap
    let internalPackages = Set.fromList $
          gpdPackageName gpd :
          map (Cabal.unqualComponentNameToPackageName . fst) (Cabal.condSubLibraries gpd)
        gpd' = gtraverseT (addBounds internalPackages installMap installedMap) gpd
        gpd'' =
          case mrev of
            Nothing -> gpd'
            Just rev -> gpd'
              { Cabal.packageDescription
               = (Cabal.packageDescription gpd')
                  { Cabal.customFieldsPD
                  = (("x-revision", show rev):)
                  $ filter (\(x, _) -> map toLower x /= "x-revision")
                  $ Cabal.customFieldsPD
                  $ Cabal.packageDescription gpd'
                  }
              }
        ident = Cabal.package $ Cabal.packageDescription gpd''
    -- Sanity rendering and reparsing the input, to ensure there are no
    -- cabal bugs, since there have been bugs here before, and currently
    -- are at the time of writing:
    --
    -- https://github.com/haskell/cabal/issues/1202
    -- https://github.com/haskell/cabal/issues/2353
    -- https://github.com/haskell/cabal/issues/4863 (current issue)
    let roundtripErrs =
          [ flow "Bug detected in Cabal library. ((parse . render . parse) === \
                 \id) does not hold for the Cabal file at"
          <+> pretty cabalfp
          , ""
          ]
        (_warnings, eres) = Cabal.runParseResult
                          $ Cabal.parseGenericPackageDescription
                          $ T.encodeUtf8
                          $ T.pack
                          $ showGenericPackageDescription gpd
    case eres of
      Right roundtripped
        | roundtripped == gpd -> pure ()
        | otherwise -> do
            prettyWarn $ vsep $ roundtripErrs ++
              [ "This seems to be fixed in development versions of Cabal, but \
                \at time of writing, the fix is not in any released versions."
              , ""
              ,  "Please see this GitHub issue for status:" <+> style Url "https://github.com/commercialhaskell/stack/issues/3549"
              , ""
              , fillSep
                [ flow "If the issue is closed as resolved, then you may be \
                       \able to fix this by upgrading to a newer version of \
                       \Stack via"
                , style Shell "stack upgrade"
                , flow "for latest stable version or"
                , style Shell "stack upgrade --git"
                , flow "for the latest development version."
                ]
              , ""
              , fillSep
                [ flow "If the issue is fixed, but updating doesn't solve the \
                       \problem, please check if there are similar open \
                       \issues, and if not, report a new issue to the Stack \
                       \issue tracker, at"
                , style Url "https://github.com/commercialhaskell/stack/issues/new"
                ]
              , ""
              , flow "If the issue is not fixed, feel free to leave a comment \
                     \on it indicating that you would like it to be fixed."
              , ""
              ]
      Left (_version, errs) -> do
        prettyWarn $ vsep $ roundtripErrs ++
          [ flow "In particular, parsing the rendered Cabal file is yielding a \
                 \parse error. Please check if there are already issues \
                 \tracking this, and if not, please report new issues to the \
                 \Stack and Cabal issue trackers, via"
          , bulletedList
            [ style Url "https://github.com/commercialhaskell/stack/issues/new"
            , style Url "https://github.com/haskell/cabal/issues/new"
            ]
          , flow $ "The parse error is: " ++ unlines (map show (toList errs))
          , ""
          ]
    pure
      ( ident
      , TLE.encodeUtf8 $ TL.pack $ showGenericPackageDescription gpd''
      )
  where
    addBounds :: Set PackageName -> InstallMap -> InstalledMap -> Dependency -> Dependency
    addBounds internalPackages installMap installedMap dep@(Dependency name range s) =
      if name `Set.member` internalPackages
        then dep
        else case foundVersion of
          Nothing -> dep
          Just version -> Dependency name (simplifyVersionRange
            $ (if toAddUpper && not (hasUpperBound range) then addUpper version else id)
            $ (if toAddLower && not (hasLowerBound range) then addLower version else id)
              range) s
      where
        foundVersion =
          case Map.lookup name installMap of
              Just (_, version) -> Just version
              Nothing ->
                  case Map.lookup name installedMap of
                      Just (_, installed) -> Just (installedVersion installed)
                      Nothing -> Nothing

    addUpper version = intersectVersionRanges
        (earlierVersion $ nextMajorVersion version)
    addLower version = intersectVersionRanges (orLaterVersion version)

    (toAddLower, toAddUpper) =
      case pvpBounds of
        PvpBoundsNone  -> (False, False)
        PvpBoundsUpper -> (False, True)
        PvpBoundsLower -> (True,  False)
        PvpBoundsBoth  -> (True,  True)

-- | Traverse a data type.
gtraverseT :: (Data a,Typeable b) => (Typeable b => b -> b) -> a -> a
gtraverseT f =
  gmapT (\x -> case cast x of
                 Nothing -> gtraverseT f x
                 Just b  -> fromMaybe x (cast (f b)))

-- | Read in a 'LocalPackage' config.  This makes some default decisions
-- about 'LocalPackage' fields that might not be appropriate for other
-- use-cases.
readLocalPackage :: HasEnvConfig env => Path Abs Dir -> RIO env LocalPackage
readLocalPackage pkgDir = do
    config  <- getDefaultPackageConfig
    (gpdio, _, cabalfp) <- loadCabalFilePath pkgDir
    gpd <- liftIO $ gpdio YesPrintWarnings
    let package = resolvePackage config gpd
    pure LocalPackage
        { lpPackage = package
        , lpWanted = False -- HACK: makes it so that sdist output goes to a log instead of a file.
        , lpCabalFile = cabalfp
        -- NOTE: these aren't the 'correct values, but aren't used in
        -- the usage of this function in this module.
        , lpTestBench = Nothing
        , lpBuildHaddocks = False
        , lpForceDirty = False
        , lpDirtyFiles = pure Nothing
        , lpNewBuildCaches = pure Map.empty
        , lpComponentFiles = pure Map.empty
        , lpComponents = Set.empty
        , lpUnbuildable = Set.empty
        }

-- | Returns a newline-separate list of paths, and the absolute path to the
-- Cabal file.
getSDistFileList :: HasEnvConfig env => LocalPackage -> Map PackageIdentifier GhcPkgId -> RIO env (String, Path Abs File)
getSDistFileList lp deps =
    withSystemTempDir (stackProgName <> "-sdist") $ \tmpdir -> do
        let bopts = defaultBuildOpts
        let boptsCli = defaultBuildOptsCLI
        baseConfigOpts <- mkBaseConfigOpts boptsCli
        locals <- projectLocalPackages
        withExecuteEnv bopts boptsCli baseConfigOpts locals
            [] [] [] Nothing -- provide empty list of globals. This is a hack around custom Setup.hs files
            $ \ee ->
            withSingleContext ac ee task deps (Just "sdist") $ \_package cabalfp _pkgDir cabal _announce _outputType -> do
                let outFile = toFilePath tmpdir FP.</> "source-files-list"
                cabal CloseOnException KeepTHLoading ["sdist", "--list-sources", outFile]
                contents <- liftIO (S.readFile outFile)
                pure (T.unpack $ T.decodeUtf8With T.lenientDecode contents, cabalfp)
  where
    package = lpPackage lp
    ac = ActionContext Set.empty [] ConcurrencyAllowed
    task = Task
        { taskProvides = PackageIdentifier (packageName package) (packageVersion package)
        , taskType = TTLocalMutable lp
        , taskConfigOpts = TaskConfigOpts
            { tcoMissing = Set.empty
            , tcoOpts = \_ -> ConfigureOpts [] []
            }
        , taskBuildHaddock = False
        , taskPresent = Map.empty
        , taskAllInOne = True
        , taskCachePkgSrc = CacheSrcLocal (toFilePath (parent $ lpCabalFile lp))
        , taskAnyMissing = True
        , taskBuildTypeConfig = False
        }

normalizeTarballPaths :: HasRunner env => [FilePath] -> RIO env [FilePath]
normalizeTarballPaths fps = do
    -- TODO: consider whether erroring out is better - otherwise the
    -- user might upload an incomplete tar?
    unless (null outsideDir) $
        logWarn $
            "Warning: These files are outside of the package directory, and will be omitted from the tarball: " <>
            displayShow outsideDir
    pure (nubOrd files)
  where
    (outsideDir, files) = partitionEithers (map pathToEither fps)
    pathToEither fp = maybe (Left fp) Right (normalizePath fp)

normalizePath :: FilePath -> Maybe FilePath
normalizePath = fmap FP.joinPath . go . FP.splitDirectories . FP.normalise
  where
    go [] = Just []
    go ("..":_) = Nothing
    go (_:"..":xs) = go xs
    go (x:xs) = (x :) <$> go xs

dirsFromFiles :: [FilePath] -> [FilePath]
dirsFromFiles dirs = Set.toAscList (Set.delete "." results)
  where
    results = foldl' (\s -> go s . FP.takeDirectory) Set.empty dirs
    go s x
      | Set.member x s = s
      | otherwise = go (Set.insert x s) (FP.takeDirectory x)

-- | Check package in given tarball. This will log all warnings
-- and will throw an exception in case of critical errors.
--
-- Note that we temporarily decompress the archive to analyze it.
checkSDistTarball
  :: HasEnvConfig env
  => SDistOpts -- ^ The configuration of what to check
  -> Path Abs File -- ^ Absolute path to tarball
  -> RIO env ()
checkSDistTarball opts tarball = withTempTarGzContents tarball $ \pkgDir' -> do
    pkgDir  <- (pkgDir' </>) `liftM`
        (parseRelDir . FP.takeBaseName . FP.takeBaseName . toFilePath $ tarball)
    --               ^ drop ".tar"     ^ drop ".gz"
    when (sdoptsBuildTarball opts) (buildExtractedTarball ResolvedPath
                                      { resolvedRelative = RelFilePath "this-is-not-used" -- ugly hack
                                      , resolvedAbsolute = pkgDir
                                      })
    unless (sdoptsIgnoreCheck opts) (checkPackageInExtractedTarball pkgDir)

checkPackageInExtractedTarball
  :: HasEnvConfig env
  => Path Abs Dir -- ^ Absolute path to tarball
  -> RIO env ()
checkPackageInExtractedTarball pkgDir = do
    (gpdio, name, _cabalfp) <- loadCabalFilePath pkgDir
    gpd <- liftIO $ gpdio YesPrintWarnings
    config  <- getDefaultPackageConfig
    let PackageDescriptionPair pkgDesc _ = resolvePackageDescription config gpd
    logInfo $
        "Checking package '" <> fromString (packageNameString name) <> "' for common mistakes"
    let pkgChecks =
          -- MSS 2017-12-12: Try out a few different variants of
          -- pkgDesc to try and provoke an error or warning. I don't
          -- know why, but when using `Just pkgDesc`, it appears that
          -- Cabal does not detect that `^>=` is used with
          -- `cabal-version: 1.24` or earlier. It seems like pkgDesc
          -- (the one we create) does not populate the `buildDepends`
          -- field, whereas flattenPackageDescription from Cabal
          -- does. In any event, using `Nothing` seems more logical
          -- for this check anyway, and the fallback to `Just pkgDesc`
          -- is just a crazy sanity check.
          case Check.checkPackage gpd Nothing of
            [] -> Check.checkPackage gpd (Just pkgDesc)
            x -> x
    fileChecks <- liftIO $ Check.checkPackageFiles minBound pkgDesc (toFilePath pkgDir)
    let checks = pkgChecks ++ fileChecks
        (errors, warnings) =
          let criticalIssue (Check.PackageBuildImpossible _) = True
              criticalIssue (Check.PackageDistInexcusable _) = True
              criticalIssue _ = False
          in List.partition criticalIssue checks
    unless (null warnings) $
        logWarn $ "Package check reported the following warnings:\n" <>
                   mconcat (List.intersperse "\n" . fmap displayShow $ warnings)
    case NE.nonEmpty errors of
        Nothing -> pure ()
        Just ne -> throwM $ CheckException ne

buildExtractedTarball :: HasEnvConfig env => ResolvedPath Dir -> RIO env ()
buildExtractedTarball pkgDir = do
  envConfig <- view envConfigL
  localPackageToBuild <- readLocalPackage $ resolvedAbsolute pkgDir
  -- We remove the path based on the name of the package
  let isPathToRemove path = do
        localPackage <- readLocalPackage path
        pure $ packageName (lpPackage localPackage) == packageName (lpPackage localPackageToBuild)
  pathsToKeep
    <- fmap Map.fromList
     $ flip filterM (Map.toList (smwProject (bcSMWanted (envConfigBuildConfig envConfig))))
     $ fmap not . isPathToRemove . resolvedAbsolute . ppResolvedDir . snd
  pp <- mkProjectPackage YesPrintWarnings pkgDir False
  let adjustEnvForBuild env =
        let updatedEnvConfig = envConfig
              { envConfigSourceMap = updatePackagesInSourceMap (envConfigSourceMap envConfig)
              , envConfigBuildConfig = updateBuildConfig (envConfigBuildConfig envConfig)
              }
            updateBuildConfig bc = bc
              { bcConfig = (bcConfig bc)
                 { configBuild = defaultBuildOpts { boptsTests = True } }
              }
        in set envConfigL updatedEnvConfig env
      updatePackagesInSourceMap sm =
        sm {smProject = Map.insert (cpName $ ppCommon pp) pp pathsToKeep}
  local adjustEnvForBuild $ build Nothing

-- | Version of 'checkSDistTarball' that first saves lazy bytestring to
-- temporary directory and then calls 'checkSDistTarball' on it.
checkSDistTarball'
  :: HasEnvConfig env
  => SDistOpts
  -> String       -- ^ Tarball name
  -> L.ByteString -- ^ Tarball contents as a byte string
  -> RIO env ()
checkSDistTarball' opts name bytes = withSystemTempDir "stack" $ \tpath -> do
    npath   <- (tpath </>) `liftM` parseRelFile name
    liftIO $ L.writeFile (toFilePath npath) bytes
    checkSDistTarball opts npath

withTempTarGzContents
  :: Path Abs File                     -- ^ Location of tarball
  -> (Path Abs Dir -> RIO env a) -- ^ Perform actions given dir with tarball contents
  -> RIO env a
withTempTarGzContents apath f = withSystemTempDir "stack" $ \tpath -> do
    archive <- liftIO $ L.readFile (toFilePath apath)
    liftIO . Tar.unpack (toFilePath tpath) . Tar.read . GZip.decompress $ archive
    f tpath

--------------------------------------------------------------------------------

-- Copy+modified from the tar package to avoid issues with lazy IO ( see
-- https://github.com/commercialhaskell/stack/issues/1344 )

packFileEntry :: FilePath -- ^ Full path to find the file on the local disk
              -> Tar.TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO Tar.Entry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  content <- S.readFile filepath
  let size = fromIntegral (S.length content)
  pure (Tar.simpleEntry tarpath (Tar.NormalFile (L.fromStrict content) size)) {
    Tar.entryPermissions = if executable perms then Tar.executableFilePermissions
                                               else Tar.ordinaryFilePermissions,
    Tar.entryTime = mtime
  }

getModTime :: FilePath -> IO Tar.EpochTime
getModTime path = do
    t <- getModificationTime path
    pure . floor . utcTimeToPOSIXSeconds $ t

getDefaultPackageConfig :: (MonadIO m, MonadReader env m, HasEnvConfig env)
  => m PackageConfig
getDefaultPackageConfig = do
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  pure PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = mempty
    , packageConfigGhcOptions = []
    , packageConfigCabalConfigOpts = []
    , packageConfigCompilerVersion = compilerVersion
    , packageConfigPlatform = platform
    }
