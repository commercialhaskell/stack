{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE RecordWildCards       #-}

-- Create a source distribution tarball
module Stack.SDist
    ( sdist
    , getSDistTarball
    , checkSDistTarball
    , checkSDistTarball'
    , SDistOpts(..)
    , PvpBoundsOpts(..)
    , DependencyConfigurationSource(..)
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Concurrent.Execute (ActionContext(..))
import           Control.Monad (when, unless, void, liftM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (liftBaseWith)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Data (Data, Typeable, cast, gmapM)
import           Data.Either (partitionEithers)
import           Data.Foldable (forM_)
import           Data.List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Extra
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock.POSIX
import           Distribution.Package (Dependency (..))
import qualified Distribution.PackageDescription.Check as Check
import           Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import qualified Distribution.Text
import           Distribution.Version hiding (Version, intersectVersionRanges)
import           Distribution.Version.Extra
import           Network.HTTP.Client.Conduit (HasHttpManager(..))
import           Path
import           Path.IO hiding (getModificationTime, getPermissions)
import           Prelude -- Fix redundant import warnings
import           Stack.Build (mkBaseConfigOpts)
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source (loadSourceMap, getDefaultPackageConfig)
import           Stack.Build.Target
import           Stack.BuildPlan (loadResolver)
import           Stack.Config (loadProjectConfig, makeConcreteResolver, resolvePackageEntry)
import           Stack.Constants
import           Stack.Package
import qualified Stack.Sig as Sig
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory (getModificationTime, getPermissions)
import qualified System.FilePath as FP

-- | Options for the PVP bounds generation.
data PvpBoundsOpts = PvpBoundsOpts
    { pvpBoundsOptsPvpBounds :: !PvpBounds
    -- TODO (sjakobi): It would be better to use a Set or HashSet here than a list if only I could
    -- figure out how to get the necessary instances for 'Resolver'.
    , pvpBoundsOptsExtraDependencyConfigurationSources :: ![DependencyConfigurationSource FilePath AbstractResolver]
    } deriving (Show)

defaultPvpBoundsOpts :: PvpBoundsOpts
defaultPvpBoundsOpts = PvpBoundsOpts
    { pvpBoundsOptsPvpBounds = PvpBoundsNone
    , pvpBoundsOptsExtraDependencyConfigurationSources = []
    }

-- | A "dependency configuration" is basically a @'Map' 'PackageName' 'Version'@.
--
-- A 'DependencyConfigurationSource' specifies such a configuration.
data DependencyConfigurationSource projectConfig resolver
    = DCSProjectConfig !projectConfig
    | DCSResolver !resolver
    | DCSCurrentConfiguration
    deriving (Eq, Ord, Show)

data SDistException
    = ProjectConfigDoesn'tExist (Path Abs File)
    | CheckException (NonEmpty Check.PackageCheck)
    -- ^ Package check with negative results.
    deriving (Typeable)

instance Exception SDistException

instance Show SDistException where
    show (ProjectConfigDoesn'tExist absFile) =
        "There is no file at " ++ toFilePath absFile
    show (CheckException xs) =
        "Package check reported the following errors:\n" ++
        (intercalate "\n" . fmap show . NE.toList $ xs)

data SDistOpts = SDistOpts
    { sdistOptsDirs :: ![FilePath]
      -- ^ The package directories of the packages for which the tarballs should be built.
      -- If no directories are specified, the tarballs for all the project-local packages the project should be built.
    , sdistOptsPvpBoundsOpts :: !(Maybe PvpBoundsOpts)
    , sdistOptsIgnoreCheck :: !Bool
      -- ^ Skip checking the packages for common mistakes?
    , sdistOptsSign :: !Bool
      -- ^ Sign packages and upload signatures?
    , sdistOptsSigServerUrl :: !String
      -- ^ URL of the signature server.
    }

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,MonadLogger m,MonadBaseControl IO m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env,HasHttpManager env)

sdist :: M env m => SDistOpts -> m ()
sdist SDistOpts{..} = do
    -- If no directories are specified, build all sdist tarballs.
    dirs' <- if null sdistOptsDirs
        then asks (Map.keys . envConfigPackages . getEnvConfig)
        else mapM resolveDir' sdistOptsDirs
    manager <- asks getHttpManager
    forM_ dirs' $ \dir -> do
        -- TODO: Instead of recomputing the known dependency versions for each package,
        -- compute them once, and pass them to getSDistTarball.
        (tarName, tarBytes) <- getSDistTarball sdistOptsPvpBoundsOpts dir
        distDir <- distDirFromDir dir
        tarPath <- (distDir </>) <$> parseRelFile tarName
        ensureDir (parent tarPath)
        liftIO $ L.writeFile (toFilePath tarPath) tarBytes
        unless sdistOptsIgnoreCheck (checkSDistTarball tarPath)
        $logInfo $ "Wrote sdist tarball to " <> T.pack (toFilePath tarPath)
        when sdistOptsSign (void $ Sig.sign manager sdistOptsSigServerUrl tarPath)

-- | Given the path to a local package, creates its source
-- distribution tarball.
--
-- While this yields a 'FilePath', the name of the tarball, this
-- tarball is not written to the disk and instead yielded as a lazy
-- bytestring.
getSDistTarball
  :: M env m
  => Maybe PvpBoundsOpts
  -> Path Abs Dir                   -- ^ Path to local package
  -> m (FilePath, L.ByteString)     -- ^ Filename and tarball contents
getSDistTarball mpvpBoundsOpts pkgDir = do
    config <- asks getConfig
    let pvpBoundsOpts =
            fromMaybe
                defaultPvpBoundsOpts { pvpBoundsOptsPvpBounds = configPvpBounds config }
                mpvpBoundsOpts
        tweakCabal = pvpBoundsOptsPvpBounds pvpBoundsOpts /= PvpBoundsNone
        pkgFp = toFilePath pkgDir
    lp <- readLocalPackage pkgDir
    $logInfo $ "Getting file list for " <> T.pack pkgFp
    (fileList, cabalfp) <-  getSDistFileList lp
    $logInfo $ "Building sdist tarball for " <> T.pack pkgFp
    files <- normalizeTarballPaths (lines fileList)
    -- NOTE: Could make this use lazy I/O to only read files as needed
    -- for upload (both GZip.compress and Tar.write are lazy).
    -- However, it seems less error prone and more predictable to read
    -- everything in at once, so that's what we're doing for now:
    let tarPath isDir fp = either error id
            (Tar.toTarPath isDir (pkgId FP.</> fp))
        packWith f isDir fp = liftIO $ f (pkgFp FP.</> fp) (tarPath isDir fp)
        packDir = packWith Tar.packDirectoryEntry True
        packFile fp
            | tweakCabal && isCabalFp fp = do
                lbs <- getCabalFileContents pvpBoundsOpts cabalfp
                return $ Tar.fileEntry (tarPath False fp) lbs
            | otherwise = packWith packFileEntry False fp
        isCabalFp fp = toFilePath pkgDir FP.</> fp == toFilePath cabalfp
        tarName = pkgId FP.<.> "tar.gz"
        pkgId = packageIdentifierString (packageIdentifier (lpPackage lp))
    dirEntries <- mapM packDir (dirsFromFiles files)
    fileEntries <- mapM packFile files
    return (tarName, GZip.compress (Tar.write (dirEntries ++ fileEntries)))

-- | Get the PVP bounds-enabled version of the given cabal file
getCabalFileContents
    :: M env m
    => PvpBoundsOpts
    -> Path Abs File   -- ^ Cabal file
    -> m L.ByteString
getCabalFileContents pvpBoundsOpts fp = do
    bs <- liftIO $ S.readFile (toFilePath fp)
    (_warnings, gpd) <- readPackageUnresolvedBS Nothing bs
    versionsWithWitnesses <- do
        let dependencyConfigurationSources =
                DCSCurrentConfiguration : pvpBoundsOptsExtraDependencyConfigurationSources pvpBoundsOpts
        dependencyConfigs <- mapMaybeM (loadDependencyConfigs fp) dependencyConfigurationSources
        return (mergeDependencyConfigs dependencyConfigs)
    gpd' <- gtraverseM (addBounds versionsWithWitnesses) gpd
    return $ TLE.encodeUtf8 $ TL.pack $ showGenericPackageDescription gpd'
  where
    addBounds :: M env m => MultiSourceDependencyConfig (Path Abs File) Resolver -> Dependency -> m Dependency
    addBounds depConfig dep@(Dependency cname range) = do
        let pkgName = fromCabalPackageName cname
        case Map.lookup pkgName depConfig of
            Nothing -> do
                $logWarn $ T.concat
                    [ T.pack (toFilePath fp)
                    , " contains unknown dependency: "
                    , T.pack (Distribution.Text.display dep)
                    , ". Can't apply pvp bounds."
                    ]
                return dep
            Just witnessedVersions -> do
                let computedVersionRange =
                        computePvpVersionRange (pvpBoundsOptsPvpBounds pvpBoundsOpts) range witnessedVersions
                    gaps = inferredVersionRangeInferenceGaps computedVersionRange
                    newRange = inferredVersionRangeInferredVersionRange computedVersionRange
                    versionRangeChanged = newRange /= simplifyVersionRange range
                when versionRangeChanged $ do
                    projectRoot <- asks (bcRoot . getBuildConfig)
                    let tryStripProjectRoot absFile =
                            case stripDir projectRoot absFile of
                                Just x -> toFilePath x
                                Nothing -> toFilePath absFile
                        formatDCS (DCSProjectConfig absFile) = T.pack (tryStripProjectRoot absFile)
                        formatDCS (DCSResolver resolver) = resolverName resolver
                        formatDCS DCSCurrentConfiguration = "current configuration"
                    $logInfo $ T.concat
                        [ "Adjusting version range for dependency "
                        , packageNameText pkgName
                        , ":"
                        ]
                    $logInfo "  Known versions:"
                    forM_ (Map.toList witnessedVersions) $ \(version, witnesses) ->
                        $logInfo $ T.concat
                            [ "    * "
                            , versionText version
                            , " ("
                            , T.intercalate ", " (map formatDCS (NE.toList witnesses))
                            , ")"
                            ]
                    $logInfo ("  Old range: " <> T.pack (Distribution.Text.display range))
                    $logInfo ("  New range: " <> T.pack (Distribution.Text.display newRange))
                    unless (isNoVersion gaps) $ do
                        $logInfo "  The new version range contains subranges compatibility with which cannot be inferred within the PVP:"
                        $logInfo ("    " <> T.pack (Distribution.Text.display gaps))
                return (Dependency cname newRange)

-- | 'Version's together with the dependendency configurations in which they are specified.
type VersionWitnesses projectConfig resolver =
    Map Version (NonEmpty (DependencyConfigurationSource projectConfig resolver))

type MultiSourceDependencyConfig projectConfig resolver =
    Map PackageName (VersionWitnesses projectConfig resolver)

mergeDependencyConfigs
    :: [(DependencyConfigurationSource projectConfig resolver, Map PackageName Version)]
    -> MultiSourceDependencyConfig projectConfig resolver
mergeDependencyConfigs xs =
    Map.unionsWith
        (Map.unionWith (<>))
        [ Map.map (\v -> Map.singleton v (dcs NE.:| [])) m
        | (dcs, m) <- xs
        ]

-- TODO: It would probably be better to use existing functionality like @stack list-dependencies"
-- to get the package versions.
loadDependencyConfigs
    :: M env m
    => Path Abs File -- ^ Cabal file of the current package
    -> DependencyConfigurationSource FilePath AbstractResolver
    -> m (Maybe (DependencyConfigurationSource (Path Abs File) Resolver, Map PackageName Version))
       -- ^ Nothing if the 'DependencyConfigurationSource' doesn't apply to the package, e.g.
       --   a project config doesn't list the package.
loadDependencyConfigs cabalFp = \case
    DCSProjectConfig fp -> do
        absFile <- resolveFile' fp
        mProjectConfig <- loadProjectConfig (Just absFile)
        case mProjectConfig of
            Just (project, _fp, _configMonoid) -> do
                packageInProject <- project `projectContainsPackage` cabalFp
                if packageInProject
                    then do
                        resolverVersions <-
                            loadResolverVersions (Just absFile) (projectResolver project)
                        let versions = projectExtraDeps project `Map.union` resolverVersions
                        return (Just (DCSProjectConfig absFile, versions))
                        -- FIXME: Respect sourceMap of project (as configured in fp)
                        -- TODO: Do we need to look at extra package dbs etc, too?
                    else do
                        $logWarn $ T.concat
                            [ "The project configuration in "
                            , T.pack (toFilePath absFile)
                            , " doesn't contain the "
                            , packageName' cabalFp
                            , " package."
                            ]
                        $logWarn "I will ignore this configuration during pvp-bounds generation."
                        return Nothing
            Nothing ->
                throwM (ProjectConfigDoesn'tExist absFile)
    DCSResolver abstractResolver -> do
        resolver <- makeConcreteResolver abstractResolver
        resolverVersions <- do
            configPath <- asks (bcStackYaml . getBuildConfig)
            loadResolverVersions (Just configPath) resolver
        sourceVersions <- do
            (_, _, _, _, sourceMap) <- loadSourceMap AllowNoTargets defaultBuildOptsCLI
            return (Map.map piiVersion sourceMap)
        return (Just (DCSResolver resolver, sourceVersions `Map.union` resolverVersions))
    DCSCurrentConfiguration -> do
        versions <- loadConfiguredPackageVersions
        return (Just (DCSCurrentConfiguration, versions))
  where
    packageName' = T.pack . FP.dropExtension . toFilePath . filename

projectContainsPackage
    :: M env m
    => Project
    -> Path Abs File -- ^ Location of the cabal file of the package
    -> m Bool
projectContainsPackage project cabalAbsFile = do
    menv <- getMinimalEnvOverride
    projectRoot <- asks (bcRoot . getBuildConfig)
    projectPackageDirs <- do
        allProjectPackageDirs <-
            concat <$> mapM (resolvePackageEntry menv projectRoot) (projectPackages project)
        -- exclude "local extra-deps"
        return [ dir | (dir, treatLikeExtraDep) <- allProjectPackageDirs, not treatLikeExtraDep ]
    return ((parent cabalAbsFile) `elem` projectPackageDirs)

loadConfiguredPackageVersions
    :: M env m
    => m (Map PackageName Version)
loadConfiguredPackageVersions = do
    (_, _, _, _, sourceMap) <- loadSourceMap AllowNoTargets defaultBuildOptsCLI
    let sourceVersions = Map.map piiVersion sourceMap
    installedVersions <- do
        menv <- getMinimalEnvOverride
        (installedMap, _, _, _) <- getInstalled menv defaultGetInstalledOpts sourceMap
        return (Map.map (installedVersion . snd) installedMap)
    return (sourceVersions `Map.union` installedVersions)

loadResolverVersions
    :: M env m
    => Maybe (Path Abs File)
    -> Resolver
    -> m (Map PackageName Version)
loadResolverVersions mConfigPath resolver = do
    (mbp, _loadedResolver) <- loadResolver mConfigPath resolver
    return (Map.map mpiVersion (mbpPackages mbp))

data InferredVersionRange = InferredVersionRange
    { inferredVersionRangeInferredVersionRange :: !VersionRange
    , inferredVersionRangeInferenceGaps :: !VersionRange
    }

computePvpVersionRange
    :: PvpBounds
    -> VersionRange -- ^ Original version range
    -> VersionWitnesses a b -- ^ Versions that are known to be compatible, must be non-empty
    -> InferredVersionRange
computePvpVersionRange pvpBounds oldRange witnesses =
    InferredVersionRange newRange gaps
  where
    newRange = simplifyVersionRange
        $ (if toAddUpper && not (hasUpper oldRange) then addUpper maxVersion else id)
        $ (if toAddLower && not (hasLower oldRange) then addLower minVersion else id)
          oldRange
      where
        (toAddLower, toAddUpper) =
          case pvpBounds of
            PvpBoundsNone  -> (False, False)
            PvpBoundsUpper -> (False, True)
            PvpBoundsLower -> (True,  False)
            PvpBoundsBoth  -> (True,  True)

        addUpper version = intersectVersionRanges
            (earlierVersion $ toCabalVersion $ nextMajorVersion version)
        addLower version = intersectVersionRanges
            (orLaterVersion (toCabalVersion version))

        minVersion = Set.findMin (Map.keysSet witnesses)
        maxVersion = Set.findMax (Map.keysSet witnesses)

    gaps = simplifyVersionRange (differenceVersionRanges newRange totalWitnessedRange)
      where
        differenceVersionRanges vr0 vr1 = intersectVersionRanges vr0 (invertVersionRange vr1)

        totalWitnessedRange =
            foldl'
                unionVersionRanges
                noVersion
                (map witnessedRange (Map.keys witnesses))
          where
            witnessedRange version =
                intersectVersionRanges
                    (earlierVersion $ toCabalVersion $ nextMajorVersion version)
                    (orLaterVersion $ toCabalVersion version)

-- | Traverse a data type.
gtraverseM :: (Data a, Typeable b, Monad m) => (b -> m b) -> a -> m a
gtraverseM f =
  gmapM (\x -> case cast x of
                 Nothing -> gtraverseM f x
                 Just b -> do
                    b' <- f b
                    return (fromMaybe x (cast b')))

-- | Read in a 'LocalPackage' config.  This makes some default decisions
-- about 'LocalPackage' fields that might not be appropriate for other
-- use-cases.
readLocalPackage :: M env m => Path Abs Dir -> m LocalPackage
readLocalPackage pkgDir = do
    cabalfp <- findOrGenerateCabalFile pkgDir
    config  <- getDefaultPackageConfig
    (warnings,package) <- readPackage config cabalfp
    mapM_ (printCabalFileWarning cabalfp) warnings
    return LocalPackage
        { lpPackage = package
        , lpWanted = False -- HACK: makes it so that sdist output goes to a log instead of a file.
        , lpDir = pkgDir
        , lpCabalFile = cabalfp
        -- NOTE: these aren't the 'correct values, but aren't used in
        -- the usage of this function in this module.
        , lpTestDeps = Map.empty
        , lpBenchDeps = Map.empty
        , lpTestBench = Nothing
        , lpForceDirty = False
        , lpDirtyFiles = Nothing
        , lpNewBuildCache = Map.empty
        , lpFiles = Set.empty
        , lpComponents = Set.empty
        , lpUnbuildable = Set.empty
        }

-- | Returns a newline-separate list of paths, and the absolute path to the .cabal file.
getSDistFileList :: M env m => LocalPackage -> m (String, Path Abs File)
getSDistFileList lp =
    withSystemTempDir (stackProgName <> "-sdist") $ \tmpdir -> do
        menv <- getMinimalEnvOverride
        let bopts = defaultBuildOpts
        let boptsCli = defaultBuildOptsCLI
        baseConfigOpts <- mkBaseConfigOpts boptsCli
        (_, _mbp, locals, _extraToBuild, _sourceMap) <- loadSourceMap NeedTargets boptsCli
        runInBase <- liftBaseWith $ \run -> return (void . run)
        withExecuteEnv menv bopts boptsCli baseConfigOpts locals
            [] [] [] -- provide empty list of globals. This is a hack around custom Setup.hs files
            $ \ee ->
            withSingleContext runInBase ac ee task Nothing (Just "sdist") $ \_package cabalfp _pkgDir cabal _announce _console _mlogFile -> do
                let outFile = toFilePath tmpdir FP.</> "source-files-list"
                cabal False ["sdist", "--list-sources", outFile]
                contents <- liftIO (readFile outFile)
                return (contents, cabalfp)
  where
    package = lpPackage lp
    ac = ActionContext Set.empty
    task = Task
        { taskProvides = PackageIdentifier (packageName package) (packageVersion package)
        , taskType = TTLocal lp
        , taskConfigOpts = TaskConfigOpts
            { tcoMissing = Set.empty
            , tcoOpts = \_ -> ConfigureOpts [] []
            }
        , taskPresent = Map.empty
        , taskAllInOne = True
        }

normalizeTarballPaths :: M env m => [FilePath] -> m [FilePath]
normalizeTarballPaths fps = do
    -- TODO: consider whether erroring out is better - otherwise the
    -- user might upload an incomplete tar?
    unless (null outsideDir) $
        $logWarn $ T.concat
            [ "Warning: These files are outside of the package directory, and will be omitted from the tarball: "
            , T.pack (show outsideDir)]
    return files
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
checkSDistTarball :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasEnvConfig env)
  => Path Abs File -- ^ Absolute path to tarball
  -> m ()
checkSDistTarball tarball = withTempTarGzContents tarball $ \pkgDir' -> do
    pkgDir  <- (pkgDir' </>) `liftM`
        (parseRelDir . FP.takeBaseName . FP.takeBaseName . toFilePath $ tarball)
    --               ^ drop ".tar"     ^ drop ".gz"
    cabalfp <- findOrGenerateCabalFile pkgDir
    name    <- parsePackageNameFromFilePath cabalfp
    config  <- getDefaultPackageConfig
    (gdesc, pkgDesc) <- readPackageDescriptionDir config pkgDir
    $logInfo $
        "Checking package '" <> packageNameText name <> "' for common mistakes"
    let pkgChecks = Check.checkPackage gdesc (Just pkgDesc)
    fileChecks <- liftIO $ Check.checkPackageFiles pkgDesc (toFilePath pkgDir)
    let checks = pkgChecks ++ fileChecks
        (errors, warnings) =
          let criticalIssue (Check.PackageBuildImpossible _) = True
              criticalIssue (Check.PackageDistInexcusable _) = True
              criticalIssue _ = False
          in partition criticalIssue checks
    unless (null warnings) $
        $logWarn $ "Package check reported the following warnings:\n" <>
                   T.pack (intercalate "\n" . fmap show $ warnings)
    case NE.nonEmpty errors of
        Nothing -> return ()
        Just ne -> throwM $ CheckException ne

-- | Version of 'checkSDistTarball' that first saves lazy bytestring to
-- temporary directory and then calls 'checkSDistTarball' on it.
checkSDistTarball' :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasEnvConfig env)
  => String       -- ^ Tarball name
  -> L.ByteString -- ^ Tarball contents as a byte string
  -> m ()
checkSDistTarball' name bytes = withSystemTempDir "stack" $ \tpath -> do
    npath   <- (tpath </>) `liftM` parseRelFile name
    liftIO $ L.writeFile (toFilePath npath) bytes
    checkSDistTarball npath

withTempTarGzContents :: (MonadIO m, MonadMask m)
  => Path Abs File         -- ^ Location of tarball
  -> (Path Abs Dir -> m a) -- ^ Perform actions given dir with tarball contents
  -> m a
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
  return (Tar.simpleEntry tarpath (Tar.NormalFile (L.fromStrict content) size)) {
    Tar.entryPermissions = if executable perms then Tar.executableFilePermissions
                                               else Tar.ordinaryFilePermissions,
    Tar.entryTime = mtime
  }

getModTime :: FilePath -> IO Tar.EpochTime
getModTime path = do
    t <- getModificationTime path
    return . floor . utcTimeToPOSIXSeconds $ t
