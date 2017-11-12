{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TypeFamilies          #-}
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
import           Control.Concurrent.Execute (ActionContext(..))
import           Stack.Prelude
import           Control.Monad.Reader.Class (local)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char (toLower)
import           Data.Data (cast)
import           Data.List
import           Data.List.Extra (nubOrd)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock.POSIX
import           Distribution.Package (Dependency (..))
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Check as Check
import qualified Distribution.PackageDescription.Parse as Cabal
import           Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Text as Cabal
import           Distribution.Version (simplifyVersionRange, orLaterVersion, earlierVersion, hasUpperBound, hasLowerBound)
import           Lens.Micro (set)
import           Path
import           Path.IO hiding (getModificationTime, getPermissions, withSystemTempDir)
import           Stack.Build (mkBaseConfigOpts, build)
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source (loadSourceMap)
import           Stack.Build.Target hiding (PackageType (..))
import           Stack.BuildPlan
import           Stack.PackageLocation (resolveMultiPackageLocation)
import           Stack.PrettyPrint
import           Stack.Constants
import           Stack.Package
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.Config
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Runner
import           Stack.Types.Version
import           System.Directory (getModificationTime, getPermissions)
import qualified System.FilePath as FP

-- | Special exception to throw when you want to fail because of bad results
-- of package check.

data SDistOpts = SDistOpts
  { sdoptsDirsToWorkWith :: [String]
  -- ^ Directories to package
  , sdoptsPvpBounds :: Maybe PvpBounds
  -- ^ PVP Bounds overrides
  , sdoptsIgnoreCheck :: Bool
  -- ^ Whether to ignore check of the package for common errors
  , sdoptsSign :: Bool
  -- ^ Whether to sign the package
  , sdoptsSignServerUrl :: String
  -- ^ The URL of the signature server
  , sdoptsBuildTarball :: Bool
  -- ^ Whether to build the tarball
  }

newtype CheckException
  = CheckException (NonEmpty Check.PackageCheck)
  deriving (Typeable)

instance Exception CheckException

instance Show CheckException where
  show (CheckException xs) =
    "Package check reported the following errors:\n" ++
    (intercalate "\n" . fmap show . NE.toList $ xs)

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
  -- ^ Filename, tarball contents, and option cabal file revision to upload
getSDistTarball mpvpBounds pkgDir = do
    config <- view configL
    let PvpBounds pvpBounds asRevision = fromMaybe (configPvpBounds config) mpvpBounds
        tweakCabal = True -- pvpBounds /= PvpBoundsNone
        pkgFp = toFilePath pkgDir
    lp <- readLocalPackage pkgDir
    logInfo $ "Getting file list for " <> T.pack pkgFp
    (fileList, cabalfp) <-  getSDistFileList lp
    logInfo $ "Building sdist tarball for " <> T.pack pkgFp
    files <- normalizeTarballPaths (lines fileList)

    -- We're going to loop below and eventually find the cabal
    -- file. When we do, we'll upload this reference, if the
    -- mpvpBounds value indicates that we should be uploading a cabal
    -- file revision.
    cabalFileRevisionRef <- liftIO (newIORef Nothing)

    -- NOTE: Could make this use lazy I/O to only read files as needed
    -- for upload (both GZip.compress and Tar.write are lazy).
    -- However, it seems less error prone and more predictable to read
    -- everything in at once, so that's what we're doing for now:
    let tarPath isDir fp = either throwString return
            (Tar.toTarPath isDir (forceUtf8Enc (pkgId FP.</> fp)))
        -- convert a String of proper characters to a String of bytes
        -- in UTF8 encoding masquerading as characters. This is
        -- necessary for tricking the tar package into proper
        -- character encoding.
        forceUtf8Enc = S8.unpack . T.encodeUtf8 . T.pack
        packWith f isDir fp = liftIO $ f (pkgFp FP.</> fp) =<< tarPath isDir fp
        packDir = packWith Tar.packDirectoryEntry True
        packFile fp
            -- This is a cabal file, we're going to tweak it, but only
            -- tweak it as a revision.
            | tweakCabal && isCabalFp fp && asRevision = do
                lbsIdent <- getCabalLbs pvpBounds (Just 1) $ toFilePath cabalfp
                liftIO (writeIORef cabalFileRevisionRef (Just lbsIdent))
                packWith packFileEntry False fp
            -- Same, except we'll include the cabal file in the
            -- original tarball upload.
            | tweakCabal && isCabalFp fp = do
                (_ident, lbs) <- getCabalLbs pvpBounds Nothing $ toFilePath cabalfp
                currTime <- liftIO getPOSIXTime -- Seconds from UNIX epoch
                tp <- liftIO $ tarPath False fp
                return $ (Tar.fileEntry tp lbs) { Tar.entryTime = floor currTime }
            | otherwise = packWith packFileEntry False fp
        isCabalFp fp = toFilePath pkgDir FP.</> fp == toFilePath cabalfp
        tarName = pkgId FP.<.> "tar.gz"
        pkgId = packageIdentifierString (packageIdentifier (lpPackage lp))
    dirEntries <- mapM packDir (dirsFromFiles files)
    fileEntries <- mapM packFile files
    mcabalFileRevision <- liftIO (readIORef cabalFileRevisionRef)
    return (tarName, GZip.compress (Tar.write (dirEntries ++ fileEntries)), mcabalFileRevision)

-- | Get the PVP bounds-enabled version of the given cabal file
getCabalLbs :: HasEnvConfig env
            => PvpBoundsType
            -> Maybe Int -- ^ optional revision
            -> FilePath
            -> RIO env (PackageIdentifier, L.ByteString)
getCabalLbs pvpBounds mrev fp = do
    path <- liftIO $ resolveFile' fp
    (_warnings, gpd) <- readPackageUnresolved path
    (_, sourceMap) <- loadSourceMap AllowNoTargets defaultBuildOptsCLI
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled menv GetInstalledOpts
                                { getInstalledProfiling = False
                                , getInstalledHaddock = False
                                , getInstalledSymbols = False
                                }
                                sourceMap
    let internalPackages = Set.fromList $
          gpdPackageName gpd :
          map (fromCabalPackageName . Cabal.unqualComponentNameToPackageName . fst) (Cabal.condSubLibraries gpd)
        gpd' = gtraverseT (addBounds internalPackages sourceMap installedMap) gpd
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
    ident <- parsePackageIdentifierFromString $ Cabal.display $ Cabal.package $ Cabal.packageDescription gpd''
    -- Sanity rendering and reparsing the input, to ensure there are no
    -- cabal bugs, since there have been bugs here before, and currently
    -- are at the time of writing:
    --
    -- https://github.com/haskell/cabal/issues/1202
    -- https://github.com/haskell/cabal/issues/2353
    -- https://github.com/haskell/cabal/issues/4863 (current issue)
    let roundtripErrs =
          [ flow "Bug detected in Cabal library. ((parse . render . parse) === id) does not hold for the cabal file at"
          <+> display path
          , ""
          ]
    case Cabal.parseGenericPackageDescription (showGenericPackageDescription gpd) of
      Cabal.ParseOk _ roundtripped
        | roundtripped == gpd -> return ()
        | otherwise -> do
            prettyWarn $ vsep $ roundtripErrs ++
              [ "This seems to be fixed in development versions of Cabal, but at time of writing, the fix is not in any released versions."
              , ""
              ,  "Please see this GitHub issue for status:" <+> styleUrl "https://github.com/commercialhaskell/stack/issues/3549"
              , ""
              , fillSep
                [ flow "If the issue is closed as resolved, then you may be able to fix this by upgrading to a newer version of stack via"
                , styleShell "stack upgrade"
                , flow "for latest stable version or"
                , styleShell "stack upgrade --git"
                , flow "for the latest development version."
                ]
              , ""
              , fillSep
                [ flow "If the issue is fixed, but updating doesn't solve the problem, please check if there are similar open issues, and if not, report a new issue to the stack issue tracker, at"
                , styleUrl "https://github.com/commercialhaskell/stack/issues/new"
                ]
              , ""
              , flow "If the issue is not fixed, feel free to leave a comment on it indicating that you would like it to be fixed."
              , ""
              ]
      Cabal.ParseFailed err -> do
        prettyWarn $ vsep $ roundtripErrs ++
          [ flow "In particular, parsing the rendered cabal file is yielding a parse error.  Please check if there are already issues tracking this, and if not, please report new issues to the stack and cabal issue trackers, via"
          , bulletedList
            [ styleUrl "https://github.com/commercialhaskell/stack/issues/new"
            , styleUrl "https://github.com/haskell/cabal/issues/new"
            ]
          , flow $ "The parse error is: " ++ show err
          , ""
          ]
    return
      ( ident
      , TLE.encodeUtf8 $ TL.pack $ showGenericPackageDescription gpd''
      )
  where
    addBounds :: Set PackageName -> SourceMap -> InstalledMap -> Dependency -> Dependency
    addBounds internalPackages sourceMap installedMap dep@(Dependency cname range) =
      if name `Set.member` internalPackages
        then dep
        else case foundVersion of
          Nothing -> dep
          Just version -> Dependency cname $ simplifyVersionRange
            $ (if toAddUpper && not (hasUpperBound range) then addUpper version else id)
            $ (if toAddLower && not (hasLowerBound range) then addLower version else id)
              range
      where
        name = fromCabalPackageName cname
        foundVersion =
          case Map.lookup name sourceMap of
              Just ps -> Just (piiVersion ps)
              Nothing ->
                  case Map.lookup name installedMap of
                      Just (_, installed) -> Just (installedVersion installed)
                      Nothing -> Nothing

    addUpper version = intersectVersionRanges
        (earlierVersion $ toCabalVersion $ nextMajorVersion version)
    addLower version = intersectVersionRanges
        (orLaterVersion (toCabalVersion version))

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
        , lpLocation = PLFilePath $ toFilePath pkgDir
        }

-- | Returns a newline-separate list of paths, and the absolute path to the .cabal file.
getSDistFileList :: HasEnvConfig env => LocalPackage -> RIO env (String, Path Abs File)
getSDistFileList lp =
    withSystemTempDir (stackProgName <> "-sdist") $ \tmpdir -> do
        menv <- getMinimalEnvOverride
        let bopts = defaultBuildOpts
        let boptsCli = defaultBuildOptsCLI
        baseConfigOpts <- mkBaseConfigOpts boptsCli
        (locals, _) <- loadSourceMap NeedTargets boptsCli
        run <- askRunInIO
        withExecuteEnv menv bopts boptsCli baseConfigOpts locals
            [] [] [] -- provide empty list of globals. This is a hack around custom Setup.hs files
            $ \ee ->
            withSingleContext run ac ee task Nothing (Just "sdist") $ \_package cabalfp _pkgDir cabal _announce _console _mlogFile -> do
                let outFile = toFilePath tmpdir FP.</> "source-files-list"
                cabal KeepTHLoading ["sdist", "--list-sources", outFile]
                contents <- liftIO (S.readFile outFile)
                return (T.unpack $ T.decodeUtf8With T.lenientDecode contents, cabalfp)
  where
    package = lpPackage lp
    ac = ActionContext Set.empty []
    task = Task
        { taskProvides = PackageIdentifier (packageName package) (packageVersion package)
        , taskType = TTFiles lp Local
        , taskConfigOpts = TaskConfigOpts
            { tcoMissing = Set.empty
            , tcoOpts = \_ -> ConfigureOpts [] []
            }
        , taskPresent = Map.empty
        , taskAllInOne = True
        , taskCachePkgSrc = CacheSrcLocal (toFilePath (lpDir lp))
        , taskAnyMissing = True
        }

normalizeTarballPaths :: HasRunner env => [FilePath] -> RIO env [FilePath]
normalizeTarballPaths fps = do
    -- TODO: consider whether erroring out is better - otherwise the
    -- user might upload an incomplete tar?
    unless (null outsideDir) $
        logWarn $ T.concat
            [ "Warning: These files are outside of the package directory, and will be omitted from the tarball: "
            , T.pack (show outsideDir)]
    return (nubOrd files)
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
    when (sdoptsBuildTarball opts) (buildExtractedTarball pkgDir)
    unless (sdoptsIgnoreCheck opts) (checkPackageInExtractedTarball pkgDir)

checkPackageInExtractedTarball
  :: HasEnvConfig env
  => Path Abs Dir -- ^ Absolute path to tarball
  -> RIO env ()
checkPackageInExtractedTarball pkgDir = do
    cabalfp <- findOrGenerateCabalFile pkgDir
    name    <- parsePackageNameFromFilePath cabalfp
    config  <- getDefaultPackageConfig
    (gdesc, PackageDescriptionPair pkgDesc _) <- readPackageDescriptionDir config pkgDir
    logInfo $
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
        logWarn $ "Package check reported the following warnings:\n" <>
                   T.pack (intercalate "\n" . fmap show $ warnings)
    case NE.nonEmpty errors of
        Nothing -> return ()
        Just ne -> throwM $ CheckException ne

buildExtractedTarball :: HasEnvConfig env => Path Abs Dir -> RIO env ()
buildExtractedTarball pkgDir = do
  projectRoot <- view projectRootL
  envConfig <- view envConfigL
  menv <- getMinimalEnvOverride
  localPackageToBuild <- readLocalPackage pkgDir
  let packageEntries = bcPackages (envConfigBuildConfig envConfig)
      getPaths = resolveMultiPackageLocation menv projectRoot
  allPackagePaths <- fmap (map fst . mconcat) (mapM getPaths packageEntries)
  -- We remove the path based on the name of the package
  let isPathToRemove path = do
        localPackage <- readLocalPackage path
        return $ packageName (lpPackage localPackage) == packageName (lpPackage localPackageToBuild)
  pathsToKeep <- filterM (fmap not . isPathToRemove) allPackagePaths
  newPackagesRef <- liftIO (newIORef Nothing)
  let adjustEnvForBuild env =
        let updatedEnvConfig = envConfig
              {envConfigPackagesRef = newPackagesRef
              ,envConfigBuildConfig = updatePackageInBuildConfig (envConfigBuildConfig envConfig)
              }
        in set envConfigL updatedEnvConfig env
      updatePackageInBuildConfig buildConfig = buildConfig
        { bcPackages = map (PLFilePath . toFilePath) $ pkgDir : pathsToKeep
        , bcConfig = (bcConfig buildConfig)
                     { configBuild = defaultBuildOpts
                       { boptsTests = True
                       }
                     }
        }
  local adjustEnvForBuild $
    build (const (return ())) Nothing defaultBuildOptsCLI

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
  return (Tar.simpleEntry tarpath (Tar.NormalFile (L.fromStrict content) size)) {
    Tar.entryPermissions = if executable perms then Tar.executableFilePermissions
                                               else Tar.ordinaryFilePermissions,
    Tar.entryTime = mtime
  }

getModTime :: FilePath -> IO Tar.EpochTime
getModTime path = do
    t <- getModificationTime path
    return . floor . utcTimeToPOSIXSeconds $ t

getDefaultPackageConfig :: (MonadIO m, MonadReader env m, HasEnvConfig env)
  => m PackageConfig
getDefaultPackageConfig = do
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  return PackageConfig
    { packageConfigEnableTests = False
    , packageConfigEnableBenchmarks = False
    , packageConfigFlags = mempty
    , packageConfigGhcOptions = []
    , packageConfigCompilerVersion = compilerVersion
    , packageConfigPlatform = platform
    }
