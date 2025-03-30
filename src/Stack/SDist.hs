{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- Types and functions related to Stack's @sdist@ command.
module Stack.SDist
  ( SDistOpts (..)
  , sdistCmd
  , getSDistTarball
  , checkSDistTarball
  , checkSDistTarball'
  , readLocalPackage
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import           Conduit ( runConduitRes, sourceLazy, sinkFileCautious )
import           Control.Concurrent.Execute
                   ( ActionContext (..), Concurrency (..) )
import           Control.Monad.Extra ( whenJust )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Char ( toLower )
import           Data.Data ( cast )
import qualified Data.Either.Extra as EE
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock.POSIX ( getPOSIXTime, utcTimeToPOSIXSeconds )
import           Distribution.Package ( Dependency (..) )
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Check as Check
import qualified Distribution.PackageDescription.Parsec as Cabal
import           Distribution.PackageDescription.PrettyPrint
                   ( showGenericPackageDescription )
import           Distribution.Simple.Utils ( cabalVersion )
import           Distribution.Version
                   ( earlierVersion, hasLowerBound, hasUpperBound, isAnyVersion
                   , orLaterVersion, simplifyVersionRange
                   )
import           Path ( (</>), parent, parseRelDir, parseRelFile )
import           Path.IO ( ensureDir, resolveDir' )
import           RIO.NonEmpty ( nonEmpty )
import qualified RIO.NonEmpty as NE
import           Stack.Build ( mkBaseConfigOpts, build, buildLocalTargets )
import           Stack.Build.Execute
                   ( ExcludeTHLoading (..), KeepOutputOpen (..) )
import           Stack.Build.ExecuteEnv ( withExecuteEnv, withSingleContext )
import           Stack.Build.Installed ( getInstalled, toInstallMap )
import           Stack.Build.Source ( projectLocalPackages )
import           Stack.BuildOpts ( defaultBuildOpts )
import           Stack.Constants ( stackProgName, stackProgName' )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Package ( resolvePackage, resolvePackageDescription )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.SourceMap ( mkProjectPackage )
import           Stack.Types.Build ( TaskType (..) )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), configFileL )
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.BuildOptsCLI ( defaultBuildOptsCLI )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), actualCompilerVersionL )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Installed
                   ( InstallMap, Installed (..), InstalledMap
                   , InstalledLibraryInfo (..), installedVersion
                   )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..), PackageConfig (..)
                   , packageIdentifier
                   )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.PvpBounds ( PvpBounds (..), PvpBoundsType (..) )
import           Stack.Types.Runner ( HasRunner, Runner )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), ProjectPackage (..), SMWanted (..)
                   , SourceMap (..), ppRoot
                   )
import qualified Stack.Types.SourceMap as SourceMap ( SourceMap (..) )
import           Stack.Types.Version
                   ( intersectVersionRanges, nextMajorVersion )
import           System.Directory
                   ( copyFile, createDirectoryIfMissing, executable
                   , getModificationTime, getPermissions
                   )
import qualified System.FilePath as FP

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.SDist" module.
data SDistPrettyException
  = CheckException (NonEmpty Check.PackageCheck)
  | CabalFilePathsInconsistentBug (Path Abs File) (Path Abs File)
  | ToTarPathException String
  deriving (Show, Typeable)

instance Pretty SDistPrettyException where
  pretty (CheckException xs) =
    "[S-6439]"
    <> line
    <> flow "Package check reported the following errors:"
    <> line
    <> bulletedList (map (string . show) (NE.toList xs) :: [StyleDoc])
  pretty (CabalFilePathsInconsistentBug cabalFP cabalFP') =
    "[S-9595]"
    <> line
    <> fillSep
         [ flow "The impossible happened! Two Cabal file paths are \
                \inconsistent:"
         , pretty cabalFP
         , "and"
         , pretty cabalFP' <> "."
         ]
  pretty (ToTarPathException e) =
    "[S-7875]"
    <> line
    <> string e

instance Exception SDistPrettyException

-- | Type representing command line options for @stack sdist@ command.
data SDistOpts = SDistOpts
  { dirsToWorkWith :: [String]
    -- ^ Directories to package
  , pvpBounds :: Maybe PvpBounds
    -- ^ PVP Bounds overrides
  , ignoreCheck :: Bool
    -- ^ Whether to ignore check of the package for common errors
  , buildTarball :: Bool
    -- ^ Whether to build the tarball
  , tarPath :: Maybe FilePath
    -- ^ Where to copy the tarball
  }

-- | Function underlying the @stack sdist@ command.
sdistCmd :: SDistOpts -> RIO Runner ()
sdistCmd sdistOpts =
  withConfig YesReexec $ withDefaultEnvConfig $ do
    -- If no directories are specified, build all sdist tarballs.
    dirs' <- if null sdistOpts.dirsToWorkWith
      then do
        dirs <- view $
          buildConfigL . to (map ppRoot . Map.elems . (.smWanted.project))
        when (null dirs) $ do
          configFile <- view configFileL
          -- We are indifferent as to whether the configuration file is a
          -- user-specific global or a project-level one.
          let eitherConfigFile = EE.fromEither configFile
          prettyErrorL
            [ style Shell "stack sdist"
            , flow "expects a list of targets, and otherwise defaults to all \
                   \of the project's packages. However, the configuration at"
            , pretty eitherConfigFile
            , flow "contains no packages, so no sdist tarballs will be \
                   \generated."
            ]
          exitFailure
        pure dirs
      else mapM resolveDir' sdistOpts.dirsToWorkWith
    forM_ dirs' $ \dir -> do
      (tarName, tarBytes, _mcabalRevision) <-
        getSDistTarball sdistOpts.pvpBounds dir
      distDir <- distDirFromDir dir
      tarPath <- (distDir </>) <$> parseRelFile tarName
      ensureDir (parent tarPath)
      runConduitRes $
        sourceLazy tarBytes .|
        sinkFileCautious (toFilePath tarPath)
      prettyInfoL
        [flow "Wrote sdist-format compressed archive to"
        , pretty tarPath <> "."
        ]
      checkSDistTarball sdistOpts tarPath
      forM_ sdistOpts.tarPath $ copyTarToTarPath tarPath tarName
 where
  copyTarToTarPath tarPath tarName targetDir = liftIO $ do
    let targetTarPath = targetDir FP.</> tarName
    createDirectoryIfMissing True $ FP.takeDirectory targetTarPath
    copyFile (toFilePath tarPath) targetTarPath

-- | Given the path to a package directory, creates a source distribution
-- tarball for the package.
--
-- While this yields a 'FilePath', the name of the tarball, this tarball is not
-- written to the disk and instead yielded as a lazy bytestring.
getSDistTarball ::
     HasEnvConfig env
  => Maybe PvpBounds
     -- ^ Override Config value
  -> Path Abs Dir
     -- ^ Path to package directory
  -> RIO
       env
       ( FilePath
       , L.ByteString
       , Maybe (PackageIdentifier, L.ByteString)
       )
     -- ^ Filename, tarball contents, and option Cabal file revision to upload
getSDistTarball mpvpBounds pkgDir = do
  config <- view configL
  let PvpBounds pvpBounds asRevision =
        fromMaybe config.pvpBounds mpvpBounds
      tweakCabal = pvpBounds /= PvpBoundsNone
      pkgFp = toFilePath pkgDir
  lp <- readLocalPackage pkgDir
  forM_ lp.package.setupDeps $ \customSetupDeps ->
    case nonEmpty (map (T.pack . packageNameString) (Map.keys customSetupDeps)) of
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
        prettyWarnS "unexpected empty custom-setup dependencies."
  sourceMap <- view $ envConfigL . to (.sourceMap)
  installMap <- toInstallMap sourceMap
  (installedMap, _globalDumpPkgs, _snapshotDumpPkgs, _localDumpPkgs) <-
    getInstalled installMap
  let deps = Map.fromList
        [ (pid, libInfo.ghcPkgId)
        | (_, Library pid libInfo) <- Map.elems installedMap]
  prettyInfoL
    [ flow "Getting the file list for"
    , style File (fromString  pkgFp) <> "."
    ]
  (fileList, cabalFP) <- getSDistFileList lp deps
  prettyInfoL
    [ flow "Building a compressed archive file in the sdist format for"
    , style File (fromString pkgFp) <> "."
    ]
  files <-
    normalizeTarballPaths (map (T.unpack . stripCR . T.pack) (lines fileList))
  -- We're going to loop below and eventually find the Cabal file. When we do,
  -- we'll upload this reference, if the mpvpBounds value indicates that we
  -- should be uploading a Cabal file revision.
  cabalFileRevisionRef <- liftIO (newIORef Nothing)
  -- NOTE: Could make this use lazy I/O to only read files as needed for upload
  -- (both GZip.compress and Tar.write are lazy). However, it seems less error
  -- prone and more predictable to read everything in at once, so that's what
  -- we're doing for now:
  let tarPath isDir fp =
        case Tar.toTarPath isDir (pkgIdName FP.</> fp) of
          Left e -> prettyThrowIO $ ToTarPathException e
          Right tp -> pure tp
      packWith f isDir fp = liftIO $ f (pkgFp FP.</> fp) =<< tarPath isDir fp
      packDir = packWith Tar.packDirectoryEntry True
      packFile fp
        -- This is a Cabal file, we're going to tweak it, but only tweak it as a
        -- revision.
        | tweakCabal && isCabalFp fp && asRevision = do
            lbsIdent <- getCabalLbs pvpBounds (Just 1) cabalFP sourceMap
            liftIO (writeIORef cabalFileRevisionRef (Just lbsIdent))
            packWith packFileEntry False fp
        -- Same, except we'll include the Cabal file in the original tarball
        -- upload.
        | tweakCabal && isCabalFp fp = do
            (_ident, lbs) <- getCabalLbs pvpBounds Nothing cabalFP sourceMap
            currTime <- liftIO getPOSIXTime -- Seconds from UNIX epoch
            tp <- liftIO $ tarPath False fp
            pure $ (Tar.fileEntry tp lbs) { Tar.entryTime = floor currTime }
        | otherwise = packWith packFileEntry False fp
      isCabalFp fp = toFilePath pkgDir FP.</> fp == toFilePath cabalFP
      tarName = pkgIdName FP.<.> "tar.gz"
      pkgIdName = packageIdentifierString pkgId
      pkgId = packageIdentifier lp.package
  dirEntries <- mapM packDir (dirsFromFiles files)
  fileEntries <- mapM packFile files
  mcabalFileRevision <- liftIO (readIORef cabalFileRevisionRef)
  pure
    ( tarName
    , GZip.compress (Tar.write (dirEntries ++ fileEntries))
    , mcabalFileRevision
    )

-- | Get the PVP bounds-enabled version of the given Cabal file
getCabalLbs ::
     HasEnvConfig env
  => PvpBoundsType
  -> Maybe Int -- ^ optional revision
  -> Path Abs File -- ^ Cabal file
  -> SourceMap
  -> RIO env (PackageIdentifier, L.ByteString)
getCabalLbs pvpBounds mrev cabalFP sourceMap = do
  (gpdio, _name, cabalFP') <-
    loadCabalFilePath (Just stackProgName') (parent cabalFP)
  gpd <- liftIO $ gpdio NoPrintWarnings
  unless (cabalFP == cabalFP') $
    prettyThrowIO $ CabalFilePathsInconsistentBug cabalFP cabalFP'
  installMap <- toInstallMap sourceMap
  (installedMap, _, _, _) <- getInstalled installMap
  let subLibPackages = Set.fromList $
          gpdPackageName gpd
        : map
            (Cabal.unqualComponentNameToPackageName . fst)
            (Cabal.condSubLibraries gpd)
      gpd' = gtraverseT (addBounds subLibPackages installMap installedMap) gpd
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
  -- Sanity rendering and reparsing the input, to ensure there are no Cabal
  -- bugs, since there have been bugs here before, and currently are at the time
  -- of writing:
  --
  -- https://github.com/haskell/cabal/issues/1202
  -- https://github.com/haskell/cabal/issues/2353
  -- https://github.com/haskell/cabal/issues/4863 (current issue)
  let roundtripErrs =
           fillSep
             [ flow "Bug detected in Cabal library. ((parse . render . parse) \
                    \=== id) does not hold for the Cabal file at"
             , pretty cabalFP
             ]
        <> blankLine
      (_warnings, eres) = Cabal.runParseResult
                        $ Cabal.parseGenericPackageDescription
                        $ T.encodeUtf8
                        $ T.pack
                        $ showGenericPackageDescription gpd
  case eres of
    Right roundtripped
      | roundtripped == gpd -> pure ()
      | otherwise -> prettyWarn $
             roundtripErrs
          <> flow "This seems to be fixed in development versions of Cabal, \
                  \but at time of writing, the fix is not in any released \
                  \versions."
          <> blankLine
          <> fillSep
               [ flow "Please see this GitHub issue for status:"
               , style Url "https://github.com/commercialhaskell/stack/issues/3549"
               ]
          <> blankLine
          <> fillSep
               [ flow "If the issue is closed as resolved, then you may be \
                      \able to fix this by upgrading to a newer version of \
                      \Stack via"
               , style Shell "stack upgrade"
               , flow "for latest stable version or"
               , style Shell "stack upgrade --git"
               , flow "for the latest development version."
               ]
          <> blankLine
          <> fillSep
               [ flow "If the issue is fixed, but updating doesn't solve the \
                      \problem, please check if there are similar open \
                      \issues, and if not, report a new issue to the Stack \
                      \issue tracker, at"
               , style Url "https://github.com/commercialhaskell/stack/issues/new"
               ]
          <> blankLine
          <> flow "If the issue is not fixed, feel free to leave a comment \
                  \on it indicating that you would like it to be fixed."
          <> blankLine
    Left (_version, errs) -> prettyWarn $
         roundtripErrs
      <> flow "In particular, parsing the rendered Cabal file is yielding a \
              \parse error. Please check if there are already issues \
              \tracking this, and if not, please report new issues to the \
              \Stack and Cabal issue trackers, via"
      <> line
      <> bulletedList
           [ style Url "https://github.com/commercialhaskell/stack/issues/new"
           , style Url "https://github.com/haskell/cabal/issues/new"
           ]
      <> line
      <> flow ("The parse error is: " <> unlines (map show (toList errs)))
      <> blankLine
  pure
    ( ident
    , TLE.encodeUtf8 $ TL.pack $ showGenericPackageDescription gpd''
    )
 where
  addBounds ::
       Set PackageName
    -> InstallMap
    -> InstalledMap
    -> Dependency
    -> Dependency
  addBounds subLibPackages installMap installedMap dep =
    if name `Set.member` subLibPackages
      then dep
      else case foundVersion of
        Nothing -> dep
        Just version -> Dependency
          name
          ( simplifyVersionRange
          $ ( if toAddUpper && not (hasUpperBound range)
                then addUpper version
                else id
            )
            -- From Cabal-3.4.0.0, 'hasLowerBound isAnyVersion' is 'True'.
          $ ( if    toAddLower
                 && (isAnyVersion range || not (hasLowerBound range))
                then addLower version
                else id
            )
            range
          )
          s
   where
    Dependency name range s = dep
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

-- | Read in a 'LocalPackage' config.  This makes some default decisions about
-- 'LocalPackage' fields that might not be appropriate for other use-cases.
readLocalPackage :: HasEnvConfig env => Path Abs Dir -> RIO env LocalPackage
readLocalPackage pkgDir = do
  config  <- getDefaultPackageConfig
  (gpdio, _, cabalFP) <- loadCabalFilePath (Just stackProgName') pkgDir
  gpd <- liftIO $ gpdio YesPrintWarnings
  let package = resolvePackage config gpd
  pure LocalPackage
    { package
    , wanted = False -- HACK: makes it so that sdist output goes to a log
                       -- instead of a file.
    , cabalFP
    -- NOTE: these aren't the 'correct' values, but aren't used in the usage of
    -- this function in this module.
    , testBench = Nothing
    , buildHaddocks = False
    , forceDirty = False
    , dirtyFiles = pure Nothing
    , newBuildCaches = pure Map.empty
    , componentFiles = pure Map.empty
    , components = Set.empty
    , unbuildable = Set.empty
    }

-- | Returns a newline-separate list of paths, and the absolute path to the
-- Cabal file.
getSDistFileList ::
     HasEnvConfig env
  => LocalPackage
  -> Map PackageIdentifier GhcPkgId
  -> RIO env (String, Path Abs File)
getSDistFileList lp deps =
  withSystemTempDir (stackProgName <> "-sdist") $ \tmpdir -> do
    let bopts = defaultBuildOpts
    let boptsCli = defaultBuildOptsCLI
    baseConfigOpts <- mkBaseConfigOpts boptsCli
    locals <- projectLocalPackages
    withExecuteEnv bopts boptsCli baseConfigOpts locals
      [] [] [] Nothing -- provide empty list of globals. This is a hack around
                       -- custom Setup.hs files
      $ \ee ->
      withSingleContext ac ee taskType deps (Just "sdist") $
        \_package cabalFP _pkgDir cabal _announce _outputType -> do
          let outFile = toFilePath tmpdir FP.</> "source-files-list"
          cabal
            CloseOnException
            KeepTHLoading
            ["sdist", "--list-sources", outFile]
          contents <- liftIO (S.readFile outFile)
          pure (T.unpack $ T.decodeUtf8With T.lenientDecode contents, cabalFP)
 where
  ac = ActionContext Set.empty [] ConcurrencyAllowed
  taskType = TTLocalMutable lp

normalizeTarballPaths ::
     (HasRunner env, HasTerm env)
  => [FilePath]
  -> RIO env [FilePath]
normalizeTarballPaths fps = do
  -- TODO: consider whether erroring out is better - otherwise the user might
  -- upload an incomplete tar?
  unless (null outsideDir) $
    prettyWarn $
         flow "These files are outside of the package directory, and will be \
              \omitted from the tarball:"
      <> line
      <> bulletedList (map (style File . fromString) outsideDir)
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

-- | Check package in given tarball. This will log all warnings and will throw
-- an exception in case of critical errors.
--
-- Note that we temporarily decompress the archive to analyze it.
checkSDistTarball ::
     HasEnvConfig env
  => SDistOpts -- ^ The configuration of what to check
  -> Path Abs File -- ^ Absolute path to tarball
  -> RIO env ()
checkSDistTarball opts tarball = withTempTarGzContents tarball $ \pkgDir' -> do
  pkgDir <- (pkgDir' </>) <$>
    (parseRelDir . FP.takeBaseName . FP.takeBaseName . toFilePath $ tarball)
  --               ^ drop ".tar"     ^ drop ".gz"
  when opts.buildTarball
    ( buildExtractedTarball ResolvedPath
        { resolvedRelative = RelFilePath "this-is-not-used" -- ugly hack
        , resolvedAbsolute = pkgDir
        }
    )
  unless opts.ignoreCheck (checkPackageInExtractedTarball pkgDir)

checkPackageInExtractedTarball ::
     HasEnvConfig env
  => Path Abs Dir -- ^ Absolute path to tarball
  -> RIO env ()
checkPackageInExtractedTarball pkgDir = do
  (gpdio, name, _cabalfp) <- loadCabalFilePath (Just stackProgName') pkgDir
  gpd <- liftIO $ gpdio YesPrintWarnings
  config <- getDefaultPackageConfig
  let pkgDesc = resolvePackageDescription config gpd
  prettyInfoL
    [ flow "Checking package"
    , style Current (fromPackageName name)
    , flow "for common mistakes using Cabal version"
    , fromString $ versionString cabalVersion <> "."
    ]
  let pkgChecks = Check.checkPackage gpd
  fileChecks <-
    liftIO $ Check.checkPackageFiles minBound pkgDesc (toFilePath pkgDir)
  let checks = pkgChecks ++ fileChecks
      (errors, warnings) =
        let criticalIssue (Check.PackageBuildImpossible _) = True
            criticalIssue (Check.PackageDistInexcusable _) = True
            criticalIssue _ = False
        in  List.partition criticalIssue checks
  unless (null warnings) $
    prettyWarn $
         flow "Package check reported the following warnings:"
      <> line
      <> bulletedList (map (fromString . show) warnings)
  whenJust (nonEmpty errors) $ \ne -> prettyThrowM $ CheckException ne

buildExtractedTarball :: HasEnvConfig env => ResolvedPath Dir -> RIO env ()
buildExtractedTarball pkgDir = do
  envConfig <- view envConfigL
  localPackageToBuild <- readLocalPackage $ resolvedAbsolute pkgDir
  -- We remove the path based on the name of the package
  let isPathToRemove path = do
        localPackage <- readLocalPackage path
        pure
          $  localPackage.package.name
          == localPackageToBuild.package.name
  pathsToKeep <- Map.fromList <$> filterM
    (fmap not . isPathToRemove . resolvedAbsolute . (.resolvedDir) . snd)
    (Map.toList envConfig.buildConfig.smWanted.project)
  pp <- mkProjectPackage YesPrintWarnings pkgDir False
  let adjustEnvForBuild env =
        let updatedEnvConfig = envConfig
              { sourceMap = updatePackagesInSourceMap envConfig.sourceMap
              , buildConfig = updateBuildConfig envConfig.buildConfig
              }
            updateBuildConfig bc = bc
              { config = bc.config { build = defaultBuildOpts { tests = True } }
              }
        in  set envConfigL updatedEnvConfig env
      updatePackagesInSourceMap sm =
        sm { SourceMap.project = Map.insert pp.projectCommon.name pp pathsToKeep }
  local adjustEnvForBuild $ build Nothing

-- | Version of 'checkSDistTarball' that first saves lazy bytestring to
-- temporary directory and then calls 'checkSDistTarball' on it.
checkSDistTarball' ::
     HasEnvConfig env
  => SDistOpts
  -> String       -- ^ Tarball name
  -> L.ByteString -- ^ Tarball contents as a byte string
  -> RIO env ()
checkSDistTarball' opts name bytes = withSystemTempDir "stack" $ \tpath -> do
  npath <- (tpath </>) <$> parseRelFile name
  liftIO $ L.writeFile (toFilePath npath) bytes
  checkSDistTarball opts npath

withTempTarGzContents ::
     Path Abs File
     -- ^ Location of tarball
  -> (Path Abs Dir -> RIO env a)
     -- ^ Perform actions given dir with tarball contents
  -> RIO env a
withTempTarGzContents apath f = withSystemTempDir "stack" $ \tpath -> do
  archive <- liftIO $ L.readFile (toFilePath apath)
  liftIO . Tar.unpack (toFilePath tpath) . Tar.read . GZip.decompress $ archive
  f tpath

--------------------------------------------------------------------------------

-- Copy+modified from the tar package to avoid issues with lazy IO ( see
-- https://github.com/commercialhaskell/stack/issues/1344 )

packFileEntry ::
     FilePath -- ^ Full path to find the file on the local disk
  -> Tar.TarPath -- ^ Path to use for the tar Entry in the archive
  -> IO Tar.Entry
packFileEntry filepath tarpath = do
  mtime <- getModTime filepath
  perms <- getPermissions filepath
  content <- S.readFile filepath
  let size = fromIntegral (S.length content)
      entryContent = Tar.NormalFile (L.fromStrict content) size
      entry = Tar.simpleEntry tarpath entryContent
  pure entry
    { Tar.entryPermissions = if executable perms
                               then Tar.executableFilePermissions
                               else Tar.ordinaryFilePermissions
    , Tar.entryTime = mtime
    }

getModTime :: FilePath -> IO Tar.EpochTime
getModTime path = do
  t <- getModificationTime path
  pure $ floor . utcTimeToPOSIXSeconds $ t

getDefaultPackageConfig ::
     (MonadIO m, MonadReader env m, HasEnvConfig env)
  => m PackageConfig
getDefaultPackageConfig = do
  platform <- view platformL
  compilerVersion <- view actualCompilerVersionL
  pure PackageConfig
    { enableTests = False
    , enableBenchmarks = False
    , flags = mempty
    , ghcOptions = []
    , cabalConfigOpts = []
    , compilerVersion
    , platform
    }
