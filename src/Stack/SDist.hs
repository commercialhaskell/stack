{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
-- Create a source distribution tarball
module Stack.SDist
    ( getSDistTarball
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import           Control.Concurrent.Execute (ActionContext(..))
import           Control.Monad (unless, void)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (liftBaseWith)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Data (Data, Typeable, cast, gmapT)
import           Data.Either (partitionEithers)
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Clock.POSIX
import           Distribution.Package (Dependency (..))
import           Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import           Distribution.Version (simplifyVersionRange, orLaterVersion, earlierVersion)
import           Distribution.Version.Extra
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import           Prelude -- Fix redundant import warnings
import           Stack.Build (mkBaseConfigOpts)
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source (loadSourceMap, localFlags)
import           Stack.Build.Target
import           Stack.Constants
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory (getModificationTime, getPermissions, Permissions(..))
import qualified System.FilePath as FP

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,MonadLogger m,MonadBaseControl IO m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env)

-- | Given the path to a local package, creates its source
-- distribution tarball.
--
-- While this yields a 'FilePath', the name of the tarball, this
-- tarball is not written to the disk and instead yielded as a lazy
-- bytestring.
getSDistTarball :: M env m
                => Maybe PvpBounds -- ^ override Config value
                -> Path Abs Dir
                -> m (FilePath, L.ByteString)
getSDistTarball mpvpBounds pkgDir = do
    config <- asks getConfig
    let pvpBounds = fromMaybe (configPvpBounds config) mpvpBounds
        tweakCabal = pvpBounds /= PvpBoundsNone
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
                lbs <- getCabalLbs pvpBounds $ toFilePath cabalfp
                return $ Tar.fileEntry (tarPath False fp) lbs
            | otherwise = packWith packFileEntry False fp
        isCabalFp fp = toFilePath pkgDir FP.</> fp == toFilePath cabalfp
        tarName = pkgId FP.<.> "tar.gz"
        pkgId = packageIdentifierString (packageIdentifier (lpPackage lp))
    dirEntries <- mapM packDir (dirsFromFiles files)
    fileEntries <- mapM packFile files
    return (tarName, GZip.compress (Tar.write (dirEntries ++ fileEntries)))

-- | Get the PVP bounds-enabled version of the given cabal file
getCabalLbs :: M env m => PvpBounds -> FilePath -> m L.ByteString
getCabalLbs pvpBounds fp = do
    bs <- liftIO $ S.readFile fp
    (_warnings, gpd) <- readPackageUnresolvedBS Nothing bs
    (_, _, _, _, sourceMap) <- loadSourceMap AllowNoTargets defaultBuildOpts
    menv <- getMinimalEnvOverride
    (installedMap, _, _, _) <- getInstalled menv GetInstalledOpts
                                { getInstalledProfiling = False
                                , getInstalledHaddock = False
                                }
                                sourceMap
    let gpd' = gtraverseT (addBounds sourceMap installedMap) gpd
    return $ TLE.encodeUtf8 $ TL.pack $ showGenericPackageDescription gpd'
  where
    addBounds :: SourceMap -> InstalledMap -> Dependency -> Dependency
    addBounds sourceMap installedMap dep@(Dependency cname range) =
      case lookupVersion (fromCabalPackageName cname) of
        Nothing -> dep
        Just version -> Dependency cname $ simplifyVersionRange
          $ (if toAddUpper && not (hasUpper range) then addUpper version else id)
          $ (if toAddLower && not (hasLower range) then addLower version else id)
            range
      where
        lookupVersion name =
          case Map.lookup name sourceMap of
              Just (PSLocal lp) -> Just $ packageVersion $ lpPackage lp
              Just (PSUpstream version _ _) -> Just version
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

-- Read in a 'LocalPackage' config.  This makes some default decisions
-- about 'LocalPackage' fields that might not be appropriate for other
-- usecases.
--
-- TODO: Dedupe with similar code in "Stack.Build.Source".
readLocalPackage :: M env m => Path Abs Dir -> m LocalPackage
readLocalPackage pkgDir = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    cabalfp <- getCabalFileName pkgDir
    name <- parsePackageNameFromFilePath cabalfp
    let config = PackageConfig
            { packageConfigEnableTests = False
            , packageConfigEnableBenchmarks = False
            , packageConfigFlags = localFlags Map.empty bconfig name
            , packageConfigCompilerVersion = envConfigCompilerVersion econfig
            , packageConfigPlatform = configPlatform $ getConfig bconfig
            }
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
        , lpDirtyFiles = Just Set.empty
        , lpNewBuildCache = Map.empty
        , lpFiles = Set.empty
        , lpComponents = Set.empty
        }

-- | Returns a newline-separate list of paths, and the absolute path to the .cabal file.
getSDistFileList :: M env m => LocalPackage -> m (String, Path Abs File)
getSDistFileList lp =
    withCanonicalizedSystemTempDirectory (stackProgName <> "-sdist") $ \tmpdir -> do
        menv <- getMinimalEnvOverride
        let bopts = defaultBuildOpts
        baseConfigOpts <- mkBaseConfigOpts bopts
        (_, _mbp, locals, _extraToBuild, sourceMap) <- loadSourceMap NeedTargets bopts
        runInBase <- liftBaseWith $ \run -> return (void . run)
        withExecuteEnv menv bopts baseConfigOpts locals
            [] [] [] -- provide empty list of globals. This is a hack around custom Setup.hs files
            sourceMap $ \ee ->
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
    --TODO: consider whether erroring out is better - otherwise the
    --user might upload an incomplete tar?
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
