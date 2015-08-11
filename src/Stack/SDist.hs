{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-- Create a source distribution tarball
module Stack.SDist
    ( getSDistTarball
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative ((<$>))
import           Control.Concurrent.Execute (ActionContext(..))
import           Control.Monad (when)
import           Control.Monad.Catch (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as L
import           Data.Either (partitionEithers)
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Stack.Build (mkBaseConfigOpts)
import           Stack.Build.Execute
import           Stack.Build.Source (loadSourceMap, localFlags)
import           Stack.Types.Build
import           Stack.Constants
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import qualified System.FilePath as FP
import           System.IO.Temp (withSystemTempDirectory)

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env)

-- | Given the path to a local package, creates its source
-- distribution tarball.
--
-- While this yields a 'FilePath', the name of the tarball, this
-- tarball is not written to the disk and instead yielded as a lazy
-- bytestring.
getSDistTarball :: M env m => Path Abs Dir -> m (FilePath, L.ByteString)
getSDistTarball pkgDir = do
    let pkgFp = toFilePath pkgDir
    lp <- readLocalPackage pkgDir
    $logInfo $ "Getting file list for " <> T.pack pkgFp
    fileList <-  getSDistFileList lp
    $logInfo $ "Building sdist tarball for " <> T.pack pkgFp
    files <- normalizeTarballPaths (lines fileList)
    liftIO $ do
        -- NOTE: Could make this use lazy I/O to only read files as needed
        -- for upload (both GZip.compress and Tar.write are lazy).
        -- However, it seems less error prone and more predictable to read
        -- everything in at once, so that's what we're doing for now:
        let packWith f isDir fp =
               f (pkgFp FP.</> fp)
                 (either error id (Tar.toTarPath isDir (pkgId FP.</> fp)))
            tarName = pkgId FP.<.> "tar.gz"
            pkgId = packageIdentifierString (packageIdentifier (lpPackage lp))
        dirEntries <- mapM (packWith Tar.packDirectoryEntry True) (dirsFromFiles files)
        fileEntries <- mapM (packWith Tar.packFileEntry False) files
        return (tarName, GZip.compress (Tar.write (dirEntries ++ fileEntries)))

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
            , packageConfigGhcVersion = envConfigGhcVersion econfig
            , packageConfigPlatform = configPlatform $ getConfig bconfig
            }
    package <- readPackage config cabalfp
    return LocalPackage
        { lpPackage = package
        , lpExeComponents = Nothing -- HACK: makes it so that sdist output goes to a log instead of a file.
        , lpDir = pkgDir
        , lpCabalFile = cabalfp
        -- NOTE: these aren't the 'correct values, but aren't used in
        -- the usage of this function in this module.
        , lpTestBench = Nothing
        , lpDirtyFiles = True
        , lpNewBuildCache = Map.empty
        , lpFiles = Set.empty
        , lpComponents = Set.empty
        }

getSDistFileList :: M env m => LocalPackage -> m String
getSDistFileList lp =
    withSystemTempDirectory (stackProgName <> "-sdist") $ \tmpdir -> do
        menv <- getMinimalEnvOverride
        let bopts = defaultBuildOpts
        baseConfigOpts <- mkBaseConfigOpts bopts
        (_mbp, locals, _extraToBuild, sourceMap) <- loadSourceMap bopts
        withExecuteEnv menv bopts baseConfigOpts locals sourceMap $ \ee -> do
            withSingleContext ac ee task (Just "sdist") $ \_package _cabalfp _pkgDir cabal _announce _console _mlogFile -> do
                let outFile = tmpdir FP.</> "source-files-list"
                cabal False ["sdist", "--list-sources", outFile]
                liftIO (readFile outFile)
  where
    package = lpPackage lp
    ac = ActionContext Set.empty
    task = Task
        { taskProvides = PackageIdentifier (packageName package) (packageVersion package)
        , taskType = TTLocal lp
        , taskConfigOpts = TaskConfigOpts
            { tcoMissing = Set.empty
            , tcoOpts = \_ -> []
            }
        , taskPresent = Set.empty
        }

normalizeTarballPaths :: M env m => [FilePath] -> m [FilePath]
normalizeTarballPaths fps = do
    --TODO: consider whether erroring out is better - otherwise the
    --user might upload an incomplete tar?
    when (not (null outsideDir)) $
        $logWarn $ T.concat
            [ "Warning: These files are outside of the package directory, and will be omitted from the tarball: "
            , T.pack (show outsideDir)]
    return files
  where
    (outsideDir, files) = partitionEithers (map pathToEither fps)
    pathToEither fp = maybe (Left fp) Right (normalizePath fp)

normalizePath :: FilePath -> (Maybe FilePath)
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
