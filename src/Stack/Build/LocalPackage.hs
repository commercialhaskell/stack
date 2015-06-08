{-# LANGUAGE OverloadedStrings #-}
-- Load information on local packages
module Stack.Build.LocalPackage
    ( loadLocals
    ) where

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Path
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Types
import           Stack.Package
import           Stack.Types
import           System.Directory hiding (findFiles, findExecutable)

loadLocals :: (MonadReader env m, HasBuildConfig env, MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m)
           => BuildOpts
           -> m [LocalPackage]
loadLocals bopts = do
    targets <- mapM parseTarget $
        case boptsTargets bopts of
            Left [] -> ["."]
            Left x -> x
            Right _ -> []
    (dirs, names0) <- case partitionEithers targets of
        ([], targets') -> return $ partitionEithers targets'
        (bad, _) -> throwM $ Couldn'tParseTargets bad
    let names = Set.fromList names0

    bconfig <- asks getBuildConfig
    lps <- forM (Set.toList $ bcPackages bconfig) $ \dir -> do
        cabalfp <- getCabalFileName dir
        name <- parsePackageNameFromFilePath cabalfp
        let wanted = isWanted dirs names dir name
        pkg <- readPackage
            PackageConfig
                { packageConfigEnableTests = wanted && boptsFinalAction bopts == DoTests
                , packageConfigEnableBenchmarks = wanted && boptsFinalAction bopts == DoBenchmarks
                , packageConfigFlags = localFlags bopts bconfig name
                , packageConfigGhcVersion = bcGhcVersion bconfig
                , packageConfigPlatform = configPlatform $ getConfig bconfig
                }
            cabalfp
        when (packageName pkg /= name) $ throwM
            $ MismatchedCabalName cabalfp (packageName pkg)
        mbuildCache <- tryGetBuildCache dir
        mconfigCache <- tryGetConfigCache dir
        fileModTimes <- getPackageFileModTimes pkg cabalfp
        return LocalPackage
            { lpPackage = pkg
            , lpWanted = wanted
            , lpLastConfigOpts =
                  fmap (map T.decodeUtf8 . configCacheOpts) mconfigCache
            , lpDirtyFiles =
                  maybe True
                        ((/= fileModTimes) . buildCacheTimes)
                        mbuildCache
            , lpCabalFile = cabalfp
            , lpDir = dir
            }

    let known = Set.fromList $ map (packageName . lpPackage) lps
        unknown = Set.difference names known
    unless (Set.null unknown) $ throwM $ UnknownTargets $ Set.toList unknown

    return lps
  where
    parseTarget t = do
        let s = T.unpack t
        isDir <- liftIO $ doesDirectoryExist s
        if isDir
            then liftM (Right . Left) $ liftIO (canonicalizePath s) >>= parseAbsDir
            else return $ case parsePackageNameFromString s of
                     Left _ -> Left t
                     Right pname -> Right $ Right pname
    isWanted dirs names dir name =
        name `Set.member` names ||
        any (`isParentOf` dir) dirs ||
        any (== dir) dirs

-- | All flags for a local package
localFlags :: BuildOpts -> BuildConfig -> PackageName -> Map FlagName Bool
localFlags bopts bconfig name = M.union
    (fromMaybe M.empty $ M.lookup name $ boptsFlags bopts)
    (fromMaybe M.empty $ M.lookup name $ bcFlags bconfig)
