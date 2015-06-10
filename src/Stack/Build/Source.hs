{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
-- Load information on package sources
module Stack.Build.Source
    ( loadSourceMap
    , SourceMap
    , PackageSource (..)
    ) where

import Network.HTTP.Client.Conduit (HasHttpManager)
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Function
import           Data.List
import qualified Data.Map                     as Map
import           Data.Map.Strict              (Map)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import           Path
import           Prelude                      hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Types
import           Stack.BuildPlan              (loadMiniBuildPlan,
                                               shadowMiniBuildPlan)
import           Stack.Constants              (wiredInPackages)
import           Stack.Package
import           Stack.Types
import           System.Directory             hiding (findExecutable, findFiles)

type SourceMap = Map PackageName PackageSource
data PackageSource
    = PSLocal LocalPackage
    | PSUpstream Version Location (Map FlagName Bool)
instance PackageInstallInfo PackageSource where
    piiVersion (PSLocal lp) = packageVersion $ lpPackage lp
    piiVersion (PSUpstream v _ _) = v

    piiLocation (PSLocal _) = Local
    piiLocation (PSUpstream _ loc _) = loc

loadSourceMap :: (MonadIO m, MonadCatch m, MonadReader env m, HasBuildConfig env, MonadBaseControl IO m, HasHttpManager env, MonadLogger m)
              => BuildOpts
              -> m (MiniBuildPlan, [LocalPackage], SourceMap)
loadSourceMap bopts = do
    bconfig <- asks getBuildConfig
    mbp0 <- case bcResolver bconfig of
        ResolverSnapshot snapName -> do
            $logDebug $ "Checking resolver: " <> renderSnapName snapName
            mbp <- loadMiniBuildPlan snapName
            return mbp
        ResolverGhc ghc -> return MiniBuildPlan
            { mbpGhcVersion = fromMajorVersion ghc
            , mbpPackages = Map.empty
            }

    locals <- loadLocals bopts

    let shadowed = Set.fromList (map (packageName . lpPackage) locals)
                <> Map.keysSet (bcExtraDeps bconfig)
        (mbp, extraDeps0) = shadowMiniBuildPlan mbp0 shadowed

        -- Add the extra deps from the stack.yaml file to the deps grabbed from
        -- the snapshot
        extraDeps1 = Map.union
            (Map.map (\v -> (v, Map.empty)) (bcExtraDeps bconfig))
            (Map.map (\mpi -> (mpiVersion mpi, mpiFlags mpi)) extraDeps0)

        -- Overwrite any flag settings with those from the config file
        extraDeps2 = Map.mapWithKey
            (\n (v, f) -> PSUpstream v Local $ fromMaybe f $ Map.lookup n $ bcFlags bconfig)
            extraDeps1

    let sourceMap = Map.unions
            [ Map.fromList $ flip map locals $ \lp ->
                let p = lpPackage lp
                 in (packageName p, PSLocal lp)
            , extraDeps2
            , flip fmap (mbpPackages mbp) $ \mpi ->
                (PSUpstream (mpiVersion mpi) Snap (mpiFlags mpi))
            ] `Map.difference` Map.fromList (map (, ()) wiredInPackages)

    return (mbp, locals, sourceMap)

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
    lps <- forM (Map.toList $ bcPackages bconfig) $ \(dir, validWanted) -> do
        cabalfp <- getCabalFileName dir
        name <- parsePackageNameFromFilePath cabalfp
        let wanted = validWanted && isWanted dirs names dir name
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
            , lpLastConfigOpts = mconfigCache
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
localFlags bopts bconfig name = Map.union
    (fromMaybe Map.empty $ Map.lookup name $ boptsFlags bopts)
    (fromMaybe Map.empty $ Map.lookup name $ bcFlags bconfig)
