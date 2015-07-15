{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Init
    ( findCabalFiles
    , initProject
    , InitOpts (..)
    , SnapPref (..)
    , Method (..)
    ) where

import           Control.Exception               (assert)
import           Control.Exception.Enclosed      (handleIO, catchAny)
import           Control.Monad                   (liftM, when)
import           Control.Monad.Catch             (MonadMask, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader            (MonadReader)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.IntMap                     as IntMap
import           Data.List                       (sort)
import           Data.List                       (isSuffixOf)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (mapMaybe)
import           Data.Monoid
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Yaml                       as Yaml
import qualified Distribution.PackageDescription as C
import           Network.HTTP.Client.Conduit     (HasHttpManager)
import           Path
import           Path.Find
import           Path.IO
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Package
import           Stack.Solver
import           Stack.Types
import           System.Directory                (getDirectoryContents)

findCabalFiles :: MonadIO m => Bool -> Path Abs Dir -> m [Path Abs File]
findCabalFiles recurse dir =
    liftIO $ findFiles dir isCabal (\subdir -> recurse && not (isIgnored subdir))
  where
    isCabal path = ".cabal" `isSuffixOf` toFilePath path

    isIgnored path = toFilePath (dirname path) `Set.member` ignoredDirs

-- | Special directories that we don't want to traverse for .cabal files
ignoredDirs :: Set FilePath
ignoredDirs = Set.fromList
    [ ".git"
    , "dist"
    , ".stack-work"
    ]

-- | Generate stack.yaml
initProject :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
            => InitOpts
            -> m ()
initProject initOpts = do
    currDir <- getWorkingDir
    let dest = currDir </> stackDotYaml
        dest' = toFilePath dest
    exists <- fileExists dest
    when (not (forceOverwrite initOpts) && exists) $
      error ("Refusing to overwrite existing stack.yaml, " <>
             "please delete before running stack init " <>
             "or if you are sure use \"--force\"")

    cabalfps <- findCabalFiles (includeSubDirs initOpts) currDir
    $logInfo $ "Writing default config file to: " <> T.pack dest'
    $logInfo $ "Basing on cabal files:"
    mapM_ (\path -> $logInfo $ "- " <> T.pack (toFilePath path)) cabalfps
    $logInfo ""

    when (null cabalfps) $ error "In order to init, you should have an existing .cabal file. Please try \"stack new\" instead"
    gpds <- mapM readPackageUnresolved cabalfps

    (r, flags, extraDeps) <- getDefaultResolver cabalfps gpds initOpts
    let p = Project
            { projectPackages = pkgs
            , projectExtraDeps = extraDeps
            , projectFlags = flags
            , projectResolver = r
            }
        pkgs = map toPkg cabalfps
        toPkg fp = PackageEntry
            { peValidWanted = Nothing
            , peExtraDepMaybe = Nothing
            , peLocation = PLFilePath $
                case stripDir currDir $ parent fp of
                    Nothing
                        | currDir == parent fp -> "."
                        | otherwise -> assert False $ toFilePath $ parent fp
                    Just rel -> toFilePath rel
            , peSubdirs = []
            }
    $logInfo $ "Selected resolver: " <> renderResolver r
    liftIO $ Yaml.encodeFile dest' p
    $logInfo $ "Wrote project config to: " <> T.pack dest'

getSnapshots' :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
              => m (Maybe Snapshots)
getSnapshots' =
    liftM Just getSnapshots `catchAny` \e -> do
        $logError $
            "Unable to download snapshot list, and therefore could " <>
            "not generate a stack.yaml file automatically"
        $logError $
            "This sometimes happens due to missing Certificate Authorities " <>
            "on your system. For more information, see:"
        $logError ""
        $logError "    https://github.com/commercialhaskell/stack/issues/234"
        $logError ""
        $logError "You can try again, or create your stack.yaml file by hand. See:"
        $logError ""
        $logError "    https://github.com/commercialhaskell/stack/wiki/stack.yaml"
        $logError ""
        $logError $ "Exception was: " <> T.pack (show e)
        return Nothing

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
                   => [Path Abs File] -- ^ cabal files
                   -> [C.GenericPackageDescription] -- ^ cabal descriptions
                   -> InitOpts
                   -> m (Resolver, Map PackageName (Map FlagName Bool), Map PackageName Version)
getDefaultResolver cabalfps gpds initOpts =
    case ioMethod initOpts of
        MethodSnapshot snapPref -> do
            msnapshots <- getSnapshots'
            names <-
                case msnapshots of
                    Nothing -> return []
                    Just snapshots -> getRecommendedSnapshots snapshots snapPref
            mpair <- findBuildPlan gpds names
            case mpair of
                Just (snap, flags) ->
                    return (ResolverSnapshot snap, flags, Map.empty)
                Nothing -> throwM $ NoMatchingSnapshot names
        MethodResolver resolver -> do
            mpair <-
                case resolver of
                    ResolverSnapshot name -> findBuildPlan gpds [name]
                    ResolverGhc _ -> return Nothing
                    ResolverCustom _ -> return Nothing
            case mpair of
                Just (snap, flags) ->
                    return (ResolverSnapshot snap, flags, Map.empty)
                Nothing -> return (resolver, Map.empty, Map.empty)
        MethodSolver -> do
            (ghcVersion, extraDeps) <- cabalSolver (map parent cabalfps) []
            return
                ( ResolverGhc ghcVersion
                , Map.filter (not . Map.null) $ fmap snd extraDeps
                , fmap fst extraDeps
                )

getRecommendedSnapshots :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
                        => Snapshots
                        -> SnapPref
                        -> m [SnapName]
getRecommendedSnapshots snapshots pref = do
    -- Get the most recent LTS and Nightly in the snapshots directory and
    -- prefer them over anything else, since odds are high that something
    -- already exists for them.
    existing <-
        liftM (reverse . sort . mapMaybe (parseSnapName . T.pack)) $
        snapshotsDir >>=
        liftIO . handleIO (const $ return [])
               . getDirectoryContents . toFilePath
    let isLTS LTS{} = True
        isLTS Nightly{} = False
        isNightly Nightly{} = True
        isNightly LTS{} = False

        names = nubOrd $ concat
            [ take 2 $ filter isLTS existing
            , take 2 $ filter isNightly existing
            , map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            , [Nightly $ snapshotsNightly snapshots]
            ]

        namesLTS = filter isLTS names
        namesNightly = filter isNightly names

    case pref of
        PrefNone -> return names
        PrefLTS -> return $ namesLTS ++ namesNightly
        PrefNightly -> return $ namesNightly ++ namesLTS

data InitOpts = InitOpts
    { ioMethod :: !Method
    -- ^ Preferred snapshots
    , forceOverwrite :: Bool
    -- ^ Overwrite existing files
    , includeSubDirs :: Bool
    -- ^ If True, include all .cabal files found in any sub directories
    }

data SnapPref = PrefNone | PrefLTS | PrefNightly

-- | Method of initializing
data Method = MethodSnapshot SnapPref | MethodResolver Resolver | MethodSolver

-- | Same semantics as @nub@, but more efficient by using the @Ord@ constraint.
nubOrd :: Ord a => [a] -> [a]
nubOrd =
    go Set.empty
  where
    go _ [] = []
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise = x : go (Set.insert x s) xs
