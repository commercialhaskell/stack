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
    , makeConcreteResolver
    , tryDeprecatedPath
    , getImplicitGlobalProjectDir
    ) where

import           Control.Exception               (assert)
import           Control.Exception.Enclosed      (catchAny, handleIO)
import           Control.Monad                   (liftM, when, zipWithM_)
import           Control.Monad.Catch             (MonadMask, MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader            (MonadReader, asks)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as L
import qualified Data.HashMap.Strict             as HM
import qualified Data.IntMap                     as IntMap
import qualified Data.Foldable                   as F
import           Data.List                       (isSuffixOf,sortBy)
import           Data.List.Extra                 (nubOrd)
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
initProject :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, HasGHCVariant env, MonadLogger m, MonadBaseControl IO m)
            => Path Abs Dir
            -> InitOpts
            -> m ()
initProject currDir initOpts = do
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
    (warnings,gpds) <- fmap unzip (mapM readPackageUnresolved cabalfps)
    zipWithM_ (mapM_ . printCabalFileWarning) cabalfps warnings

    (r, flags, extraDeps) <- getDefaultResolver cabalfps gpds initOpts
    let p = Project
            { projectPackages = pkgs
            , projectExtraDeps = extraDeps
            , projectFlags = flags
            , projectResolver = r
            , projectCompiler = Nothing
            , projectExtraPackageDBs = []
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
    $logInfo $ "Selected resolver: " <> resolverName r
    liftIO $ L.writeFile dest' $ B.toLazyByteString $ renderStackYaml p
    $logInfo $ "Wrote project config to: " <> T.pack dest'

-- | Render a stack.yaml file with comments, see:
-- https://github.com/commercialhaskell/stack/issues/226
renderStackYaml :: Project -> B.Builder
renderStackYaml p =
    case Yaml.toJSON p of
        Yaml.Object o -> renderObject o
        _ -> assert False $ B.byteString $ Yaml.encode p
  where
    renderObject o =
        B.byteString "# For more information, see: https://github.com/commercialhaskell/stack/blob/release/doc/yaml_configuration.md\n\n" <>
        F.foldMap (goComment o) comments <>
        goOthers (o `HM.difference` HM.fromList comments) <>
        B.byteString
            "# Control whether we use the GHC we find on the path\n\
            \# system-ghc: true\n\n\
            \# Require a specific version of stack, using version ranges\n\
            \# require-stack-version: -any # Default\n\
            \# require-stack-version: >= 1.0.0\n\n\
            \# Override the architecture used by stack, especially useful on Windows\n\
            \# arch: i386\n\
            \# arch: x86_64\n\n\
            \# Extra directories used by stack for building\n\
            \# extra-include-dirs: [/path/to/dir]\n\
            \# extra-lib-dirs: [/path/to/dir]\n"

    comments =
        [ ("resolver", "Specifies the GHC version and set of packages available (e.g., lts-3.5, nightly-2015-09-21, ghc-7.10.2)")
        , ("packages", "Local packages, usually specified by relative directory name")
        , ("extra-deps", "Packages to be pulled from upstream that are not in the resolver (e.g., acme-missiles-0.3)")
        , ("flags", "Override default flag values for local packages and extra-deps")
        , ("extra-package-dbs", "Extra package databases containing global packages")
        ]

    goComment o (name, comment) =
        case HM.lookup name o of
            Nothing -> assert False mempty
            Just v ->
                B.byteString "# " <>
                B.byteString comment <>
                B.byteString "\n" <>
                B.byteString (Yaml.encode $ Yaml.object [(name, v)]) <>
                B.byteString "\n"

    goOthers o
        | HM.null o = mempty
        | otherwise = assert False $ B.byteString $ Yaml.encode o

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
        $logError "    https://github.com/commercialhaskell/stack/blob/release/doc/yaml_configuration.md"
        $logError ""
        $logError $ "Exception was: " <> T.pack (show e)
        return Nothing

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, HasGHCVariant env, MonadLogger m, MonadBaseControl IO m)
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
        MethodResolver aresolver -> do
            resolver <- makeConcreteResolver aresolver
            mpair <-
                case resolver of
                    ResolverSnapshot name -> findBuildPlan gpds [name]
                    ResolverCompiler _ -> return Nothing
                    ResolverCustom _ _ -> return Nothing
            case mpair of
                Just (snap, flags) ->
                    return (ResolverSnapshot snap, flags, Map.empty)
                Nothing -> return (resolver, Map.empty, Map.empty)
        MethodSolver -> do
            (compilerVersion, extraDeps) <- cabalSolver Ghc (map parent cabalfps) Map.empty Map.empty []
            return
                ( ResolverCompiler compilerVersion
                , Map.filter (not . Map.null) $ fmap snd extraDeps
                , fmap fst extraDeps
                )

getRecommendedSnapshots :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, HasGHCVariant env, MonadLogger m, MonadBaseControl IO m)
                        => Snapshots
                        -> SnapPref
                        -> m [SnapName]
getRecommendedSnapshots snapshots pref = do
    -- Get the most recent LTS and Nightly in the snapshots directory and
    -- prefer them over anything else, since odds are high that something
    -- already exists for them.
    existing <-
        liftM (sortBy (flip compare) . mapMaybe (parseSnapName . T.pack)) $
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
    { ioMethod       :: !Method
    -- ^ Preferred snapshots
    , forceOverwrite :: Bool
    -- ^ Overwrite existing files
    , includeSubDirs :: Bool
    -- ^ If True, include all .cabal files found in any sub directories
    }

data SnapPref = PrefNone | PrefLTS | PrefNightly

-- | Method of initializing
data Method = MethodSnapshot SnapPref | MethodResolver AbstractResolver | MethodSolver

-- | Turn an 'AbstractResolver' into a 'Resolver'.
makeConcreteResolver :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, HasHttpManager env, MonadLogger m)
                     => AbstractResolver
                     -> m Resolver
makeConcreteResolver (ARResolver r) = return r
makeConcreteResolver ar = do
    snapshots <- getSnapshots
    r <-
        case ar of
            ARResolver r -> assert False $ return r
            ARGlobal -> do
                config <- asks getConfig
                implicitGlobalDir <- getImplicitGlobalProjectDir config
                let fp = implicitGlobalDir </> stackDotYaml
                (ProjectAndConfigMonoid project _, _warnings) <-
                    liftIO (Yaml.decodeFileEither $ toFilePath fp)
                    >>= either throwM return
                return $ projectResolver project
            ARLatestNightly -> return $ ResolverSnapshot $ Nightly $ snapshotsNightly snapshots
            ARLatestLTSMajor x ->
                case IntMap.lookup x $ snapshotsLts snapshots of
                    Nothing -> error $ "No LTS release found with major version " ++ show x
                    Just y -> return $ ResolverSnapshot $ LTS x y
            ARLatestLTS
                | IntMap.null $ snapshotsLts snapshots -> error "No LTS releases found"
                | otherwise ->
                    let (x, y) = IntMap.findMax $ snapshotsLts snapshots
                     in return $ ResolverSnapshot $ LTS x y
    $logInfo $ "Selected resolver: " <> resolverName r
    return r

-- | Get the location of the implicit global project directory.
-- If the directory already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getImplicitGlobalProjectDir
    :: (MonadIO m, MonadLogger m)
    => Config -> m (Path Abs Dir)
getImplicitGlobalProjectDir config =
    --TEST no warning printed
    liftM fst $ tryDeprecatedPath
        Nothing
        dirExists
        (implicitGlobalProjectDir stackRoot)
        (implicitGlobalProjectDirDeprecated stackRoot)
  where
    stackRoot = configStackRoot config

-- | If deprecated path exists, use it and print a warning.
-- Otherwise, return the new path.
tryDeprecatedPath
    :: (MonadIO m, MonadLogger m)
    => Maybe T.Text -- ^ Description of file for warning (if Nothing, no deprecation warning is displayed)
    -> (Path Abs a -> m Bool) -- ^ Test for existence
    -> Path Abs a -- ^ New path
    -> Path Abs a -- ^ Deprecated path
    -> m (Path Abs a, Bool) -- ^ (Path to use, whether it already exists)
tryDeprecatedPath mWarningDesc exists new old = do
    newExists <- exists new
    if newExists
        then return (new, True)
        else do
            oldExists <- exists old
            if oldExists
                then do
                    case mWarningDesc of
                        Nothing -> return ()
                        Just desc ->
                            $logWarn $ T.concat
                                [ "Warning: Location of ", desc, " at '"
                                , T.pack (toFilePath old)
                                , "' is deprecated; rename it to '"
                                , T.pack (toFilePath new)
                                , "' instead" ]
                    return (old, True)
                else return (new, False)
