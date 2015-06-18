{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Init
    ( findCabalFiles
    , initProject
    , InitOpts (..)
    , initOptsParser
    , readResolver
    ) where

import           Control.Exception               (assert)
import           Control.Exception.Enclosed      (handleIO, tryAny, catchAny)
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
import           Options.Applicative
import           Options.Applicative.Types       (readerAsk)
import           Path
import           Path.Find
import           Path.IO
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Package
import           Stack.Solver
import           Stack.Types
import           System.Directory                (getDirectoryContents)

findCabalFiles :: MonadIO m => Path Abs Dir -> m [Path Abs File]
findCabalFiles dir =
    liftIO $ findFiles dir isCabal (not . isIgnored)
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
            => Maybe Resolver -- ^ force this resolver to be used
            -> InitOpts
            -> m ()
initProject mresolver initOpts = do
    currDir <- getWorkingDir
    let dest = currDir </> stackDotYaml
        dest' = toFilePath dest
    exists <- fileExists dest
    when exists $ error "Refusing to overwrite existing stack.yaml, please delete before running stack init"

    cabalfps <- findCabalFiles currDir
    $logInfo $ "Writing default config file to: " <> T.pack dest'
    $logInfo $ "Basing on cabal files:"
    mapM_ (\path -> $logInfo $ "- " <> T.pack (toFilePath path)) cabalfps
    $logInfo ""

    when (null cabalfps) $ error "In order to init, you should have an existing .cabal file. Please try \"stack new\" instead"
    gpds <- mapM readPackageUnresolved cabalfps

    (r, flags, extraDeps) <- getDefaultResolver cabalfps gpds mresolver initOpts
    let p = Project
            { projectPackages = pkgs
            , projectExtraDeps = extraDeps
            , projectFlags = flags
            , projectResolver = r
            }
        pkgs = map toPkg cabalfps
        toPkg fp = PackageEntry
            { peValidWanted = True
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
                   -> Maybe Resolver -- ^ resolver override
                   -> InitOpts
                   -> m (Resolver, Map PackageName (Map FlagName Bool), Map PackageName Version)
getDefaultResolver cabalfps gpds mresolver initOpts = do
    names <-
        case mresolver of
            Nothing | ioUseSolver initOpts -> return []
            Nothing -> do
                msnapshots <- getSnapshots'
                case msnapshots of
                    Nothing -> return []
                    Just snapshots -> getRecommendedSnapshots snapshots initOpts
            Just resolver ->
                return $
                    case resolver of
                        ResolverSnapshot name -> [name]
                        ResolverGhc _ -> []
    mpair <- findBuildPlan gpds names
    case mpair of
        Just (snap, flags) ->
            return (ResolverSnapshot snap, flags, Map.empty)
        Nothing ->
            case mresolver of
                Nothing ->
                    case ioFallback initOpts of
                        Nothing -> do
                            eres <- tryAny $ cabalSolver cabalfps
                            case eres of
                                Left e -> do
                                    $logInfo $ T.pack $ "Using cabal solver failed: " ++ show e
                                    throwM $ NoMatchingSnapshot names
                                Right (ghcVersion, extraDeps) -> do
                                    return
                                        ( ResolverGhc ghcVersion
                                        , Map.filter (not . Map.null) $ fmap snd extraDeps
                                        , fmap fst extraDeps
                                        )
                        Just resolver -> return (resolver, Map.empty, Map.empty)
                Just resolver -> return (resolver, Map.empty, Map.empty)

getRecommendedSnapshots :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
                        => Snapshots
                        -> InitOpts
                        -> m [SnapName]
getRecommendedSnapshots snapshots initOpts = do
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

    case ioPref initOpts of
        PrefNone -> return names
        PrefLTS -> return $ namesLTS ++ namesNightly
        PrefNightly -> return $ namesNightly ++ namesLTS

data InitOpts = InitOpts
    { ioPref     :: !SnapPref
    -- ^ Preferred snapshots
    , ioFallback :: !(Maybe Resolver)
    , ioUseSolver :: !Bool
    -- ^ Force usage of a dependency solver instead of snapshots
    }

data SnapPref = PrefNone | PrefLTS | PrefNightly

initOptsParser :: Parser InitOpts
initOptsParser = InitOpts
    <$> pref
    <*> optional fallback
    <*> flag False True
            (long "use-solver" <>
             help "Force usage of a dependency solver")
  where
    pref =
        flag' PrefLTS
            (long "prefer-lts" <>
             help "Prefer LTS snapshots over Nightly snapshots") <|>
        flag' PrefNightly
            (long "prefer-nightly" <>
             help "Prefer Nightly snapshots over LTS snapshots") <|>
        pure PrefNone

    fallback = option readResolver
        (long "fallback" <>
         metavar "RESOLVER" <>
         help "Fallback resolver if none of the tested snapshots work")

readResolver :: ReadM Resolver
readResolver = do
    s <- readerAsk
    case parseResolver $ T.pack s of
        Left e -> readerError $ show e
        Right x -> return x

-- | Same semantics as @nub@, but more efficient by using the @Ord@ constraint.
nubOrd :: Ord a => [a] -> [a]
nubOrd =
    go Set.empty
  where
    go _ [] = []
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise = x : go (Set.insert x s) xs
