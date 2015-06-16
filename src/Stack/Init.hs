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
import           Control.Exception.Enclosed      (handleIO)
import           Control.Monad                   (liftM, when)
import           Control.Monad.Catch             (MonadCatch, SomeException,
                                                  catch, throwM)
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
initProject :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
            => Maybe Resolver -- ^ force this resolver to be used
            -> InitOpts
            -> m ()
initProject mresolver initOpts = do
    currDir <- getWorkingDir
    let dest = currDir </> stackDotYaml
        dest' = toFilePath dest
    exists <- fileExists dest
    when exists $ error "Invariant violated: in toBuildConfig's Nothing branch, and the stack.yaml file exists"

    cabalfps <- findCabalFiles currDir
    $logInfo $ "Writing default config file to: " <> T.pack dest'
    $logInfo $ "Basing on cabal files:"
    mapM_ (\path -> $logInfo $ "- " <> T.pack (toFilePath path)) cabalfps
    $logInfo ""

    when (null cabalfps) $ error "In order to init, you should have an existing .cabal file. Please try \"stack new\" instead"
    gpds <- mapM readPackageUnresolved cabalfps

    (r, flags) <- getDefaultResolver gpds mresolver initOpts
    let p = Project
            { projectPackages = pkgs
            , projectExtraDeps = Map.empty
            , projectFlags = flags
            , projectResolver = r
            }
        pkgs = map toPkg cabalfps
        toPkg fp = PackageEntry
            { peValidWanted = True
            , peLocation = PLFilePath $
                case stripDir currDir $ parent fp of
                    Nothing -> assert False $ toFilePath fp
                    Just rel -> toFilePath rel
            , peSubdirs = []
            }
    $logInfo $ "Selected resolver: " <> renderResolver r
    liftIO $ Yaml.encodeFile dest' p
    $logInfo $ "Wrote project config to: " <> T.pack dest'

getSnapshots' :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
              => m Snapshots
getSnapshots' =
    getSnapshots `catch` \e -> do
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
        throwM (e :: SomeException)

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
                   => [C.GenericPackageDescription] -- ^ cabal files
                   -> Maybe Resolver -- ^ resolver override
                   -> InitOpts
                   -> m (Resolver, Map PackageName (Map FlagName Bool))
getDefaultResolver gpds mresolver initOpts = do
    names <-
        case mresolver of
            Nothing -> do
                snapshots <- getSnapshots'
                getRecommendedSnapshots snapshots initOpts
            Just resolver ->
                return $
                    case resolver of
                        ResolverSnapshot name -> [name]
                        ResolverGhc _ -> []
    mpair <- findBuildPlan gpds names
    case mpair of
        Just (snap, flags) ->
            return (ResolverSnapshot snap, flags)
        Nothing ->
            case mresolver of
                Nothing ->
                    case ioFallback initOpts of
                        Nothing -> throwM $ NoMatchingSnapshot names
                        Just resolver -> return (resolver, Map.empty)
                Just resolver -> return (resolver, Map.empty)

getRecommendedSnapshots :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
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
    }

data SnapPref = PrefNone | PrefLTS | PrefNightly

initOptsParser :: Parser InitOpts
initOptsParser = InitOpts
    <$> pref
    <*> optional fallback
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
