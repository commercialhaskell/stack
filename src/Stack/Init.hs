{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Init
    ( findCabalFiles
    , initProject
    ) where

import           Control.Exception               (assert)
import           Control.Monad                   (when)
import           Control.Monad.Catch             (MonadCatch, SomeException,
                                                  catch, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader            (MonadReader)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.IntMap                     as IntMap
import           Data.List                       (isSuffixOf)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
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
import           Stack.Types

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

initProject :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
            => m ()
initProject = do
    currDir <- getWorkingDir
    let dest = currDir </> stackDotYaml
        dest' = toFilePath dest
    exists <- fileExists dest
    when exists $ error "Invariant violated: in toBuildConfig's Nothing branch, and the stack.yaml file exists"
    $logInfo $ "Writing default config file to: " <> T.pack dest'

    cabalfps <- findCabalFiles currDir
    when (null cabalfps) $ error "In order to init, you should have an existing .cabal file. Please try \"stack new\" instead"
    gpds <- mapM readPackageUnresolved cabalfps

    (r, flags) <- getDefaultResolver gpds
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
    liftIO $ Yaml.encodeFile dest' p
    $logInfo $ "Wrote project config to: " <> T.pack dest'

-- | Get the default resolver value
getDefaultResolver :: (MonadIO m, MonadCatch m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
                   => [C.GenericPackageDescription] -- ^ cabal files
                   -> m (Resolver, Map PackageName (Map FlagName Bool))
getDefaultResolver gpds = do
    snapshots <- getSnapshots `catch` \e -> do
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
    mpair <- findBuildPlan gpds snapshots
    case mpair of
        Just (snap, flags) ->
            return (ResolverSnapshot snap, flags)
        Nothing -> do
            let snap = case IntMap.maxViewWithKey (snapshotsLts snapshots) of
                    Just ((x, y), _) -> LTS x y
                    Nothing -> Nightly $ snapshotsNightly snapshots
            $logWarn $ T.concat
                [ "No matching snapshot was found for your package, "
                , "falling back to: "
                , renderSnapName snap
                ]
            $logWarn "This behavior will improve in the future, please see: https://github.com/commercialhaskell/stack/issues/253"
            return (ResolverSnapshot snap, Map.empty)
