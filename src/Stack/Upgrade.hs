{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Upgrade (upgrade) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader        (MonadReader, asks)
import           Control.Monad.Trans.Control
import           Data.Foldable               (forM_)
import qualified Data.Map                    as Map
import           Data.Monoid                 ((<>))
import qualified Data.Monoid
import qualified Data.Set                    as Set
import qualified Data.Text as T
import           Development.GitRev          (gitHash)
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager)
import           Path
import           Path.IO
import qualified Paths_stack as Paths
import           Stack.Build
import           Stack.Types.Build
import           Stack.Config
import           Stack.Fetch
import           Stack.PackageIndex
import           Stack.Setup
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           System.Process              (readProcess)
import           System.Process.Run

upgrade :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, HasTerminal env, HasReExec env, HasLogLevel env, MonadBaseControl IO m)
        => Maybe String -- ^ git repository to use
        -> Maybe AbstractResolver
        -> m ()
upgrade gitRepo mresolver = withCanonicalizedSystemTempDirectory "stack-upgrade" $ \tmp -> do
    menv <- getMinimalEnvOverride
    mdir <- case gitRepo of
      Just repo -> do
        remote <- liftIO $ readProcess "git" ["ls-remote", repo, "master"] []
        let latestCommit = head . words $ remote
        if (latestCommit == $gitHash) then do
          $logInfo "Already up-to-date, no upgrade required"
          return Nothing
        else do $logInfo "Cloning stack"
                runIn tmp "git" menv
                    [ "clone"
                    , repo
                    , "stack"
                    , "--depth"
                    , "1"
                    ]
                    Nothing
                return $ Just $ tmp </> $(mkRelDir "stack")
      Nothing -> do
        updateAllIndices menv
        caches <- getPackageCaches menv
        let latest = Map.fromListWith max
                   $ map toTuple
                   $ Map.keys

                   -- Mistaken upload to Hackage, just ignore it
                   $ Map.delete (PackageIdentifier
                        $(mkPackageName "stack")
                        $(mkVersion "9.9.9"))

                     caches
        case Map.lookup $(mkPackageName "stack") latest of
            Nothing -> error "No stack found in package indices"
            Just version | version <= fromCabalVersion Paths.version -> do
                $logInfo "Already at latest version, no upgrade required"
                return Nothing
            Just version -> do
                let ident = PackageIdentifier $(mkPackageName "stack") version
                paths <- unpackPackageIdents menv tmp Nothing $ Set.singleton ident
                case Map.lookup ident paths of
                    Nothing -> error "Stack.Upgrade.upgrade: invariant violated, unpacked directory not found"
                    Just path -> return $ Just path

    manager <- asks getHttpManager
    logLevel <- asks getLogLevel
    terminal <- asks getTerminal
    reExec <- asks getReExec
    config <- asks getConfig

    forM_ mdir $ \dir -> liftIO $ do
        bconfig <- runStackLoggingT manager logLevel terminal reExec $ do
            lc <- loadConfig
                (configConfigMonoid config <> Data.Monoid.mempty
                    { configMonoidInstallGHC = Just True
                    })
                (Just $ dir </> $(mkRelFile "stack.yaml"))
            lcLoadBuildConfig lc mresolver
        envConfig1 <- runStackT manager logLevel bconfig terminal reExec $ setupEnv $ Just $
            "Try rerunning with --install-ghc to install the correct GHC into " <>
            T.pack (toFilePath (configLocalPrograms config))
        runStackT manager logLevel envConfig1 terminal reExec $
          build (const $ return ()) Nothing defaultBuildOpts
            { boptsTargets = ["stack"]
            , boptsInstallExes = True
            }
