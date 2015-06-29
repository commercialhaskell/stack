{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Upgrade (upgrade) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager)
import           Path
import           Stack.Build
import           Stack.Build.Types
import           Stack.Config
import           Stack.Fetch
import           Stack.PackageIndex
import           Stack.Setup
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           System.IO.Temp              (withSystemTempDirectory)
import           System.Process.Run

upgrade :: (MonadIO m, MonadMask m, MonadReader env m, HasConfig env, HasHttpManager env, MonadLogger m, HasTerminal env, HasLogLevel env, MonadBaseControl IO m)
        => Bool -- ^ use Git?
        -> Maybe Resolver
        -> m ()
upgrade fromGit mresolver = withSystemTempDirectory "stack-upgrade" $ \tmp' -> do
    menv <- getMinimalEnvOverride
    tmp <- parseAbsDir tmp'
    dir <-
        if fromGit
            then do
                $logInfo "Cloning stack"
                runIn tmp "git" menv
                    [ "clone"
                    , "git@github.com:commercialhaskell/stack" -- TODO allow to be configured
                    , "stack"
                    , "--depth"
                    , "1"
                    ]
                    Nothing
                return $ tmp </> $(mkRelDir "stack")
            else do
                updateAllIndices menv
                caches <- getPackageCaches menv
                let latest = Map.fromListWith max
                           $ map toTuple
                           $ Map.keys caches
                case Map.lookup $(mkPackageName "stack") latest of
                    Nothing -> error "No stack found in package indices"
                    Just version -> do
                        let ident = PackageIdentifier $(mkPackageName "stack") version
                        paths <- unpackPackageIdents menv tmp Nothing $ Set.singleton ident
                        case Map.lookup ident paths of
                            Nothing -> error "Stack.Upgrade.upgrade: invariant violated, unpacked directory not found"
                            Just path -> return path

    manager <- asks getHttpManager
    logLevel <- asks getLogLevel
    terminal <- asks getTerminal
    configMonoid <- asks $ configConfigMonoid . getConfig

    liftIO $ do
        bconfig <- runStackLoggingT manager logLevel terminal $ do
            lc <- loadConfig
                configMonoid
                (Just $ dir </> $(mkRelFile "stack.yaml"))
            lcLoadBuildConfig lc mresolver ThrowException
        envConfig1 <- runStackT manager logLevel bconfig terminal setupEnv
        runStackT manager logLevel envConfig1 terminal $ build BuildOpts
            { boptsTargets = ["stack"]
            , boptsLibProfile = False
            , boptsExeProfile = False
            , boptsEnableOptimizations = Nothing
            , boptsHaddock = False
            , boptsHaddockDeps = Nothing
            , boptsFinalAction = DoNothing
            , boptsDryrun = False
            , boptsGhcOptions = []
            , boptsFlags = Map.empty
            , boptsInstallExes = DefaultInstall
            , boptsPreFetch = False
            , boptsTestArgs = []
            , boptsOnlySnapshot = False
            , boptsCoverage = False
            }
