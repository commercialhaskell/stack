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
import qualified Data.Set                    as Set
import           Network.HTTP.Client.Conduit (HasHttpManager, getHttpManager)
import           Path
import qualified Paths_stack as Paths
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
        -> Maybe AbstractResolver
        -> m ()
upgrade fromGit mresolver = withSystemTempDirectory "stack-upgrade" $ \tmp' -> do
    menv <- getMinimalEnvOverride
    tmp <- parseAbsDir tmp'
    mdir <-
        if fromGit
            then do
                $logInfo "Cloning stack"
                runIn tmp "git" menv
                    [ "clone"
                    , "https://github.com/commercialhaskell/stack" -- TODO allow to be configured
                    , "stack"
                    , "--depth"
                    , "1"
                    ]
                    Nothing
                return $ Just $ tmp </> $(mkRelDir "stack")
            else do
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
    configMonoid <- asks $ configConfigMonoid . getConfig

    forM_ mdir $ \dir -> liftIO $ do
        bconfig <- runStackLoggingT manager logLevel terminal $ do
            lc <- loadConfig
                configMonoid
                (Just $ dir </> $(mkRelFile "stack.yaml"))
            lcLoadBuildConfig lc mresolver
        envConfig1 <- runStackT manager logLevel bconfig terminal setupEnv
        runStackT manager logLevel envConfig1 terminal $ build (const $ return ()) defaultBuildOpts
            { boptsTargets = ["stack"]
            , boptsInstallExes = True
            }
