{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Upgrade
    ( upgrade
    , UpgradeOpts
    , upgradeOpts
    ) where

import           Control.Exception.Safe      (catchAny)
import           Control.Monad               (unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Foldable               (forM_)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
import           Data.Monoid.Extra
import qualified Data.Text as T
import           Lens.Micro                  (set)
import           Options.Applicative
import           Path
import           Path.IO
import qualified Paths_stack as Paths
import           Stack.Build
import           Stack.Config
import           Stack.Fetch
import           Stack.PackageIndex
import           Stack.Setup
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Resolver
import           Stack.Types.StackT
import           System.Exit                 (ExitCode (ExitSuccess))
import           System.Process              (rawSystem, readProcess)
import           System.Process.Run

upgradeOpts :: Parser UpgradeOpts
upgradeOpts = UpgradeOpts
    <$> (sourceOnly <|> optional binaryOpts)
    <*> (binaryOnly <|> optional sourceOpts)
  where
    binaryOnly = flag' Nothing (long "binary-only" <> help "Do not use a source upgrade path")
    sourceOnly = flag' Nothing (long "source-only" <> help "Do not use a binary upgrade path")

    binaryOpts = BinaryOpts
        <$> optional (strOption
              ( long "binary-platform"
             <> help "Platform type for archive to download"
             <> showDefault))
        <*> switch
         (long "force-download" <>
          help "Download a stack executable, even if the version number is older than what we have")
        <*> optional (strOption
         (long "binary-version" <>
          help "Download a specific version, even if it's out of date"))
        <*> optional (strOption
         (long "github-org" <>
          help "Github organization name"))
        <*> optional (strOption
         (long "github-repo" <>
          help "Github repository name"))

    sourceOpts = SourceOpts
        <$> ((\fromGit repo -> if fromGit then Just repo else Nothing)
                <$> switch
                    ( long "git"
                    <> help "Clone from Git instead of downloading from Hackage (more dangerous)" )
                <*> strOption
                    ( long "git-repo"
                    <> help "Clone from specified git repository"
                    <> value "https://github.com/commercialhaskell/stack"
                    <> showDefault ))

data BinaryOpts = BinaryOpts
    { _boPlatform :: !(Maybe String)
    , _boForce :: !Bool
    -- ^ force a download, even if the downloaded version is older
    -- than what we are
    , _boVersion :: !(Maybe String)
    -- ^ specific version to download
    , _boGithubOrg :: !(Maybe String)
    , _boGithubRepo :: !(Maybe String)
    }
    deriving Show
data SourceOpts = SourceOpts
    { _soRepo :: !(Maybe String)
    }
    deriving Show

data UpgradeOpts = UpgradeOpts
    { _uoBinary :: !(Maybe BinaryOpts)
    , _uoSource :: !(Maybe SourceOpts)
    }
    deriving Show

upgrade :: (StackM env m, HasConfig env)
        => ConfigMonoid
        -> Maybe AbstractResolver
        -> Maybe String -- ^ git hash at time of building, if known
        -> UpgradeOpts
        -> m ()
upgrade gConfigMonoid mresolver builtHash (UpgradeOpts mbo mso) =
    case (mbo, mso) of
        -- FIXME It would be far nicer to capture this case in the
        -- options parser itself so we get better error messages, but
        -- I can't think of a way to make it happen.
        (Nothing, Nothing) -> error "You must allow either binary or source upgrade paths"
        (Just bo, Nothing) -> binary bo
        (Nothing, Just so) -> source so
        (Just bo, Just so) -> binary bo `catchAny` \e -> do
            $logWarn "Exception occured when trying to perform binary upgrade:"
            $logWarn $ T.pack $ show e
            $logWarn "Falling back to source upgrade"

            source so
  where
    binary bo = binaryUpgrade bo
    source so = sourceUpgrade gConfigMonoid mresolver builtHash so

binaryUpgrade
  :: (StackM env m, HasConfig env)
  => BinaryOpts
  -> m ()
binaryUpgrade (BinaryOpts mplatform force' mver morg mrepo) = do
    platforms0 <-
      case mplatform of
        Nothing -> preferredPlatforms
        Just p -> return [("windows" `T.isInfixOf` T.pack p, p)]
    archiveInfo <- downloadStackReleaseInfo morg mrepo mver

    let mdownloadVersion = getDownloadVersion archiveInfo
        force =
          case mver of
            Nothing -> force'
            Just _ -> True -- specifying a version implies we're forcing things
    isNewer <-
        case mdownloadVersion of
            Nothing -> do
                $logError "Unable to determine upstream version from Github metadata"
                unless force $
                    $logError "Rerun with --force-download to force an upgrade"
                return False
            Just downloadVersion -> do
                $logInfo $ T.concat
                    [ "Current Stack version: "
                    , versionText stackVersion
                    , ", available download version: "
                    , versionText downloadVersion
                    ]
                return $ downloadVersion > stackVersion

    toUpgrade <- case (force, isNewer) of
        (False, False) -> do
            $logInfo "Skipping binary upgrade, your version is already more recent"
            return False
        (True, False) -> do
            $logInfo "Forcing binary upgrade"
            return True
        (_, True) -> do
            $logInfo "Newer version detected, downloading"
            return True
    when toUpgrade $ do
        config <- view configL
        downloadStackExe platforms0 archiveInfo (configLocalBin config) $ \tmpFile -> do
            -- Sanity check!
            ec <- rawSystem (toFilePath tmpFile) ["--version"]

            unless (ec == ExitSuccess)
                    $ error "Non-success exit code from running newly downloaded executable"

sourceUpgrade
  :: (StackM env m, HasConfig env)
  => ConfigMonoid
  -> Maybe AbstractResolver
  -> Maybe String
  -> SourceOpts
  -> m ()
sourceUpgrade gConfigMonoid mresolver builtHash (SourceOpts gitRepo) =
  withSystemTempDir "stack-upgrade" $ \tmp -> do
    menv <- getMinimalEnvOverride
    mdir <- case gitRepo of
      Just repo -> do
        remote <- liftIO $ readProcess "git" ["ls-remote", repo, "master"] []
        let latestCommit = head . words $ remote
        when (isNothing builtHash) $
            $logWarn $ "Information about the commit this version of stack was "
                    <> "built from is not available due to how it was built. "
                    <> "Will continue by assuming an upgrade is needed "
                    <> "because we have no information to the contrary."
        if builtHash == Just latestCommit
            then do
                $logInfo "Already up-to-date, no upgrade required"
                return Nothing
            else do
                $logInfo "Cloning stack"
                -- NOTE: "--recursive" was added after v1.0.0 (and before the
                -- next release).  This means that we can't use submodules in
                -- the stack repo until we're comfortable with "stack upgrade
                -- --git" not working for earlier versions.
                let args = [ "clone", repo , "stack", "--depth", "1", "--recursive"]
                runCmd (Cmd (Just tmp) "git" menv args) Nothing
                return $ Just $ tmp </> $(mkRelDir "stack")
      Nothing -> do
        updateAllIndices menv
        (caches, _gitShaCaches) <- getPackageCaches
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
                paths <- unpackPackageIdents menv tmp Nothing
                    -- accept latest cabal revision by not supplying a Git SHA
                    $ Map.singleton ident Nothing
                case Map.lookup ident paths of
                    Nothing -> error "Stack.Upgrade.upgrade: invariant violated, unpacked directory not found"
                    Just path -> return $ Just path

    forM_ mdir $ \dir -> do
        lc <- loadConfig
            gConfigMonoid
            mresolver
            (Just $ dir </> $(mkRelFile "stack.yaml"))
        bconfig <- lcLoadBuildConfig lc Nothing
        envConfig1 <- runInnerStackT bconfig $ setupEnv $ Just $
            "Try rerunning with --install-ghc to install the correct GHC into " <>
            T.pack (toFilePath (configLocalPrograms (view configL bconfig)))
        runInnerStackT (set (buildOptsL.buildOptsInstallExesL) True envConfig1) $
            build (const $ return ()) Nothing defaultBuildOptsCLI
                { boptsCLITargets = ["stack"]
                }
