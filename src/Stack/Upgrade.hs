{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Stack.Upgrade
    ( upgrade
    , UpgradeOpts
    , upgradeOpts
    ) where

import           Stack.Prelude               hiding (force, Display (..))
import qualified Data.Text as T
import           Distribution.Version        (mkVersion')
import           Lens.Micro                  (set)
import           Options.Applicative
import           Path
import qualified Paths_stack as Paths
import           Stack.Build
import           Stack.Config
import           Stack.Constants
import           Stack.DefaultColorWhen (defaultColorWhen)
import           Stack.PrettyPrint
import           Stack.Setup
import           Stack.Types.Config
import           Stack.Types.Resolver
import           System.Exit                 (ExitCode (ExitSuccess))
import           System.Process              (rawSystem, readProcess)
import           RIO.Process

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
          help "Download the latest available stack executable")
        <*> optional (strOption
         (long "binary-version" <>
          help "Download a specific stack version"))
        <*> optional (strOption
         (long "github-org" <>
          help "Github organization name"))
        <*> optional (strOption
         (long "github-repo" <>
          help "Github repository name"))

    sourceOpts = SourceOpts
        <$> ((\fromGit repo branch -> if fromGit then Just (repo, branch) else Nothing)
                <$> switch
                    ( long "git"
                    <> help "Clone from Git instead of downloading from Hackage (more dangerous)" )
                <*> strOption
                    ( long "git-repo"
                    <> help "Clone from specified git repository"
                    <> value "https://github.com/commercialhaskell/stack"
                    <> showDefault )
                <*> strOption
                    ( long "git-branch"
                   <> help "Clone from this git branch"
                   <> value "master"
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
newtype SourceOpts = SourceOpts (Maybe (String, String)) -- repo and branch
    deriving Show

data UpgradeOpts = UpgradeOpts
    { _uoBinary :: !(Maybe BinaryOpts)
    , _uoSource :: !(Maybe SourceOpts)
    }
    deriving Show

upgrade :: HasConfig env
        => ConfigMonoid
        -> Maybe AbstractResolver
        -> Maybe String -- ^ git hash at time of building, if known
        -> UpgradeOpts
        -> RIO env ()
upgrade gConfigMonoid mresolver builtHash (UpgradeOpts mbo mso) =
    case (mbo, mso) of
        -- FIXME It would be far nicer to capture this case in the
        -- options parser itself so we get better error messages, but
        -- I can't think of a way to make it happen.
        (Nothing, Nothing) -> throwString "You must allow either binary or source upgrade paths"
        (Just bo, Nothing) -> binary bo
        (Nothing, Just so) -> source so
        -- See #2977 - if --git or --git-repo is specified, do source upgrade.
        (_, Just so@(SourceOpts (Just _))) -> source so
        (Just bo, Just so) -> binary bo `catchAny` \e -> do
            prettyWarnL
               [ flow "Exception occured when trying to perform binary upgrade:"
               , fromString . show $ e
               , line <> flow "Falling back to source upgrade"
               ]

            source so
  where
    binary bo = binaryUpgrade bo
    source so = sourceUpgrade gConfigMonoid mresolver builtHash so

binaryUpgrade :: HasConfig env => BinaryOpts -> RIO env ()
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
                prettyErrorL $
                    flow "Unable to determine upstream version from Github metadata"
                  :
                  [ line <> flow "Rerun with --force-download to force an upgrade"
                    | not force]
                return False
            Just downloadVersion -> do
                prettyInfoL
                    [ flow "Current Stack version:"
                    , fromString (versionString stackVersion) <> ","
                    , flow "available download version:"
                    , fromString (versionString downloadVersion)
                    ]
                return $ downloadVersion > stackVersion

    toUpgrade <- case (force, isNewer) of
        (False, False) -> do
            prettyInfoS "Skipping binary upgrade, you are already running the most recent version"
            return False
        (True, False) -> do
            prettyInfoS "Forcing binary upgrade"
            return True
        (_, True) -> do
            prettyInfoS "Newer version detected, downloading"
            return True
    when toUpgrade $ do
        config <- view configL
        downloadStackExe platforms0 archiveInfo (configLocalBin config) True $ \tmpFile -> do
            -- Sanity check!
            ec <- rawSystem (toFilePath tmpFile) ["--version"]

            unless (ec == ExitSuccess)
                    $ throwString "Non-success exit code from running newly downloaded executable"

sourceUpgrade
  :: HasConfig env
  => ConfigMonoid
  -> Maybe AbstractResolver
  -> Maybe String
  -> SourceOpts
  -> RIO env ()
sourceUpgrade gConfigMonoid mresolver builtHash (SourceOpts gitRepo) =
  withSystemTempDir "stack-upgrade" $ \tmp -> do
    mdir <- case gitRepo of
      Just (repo, branch) -> do
        remote <- liftIO $ System.Process.readProcess "git" ["ls-remote", repo, branch] []
        latestCommit <-
          case words remote of
            [] -> throwString $ "No commits found for branch " ++ branch ++ " on repo " ++ repo
            x:_ -> return x
        when (isNothing builtHash) $
            prettyWarnS $
                       "Information about the commit this version of stack was "
                    <> "built from is not available due to how it was built. "
                    <> "Will continue by assuming an upgrade is needed "
                    <> "because we have no information to the contrary."
        if builtHash == Just latestCommit
            then do
                prettyInfoS "Already up-to-date, no upgrade required"
                return Nothing
            else do
                prettyInfoS "Cloning stack"
                -- NOTE: "--recursive" was added after v1.0.0 (and before the
                -- next release).  This means that we can't use submodules in
                -- the stack repo until we're comfortable with "stack upgrade
                -- --git" not working for earlier versions.
                let args = [ "clone", repo , "stack", "--depth", "1", "--recursive", "--branch", branch]
                withWorkingDir (toFilePath tmp) $ proc "git" args runProcess_
                -- On Windows 10, an upstream issue with the `git clone` command
                -- means that command clears, but does not then restore, the
                -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals.
                -- The folowing hack re-enables the lost ANSI-capability.
                when osIsWindows $ void $ liftIO defaultColorWhen
                return $ Just $ tmp </> relDirStackProgName
      Nothing -> do
        void $ updateHackageIndex
             $ Just "Updating index to make sure we find the latest Stack version"
        mversion <- getLatestHackageVersion "stack" UsePreferredVersions
        pir@(PackageIdentifierRevision _ version _) <-
          case mversion of
            Nothing -> throwString "No stack found in package indices"
            Just version -> pure version

        if version <= mkVersion' Paths.version
            then do
                prettyInfoS "Already at latest version, no upgrade required"
                return Nothing
            else do
                suffix <- parseRelDir $ "stack-" ++ versionString version
                let dir = tmp </> suffix
                unpackPackageLocation dir $ PLIHackage pir Nothing
                pure $ Just dir

    forM_ mdir $ \dir ->
      loadConfig
      gConfigMonoid
      mresolver
      (SYLOverride $ dir </> stackDotYaml) $ \lc -> do
        bconfig <- liftIO $ lcLoadBuildConfig lc Nothing
        envConfig1 <- runRIO bconfig $ setupEnv $ Just $
            "Try rerunning with --install-ghc to install the correct GHC into " <>
            T.pack (toFilePath (configLocalPrograms (view configL bconfig)))
        runRIO (set (buildOptsL.buildOptsInstallExesL) True envConfig1) $
            build Nothing Nothing defaultBuildOptsCLI
                { boptsCLITargets = ["stack"]
                }
