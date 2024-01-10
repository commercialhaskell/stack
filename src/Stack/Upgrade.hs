{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and functions related to Stack's @upgrade@ command.
module Stack.Upgrade
  ( UpgradeOpts (..)
  , BinaryOpts (..)
  , SourceOpts (..)
  , upgradeCmd
  , upgrade
  ) where

import qualified Data.Text as T
import           Path ( (</>), parseRelDir )
import           RIO.Process ( proc, runProcess_, withWorkingDir )
import           Stack.Build ( build )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.BuildInfo ( maybeGitHash )
import           Stack.Constants ( relDirStackProgName, stackDotYaml )
import           Stack.Prelude hiding ( force, Display (..) )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withEnvConfig
                   , withGlobalProject )
import           Stack.Setup
                   ( downloadStackExe, downloadStackReleaseInfo
                   , getDownloadVersion, preferredPlatforms, stackVersion
                   )
import           Stack.Types.BuildOpts
                   ( BuildOptsCLI (..), buildOptsInstallExesL
                   , defaultBuildOptsCLI
                   )
import           Stack.Types.Config ( Config (..), HasConfig (..), buildOptsL )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           System.Process ( rawSystem, readProcess )

-- | Type representing \'pretty\' exceptions thrown by functions in the
-- "Stack.Upgrade" module.
data UpgradePrettyException
  = ResolverOptionInvalid
  | NeitherBinaryOrSourceSpecified
  | ExecutableFailure
  | CommitsNotFound String String
  | StackInPackageIndexNotFound
  | VersionWithNoRevision
  deriving (Show, Typeable)

instance Pretty UpgradePrettyException where
  pretty ResolverOptionInvalid =
    "[S-8761]"
    <> line
    <> fillSep
         [ "The"
         , style Shell "--resolver"
         , flow "option cannot be used with Stack's"
         , style Shell "upgrade"
         , "command."
         ]
  pretty NeitherBinaryOrSourceSpecified =
    "[S-3642]"
    <> line
    <> flow "You must allow either binary or source upgrade paths."
  pretty ExecutableFailure =
    "[S-8716]"
    <> line
    <> flow "Non-success exit code from running newly downloaded executable."
  pretty (CommitsNotFound branch repo) =
    "[S-7114]"
    <> line
    <> fillSep
         [ flow "No commits found for branch"
         , style Current (fromString branch)
         , flow "on repo"
         , style Url (fromString repo) <> "."
         ]
  pretty StackInPackageIndexNotFound =
    "[S-9668]"
    <> line
    <> flow "No Stack version found in package indices."
  pretty VersionWithNoRevision =
    "[S-6648]"
    <> line
    <> flow "Latest version with no revision."

instance Exception UpgradePrettyException

-- | Type representing options for upgrading Stack with a binary executable
-- file.
data BinaryOpts = BinaryOpts
  { _boPlatform :: !(Maybe String)
  , _boForce :: !Bool
    -- ^ Force a download, even if the downloaded version is older than what we
    -- are.
  , _boOnlyLocalBin :: !Bool
    -- ^ Only download to Stack's local binary directory.
  , _boVersion :: !(Maybe String)
    -- ^ Specific version to download
  , _boGitHubOrg :: !(Maybe String)
  , _boGitHubRepo :: !(Maybe String)
  }
  deriving Show

-- | Type representing options for upgrading Stack from source code.
newtype SourceOpts
  = SourceOpts (Maybe (String, String)) -- repo and branch
  deriving Show

-- | Type representing command line options for the @stack upgrade@ command.
data UpgradeOpts = UpgradeOpts
  { _uoBinary :: !(Maybe BinaryOpts)
  , _uoSource :: !(Maybe SourceOpts)
  }
  deriving Show

-- | Function underlying the @stack upgrade@ command.
upgradeCmd :: UpgradeOpts -> RIO Runner ()
upgradeCmd upgradeOpts = do
  go <- view globalOptsL
  case go.globalResolver of
    Just _ -> prettyThrowIO ResolverOptionInvalid
    Nothing -> withGlobalProject $ upgrade maybeGitHash upgradeOpts

upgrade ::
     Maybe String -- ^ git hash at time of building, if known
  -> UpgradeOpts
  -> RIO Runner ()
upgrade builtHash (UpgradeOpts mbo mso) = case (mbo, mso) of
  -- FIXME It would be far nicer to capture this case in the options parser
  -- itself so we get better error messages, but I can't think of a way to
  -- make it happen.
  (Nothing, Nothing) -> prettyThrowIO NeitherBinaryOrSourceSpecified
  (Just bo, Nothing) -> binary bo
  (Nothing, Just so) -> source so
  -- See #2977 - if --git or --git-repo is specified, do source upgrade.
  (_, Just so@(SourceOpts (Just _))) -> source so
  (Just bo, Just so) -> binary bo `catchAny` \e -> do
    prettyWarn $
         flow "When trying to perform binary upgrade, Stack encountered the \
              \following error:"
      <> blankLine
      <> ppException e
      <> blankLine
      <> flow "Falling back to source upgrade."
    source so
 where
  binary = binaryUpgrade
  source = sourceUpgrade builtHash

binaryUpgrade :: BinaryOpts -> RIO Runner ()
binaryUpgrade (BinaryOpts mplatform force' onlyLocalBin mver morg mrepo) =
  withConfig NoReexec $ do
    platforms0 <-
      case mplatform of
        Nothing -> preferredPlatforms
        Just p -> pure [("windows" `T.isInfixOf` T.pack p, p)]
    archiveInfo <- downloadStackReleaseInfo morg mrepo mver
    let mdownloadVersion = getDownloadVersion archiveInfo
        force =
          case mver of
            Nothing -> force'
            Just _ -> True -- specifying a version implies we're forcing things
    isNewer <-
      case mdownloadVersion of
        Nothing -> do
          prettyError $
               flow "Unable to determine upstream version from GitHub metadata."
            <> if force
                 then mempty
                 else
                      line
                   <> fillSep
                        [ flow "Rerun with"
                        , style Shell "--force-download"
                        , flow "to force an upgrade."
                        ]
          pure False
        Just downloadVersion -> do
          prettyInfoL
            [ flow "Current Stack version:"
            , fromString (versionString stackVersion) <> ";"
            , flow "available download version:"
            , fromString (versionString downloadVersion) <> "."
            ]
          pure $ downloadVersion > stackVersion
    toUpgrade <- case (force, isNewer) of
      (False, False) -> do
        prettyInfoS "Skipping binary upgrade, you are already running the most \
                    \recent version."
        pure False
      (True, False) -> do
        prettyInfoS "Forcing binary upgrade."
        pure True
      (_, True) -> do
        prettyInfoS "Newer version detected, downloading."
        pure True
    when toUpgrade $ do
      config <- view configL
      downloadStackExe
        platforms0 archiveInfo config.configLocalBin (not onlyLocalBin) $
          \tmpFile -> do
            -- Sanity check!
            ec <- rawSystem (toFilePath tmpFile) ["--version"]
            unless (ec == ExitSuccess) (prettyThrowIO ExecutableFailure)

sourceUpgrade ::
     Maybe String
  -> SourceOpts
  -> RIO Runner ()
sourceUpgrade builtHash (SourceOpts gitRepo) =
  withSystemTempDir "stack-upgrade" $ \tmp -> do
    mdir <- case gitRepo of
      Just (repo, branch) -> do
        remote <- liftIO $ System.Process.readProcess
          "git"
          ["ls-remote", repo, branch]
          []
        latestCommit <-
          case words remote of
            [] -> prettyThrowIO $ CommitsNotFound branch repo
            x:_ -> pure x
        when (isNothing builtHash) $
          prettyWarnS
            "Information about the commit this version of Stack was built from \
            \is not available due to how it was built. Will continue by \
            \assuming an upgrade is needed because we have no information to \
            \the contrary."
        if builtHash == Just latestCommit
            then do
              prettyInfoS "Already up-to-date, no upgrade required."
              pure Nothing
            else do
              prettyInfoS "Cloning stack."
              -- NOTE: "--recursive" was added after v1.0.0 (and before the next
              -- release).  This means that we can't use submodules in the Stack
              -- repo until we're comfortable with "stack upgrade --git" not
              -- working for earlier versions.
              let args =
                    [ "clone"
                    , repo
                    , "stack"
                    , "--depth"
                    , "1"
                    , "--recursive"
                    , "--branch"
                    , branch
                    ]
              withWorkingDir (toFilePath tmp) $ proc "git" args runProcess_
              pure $ Just $ tmp </> relDirStackProgName
      -- We need to access the Pantry database to find out about the latest
      -- Stack available on Hackage. We first use a standard Config to do this,
      -- and once we have the source load up the stack.yaml from inside that
      -- source.
      Nothing -> withConfig NoReexec $ do
        void
          $ updateHackageIndex
          $ Just "Updating index to make sure we find the latest Stack version."
        mversion <- getLatestHackageVersion
          YesRequireHackageIndex
          "stack"
          UsePreferredVersions
        (PackageIdentifierRevision _ version _) <-
          case mversion of
            Nothing -> prettyThrowIO StackInPackageIndexNotFound
            Just version -> pure version
        if version <= stackVersion
          then do
            prettyInfoS "Already at latest version, no upgrade required."
            pure Nothing
          else do
            suffix <- parseRelDir $ "stack-" ++ versionString version
            let dir = tmp </> suffix
            mrev <- getLatestHackageRevision
              YesRequireHackageIndex
              "stack"
              version
            case mrev of
              Nothing -> prettyThrowIO VersionWithNoRevision
              Just (_rev, cfKey, treeKey) -> do
                let ident = PackageIdentifier "stack" version
                unpackPackageLocation dir $ PLIHackage ident cfKey treeKey
                pure $ Just dir

    let modifyGO dir go = go
          { globalResolver = Nothing -- always use the resolver settings in the
                                     -- stack.yaml file
          , globalStackYaml = SYLOverride $ dir </> stackDotYaml
          }
        boptsCLI = defaultBuildOptsCLI
          { boptsCLITargets = ["stack"]
          }
    forM_ mdir $ \dir ->
      local (over globalOptsL (modifyGO dir))
        $ withConfig NoReexec
        $ withEnvConfig AllowNoTargets boptsCLI
        $ local (set (buildOptsL . buildOptsInstallExesL) True)
        $ build Nothing
