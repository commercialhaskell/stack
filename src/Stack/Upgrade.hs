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

import qualified Codec.Archive.Tar           as Tar
import           Control.Exception.Safe      (catchAny, throwM)
import           Control.Monad               (guard, liftM, unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader        (MonadReader, asks)
import           Data.Aeson                  (Value (Array, Object, String))
import qualified Data.ByteString.Lazy        as L
import           Data.Conduit                ((.|))
import           Data.Conduit.Lazy           (lazyConsume)
import           Data.Conduit.Zlib           (ungzip)
import           Data.Foldable               (fold, forM_)
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
import           Data.Monoid.Extra
import qualified Data.Text as T
import qualified Data.Version
import           Distribution.System         (Platform (Platform), Arch (..), OS (..))
import           Lens.Micro                  (set)
import           Network.HTTP.Client         (parseUrlThrow)
import           Network.HTTP.Simple         (Request, httpJSON, withResponse,
                                              setRequestHeader, getResponseBody)
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
import           Stack.Types.Internal
import           Stack.Types.Resolver
import           Stack.Types.StackT
import qualified System.Directory            as IO
import           System.Exit                 (ExitCode (ExitSuccess))
import qualified System.FilePath             as FP
import           System.Process              (rawSystem, readProcess)
import           System.Process.Run
import           Text.ParserCombinators.ReadP (readP_to_S)

#if !WINDOWS
import           System.Posix.Files (setFileMode)
#endif

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

newtype StackReleaseInfo = StackReleaseInfo Value

downloadStackReleaseInfo :: MonadIO m => m StackReleaseInfo
downloadStackReleaseInfo = do
    -- FIXME make the Github repo configurable?
    let req = setUserAgent "https://api.github.com/repos/commercialhaskell/stack/releases/latest"
    liftM (StackReleaseInfo . getResponseBody) (httpJSON req)

setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" ["Haskell Stack Upgrade"]

getDownloadVersion :: StackReleaseInfo -> Maybe Data.Version.Version
getDownloadVersion (StackReleaseInfo val) = do
    Object o <- Just val
    String rawName <- HashMap.lookup "name" o
    case filter (null . snd)
        $ readP_to_S Data.Version.parseVersion
        $ T.unpack $ T.drop 1 rawName of
        (v, _):_ -> Just v
        [] -> Nothing

preferredPlatforms :: (MonadReader env m, HasPlatform env) => m [String]
preferredPlatforms = do
    Platform arch' os' <- asks getPlatform
    os <-
      case os' of
        Linux -> return "linux"
        Windows -> return "windows"
        OSX -> return "osx"
        FreeBSD -> return "freebsd"
        _ -> error $ "Binary upgrade not yet supported on OS: " ++ show os'
    arch <-
      case arch' of
        I386 -> return "i386"
        X86_64 -> return "x86_64"
        Arm -> return "arm"
        _ -> error $ "Binary upgrade not yet supported on arch: " ++ show arch'
    hasgmp4 <- return False -- FIXME import relevant code from Stack.Setup? checkLib $(mkRelFile "libgmp.so.3")
    let suffixes
          | hasgmp4 = ["-static", "-gmp4", ""]
          | otherwise = ["-static", ""]
    return $ map (\suffix -> concat [os, "-", arch, suffix]) suffixes

binaryUpgrade
  :: (StackM env m, HasConfig env)
  => BinaryOpts
  -> m ()
binaryUpgrade (BinaryOpts mplatform force) = do
    platforms0 <- maybe preferredPlatforms (return . return) mplatform
    archiveInfo <- downloadStackReleaseInfo

    let mdownloadVersion = getDownloadVersion archiveInfo
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
                    , T.pack $ Data.Version.showVersion Paths.version
                    , ", available download version: "
                    , T.pack $ Data.Version.showVersion downloadVersion
                    ]
                return $ downloadVersion > Paths.version

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
        config <- askConfig
        let destFile = toFilePath (configLocalBin config </> $(mkRelFile "stack"))
#if WINDOWS
                            FP.<.> "exe"
#endif

        downloadStackExe platforms0 archiveInfo destFile

downloadStackExe
    :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env)
    => [String] -- ^ acceptable platforms
    -> StackReleaseInfo
    -> FilePath -- ^ destination
    -> m ()
downloadStackExe platforms0 archiveInfo destFile = do
    archiveURL <-
      let loop [] = error $ "Unable to find binary Stack archive for platforms: " ++ unwords platforms0
          loop (p':ps) = do
            let p = T.pack p'
            $logInfo $ "Querying for archive location for platform: " <> p
            case findArchive archiveInfo p of
              Just x -> return x
              Nothing -> loop ps
       in loop platforms0

    $logInfo $ "Downloading from: " <> archiveURL

    liftIO $ do
      case () of
        ()
          | ".tar.gz" `T.isSuffixOf` archiveURL -> handleTarball archiveURL
          | ".zip" `T.isSuffixOf` archiveURL -> error "FIXME: Handle zip files"
          | otherwise -> error $ "Unknown archive format for Stack archive: " ++ T.unpack archiveURL

    $logInfo "Download complete, testing executable"

    liftIO $ do
      absTmpFile <- IO.canonicalizePath tmpFile

#if !WINDOWS
      setFileMode absTmpFile 0o755
#endif

      -- Sanity check!
      ec <- rawSystem absTmpFile ["--version"]

      unless (ec == ExitSuccess)
              $ error $ "Non-success exit code from running newly downloaded executable"

      IO.renameFile tmpFile destFile

    $logInfo $ T.pack $ "New stack executable available at " ++ destFile
  where
    tmpFile = destFile FP.<.> "tmp"

    findArchive (StackReleaseInfo val) pattern = do
        Object top <- return val
        Array assets <- HashMap.lookup "assets" top
        getFirst $ fold $ fmap (First . findMatch pattern') assets
      where
        pattern' = mconcat ["-", pattern, "."]

        findMatch pattern'' (Object o) = do
            String name <- HashMap.lookup "name" o
            guard $ not $ ".asc" `T.isSuffixOf` name
            guard $ pattern'' `T.isInfixOf` name
            String url <- HashMap.lookup "browser_download_url" o
            Just url
        findMatch _ _ = Nothing

    handleTarball :: T.Text -> IO ()
    handleTarball url = do
        req <- fmap setUserAgent $ parseUrlThrow $ T.unpack url
        withResponse req $ \res -> do
            entries <- fmap (Tar.read . L.fromChunks)
                     $ lazyConsume
                     $ getResponseBody res .| ungzip
            let loop Tar.Done = error $ concat
                    [ "Stack executable "
                    , show exeName
                    , " not found in archive from "
                    , T.unpack url
                    ]
                loop (Tar.Fail e) = throwM e
                loop (Tar.Next e es)
                    | Tar.entryPath e == exeName =
                        case Tar.entryContent e of
                            Tar.NormalFile lbs _ -> L.writeFile tmpFile lbs
                            _ -> error $ concat
                                [ "Invalid file type for tar entry named "
                                , exeName
                                , " downloaded from "
                                , T.unpack url
                                ]
                    | otherwise = loop es
            loop entries
      where
        -- The takeBaseName drops the .gz, dropExtension drops the .tar
        exeName = FP.dropExtension (FP.takeBaseName (T.unpack url)) FP.</> "stack"

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
        caches <- getPackageCaches
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
            T.pack (toFilePath (configLocalPrograms (getConfig bconfig)))
        runInnerStackT (set (envConfigBuildOpts.buildOptsInstallExes) True envConfig1) $
            build (const $ return ()) Nothing defaultBuildOptsCLI
                { boptsCLITargets = ["stack"]
                }
