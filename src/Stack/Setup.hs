{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Stack.Setup
  ( setupEnv
  ) where

import Control.Exception (Exception)
import Control.Exception.Enclosed (handleIO)
import Control.Monad (liftM, when)
import Control.Monad.Catch (MonadThrow, throwM, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.IORef
import qualified Data.List as List
import Data.Monoid
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Client.Conduit
import System.Directory
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (searchPathSeparator)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (rawSystem)
import Stack.Types
import Distribution.System (OS (..), Arch (..), Platform (..))
import Stack.Build.Types
import qualified Data.ByteString.Char8 as S8
import Path (Path, Abs, Dir, parseRelDir, parseAbsDir, parseRelFile, mkRelFile)
import qualified Path
import Control.Monad.Logger
import qualified Data.Text as T
import Data.List (intercalate)
import Path (toFilePath)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Process.Read
import Stack.GhcPkg
import qualified System.FilePath as FP
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Network.HTTP.Download (download)

data DownloadPair = DownloadPair Version Text
instance FromJSON DownloadPair where
    parseJSON = withObject "DownloadPair" $ \o -> DownloadPair
        <$> o .: "version"
        <*> o .: "url"

data SetupInfo = SetupInfo
    { siSevenzExe :: Text
    , siSevenzDll :: Text
    , siPortableGit :: DownloadPair
    , siGHCs :: Map Text (Map MajorVersion DownloadPair)
    }
instance FromJSON SetupInfo where
    parseJSON = withObject "SetupInfo" $ \o -> SetupInfo
        <$> o .: "sevenzexe"
        <*> o .: "sevenzdll"
        <*> o .: "portable-git"
        <*> o .: "ghc"

-- | Download the most recent SetupInfo
getSetupInfo :: (MonadIO m, MonadThrow m) => Manager -> m SetupInfo
getSetupInfo manager = do
    bss <- liftIO $ flip runReaderT manager
         $ withResponse req $ \res -> responseBody res $$ CL.consume
    let bs = S8.concat bss
    either throwM return $ Yaml.decodeEither' bs
  where
    req = "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup.yaml"

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (MonadIO m, MonadMask m, MonadLogger m)
         => Bool -- ^ install GHC if missing?
         -> Manager
         -> BuildConfig
         -> m BuildConfig
setupEnv installIfMissing manager bconfig = do
    -- Check the available GHCs
    menv0 <- liftIO $ configEnvOverride (bcConfig bconfig) EnvSettings
            { esIncludeLocals = False
            , esIncludeGhcPackagePath = False
            }
    let expected = bcGhcVersion bconfig
        platform = configPlatform (getConfig bconfig)
    minstalled <- getInstalledGHC menv0
    let needLocal = case minstalled of
            Nothing -> True
            Just installed ->
                -- we allow a newer version of GHC within the same major series
                getMajorVersion installed /= getMajorVersion expected ||
                expected > installed

    -- If we need to install a GHC, try to do so
    mghcBin <- if needLocal
        then do
            $logDebug "Looking for a local copy of GHC"
            mghcBin <- getLocalGHC bconfig expected
            case mghcBin of
                Just ghcBin -> do
                    $logDebug $ "Local copy found at: " <> T.intercalate ", " (map T.pack ghcBin)
                    return $ Just ghcBin
                Nothing
                    | installIfMissing -> do
                        $logDebug $ "None found, installing: " <> versionText expected
                        ghcBin <- installLocalGHC manager bconfig expected
                        return $ Just ghcBin
                    | otherwise ->
                        throwM $ GHCVersionMismatch minstalled (bcGhcVersion bconfig)
        else return Nothing

    -- Modify the initial environment to include the GHC path, if a local GHC
    -- is being used
    let env0 = case mghcBin of
            Nothing -> unEnvOverride menv0
            Just ghcBin ->
                let x = unEnvOverride menv0
                    mpath = Map.lookup "PATH" x
                    path = T.intercalate (T.singleton searchPathSeparator)
                        $ map T.pack ghcBin ++ maybe [] return mpath
                 in Map.insert "PATH" path x

    -- extra installation bin directories
    mkDirs <- runReaderT extraBinDirs bconfig
    let mpath = Map.lookup "PATH" env0
        depsPath = mkPath (mkDirs False) mpath
        localsPath = mkPath (mkDirs True) mpath

    deps <- runReaderT packageDatabaseDeps bconfig
    depsExists <- liftIO $ doesDirectoryExist $ toFilePath deps
    localdb <- runReaderT packageDatabaseLocal bconfig
    localdbExists <- liftIO $ doesDirectoryExist $ toFilePath localdb
    global <- mkEnvOverride platform (Map.insert "PATH" depsPath env0) >>= getGlobalDB
    let mkGPP locals = T.pack $ intercalate [searchPathSeparator] $ concat
            [ [toFilePath localdb | locals && localdbExists]
            , [toFilePath deps | depsExists]
            , [toFilePath global]
            ]

    envRef <- liftIO $ newIORef Map.empty
    let getEnvOverride' es = do
            m <- readIORef envRef
            case Map.lookup es m of
                Just eo -> return eo
                Nothing -> do
                    eo <- mkEnvOverride platform
                        $ Map.insert "PATH" (if esIncludeLocals es then localsPath else depsPath)
                        $ (if esIncludeGhcPackagePath es
                                then Map.insert "GHC_PACKAGE_PATH" (mkGPP (esIncludeLocals es))
                                else id)

                        -- For reasoning and duplication, see: https://github.com/fpco/stack/issues/70
                        $ Map.insert "HASKELL_PACKAGE_SANDBOX" (T.pack $ toFilePath deps)
                        $ Map.insert "HASKELL_PACKAGE_SANDBOXES"
                            (T.pack $ if esIncludeLocals es
                                {- This is what we'd ideally want to provide, but
                                 - HASKELL_PACKAGE_SANDBOX isn't set up to respect it. Need
                                 - to figure out a better solution, maybe creating a
                                 - combined database and passing that in?
                                 - -}
                                then intercalate [searchPathSeparator]
                                        [ toFilePath localdb
                                        , toFilePath deps
                                        ]
                                else toFilePath deps)
                        $ env0
                    !() <- atomicModifyIORef envRef $ \m' ->
                        (Map.insert es eo m', ())
                    return eo
    return bconfig { bcConfig = (bcConfig bconfig) { configEnvOverride = getEnvOverride' } }
  where
    mkPath dirs mpath = T.pack $ intercalate [searchPathSeparator]
        (map toFilePath dirs ++ maybe [] (return . T.unpack) mpath)

-- | Get the major version of the installed GHC, if available
getInstalledGHC :: (MonadIO m) => EnvOverride -> m (Maybe Version)
getInstalledGHC menv = do
    exists <- doesExecutableExist menv "ghc"
    if exists
        then do
            eres <- liftIO $ tryProcessStdout menv "ghc" ["--numeric-version"]
            return $ do
                Right bs <- Just eres
                parseVersion $ S8.takeWhile isValidChar bs
        else return Nothing
  where
    isValidChar '.' = True
    isValidChar c = '0' <= c && c <= '9'

-- | Get the bin directory for a local copy of GHC meeting the given version
-- requirement, if it exists
getLocalGHC :: (HasConfig config, MonadIO m, MonadLogger m, MonadThrow m)
            => config
            -> Version
            -> m (Maybe [FilePath])
getLocalGHC config' expected = do
    let dir = toFilePath $ configLocalGHCs $ getConfig config'
    contents <- liftIO $ handleIO (const $ return []) (getDirectoryContents dir)
    pairs <- liftIO $ fmap catMaybes $ mapM (toMaybePair dir) contents
    case listToMaybe $ reverse $ map snd $ sortBy (comparing fst) pairs of
        Nothing -> return Nothing
        Just (ghcDir, ghcBin) ->
            case configPlatform $ getConfig config' of
                Platform _ Windows -> do
                    gitDirs' <- getGitDirs Nothing dir contents
                    return $ Just
                        $ ghcBin
                        : (ghcDir FP.</> "mingw" FP.</> "bin")
                        : gitDirs'
                _ -> return $ Just [ghcBin]
  where
    expectedMajor = getMajorVersion expected
    toMaybePair root name
        | Just noGhc <- List.stripPrefix "ghc-" name
        , Just version <- parseVersionFromString noGhc
        , getMajorVersion version == expectedMajor
        , version >= expected = do
            let dir = root FP.</> name
                bin = dir FP.</> "bin"
                ghc = bin FP.</> "ghc" ++ exeSuffix
            exists <- doesFileExist ghc
            return $ if exists
                then Just (version, (dir, bin))
                else Nothing
        | otherwise = return Nothing

    exeSuffix =
        case configPlatform $ getConfig config' of
            Platform _ Windows -> ".exe"
            _ -> ""

-- | Get the installation directories for Git on Windows
getGitDirs :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Maybe (SetupInfo, Manager) -- ^ Just if we want to setup
           -> FilePath -- ^ root installation directory
           -> [FilePath] -- ^ contents of that directory
           -> m [FilePath] -- ^ extra directories for the PATH
getGitDirs mSetup root contents = do
    pairs <- liftM catMaybes $ mapM toMaybePair contents
    case listToMaybe $ reverse $ map snd $ sortBy (comparing fst) pairs of
        Just dirs -> return dirs
        Nothing -> case mSetup of
            Just (si, manager) -> setupGit si manager root
            Nothing -> do
                $logWarn "Using local GHC installation, but no local Git available"
                return []
  where
    toMaybePair name
        | Just noGit <- List.stripPrefix "git-" name
        , Just version <- parseVersionFromString noGit = do
            let dir = root FP.</> name
                dirs = gitDirs dir
            exists <- liftIO $ doesDirectoryExist $ head dirs
            return $ if exists
                then Just (version, gitDirs dir)
                else Nothing
        | otherwise = return Nothing

-- | Install a local copy of GHC in the given major version with at least the
-- given version. In other words, if 7.8.3 is specified, 7.8.4 may be selected.
-- Return the bin directory.
installLocalGHC :: (MonadIO m, MonadLogger m, HasConfig env, MonadThrow m, MonadMask m)
                => Manager -> env -> Version -> m [FilePath]
installLocalGHC manager config' version = do
    rel <- parseRelDir $ "ghc-" <> versionString version
    let dest = configLocalGHCs (getConfig config') Path.</> rel
    si <- getSetupInfo manager
    $logDebug $ "Attempting to install GHC " <> versionText version <>
                " to " <> T.pack (toFilePath dest)
    let posix' oskey = posix si oskey manager version dest
        windows' = windows si manager version dest (configLocalGHCs $ getConfig config')
    flip runReaderT config' $ case configPlatform $ getConfig config' of
        Platform I386 Linux -> posix' "linux32"
        Platform X86_64 Linux -> posix' "linux64"
        Platform I386 OSX -> posix' "macosx"
        Platform X86_64 OSX -> posix' "macosx"
        Platform I386 FreeBSD -> posix' "freebsd32"
        Platform X86_64 FreeBSD -> posix' "freebsd64"
        Platform I386 Windows -> windows'
        Platform X86_64 Windows -> windows'
        Platform arch os -> throwM $ UnsupportedSetupCombo os arch

data SetupException = UnsupportedSetupCombo OS Arch
                    | MissingDependencies [String]
                    | UnknownGHCVersion Version (Set MajorVersion)
                    | UnknownOSKey Text
    deriving Typeable
instance Exception SetupException
instance Show SetupException where
    show (UnsupportedSetupCombo os arch) = concat
        [ "I don't know how to install GHC for "
        , show (os, arch)
        , ", please install manually"
        ]
    show (MissingDependencies tools) =
        "The following executables are missing and must be installed:" ++
        intercalate ", " tools
    show (UnknownGHCVersion version known) = concat
        [ "No information found for GHC version "
        , versionString version
        , ". Known GHC major versions: "
        , intercalate ", " (map show $ Set.toList known)
        ]
    show (UnknownOSKey oskey) =
        "Unable to find installation URLs for OS key: " ++
        T.unpack oskey

-- | Get the DownloadPair for the given OS and GHC version
getGHCPair :: MonadThrow m
           => SetupInfo
           -> Text -- ^ OS key
           -> Version -- ^ GHC version
           -> m DownloadPair
getGHCPair si osKey version =
    case Map.lookup osKey $ siGHCs si of
        Nothing -> throwM $ UnknownOSKey osKey
        Just m ->
            case Map.lookup (getMajorVersion version) m of
                Nothing -> throwM $ UnknownGHCVersion version (Map.keysSet m)
                Just pair -> return pair

-- | Install GHC for 64-bit Linux
posix :: (MonadIO m, MonadLogger m, MonadReader env m, HasConfig env, MonadThrow m, MonadMask m)
      => SetupInfo
      -> Text -- ^ OS key
      -> Manager
      -> Version
      -> Path Abs Dir
      -> m [FilePath]
posix si osKey manager reqVersion dest = do
    menv <- getMinimalEnvOverride
    DownloadPair version url <- getGHCPair si osKey reqVersion
    let ghcExtension = FP.takeExtension (T.unpack url)
        zipTool = case ghcExtension of
                    ".xz"  -> ["xz"]
                    ".bz2" -> ["bzip2"]
                    _      -> []
    checkDependencies (concat [["make", "tar"], zipTool])
    let dirPiece = "ghc-" <> versionString version
    req <- parseUrl $ T.unpack url
    withSystemTempDirectory "stack-setup" $ \root' -> do
        root <- parseAbsDir root'
        ghcFilename <- parseRelFile ("ghc.tar" ++ ghcExtension)
        let file = root Path.</> ghcFilename
        dir <- liftM (root Path.</>) $ parseRelDir dirPiece
        $logInfo $ "Downloading from: " <> url
        runReaderT (download req file) manager
        $logInfo "Unpacking"
        runIn root "tar" menv ["xf", toFilePath file] Nothing
        $logInfo "Configuring"
        runIn dir (toFilePath $ dir Path.</> $(mkRelFile "configure"))
              menv ["--prefix=" ++ toFilePath dest] Nothing
        $logInfo "Installing"
        runIn dir "make" menv ["install"] Nothing
        $logInfo "GHC installed!"
    return [toFilePath dest FP.</> "bin"]

-- | Download 7z as necessary, and get a function for unpacking things.
--
-- Returned function takes an unpack directory and archive.
setup7z :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadIO m, MonadIO n)
        => SetupInfo
        -> FilePath -- ^ install root (Programs\)
        -> m (FilePath -> FilePath -> n ())
setup7z si root = do
    go (siSevenzDll si) dll
    go (siSevenzExe si) exe
    return $ \outdir archive -> liftIO $ do
        ec <- rawSystem exe
            [ "x"
            , "-o" ++ outdir
            , "-y"
            , archive
            ]
        when (ec /= ExitSuccess)
            $ error $ "Problem while decompressing " ++ archive
  where
    dir = root FP.</> "7z"

    go url fp = do
        path <- Path.parseAbsFile fp
        req <- parseUrl $ T.unpack url
        download req path

    exe = dir FP.</> "7z.exe"
    dll = dir FP.</> "7z.dll"

-- | Install PortableGit and get a list of directories to add to PATH
setupGit :: (MonadLogger m, MonadThrow m, MonadIO m)
         => SetupInfo
         -> Manager
         -> FilePath -- ^ root to install into (Programs\)
         -> m [FilePath]
setupGit si manager root = do
    $logInfo "Downloading PortableGit"
    run7z <- runReaderT (setup7z si root) manager
    req <- parseUrl $ T.unpack url
    dest <- Path.parseAbsFile $ dir FP.<.> "7z"
    runReaderT (download req dest) manager
    liftIO $ createDirectoryIfMissing True dir
    run7z dir $ toFilePath dest
    return $ gitDirs dir
  where
    dir = root FP.</> name
    DownloadPair gitVersion url = siPortableGit si
    name = "git-" <> versionString gitVersion

-- | Windows: Turn a base Git installation directory into a list of bin paths.
gitDirs :: FilePath -> [FilePath]
gitDirs dir =
    [ dir FP.</> "cmd"
    , dir FP.</> "usr" FP.</> "bin"
    ]

-- | Perform necessary installation for Windows
windows :: (MonadLogger m, MonadThrow m, MonadIO m)
        => SetupInfo
        -> Manager
        -> Version
        -> Path Abs Dir -- ^ GHC install root (Programs\ghc-X.Y.Z)
        -> Path Abs Dir -- ^ installation root (Programs\)
        -> m [FilePath]
windows si manager reqVersion dest progDir = do
    let root = toFilePath progDir
    contents <- liftIO $ handleIO (const $ return []) (getDirectoryContents root)
    gitDirs' <- getGitDirs (Just (si, manager)) root contents
    -- Note: we always use 32-bit Windows as the 64-bit version has problems
    DownloadPair version url <- getGHCPair si "windows32" reqVersion
    let dirPiece = "ghc-" <> versionString version
    req <- parseUrl $ T.unpack url
    piece <- Path.parseRelFile $ dirPiece ++ ".tar.xz"
    let destXZ = progDir Path.</> piece
    $logInfo $ "Downloading GHC from: " <> url
    runReaderT (download req destXZ) manager
    run7z <- runReaderT (setup7z si root) manager
    run7z (toFilePath progDir) (toFilePath destXZ)
    run7z (toFilePath progDir)
          (toFilePath progDir FP.</> dirPiece FP.<.> "tar")
    return $
        [ toFilePath $ dest Path.</> $(Path.mkRelDir "bin")
        , toFilePath $ dest Path.</> $(Path.mkRelDir "mingw")
                            Path.</> $(Path.mkRelDir "bin")
        ] ++ gitDirs'

-- | Check if given processes appear to be present, throwing an exception if
-- missing.
checkDependencies :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
                  => [String] -> m ()
checkDependencies tools = do
    menv <- getMinimalEnvOverride
    missing <- liftM catMaybes $ mapM (check menv) tools
    if null missing
        then return ()
        else throwM $ MissingDependencies missing
  where
    check menv tool = do
        exists <- doesExecutableExist menv tool
        return $ if exists then Nothing else Just tool
