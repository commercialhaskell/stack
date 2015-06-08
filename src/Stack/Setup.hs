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
  , ensureGHC
  , SetupOpts (..)
  ) where

import Control.Applicative
import Control.Exception (Exception)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Control.Monad (liftM, when)
import Control.Monad.Catch (MonadThrow, throwM, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Data.Aeson
import Data.IORef
import Data.Monoid
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Client.Conduit
import Path
import Path.IO
import System.Directory
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (searchPathSeparator)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (rawSystem)
import Stack.GhcPkg (getGlobalDB)
import Stack.Types
import Distribution.System (OS (..), Arch (..), Platform (..))
import Stack.Build.Types
import qualified Data.ByteString.Char8 as S8
import Control.Monad.Logger
import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Process.Read
import qualified System.FilePath as FP
import Network.HTTP.Download (download)
import Prelude -- Fix AMP warning

data SetupOpts = SetupOpts
    { soptsInstallIfMissing :: !Bool
    , soptsUseSystem :: !Bool
    , soptsExpected :: !Version
    , soptsStackYaml :: !(Maybe (Path Abs File))
    -- ^ If we got the desired GHC version from that file
    , soptsForceReinstall :: !Bool
    }
    deriving Show
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

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasBuildConfig env, HasHttpManager env)
         => Bool -- ^ allow system GHC
         -> Bool -- ^ install if missing?
         -> m BuildConfig
setupEnv useSystem installIfMissing = do
    bconfig <- asks getBuildConfig
    let platform = getPlatform bconfig
        sopts = SetupOpts
            { soptsInstallIfMissing = installIfMissing
            , soptsUseSystem = useSystem
            , soptsExpected = bcGhcVersion bconfig
            , soptsStackYaml = Just $ bcStackYaml bconfig
            , soptsForceReinstall = False
            }
    mghcBin <- ensureGHC sopts
    menv0 <- getMinimalEnvOverride

    -- Modify the initial environment to include the GHC path, if a local GHC
    -- is being used
    let env0 = case mghcBin of
            Nothing -> unEnvOverride menv0
            Just ghcBin ->
                let x = unEnvOverride menv0
                    mpath = Map.lookup "PATH" x
                    path = T.intercalate (T.singleton searchPathSeparator)
                        $ map (stripTrailingSlashT . T.pack) ghcBin
                       ++ maybe [] return mpath
                 in Map.insert "PATH" path x

    -- Remove potentially confusing environment variables
        env1 = Map.delete "GHC_PACKAGE_PATH"
             $ Map.delete "HASKELL_PACKAGE_SANDBOX"
             $ Map.delete "HASKELL_PACKAGE_SANDBOXES"
               env0

    -- extra installation bin directories
    mkDirs <- runReaderT extraBinDirs bconfig
    let mpath = Map.lookup "PATH" env1
        depsPath = mkPath (mkDirs False) mpath
        localsPath = mkPath (mkDirs True) mpath

    deps <- runReaderT packageDatabaseDeps bconfig
    depsExists <- liftIO $ doesDirectoryExist $ toFilePath deps
    localdb <- runReaderT packageDatabaseLocal bconfig
    localdbExists <- liftIO $ doesDirectoryExist $ toFilePath localdb
    globalDB <- mkEnvOverride platform env1 >>= getGlobalDB
    let mkGPP locals = T.pack $ intercalate [searchPathSeparator] $ concat
            [ [toFilePath localdb | locals && localdbExists]
            , [toFilePath deps | depsExists]
            , [toFilePath globalDB]
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
                                then intercalate [searchPathSeparator]
                                        [ toFilePath localdb
                                        , toFilePath deps
                                        , ""
                                        ]
                                else intercalate [searchPathSeparator]
                                        [ toFilePath deps
                                        , ""
                                        ])
                        $ env1
                    !() <- atomicModifyIORef envRef $ \m' ->
                        (Map.insert es eo m', ())
                    return eo
    return bconfig { bcConfig = (bcConfig bconfig) { configEnvOverride = getEnvOverride' } }
  where
    mkPath dirs mpath = T.pack $ intercalate [searchPathSeparator]
        (map (stripTrailingSlashS . toFilePath) dirs ++ maybe [] (return . T.unpack) mpath)

    stripTrailingSlashS = T.unpack . stripTrailingSlashT . T.pack
    stripTrailingSlashT t = fromMaybe t $ T.stripSuffix
            (T.singleton FP.pathSeparator)
            t

-- | Ensure GHC is installed and provide the PATHs to add if necessary
ensureGHC :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env)
          => SetupOpts
          -> m (Maybe [FilePath])
ensureGHC sopts = do
    -- Check the available GHCs
    menv0 <- getMinimalEnvOverride

    msystem <-
        if soptsUseSystem sopts
            then getSystemGHC menv0
            else return Nothing

    let needLocal = case msystem of
            Nothing -> True
            Just system ->
                -- we allow a newer version of GHC within the same major series
                getMajorVersion system /= getMajorVersion expected ||
                expected > system

    -- If we need to install a GHC, try to do so
    if needLocal
        then do
            config <- asks getConfig
            let tools =
                    case configPlatform config of
                        Platform _ Windows ->
                            [ ($(mkPackageName "ghc"), Just expected)
                            , ($(mkPackageName "git"), Nothing)
                            ]
                        _ ->
                            [ ($(mkPackageName "ghc"), Just expected)
                            ]

            -- Avoid having to load it twice
            siRef <- liftIO $ newIORef Nothing
            manager <- asks getHttpManager
            let getSetupInfo' = liftIO $ do
                    msi <- readIORef siRef
                    case msi of
                        Just si -> return si
                        Nothing -> do
                            si <- getSetupInfo manager
                            writeIORef siRef $ Just si
                            return si

            installed <- runReaderT listInstalled config
            idents <- mapM (ensureTool sopts installed getSetupInfo' msystem) tools
            paths <- runReaderT (mapM binDirs $ catMaybes idents) config
            -- TODO: strip the trailing slash for prettier PATH output
            return $ Just $ map toFilePath $ concat paths
        else return Nothing
  where
    expected = soptsExpected sopts

-- | Get the major version of the system GHC, if available
getSystemGHC :: (MonadIO m) => EnvOverride -> m (Maybe Version)
getSystemGHC menv = do
    exists <- doesExecutableExist menv "ghc"
    if exists
        then do
            eres <- liftIO $ tryProcessStdout Nothing menv "ghc" ["--numeric-version"]
            return $ do
                Right bs <- Just eres
                parseVersion $ S8.takeWhile isValidChar bs
        else return Nothing
  where
    isValidChar '.' = True
    isValidChar c = '0' <= c && c <= '9'

data DownloadPair = DownloadPair Version Text
    deriving Show
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
    deriving Show
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

markInstalled :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m)
              => PackageIdentifier -- ^ e.g., ghc-7.8.4, git-2.4.0.1
              -> m ()
markInstalled ident = do
    dir <- asks $ configLocalPrograms . getConfig
    fpRel <- parseRelFile $ packageIdentifierString ident ++ ".installed"
    liftIO $ writeFile (toFilePath $ dir </> fpRel) "installed"

unmarkInstalled :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m)
                => PackageIdentifier
                -> m ()
unmarkInstalled ident = do
    dir <- asks $ configLocalPrograms . getConfig
    fpRel <- parseRelFile $ packageIdentifierString ident ++ ".installed"
    removeFileIfExists $ dir </> fpRel

listInstalled :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m)
              => m [PackageIdentifier]
listInstalled = do
    dir <- asks $ configLocalPrograms . getConfig
    liftIO $ createDirectoryIfMissing True $ toFilePath dir
    (_, files) <- listDirectory dir
    return $ mapMaybe toIdent files
  where
    toIdent fp = do
        x <- T.stripSuffix ".installed" $ T.pack $ toFilePath $ filename fp
        parsePackageIdentifierFromString $ T.unpack x

installDir :: (MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m)
           => PackageIdentifier
           -> m (Path Abs Dir)
installDir ident = do
    config <- asks getConfig
    reldir <- parseRelDir $ packageIdentifierString ident
    return $ configLocalPrograms config </> reldir

-- | Binary directories for the given installed package
binDirs :: (MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m)
        => PackageIdentifier
        -> m [Path Abs Dir]
binDirs ident = do
    config <- asks getConfig
    dir <- installDir ident
    case (configPlatform config, packageNameString $ packageIdentifierName ident) of
        (Platform _ Windows, "ghc") -> return
            [ dir </> $(mkRelDir "bin")
            , dir </> $(mkRelDir "mingw") </> $(mkRelDir "bin")
            ]
        (Platform _ Windows, "git") -> return
            [ dir </> $(mkRelDir "cmd")
            , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
            ]
        (_, "ghc") -> return
            [ dir </> $(mkRelDir "bin")
            ]
        (Platform _ x, tool) -> do
            $logWarn $ "binDirs: unexpected OS/tool combo: " <> T.pack (show (x, tool))
            return []

ensureTool :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env)
           => SetupOpts
           -> [PackageIdentifier] -- ^ already installed
           -> m SetupInfo
           -> Maybe Version -- ^ installed GHC
           -> (PackageName, Maybe Version)
           -> m (Maybe PackageIdentifier)
ensureTool sopts installed getSetupInfo' msystem (name, mversion)
    | not $ null available = return $ Just $ PackageIdentifier name $ maximum available
    | not $ soptsInstallIfMissing sopts =
        if name == $(mkPackageName "ghc")
            then throwM $ GHCVersionMismatch msystem (soptsExpected sopts) (soptsStackYaml sopts)
            else do
                $logWarn $ "Continuing despite missing tool: " <> T.pack (packageNameString name)
                return Nothing
    | otherwise = do
        si <- getSetupInfo'
        (pair@(DownloadPair version _), installer) <-
            case packageNameString name of
                "git" -> do
                    let pair = siPortableGit si
                    return (pair, installGitWindows)
                "ghc" -> do
                    osKey <- getOSKey
                    pairs <-
                        case Map.lookup osKey $ siGHCs si of
                            Nothing -> throwM $ UnknownOSKey osKey
                            Just pairs -> return pairs
                    version <-
                        case mversion of
                            Nothing -> error "invariant violated: ghc must have a version"
                            Just version -> return version
                    pair <-
                        case Map.lookup (getMajorVersion version) pairs of
                            Nothing -> throwM $ UnknownGHCVersion version (Map.keysSet pairs)
                            Just pair -> return pair
                    platform <- asks $ configPlatform . getConfig
                    let installer =
                            case platform of
                                Platform _ Windows -> installGHCWindows
                                _ -> installGHCPosix
                    return (pair, installer)
                x -> error $ "Invariant violated: ensureTool on " ++ x
        let ident = PackageIdentifier name version

        (file, at) <- downloadPair pair ident
        dir <- installDir ident
        unmarkInstalled ident
        installer si file at dir ident

        markInstalled ident
        return $ Just ident
  where
    available
        | soptsForceReinstall sopts = []
        | otherwise = filter goodVersion
                    $ map packageIdentifierVersion
                    $ filter (\pi' -> packageIdentifierName pi' == name) installed

    goodVersion =
        case mversion of
            Nothing -> const True
            Just expected -> \actual ->
                getMajorVersion expected == getMajorVersion actual &&
                actual >= expected

getOSKey :: (MonadReader env m, MonadThrow m, HasConfig env) => m Text
getOSKey = do
    platform <- asks $ configPlatform . getConfig
    case platform of
        Platform I386 Linux -> return "linux32"
        Platform X86_64 Linux -> return "linux64"
        Platform I386 OSX -> return "macosx"
        Platform X86_64 OSX -> return "macosx"
        Platform I386 FreeBSD -> return "freebsd32"
        Platform X86_64 FreeBSD -> return "freebsd64"
        Platform I386 Windows -> return "windows32"
        -- Note: we always use 32-bit Windows as the 64-bit version has problems
        Platform X86_64 Windows -> return "windows32"
        Platform arch os -> throwM $ UnsupportedSetupCombo os arch

downloadPair :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env)
             => DownloadPair
             -> PackageIdentifier
             -> m (Path Abs File, ArchiveType)
downloadPair (DownloadPair _ url) ident = do
    config <- asks getConfig
    at <-
        case extension of
            ".tar.xz" -> return TarXz
            ".tar.bz2" -> return TarBz2
            ".7z.exe" -> return SevenZ
            _ -> error $ "Unknown extension: " ++ extension
    relfile <- parseRelFile $ packageIdentifierString ident ++ extension
    let path = configLocalPrograms config </> relfile
    chattyDownload url path
    return (path, at)
  where
    extension =
        loop $ T.unpack url
      where
        loop fp
            | ext `elem` [".tar", ".bz2", ".xz", ".exe", ".7z"] = loop fp' ++ ext
            | otherwise = ""
          where
            (fp', ext) = FP.splitExtension fp

data ArchiveType
    = TarBz2
    | TarXz
    | SevenZ

installGHCPosix :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env)
                => SetupInfo
                -> Path Abs File
                -> ArchiveType
                -> Path Abs Dir
                -> PackageIdentifier
                -> m ()
installGHCPosix _ archiveFile archiveType destDir ident = do
    menv <- getMinimalEnvOverride
    zipTool <-
        case archiveType of
            TarXz -> return "xz"
            TarBz2 -> return "bzip2"
            SevenZ -> error "Don't know how to deal with .7z files on non-Windows"
    checkDependencies $ zipTool : ["make", "tar"]

    withSystemTempDirectory "stack-setup" $ \root' -> do
        root <- parseAbsDir root'
        dir <- liftM (root Path.</>) $ parseRelDir $ packageIdentifierString ident

        $logInfo $ "Unpacking " <> T.pack (toFilePath archiveFile)
        runIn root "tar" menv ["xf", toFilePath archiveFile] Nothing

        $logInfo "Configuring"
        runIn dir (toFilePath $ dir Path.</> $(mkRelFile "configure"))
              menv ["--prefix=" ++ toFilePath destDir] Nothing

        $logInfo "Installing"
        runIn dir "make" menv ["install"] Nothing

        $logInfo $ "GHC installed to " <> T.pack (toFilePath destDir)
  where
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

installGHCWindows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env)
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> PackageIdentifier
                  -> m ()
installGHCWindows si archiveFile archiveType destDir _ = do
    case archiveType of
        TarXz -> return ()
        _ -> error $ "GHC on Windows must be a .tar.xz file"
    tarFile <-
        case T.stripSuffix ".xz" $ T.pack $ toFilePath archiveFile of
            Nothing -> error $ "Invalid GHC filename: " ++ show archiveFile
            Just x -> parseAbsFile $ T.unpack x

    config <- asks getConfig
    run7z <- setup7z si config

    run7z (parent archiveFile) archiveFile
    run7z (parent archiveFile) tarFile

    $logInfo $ "GHC installed to " <> T.pack (toFilePath destDir)

installGitWindows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env)
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> PackageIdentifier
                  -> m ()
installGitWindows si archiveFile archiveType destDir _ = do
    case archiveType of
        SevenZ -> return ()
        _ -> error $ "Git on Windows must be a 7z archive"

    config <- asks getConfig
    run7z <- setup7z si config
    run7z destDir archiveFile

-- | Download 7z as necessary, and get a function for unpacking things.
--
-- Returned function takes an unpack directory and archive.
setup7z :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadIO m, MonadIO n, MonadLogger m)
        => SetupInfo
        -> Config
        -> m (Path Abs Dir -> Path Abs File -> n ())
setup7z si config = do
    chattyDownload (siSevenzDll si) dll
    chattyDownload (siSevenzExe si) exe
    return $ \outdir archive -> liftIO $ do
        ec <- rawSystem (toFilePath exe)
            [ "x"
            , "-o" ++ toFilePath outdir
            , "-y"
            , toFilePath archive
            ]
        when (ec /= ExitSuccess)
            $ error $ "Problem while decompressing " ++ toFilePath archive
  where
    dir = configLocalPrograms config </> $(mkRelDir "7z")
    exe = dir </> $(mkRelFile "7z.exe")
    dll = dir </> $(mkRelFile "7z.dll")

chattyDownload :: (MonadReader env m, HasHttpManager env, MonadIO m, MonadLogger m, MonadThrow m)
               => Text -- ^ URL
               -> Path Abs File -- ^ destination
               -> m ()
chattyDownload url path = do
    req <- parseUrl $ T.unpack url
    $logInfo $ T.concat
        [ "Downloading from "
        , url
        , " to "
        , T.pack $ toFilePath path
        ]
    x <- download req path -- TODO add progress indicator
    if x
        then $logInfo "Download complete"
        else $logInfo "File already downloaded"
