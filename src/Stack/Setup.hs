{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
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

import           Control.Applicative
import           Control.Exception.Enclosed (catchIO)
import           Control.Monad (liftM, when, join, void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import           Control.Monad.State (get, put, modify)
import           Control.Monad.Trans.Control
import           Data.Aeson.Extended
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit (Conduit, ($$), (=$), await, yield, awaitForever)
import           Data.Conduit.Lift (evalStateC)
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.IORef
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Data.Typeable (Typeable)
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (..), Arch (..), Platform (..))
import           Distribution.Text (simpleParse)
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Download (verifiedDownload, DownloadRequest(..), drRetriesDefault)
import           Path
import           Path.IO
import           Prelude -- Fix AMP warning
import           Safe (headMay, readMay)
import           Stack.Build.Types
import           Stack.Constants (distRelativeDir)
import           Stack.GhcPkg (createDatabase, getCabalPkgVer, getGlobalDB)
import           Stack.Solver (getGhcVersion)
import           Stack.Types
import           Stack.Types.StackT
import           System.Environment (getExecutablePath)
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath (searchPathSeparator)
import qualified System.FilePath as FP
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (rawSystem)
import           System.Process.Read
import           Text.Printf (printf)

data SetupOpts = SetupOpts
    { soptsInstallIfMissing :: !Bool
    , soptsUseSystem :: !Bool
    , soptsExpected :: !Version
    , soptsStackYaml :: !(Maybe (Path Abs File))
    -- ^ If we got the desired GHC version from that file
    , soptsForceReinstall :: !Bool
    , soptsSanityCheck :: !Bool
    -- ^ Run a sanity check on the selected GHC
    , soptsSkipGhcCheck :: !Bool
    -- ^ Don't check for a compatible GHC version/architecture
    , soptsSkipMsys :: !Bool
    -- ^ Do not use a custom msys installation on Windows
    }
    deriving Show
data SetupException = UnsupportedSetupCombo OS Arch
                    | MissingDependencies [String]
                    | UnknownGHCVersion Text Version (Set MajorVersion)
                    | UnknownOSKey Text
                    | GHCSanityCheckCompileFailed ReadProcessException (Path Abs File)
    deriving Typeable
instance Exception SetupException
instance Show SetupException where
    show (UnsupportedSetupCombo os arch) = concat
        [ "I don't know how to install GHC for "
        , show (os, arch)
        , ", please install manually"
        ]
    show (MissingDependencies tools) =
        "The following executables are missing and must be installed: " ++
        intercalate ", " tools
    show (UnknownGHCVersion oskey version known) = concat
        [ "No information found for GHC version "
        , versionString version
        , ".\nSupported GHC major versions for OS key '" ++ T.unpack oskey ++ "': "
        , intercalate ", " (map show $ Set.toList known)
        ]
    show (UnknownOSKey oskey) =
        "Unable to find installation URLs for OS key: " ++
        T.unpack oskey
    show (GHCSanityCheckCompileFailed e ghc) = concat
        [ "The GHC located at "
        , toFilePath ghc
        , " failed to compile a sanity check. Please see:\n\n"
        , "    https://github.com/commercialhaskell/stack/wiki/Downloads\n\n"
        , "for more information. Exception was:\n"
        , show e
        ]

-- | Modify the environment variables (like PATH) appropriately, possibly doing installation too
setupEnv :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasBuildConfig env, HasHttpManager env, MonadBaseControl IO m)
         => m EnvConfig
setupEnv = do
    bconfig <- asks getBuildConfig
    let platform = getPlatform bconfig
        sopts = SetupOpts
            { soptsInstallIfMissing = configInstallGHC $ bcConfig bconfig
            , soptsUseSystem = configSystemGHC $ bcConfig bconfig
            , soptsExpected = bcGhcVersionExpected bconfig
            , soptsStackYaml = Just $ bcStackYaml bconfig
            , soptsForceReinstall = False
            , soptsSanityCheck = False
            , soptsSkipGhcCheck = configSkipGHCCheck $ bcConfig bconfig
            , soptsSkipMsys = configSkipMsys $ bcConfig bconfig
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
             $ Map.delete "HASKELL_DIST_DIR"
               env0

    menv1 <- mkEnvOverride platform env1
    ghcVer <- getGhcVersion menv1
    cabalVer <- getCabalPkgVer menv1
    let envConfig0 = EnvConfig
            { envConfigBuildConfig = bconfig
            , envConfigCabalVersion = cabalVer
            , envConfigGhcVersion = ghcVer
            }

    -- extra installation bin directories
    mkDirs <- runReaderT extraBinDirs envConfig0
    let mpath = Map.lookup "PATH" env1
        mkDirs' = map toFilePath . mkDirs
        depsPath = augmentPath (mkDirs' False) mpath
        localsPath = augmentPath (mkDirs' True) mpath

    deps <- runReaderT packageDatabaseDeps envConfig0
    createDatabase menv1 deps
    localdb <- runReaderT packageDatabaseLocal envConfig0
    createDatabase menv1 localdb
    globalDB <- mkEnvOverride platform env1 >>= getGlobalDB
    let mkGPP locals = T.pack $ intercalate [searchPathSeparator] $ concat
            [ [toFilePathNoTrailingSlash localdb | locals]
            , [toFilePathNoTrailingSlash deps]
            , [toFilePathNoTrailingSlash globalDB]
            ]

    distDir <- runReaderT distRelativeDir envConfig0

    executablePath <- liftIO getExecutablePath

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

                        $ (if esStackExe es
                                then Map.insert "STACK_EXE" (T.pack executablePath)
                                else id)

                        -- For reasoning and duplication, see: https://github.com/fpco/stack/issues/70
                        $ Map.insert "HASKELL_PACKAGE_SANDBOX" (T.pack $ toFilePathNoTrailingSlash deps)
                        $ Map.insert "HASKELL_PACKAGE_SANDBOXES"
                            (T.pack $ if esIncludeLocals es
                                then intercalate [searchPathSeparator]
                                        [ toFilePathNoTrailingSlash localdb
                                        , toFilePathNoTrailingSlash deps
                                        , ""
                                        ]
                                else intercalate [searchPathSeparator]
                                        [ toFilePathNoTrailingSlash deps
                                        , ""
                                        ])
                        $ Map.insert "HASKELL_DIST_DIR" (T.pack $ toFilePathNoTrailingSlash distDir)
                        $ env1
                    !() <- atomicModifyIORef envRef $ \m' ->
                        (Map.insert es eo m', ())
                    return eo

    return EnvConfig
        { envConfigBuildConfig = bconfig
            { bcConfig = (bcConfig bconfig) { configEnvOverride = getEnvOverride' }
            }
        , envConfigCabalVersion = cabalVer
        , envConfigGhcVersion = ghcVer
        }

-- | Augment the PATH environment variable with the given extra paths
augmentPath :: [FilePath] -> Maybe Text -> Text
augmentPath dirs mpath =
    T.pack $ intercalate [searchPathSeparator]
        (map stripTrailingSlashS dirs ++ maybe [] (return . T.unpack) mpath)
  where
    stripTrailingSlashS = T.unpack . stripTrailingSlashT . T.pack

stripTrailingSlashT :: Text -> Text
stripTrailingSlashT t = fromMaybe t $ T.stripSuffix
        (T.singleton FP.pathSeparator)
        t

-- | Ensure GHC is installed and provide the PATHs to add if necessary
ensureGHC :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
          => SetupOpts
          -> m (Maybe [FilePath])
ensureGHC sopts = do
    -- Check the available GHCs
    menv0 <- getMinimalEnvOverride

    msystem <-
        if soptsUseSystem sopts
            then getSystemGHC menv0
            else return Nothing

    Platform expectedArch _ <- asks getPlatform

    let needLocal = case msystem of
            Nothing -> True
            Just _ | soptsSkipGhcCheck sopts -> False
            Just (system, arch) ->
                -- we allow a newer version of GHC within the same major series
                getMajorVersion system /= getMajorVersion expected ||
                expected > system ||
                arch /= expectedArch

    -- If we need to install a GHC, try to do so
    mpaths <- if needLocal
        then do
            config <- asks getConfig
            let tools =
                    case configPlatform config of
                        Platform _ os | isWindows os ->
                              ($(mkPackageName "ghc"), Just expected)
                            : (if soptsSkipMsys sopts
                                then []
                                else [($(mkPackageName "git"), Nothing)])
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
            idents <- mapM (ensureTool menv0 sopts installed getSetupInfo' msystem) tools
            paths <- runReaderT (mapM binDirs $ catMaybes idents) config
            return $ Just $ map toFilePathNoTrailingSlash $ concat paths
        else return Nothing

    when (soptsSanityCheck sopts) $ do
        menv <-
            case mpaths of
                Nothing -> return menv0
                Just paths -> do
                    config <- asks getConfig
                    let m0 = unEnvOverride menv0
                        path0 = Map.lookup "PATH" m0
                        path = augmentPath paths path0
                        m = Map.insert "PATH" path m0
                    mkEnvOverride (configPlatform config) m
        sanityCheck menv

    return mpaths
  where
    expected = soptsExpected sopts

-- | Get the major version of the system GHC, if available
getSystemGHC :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => EnvOverride -> m (Maybe (Version, Arch))
getSystemGHC menv = do
    exists <- doesExecutableExist menv "ghc"
    if exists
        then do
            eres <- tryProcessStdout Nothing menv "ghc" ["--info"]
            return $ do
                Right bs <- Just eres
                pairs <- readMay $ S8.unpack bs :: Maybe [(String, String)]
                version <- lookup "Project version" pairs >>= parseVersionFromString
                arch <- lookup "Target platform" pairs >>= simpleParse . takeWhile (/= '-')
                Just (version, arch)
        else return Nothing

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
    createTree dir
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
        (Platform _ (isWindows -> True), "ghc") -> return
            [ dir </> $(mkRelDir "bin")
            , dir </> $(mkRelDir "mingw") </> $(mkRelDir "bin")
            ]
        (Platform _ (isWindows -> True), "git") -> return
            [ dir </> $(mkRelDir "cmd")
            , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
            ]
        (_, "ghc") -> return
            [ dir </> $(mkRelDir "bin")
            ]
        (Platform _ x, tool) -> do
            $logWarn $ "binDirs: unexpected OS/tool combo: " <> T.pack (show (x, tool))
            return []

ensureTool :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
           => EnvOverride
           -> SetupOpts
           -> [PackageIdentifier] -- ^ already installed
           -> m SetupInfo
           -> Maybe (Version, Arch) -- ^ installed GHC
           -> (PackageName, Maybe Version)
           -> m (Maybe PackageIdentifier)
ensureTool menv sopts installed getSetupInfo' msystem (name, mversion)
    | not $ null available = return $ Just $ PackageIdentifier name $ maximum available
    | not $ soptsInstallIfMissing sopts =
        if name == $(mkPackageName "ghc")
            then do
                Platform arch _ <- asks getPlatform
                throwM $ GHCVersionMismatch msystem (soptsExpected sopts, arch) (soptsStackYaml sopts)
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
                    osKey <- getOSKey menv
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
                            Nothing -> throwM $ UnknownGHCVersion osKey version (Map.keysSet pairs)
                            Just pair -> return pair
                    platform <- asks $ configPlatform . getConfig
                    let installer =
                            case platform of
                                Platform _ os | isWindows os -> installGHCWindows
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

getOSKey :: (MonadReader env m, MonadThrow m, HasConfig env, MonadLogger m, MonadIO m, MonadCatch m, MonadBaseControl IO m)
         => EnvOverride -> m Text
getOSKey menv = do
    platform <- asks $ configPlatform . getConfig
    case platform of
        Platform I386 Linux -> ("linux32" <>) <$> getLinuxSuffix
        Platform X86_64 Linux -> ("linux64" <>) <$> getLinuxSuffix
        Platform I386 OSX -> return "macosx"
        Platform X86_64 OSX -> return "macosx"
        Platform I386 FreeBSD -> return "freebsd32"
        Platform X86_64 FreeBSD -> return "freebsd64"
        Platform I386 OpenBSD -> return "openbsd32"
        Platform X86_64 OpenBSD -> return "openbsd64"
        Platform I386 Windows -> return "windows32"
        Platform X86_64 Windows -> return "windows64"

        Platform I386 (OtherOS "windowsintegersimple") -> return "windowsintegersimple32"
        Platform X86_64 (OtherOS "windowsintegersimple") -> return "windowsintegersimple64"

        Platform arch os -> throwM $ UnsupportedSetupCombo os arch
  where
    getLinuxSuffix = do
        executablePath <- liftIO getExecutablePath
        elddOut <- tryProcessStdout Nothing menv "ldd" [executablePath]
        return $ case elddOut of
            Left _ -> ""
            Right lddOut -> if hasLineWithFirstWord "libgmp.so.3" lddOut then "-gmp4" else ""
    hasLineWithFirstWord w =
      elem (Just w) . map (headMay . T.words) . T.lines . T.decodeUtf8With T.lenientDecode

downloadPair :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
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
    chattyDownload (packageIdentifierText ident) url path
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

installGHCPosix :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                => SetupInfo
                -> Path Abs File
                -> ArchiveType
                -> Path Abs Dir
                -> PackageIdentifier
                -> m ()
installGHCPosix _ archiveFile archiveType destDir ident = do
    menv <- getMinimalEnvOverride
    zipTool' <-
        case archiveType of
            TarXz -> return "xz"
            TarBz2 -> return "bzip2"
            SevenZ -> error "Don't know how to deal with .7z files on non-Windows"
    (zipTool, makeTool, tarTool) <- checkDependencies $ (,,)
        <$> checkDependency zipTool'
        <*> (checkDependency "gmake" <|> checkDependency "make")
        <*> checkDependency "tar"

    $logDebug $ "ziptool: " <> T.pack zipTool
    $logDebug $ "make: " <> T.pack makeTool
    $logDebug $ "tar: " <> T.pack tarTool

    withSystemTempDirectory "stack-setup" $ \root' -> do
        root <- parseAbsDir root'
        dir <- liftM (root Path.</>) $ parseRelDir $ packageIdentifierString ident

        $logSticky $ "Unpacking GHC ..."
        $logDebug $ "Unpacking " <> T.pack (toFilePath archiveFile)
        readInNull root tarTool menv ["xf", toFilePath archiveFile] Nothing

        $logSticky "Configuring GHC ..."
        readInNull dir (toFilePath $ dir Path.</> $(mkRelFile "configure"))
                   menv ["--prefix=" ++ toFilePath destDir] Nothing

        $logSticky "Installing GHC ..."
        readInNull dir makeTool menv ["install"] Nothing

        $logStickyDone $ "Installed GHC."
        $logDebug $ "GHC installed to " <> T.pack (toFilePath destDir)
  where
    -- | Check if given processes appear to be present, throwing an exception if
    -- missing.
    checkDependencies :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
                      => CheckDependency a -> m a
    checkDependencies (CheckDependency f) = do
        menv <- getMinimalEnvOverride
        liftIO (f menv) >>= either (throwM . MissingDependencies) return

checkDependency :: String -> CheckDependency String
checkDependency tool = CheckDependency $ \menv -> do
    exists <- doesExecutableExist menv tool
    return $ if exists then Right tool else Left [tool]

newtype CheckDependency a = CheckDependency (EnvOverride -> IO (Either [String] a))
    deriving Functor
instance Applicative CheckDependency where
    pure x = CheckDependency $ \_ -> return (Right x)
    CheckDependency f <*> CheckDependency x = CheckDependency $ \menv -> do
        f' <- f menv
        x' <- x menv
        return $
            case (f', x') of
                (Left e1, Left e2) -> Left $ e1 ++ e2
                (Left e, Right _) -> Left e
                (Right _, Left e) -> Left e
                (Right f'', Right x'') -> Right $ f'' x''
instance Alternative CheckDependency where
    empty = CheckDependency $ \_ -> return $ Left []
    CheckDependency x <|> CheckDependency y = CheckDependency $ \menv -> do
        res1 <- x menv
        case res1 of
            Left _ -> y menv
            Right x' -> return $ Right x'

installGHCWindows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
                  => SetupInfo
                  -> Path Abs File
                  -> ArchiveType
                  -> Path Abs Dir
                  -> PackageIdentifier
                  -> m ()
installGHCWindows si archiveFile archiveType destDir _ = do
    suffix <-
        case archiveType of
            TarXz -> return ".xz"
            TarBz2 -> return ".bz2"
            _ -> error $ "GHC on Windows must be a tarball file"
    tarFile <-
        case T.stripSuffix suffix $ T.pack $ toFilePath archiveFile of
            Nothing -> error $ "Invalid GHC filename: " ++ show archiveFile
            Just x -> parseAbsFile $ T.unpack x

    config <- asks getConfig
    run7z <- setup7z si config

    run7z (parent archiveFile) archiveFile
    run7z (parent archiveFile) tarFile
    removeFile tarFile `catchIO` \e ->
        $logWarn (T.concat
            [ "Exception when removing "
            , T.pack $ toFilePath tarFile
            , ": "
            , T.pack $ show e
            ])

    $logInfo $ "GHC installed to " <> T.pack (toFilePath destDir)

installGitWindows :: (MonadIO m, MonadMask m, MonadLogger m, MonadReader env m, HasConfig env, HasHttpManager env, MonadBaseControl IO m)
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
setup7z :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadIO m, MonadIO n, MonadLogger m, MonadBaseControl IO m)
        => SetupInfo
        -> Config
        -> m (Path Abs Dir -> Path Abs File -> n ())
setup7z si config = do
    chattyDownload "7z.dll" (siSevenzDll si) dll
    chattyDownload "7z.exe" (siSevenzExe si) exe
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

chattyDownload :: (MonadReader env m, HasHttpManager env, MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
               => Text
               -> Text -- ^ URL
               -> Path Abs File -- ^ destination
               -> m ()
chattyDownload label url path = do
    req <- parseUrl $ T.unpack url
    $logSticky $ T.concat
      [ "Preparing to download "
      , label
      , " ..."
      ]
    $logDebug $ T.concat
      [ "Downloading from "
      , url
      , " to "
      , T.pack $ toFilePath path
      , " ..."
      ]

    let dReq = DownloadRequest
            { drRequest = req
            , drHashChecks = []
            , drLengthCheck = Nothing
            , drRetries = drRetriesDefault
            }
    runInBase <- liftBaseWith $ \run -> return (void . run)
    x <- verifiedDownload dReq path (chattyDownloadProgress runInBase)
    if x
        then $logStickyDone ("Downloaded " <> label <> ".")
        else $logStickyDone "Already downloaded."
  where
    chattyDownloadProgress runInBase mcontentLength = do
        _ <- liftIO $ runInBase $ $logSticky $
          label <> ": download has begun"
        CL.map (Sum . S.length)
          =$ chunksOverTime 1
          =$ go
      where
        go = evalStateC 0 $ awaitForever $ \(Sum size) -> do
            modify (+ size)
            totalSoFar <- get
            liftIO $ runInBase $ $logSticky $ T.pack $
              case mcontentLength of
                Nothing -> chattyProgressNoTotal totalSoFar
                Just 0 -> chattyProgressNoTotal totalSoFar
                Just total -> chattyProgressWithTotal totalSoFar total
        -- Example: ghc: 42.13 KiB downloaded...
        chattyProgressNoTotal totalSoFar =
            printf ("%s: " <> bytesfmt "%7.2f" totalSoFar <> " downloaded...")
                   (T.unpack label)
        -- Example: ghc: 50.00 MiB / 100.00 MiB (50.00%) downloaded...
        chattyProgressWithTotal totalSoFar total =
          printf ("%s: " <>
                  bytesfmt "%7.2f" totalSoFar <> " / " <>
                  bytesfmt "%.2f" total <>
                  " (%6.2f%%) downloaded...")
                 (T.unpack label)
                 percentage
          where percentage :: Double
                percentage = (fromIntegral totalSoFar / fromIntegral total * 100)

-- | Given a printf format string for the decimal part and a number of
-- bytes, formats the bytes using an appropiate unit and returns the
-- formatted string.
--
-- >>> bytesfmt "%.2" 512368
-- "500.359375 KiB"
bytesfmt :: Integral a => String -> a -> String
bytesfmt formatter bs = printf (formatter <> " %s")
                               (fromIntegral (signum bs) * dec :: Double)
                               (bytesSuffixes !! i)
  where
    (dec,i) = getSuffix (abs bs)
    getSuffix n = until p (\(x,y) -> (x / 1024, y+1)) (fromIntegral n,0)
      where p (n',numDivs) = n' < 1024 || numDivs == (length bytesSuffixes - 1)
    bytesSuffixes :: [String]
    bytesSuffixes = ["B","KiB","MiB","GiB","TiB","PiB","EiB","ZiB","YiB"]

-- Await eagerly (collect with monoidal append),
-- but space out yields by at least the given amount of time.
-- The final yield may come sooner, and may be a superfluous mempty.
-- Note that Integer and Float literals can be turned into NominalDiffTime
-- (these literals are interpreted as "seconds")
chunksOverTime :: (Monoid a, MonadIO m) => NominalDiffTime -> Conduit a m a
chunksOverTime diff = do
    currentTime <- liftIO getCurrentTime
    evalStateC (currentTime, mempty) go
  where
    -- State is a tuple of:
    -- * the last time a yield happened (or the beginning of the sink)
    -- * the accumulated awaits since the last yield
    go = await >>= \case
      Nothing -> do
        (_, acc) <- get
        yield acc
      Just a -> do
        (lastTime, acc) <- get
        let acc' = acc <> a
        currentTime <- liftIO getCurrentTime
        if diff < diffUTCTime currentTime lastTime
          then put (currentTime, mempty) >> yield acc'
          else put (lastTime,    acc')
        go


-- | Perform a basic sanity check of GHC
sanityCheck :: (MonadIO m, MonadMask m, MonadLogger m, MonadBaseControl IO m)
            => EnvOverride
            -> m ()
sanityCheck menv = withSystemTempDirectory "stack-sanity-check" $ \dir -> do
    dir' <- parseAbsDir dir
    let fp = toFilePath $ dir' </> $(mkRelFile "Main.hs")
    liftIO $ writeFile fp $ unlines
        [ "import Distribution.Simple" -- ensure Cabal library is present
        , "main = putStrLn \"Hello World\""
        ]
    ghc <- join $ findExecutable menv "ghc"
    $logDebug $ "Performing a sanity check on: " <> T.pack (toFilePath ghc)
    eres <- tryProcessStdout (Just dir') menv "ghc"
        [ fp
        , "-no-user-package-db"
        ]
    case eres of
        Left e -> throwM $ GHCSanityCheckCompileFailed e ghc
        Right _ -> return () -- TODO check that the output of running the command is correct

toFilePathNoTrailingSlash :: Path loc Dir -> FilePath
toFilePathNoTrailingSlash = FP.dropTrailingPathSeparator . toFilePath
