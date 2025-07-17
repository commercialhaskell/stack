{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NoMonoLocalBinds      #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Path
Description : Types and functions related to Stack's @path@ command.
License     : BSD-3-Clause

Types and functions related to Stack's @path@ command.
-}

module Stack.Path
  ( EnvConfigPathInfo
  , UseHaddocks
  , path
  , pathsFromRunner
  , pathsFromConfig
  , pathsFromEnvConfig
  ) where

import           Control.Exception ( throw )
import           Data.List ( intercalate )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Path ( (</>), parent )
import           Path.Extra ( toFilePathNoTrailingSep )
import           RIO.Process ( HasProcessContext (..), exeSearchPathL )
import           Stack.Config ( determineStackRootAndOwnership )
import           Stack.Constants
                   ( docDirSuffix, stackGlobalConfigOptionName
                   , stackRootOptionName
                   )
import           Stack.Constants.Config ( distRelativeDir )
import           Stack.GhcPkg as GhcPkg
import           Stack.Prelude hiding ( pi )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), configFileL )
import           Stack.Types.BuildOptsMonoid ( buildOptsMonoidHaddockL )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..), getCompilerPath )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), userGlobalConfigFileL )
import           Stack.Types.Config.Exception ( ConfigPrettyException (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig, HasEnvConfig (..), bindirCompilerTools
                   , hpcReportDir, installationRootDeps, installationRootLocal
                   , packageDatabaseDeps, packageDatabaseExtra
                   , packageDatabaseLocal
                   )
import qualified Stack.Types.EnvConfig as EnvConfig
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.GlobalOpts
                   ( GlobalOpts (..), globalOptsBuildOptsMonoidL )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner (..), Runner, globalOptsL )
import qualified System.FilePath as FP

-- | Print out useful path information in a human-readable format (and support
-- others later).
path :: [Text] -> RIO Runner ()
path keys = do
  let -- filter the chosen paths in flags (keys), or show all of them if no
      -- specific paths chosen.
      filterKeys (_, key, _) = null keys || elem key keys
      goodPathsFromRunner = null keys || elem stackRootOptionName' keys
      goodPathsFromConfig = filter filterKeys pathsFromConfig
      goodPathsFromEnvConfig = filter filterKeys pathsFromEnvConfig
      toKeyPath (_, key, p) = (key, p)
      goodPathsFromConfig' = map toKeyPath goodPathsFromConfig
      singlePath = (if goodPathsFromRunner then 1 else 0) +
        length goodPathsFromConfig + length goodPathsFromEnvConfig == 1
      toEither (_, k, UseHaddocks a) = Left (k, a)
      toEither (_, k, WithoutHaddocks a) = Right (k, a)
      (with, without) = partitionEithers $ map toEither goodPathsFromEnvConfig
  when goodPathsFromRunner $ printKeysWithRunner singlePath
  unless (null goodPathsFromConfig') $
    runHaddockWithConfig $ printKeysWithConfig goodPathsFromConfig' singlePath
  unless (null without) $
    runHaddockWithEnvConfig False $ printKeysWithEnvConfig without singlePath
  unless (null with) $
    runHaddockWithEnvConfig True $ printKeysWithEnvConfig with singlePath

printKeysWithRunner ::
     Bool
  -> RIO Runner ()
printKeysWithRunner single = do
  clArgs <- view $ globalOptsL . to (.configMonoid)
  liftIO $ do
    (_, stackRoot, _) <- determineStackRootAndOwnership clArgs
    let prefix = if single then "" else stackRootOptionName' <> ": "
    T.putStrLn $ prefix <> T.pack (toFilePathNoTrailingSep stackRoot)

printKeysWithConfig ::
     HasConfig env
  => [(Text, Config -> Text)]
  -> Bool
  -> RIO env ()
printKeysWithConfig extractors single =
  view configL >>= printKeys extractors single

printKeysWithEnvConfig ::
     HasEnvConfig env
  => [(Text, EnvConfigPathInfo -> Text)]
  -> Bool
  -> RIO env ()
printKeysWithEnvConfig extractors single =
  fillEnvConfigPathInfo >>= printKeys extractors single

printKeys ::
     [(Text, info -> Text)]
  -> Bool
  -> info
  -> RIO env ()
printKeys extractors single info = do
  liftIO $ forM_ extractors $ \(key, extractPath) -> do
    let prefix = if single then "" else key <> ": "
    T.putStrLn $ prefix <> extractPath info

runHaddockWithEnvConfig :: Bool -> RIO EnvConfig () -> RIO Runner ()
runHaddockWithEnvConfig x action = runHaddock x (withDefaultEnvConfig action)

runHaddockWithConfig :: RIO Config () -> RIO Runner ()
runHaddockWithConfig = runHaddock False

runHaddock :: Bool -> RIO Config () -> RIO Runner ()
runHaddock x action = local modifyConfig $ withConfig YesReexec action
 where
  modifyConfig = set
    (globalOptsL . globalOptsBuildOptsMonoidL . buildOptsMonoidHaddockL)
    (Just x)

fillEnvConfigPathInfo :: HasEnvConfig env => RIO env EnvConfigPathInfo
fillEnvConfigPathInfo = do
  -- We must use a BuildConfig from an EnvConfig to ensure that it contains the
  -- full environment info including GHC paths etc.
  buildConfig <- view $ envConfigL . buildConfigL
  -- This is the modified 'bin-path',
  -- including the local GHC or MSYS if not configured to operate on
  -- global GHC.
  -- It was set up in 'withBuildConfigAndLock -> withBuildConfigExt -> setupEnv'.
  -- So it's not the *minimal* override path.
  snapDb <- packageDatabaseDeps
  localDb <- packageDatabaseLocal
  extraDbs <- packageDatabaseExtra
  globalDb <- view $ compilerPathsL . to (.globalDB)
  snapRoot <- installationRootDeps
  localRoot <- installationRootLocal
  toolsDir <- bindirCompilerTools
  hoogleRoot <- EnvConfig.hoogleRoot
  distDir <- distRelativeDir
  hpcDir <- hpcReportDir
  compiler <- getCompilerPath
  pure EnvConfigPathInfo
    { buildConfig
    , snapDb
    , localDb
    , globalDb
    , snapRoot
    , localRoot
    , toolsDir
    , hoogleRoot
    , distDir
    , hpcDir
    , extraDbs
    , compiler
    }

-- | Type representing information needed to generate an appropriate string for
-- paths of interest to a user which require an 'EnvConfig'.
data EnvConfigPathInfo = EnvConfigPathInfo
  { buildConfig  :: !BuildConfig
  , snapDb       :: !(Path Abs Dir)
  , localDb      :: !(Path Abs Dir)
  , globalDb     :: !(Path Abs Dir)
  , snapRoot     :: !(Path Abs Dir)
  , localRoot    :: !(Path Abs Dir)
  , toolsDir     :: !(Path Abs Dir)
  , hoogleRoot   :: !(Path Abs Dir)
  , distDir      :: Path Rel Dir
  , hpcDir       :: !(Path Abs Dir)
  , extraDbs     :: ![Path Abs Dir]
  , compiler     :: !(Path Abs File)
  }

instance HasPlatform EnvConfigPathInfo where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasLogFunc EnvConfigPathInfo where
  logFuncL = configL . logFuncL

instance HasRunner EnvConfigPathInfo where
  runnerL = configL . runnerL

instance HasStylesUpdate EnvConfigPathInfo where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasTerm EnvConfigPathInfo where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

instance HasGHCVariant EnvConfigPathInfo where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasConfig EnvConfigPathInfo where
  configL = buildConfigL . lens (.config) (\x y -> x { config = y })
  {-# INLINE configL #-}

instance HasPantryConfig EnvConfigPathInfo where
  pantryConfigL = configL . pantryConfigL

instance HasProcessContext EnvConfigPathInfo where
  processContextL = configL . processContextL

instance HasBuildConfig EnvConfigPathInfo where
  buildConfigL =
    lens (.buildConfig) (\x y -> x { buildConfig = y }) . buildConfigL

-- | Type representing whether or not building Haddocks is required.
data UseHaddocks a
  = UseHaddocks a
    -- ^ Building Haddocks is required.
  | WithoutHaddocks a
    -- ^ Building Haddocks is not required.

-- | The paths of interest to a user which do require a t'Config' or
-- 'EnvConfig'. The first tuple string is used for a description that the
-- optparse flag uses, and the second string as a machine-readable key and also
-- for @--foo@ flags. The user can choose a specific path to list like
-- @--stack-root@. But really it's mainly for the documentation aspect.
pathsFromRunner :: (String, Text)
pathsFromRunner = ("Global Stack root directory", stackRootOptionName')

-- | The paths of interest to a user which do require an 'EnvConfig'. The first
-- tuple string is used for a description that the optparse flag uses, and the
-- second string as a machine-readable key and also for @--foo@ flags. The user
-- can choose a specific path to list like @--stack-root@. But really it's
-- mainly for the documentation aspect.
--
-- When printing output we generate t'Config' and pass it to the function
-- to generate an appropriate string. Trailing slashes are removed, see #506.
pathsFromConfig :: [(String, Text, Config -> Text)]
pathsFromConfig =
  [ ( "User-specific global configuration file"
    , T.pack stackGlobalConfigOptionName
    , view (userGlobalConfigFileL . to toFilePath . to T.pack)
    )
  , ( "Install location for GHC and other core tools (see 'stack ls tools' command)"
    , "programs"
    , view (configL . to (.localPrograms) . to toFilePathNoTrailingSep . to T.pack)
    )
  , ( "Directory where Stack installs executables (e.g. ~/.local/bin (Unix-like OSs) or %APPDATA%\\local\\bin (Windows))"
    , "local-bin"
    , view $ configL . to (.localBin) . to toFilePathNoTrailingSep . to T.pack
    )
  ]

-- | The paths of interest to a user which require a 'EnvConfig'. The first
-- tuple string is used for a description that the optparse flag uses, and the
-- second string as a machine-readable key and also for @--foo@ flags. The user
-- can choose a specific path to list like @--project-root@. But really it's
-- mainly for the documentation aspect.
--
-- When printing output we generate t'EnvConfigPathInfo' and pass it to the
-- function to generate an appropriate string. Trailing slashes are removed, see
-- #506.
pathsFromEnvConfig :: [(String, Text, UseHaddocks (EnvConfigPathInfo -> Text))]
pathsFromEnvConfig =
  [ ( "Project root (derived from the project-level configuration file; \
      \stack.yaml, by default)"
    , "project-root"
    , WithoutHaddocks $ view (configFileL . to toProjectConfigFileRootPath)
    )
  , ( "Project-level configuration file (stack.yaml, by default)"
    , "config-location"
    , WithoutHaddocks $ view (configFileL . to toProjectConfigFilePath)
    )
  , ( "PATH environment variable"
    , "bin-path"
    , WithoutHaddocks $
        T.pack . intercalate [FP.searchPathSeparator] . view exeSearchPathL
    )
  , ( "Compiler binary (e.g. ghc)"
    , "compiler-exe"
    , WithoutHaddocks $ T.pack . toFilePath . (.compiler)
    )
  , ( "Directory containing the compiler binary (e.g. ghc)"
    , "compiler-bin"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . parent . (.compiler)
    )
  , ( "Directory containing binaries specific to a particular compiler"
    , "compiler-tools-bin"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.toolsDir)
    )
  , ( "Extra include directories"
    , "extra-include-dirs"
    , WithoutHaddocks $
        T.intercalate ", " . map T.pack . (.extraIncludeDirs) . view configL
    )
  , ( "Extra library directories"
    , "extra-library-dirs"
    , WithoutHaddocks $
        T.intercalate ", " . map T.pack . (.extraLibDirs) . view configL
    )
  , ( "Snapshot package database"
    , "snapshot-pkg-db"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.snapDb)
    )
  , ( "Local project package database"
    , "local-pkg-db"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.localDb)
    )
  , ( "Global package database"
    , "global-pkg-db"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.globalDb)
    )
  , ( "GHC_PACKAGE_PATH environment variable"
    , "ghc-package-path"
    , WithoutHaddocks $
        \pi -> mkGhcPackagePath
                 True
                 pi.localDb
                 pi.snapDb
                 pi.extraDbs
                 pi.globalDb
    )
  , ( "Snapshot installation root"
    , "snapshot-install-root"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.snapRoot)
    )
  , ( "Local project installation root"
    , "local-install-root"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.localRoot)
    )
  , ( "Snapshot documentation root"
    , "snapshot-doc-root"
    , UseHaddocks $
        \pi -> T.pack (toFilePathNoTrailingSep (pi.snapRoot </> docDirSuffix))
    )
  , ( "Local project documentation root"
    , "local-doc-root"
    , UseHaddocks $
        \pi -> T.pack (toFilePathNoTrailingSep (pi.localRoot </> docDirSuffix))
    )
  , ( "Local project documentation root"
    , "local-hoogle-root"
    , UseHaddocks $ T.pack . toFilePathNoTrailingSep . (.hoogleRoot)
    )
  , ( "Dist work directory, relative to package directory"
    , "dist-dir"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.distDir)
    )
  , ( "Where HPC reports and tix files are stored"
    , "local-hpc-root"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.hpcDir)
    )
  ]
 where
  toProjectConfigFileRootPath :: Either (Path Abs File) (Path Abs File) -> Text
  toProjectConfigFileRootPath (Left _) =
    throw $ PrettyException ConfigFileNotProjectLevelBug
  toProjectConfigFileRootPath (Right projectConfigFile) =
    T.pack $ toFilePathNoTrailingSep $ parent projectConfigFile
  toProjectConfigFilePath :: Either (Path Abs File) (Path Abs File) -> Text
  toProjectConfigFilePath (Left _) =
    throw $ PrettyException ConfigFileNotProjectLevelBug
  toProjectConfigFilePath (Right projectConfigFile) =
    T.pack $ toFilePath projectConfigFile

-- | 'Text' equivalent of 'stackRootOptionName'.
stackRootOptionName' :: Text
stackRootOptionName' = T.pack stackRootOptionName
