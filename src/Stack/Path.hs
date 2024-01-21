{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Types and functions related to Stack's @path@ command.
module Stack.Path
  ( PathInfo
  , path
  , paths
  ) where

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
                   ( BuildConfig (..), HasBuildConfig (..), projectRootL
                   , stackYamlL
                   )
import           Stack.Types.BuildOptsMonoid ( buildOptsMonoidHaddockL )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..), getCompilerPath )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), stackGlobalConfigL, stackRootL
                   )
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
-- Distinguish a request for only the Stack root, as such a request does not
-- require 'withDefaultEnvConfig'.
path [key] | key == stackRootOptionName' = do
  clArgs <- view $ globalOptsL . to (.configMonoid)
  liftIO $ do
    (_, stackRoot, _) <- determineStackRootAndOwnership clArgs
    T.putStrLn $ T.pack $ toFilePathNoTrailingSep stackRoot
path keys = do
  let -- filter the chosen paths in flags (keys), or show all of them if no
      -- specific paths chosen.
      goodPaths = filter
        ( \(_, key, _) -> null keys || elem key keys )
        paths
      singlePath = length goodPaths == 1
      toEither (_, k, UseHaddocks p) = Left (k, p)
      toEither (_, k, WithoutHaddocks p) = Right (k, p)
      (with, without) = partitionEithers $ map toEither goodPaths
  runHaddock True $ printKeys with singlePath
  runHaddock False $ printKeys without singlePath

printKeys ::
     HasEnvConfig env
  => [(Text, PathInfo -> Text)]
  -> Bool
  -> RIO env ()
printKeys extractors single = do
  pathInfo <- fillPathInfo
  liftIO $ forM_ extractors $ \(key, extractPath) -> do
    let prefix = if single then "" else key <> ": "
    T.putStrLn $ prefix <> extractPath pathInfo

runHaddock :: Bool -> RIO EnvConfig () -> RIO Runner ()
runHaddock x action = local modifyConfig $
  withConfig YesReexec $
    withDefaultEnvConfig action
 where
  modifyConfig = set
    (globalOptsL . globalOptsBuildOptsMonoidL . buildOptsMonoidHaddockL)
    (Just x)

fillPathInfo :: HasEnvConfig env => RIO env PathInfo
fillPathInfo = do
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
  pure PathInfo
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

-- | Type representing information passed to all the path printers.
data PathInfo = PathInfo
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

instance HasPlatform PathInfo where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasLogFunc PathInfo where
  logFuncL = configL . logFuncL

instance HasRunner PathInfo where
  runnerL = configL . runnerL

instance HasStylesUpdate PathInfo where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasTerm PathInfo where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

instance HasGHCVariant PathInfo where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasConfig PathInfo where
  configL = buildConfigL . lens (.config) (\x y -> x { config = y })
  {-# INLINE configL #-}

instance HasPantryConfig PathInfo where
  pantryConfigL = configL . pantryConfigL

instance HasProcessContext PathInfo where
  processContextL = configL . processContextL

instance HasBuildConfig PathInfo where
  buildConfigL =
    lens (.buildConfig) (\x y -> x { buildConfig = y }) . buildConfigL

data UseHaddocks a
  = UseHaddocks a
  | WithoutHaddocks a

-- | The paths of interest to a user. The first tuple string is used for a
-- description that the optparse flag uses, and the second string as a
-- machine-readable key and also for @--foo@ flags. The user can choose a
-- specific path to list like @--stack-root@. But really it's mainly for the
-- documentation aspect.
--
-- When printing output we generate @PathInfo@ and pass it to the function to
-- generate an appropriate string. Trailing slashes are removed, see #506.
paths :: [(String, Text, UseHaddocks (PathInfo -> Text))]
paths =
  [ ( "Global Stack root directory"
    , stackRootOptionName'
    , WithoutHaddocks $
        view (stackRootL . to toFilePathNoTrailingSep . to T.pack))
  , ( "Global Stack configuration file"
    , T.pack stackGlobalConfigOptionName
    , WithoutHaddocks $ view (stackGlobalConfigL . to toFilePath . to T.pack))
  , ( "Project root (derived from stack.yaml file)"
    , "project-root"
    , WithoutHaddocks $
        view (projectRootL . to toFilePathNoTrailingSep . to T.pack))
  , ( "Configuration location (where the stack.yaml file is)"
    , "config-location"
    , WithoutHaddocks $ view (stackYamlL . to toFilePath . to T.pack))
  , ( "PATH environment variable"
    , "bin-path"
    , WithoutHaddocks $
        T.pack . intercalate [FP.searchPathSeparator] . view exeSearchPathL)
  , ( "Install location for GHC and other core tools (see 'stack ls tools' command)"
    , "programs"
    , WithoutHaddocks $
        view (configL . to (.localPrograms) . to toFilePathNoTrailingSep . to T.pack))
  , ( "Compiler binary (e.g. ghc)"
    , "compiler-exe"
    , WithoutHaddocks $ T.pack . toFilePath . (.compiler) )
  , ( "Directory containing the compiler binary (e.g. ghc)"
    , "compiler-bin"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . parent . (.compiler) )
  , ( "Directory containing binaries specific to a particular compiler"
    , "compiler-tools-bin"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.toolsDir) )
  , ( "Directory where Stack installs executables (e.g. ~/.local/bin (Unix-like OSs) or %APPDATA%\\local\\bin (Windows))"
    , "local-bin"
    , WithoutHaddocks $
        view $ configL . to (.localBin) . to toFilePathNoTrailingSep . to T.pack)
  , ( "Extra include directories"
    , "extra-include-dirs"
    , WithoutHaddocks $
        T.intercalate ", " . map T.pack . (.extraIncludeDirs) . view configL )
  , ( "Extra library directories"
    , "extra-library-dirs"
    , WithoutHaddocks $
        T.intercalate ", " . map T.pack . (.extraLibDirs) . view configL )
  , ( "Snapshot package database"
    , "snapshot-pkg-db"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.snapDb) )
  , ( "Local project package database"
    , "local-pkg-db"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.localDb) )
  , ( "Global package database"
    , "global-pkg-db"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.globalDb) )
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
    , WithoutHaddocks $
        T.pack . toFilePathNoTrailingSep . (.snapRoot) )
  , ( "Local project installation root"
    , "local-install-root"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.localRoot) )
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
    , UseHaddocks $ T.pack . toFilePathNoTrailingSep . (.hoogleRoot))
  , ( "Dist work directory, relative to package directory"
    , "dist-dir"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.distDir) )
  , ( "Where HPC reports and tix files are stored"
    , "local-hpc-root"
    , WithoutHaddocks $ T.pack . toFilePathNoTrailingSep . (.hpcDir) )
  ]

-- | 'Text' equivalent of 'stackRootOptionName'.
stackRootOptionName' :: Text
stackRootOptionName' = T.pack stackRootOptionName
