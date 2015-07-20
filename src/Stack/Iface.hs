{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO(DanBurton): remove the following once the module is done.
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-imports #-}

module Stack.Iface where

import Data.Map (Map)
import Data.ByteString(ByteString)
import Distribution.ModuleName (ModuleName)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Logger
import Path
import Path.IO (fileExists)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Data.Monoid
import Data.Foldable (foldMap)
import Distribution.PackageDescription
import qualified Distribution.ModuleName as ModuleName
import System.Process (readProcess)
import System.FilePath (dropExtension, addExtension)

import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.Constants
import           Stack.Package
import           Stack.Types

--type M m env = (MonadLogger m, MonadIO m, MonadCatch m, MonadReader env m, HasEnvConfig env)

data TargetModules = TargetModules
    { targetIsExecutable   :: Bool
      -- ^ Implies "Main" as a target module if True.
      -- benchmark and test targets are also executable.
    , targetExposedModules :: [ModuleName]
    , targetOtherModules   :: [ModuleName]
    }
  deriving (Show)

type ShowIface = Path Abs File -> IO ByteString

-- All of the compiled modules for a given target
-- can be found in a single directory tree.
detectFiles :: ShowIface -> Path Abs Dir -- place to find .hi files
            -> TargetModules -> IO [FilePath]
detectFiles showIface hiDir targetModules = do
    let targetFilesRel :: [FilePath] -- (Relative) FilePath representation of modules.
        targetFilesRel
            =  if targetIsExecutable targetModules
                   then ["Main"]
                   else []
            <> map ModuleName.toFilePath (targetExposedModules targetModules)
            <> map ModuleName.toFilePath (targetOtherModules targetModules)
    let targetHiFilesAbs :: [Path Abs File]
        targetHiFilesAbs = concatMap toHi targetFilesRel
          where
            toHi :: FilePath -> [Path Abs File]
            toHi fp = case pathHiExtMay of
                Just pathHiExt -> [hiDir </> pathHiExt]
                Nothing -> [] -- warn?
              where
                pathHiExtMay
                    = parseRelFile
                    $ addExtension fp "hi"

    depFiles <- fmap concat $ forM targetHiFilesAbs $ \file -> do
        exists <- fileExists file
        if exists
          then do
            iface <- showIface file
            return $ findDepFiles iface
          else return [] -- warn?

    return depFiles


findDepFiles :: ByteString -> [FilePath]
findDepFiles bs = depFiles
  where
    text = Text.decodeUtf8 bs
    ts = Text.lines text
    depFiles = map Text.unpack $ mapMaybe f ts
    f     = Text.stripPrefix "addDependentFile \""
        >=> Text.stripSuffix "\""

-- Map from Target to TargetModules
targetModules :: PackageDescription -> Map Target TargetModules
targetModules pDesc
     = foldMap libraryTargetModules    (library     pDesc)
    <> foldMap executableTargetModules (executables pDesc)
    <> foldMap testSuiteTargetModules  (testSuites  pDesc)
    <> foldMap benchmarkTargetModules  (benchmarks  pDesc)

libraryTargetModules :: Library -> Map Target TargetModules
libraryTargetModules lib = Map.singleton TargetLibrary $
    TargetModules
        { targetIsExecutable = False
        , targetExposedModules = exposedModules lib
        , targetOtherModules = otherModules (libBuildInfo lib)
        }

executableTargetModules :: Executable -> Map Target TargetModules
executableTargetModules exe = Map.singleton (TargetExecutable (exeName exe)) $
    TargetModules
        { targetIsExecutable = True
        , targetExposedModules = []
        , targetOtherModules = otherModules (buildInfo exe)
        }

testSuiteTargetModules :: TestSuite -> Map Target TargetModules
testSuiteTargetModules test = Map.singleton (TargetExecutable (testName test)) $
    TargetModules
        { targetIsExecutable = True
        , targetExposedModules = []
        , targetOtherModules = otherModules (testBuildInfo test)
        }

benchmarkTargetModules :: Benchmark -> Map Target TargetModules
benchmarkTargetModules bench = Map.singleton (TargetExecutable (benchmarkName bench)) $
    TargetModules
        { targetIsExecutable = True
        , targetExposedModules = []
        , targetOtherModules = otherModules (benchmarkBuildInfo bench)
        }

data CompilationContext = CompilationContext
    { ccPackageName :: String
    , ccPackageVersion :: Version
    , ccProjectRoot :: Path Abs Dir
    , ccGhcVersion :: Version
    , ccArch :: String
    , ccSnapshot :: String
    , ccCabalLibVersion :: Version
    }

targetHiDir :: MonadThrow m => CompilationContext -> Target -> m (Path Abs Dir)
targetHiDir cc TargetLibrary = do
    let showGhcVer = versionString (ccGhcVersion cc)
    let showArch = ccArch cc
    let showPackageAndVersion = ccPackageName cc <> "-" <> versionString (ccPackageVersion cc)

    arch <- parseRelDir (ccArch cc)
    snapshot <- parseRelDir (ccSnapshot cc)
    ghcVer <- parseRelDir showGhcVer
    archGhc <- parseRelDir (showArch <> "-ghc-" <> showGhcVer)
    packageAndVersion <- parseRelDir showPackageAndVersion

    return $ ccProjectRoot cc </> $(mkRelDir ".stack-work/install")
        </> arch </> snapshot </> ghcVer
        </> $(mkRelDir "lib") </> archGhc </> packageAndVersion
targetHiDir cc (TargetExecutable exeName) = do
    let showCabalVersion = versionString (ccCabalLibVersion cc)

    arch <- parseRelDir (ccArch cc)
    cabalWithVer <- parseRelDir ("Cabal-" <> showCabalVersion)
    exe <- parseRelDir exeName
    exeTmp <- parseRelDir (exeName <> "-tmp")

    return $ ccProjectRoot cc </> $(mkRelDir ".stack-work/dist")
        </> arch </> cabalWithVer
        </> $(mkRelDir "build") </> exe </> exeTmp

data Target
    = TargetLibrary
    | TargetExecutable String
  deriving (Eq, Ord, Show)

sampleRun :: IO ()
sampleRun = do
  let showIface arg = do
          str <- readProcess "ghc" ["--show-iface", toFilePath arg] ""
          return $ S8.pack str
  --let hiDir =
  --        -- $(mkAbsDir "/home/dan/dep-file-test/.stack-work/install/x86_64-linux/lts-2.13/7.8.4/lib/x86_64-linux-ghc-7.8.4/dep-file-test-0.1.0.0")
  --        $(mkAbsDir "/home/dan/dep-file-test/.stack-work/dist/x86_64-linux/Cabal-1.18.1.5/build/dep-file-test/dep-file-test-tmp")
  sampleProjectRoot <- parseAbsDir "/home/dan/dep-file-test"
  let ctx = CompilationContext
          { ccPackageName = "dep-file-test"
          , ccPackageVersion = $(mkVersion "0.1.0.0")
          , ccProjectRoot = sampleProjectRoot
          , ccGhcVersion = $(mkVersion "7.8.4")
          , ccArch = "x86_64-linux"
          , ccSnapshot = "lts-2.13"
          , ccCabalLibVersion = $(mkVersion "1.18.1.5")
          }

  hiDir <- targetHiDir ctx (TargetExecutable "dep-file-test")
  let targetModules = TargetModules
          { targetIsExecutable   = True
          , targetExposedModules = []
          , targetOtherModules   = []
          }
  files <- detectFiles showIface hiDir targetModules
  mapM_ print files

--iface :: M m env => m ()
--iface = do
--  let print' :: (Show a, MonadIO m) => a -> m ()
--      print' = liftIO . print
--  localInstallRoot <- installationRootLocal
--  print' localInstallRoot

--  dist <- distRelativeDir
--  print' dist

--  (lps, _, _) <- loadLocals defaultBuildOpts Map.empty
--  forM_ lps $ \lp -> do
--    print' $ packageName $ lpPackage lp

--  return ()
