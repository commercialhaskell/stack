{- stack script
   --snapshot lts-24.18
   --ghc-options -Wall
-}

-- As no packages are specified in the `stack script` command in the Stack
-- interpreter options comment, Stack deduces the required packages from the
-- module imports, being: Cabal, base, bytestring, directory, extra, process,
-- shake, tar, zip-archive and zlib. These are either GHC boot packages or in
-- the snapshot. Stackage LTS Haskell 24.18 does not include boot packages
-- directly. As GHC 9.10.3 boot packages Cabal and Cabal-syntax expose modules
-- with the same names, the language extension PackageImports is required.

-- EXPERIMENTAL

-- This corresponds to release.hs but is intended to be run only on
-- macOS/AArch64 in order to create a statically-linked Linux/AArch64 version of
-- Stack:
--
-- Install pre-requisites:
--
-- > brew install docker
-- > brew install colima
--
-- Start colima and run script:
--
-- > colima start
-- > release-linux-aarch64.hs build --alpine --build-args --docker-stack-exe=image
--
-- Could be incorporated into release.hs, in due course.

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE PatternSynonyms     #-}

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.GZip as GZip
import           Control.Exception ( tryJust )
import           Control.Monad ( guard )
import qualified Data.ByteString.Lazy.Char8 as L8
import           "extra" Data.List.Extra ( stripPrefix )
import           Development.Shake
                   ( Action, Change (..), pattern Chatty, Rules
                   , ShakeOptions (..), (%>), actionOnException, alwaysRerun
                   , cmd, copyFileChanged, getDirectoryFiles, liftIO, need
                   , phony, shakeArgsWith, shakeOptions, want
                   )
import           Development.Shake.FilePath
                   ( (<.>), (</>), exe, takeDirectory, toStandard )
import           "Cabal" Distribution.PackageDescription
                   ( PackageDescription (..), packageDescription, pkgVersion
                   )
import           Distribution.Simple.PackageDescription
                   ( readGenericPackageDescription )
import           "Cabal" Distribution.System
                   ( Arch, OS (..), Platform (..), buildPlatform )
import           "Cabal" Distribution.Text ( display )
import           Distribution.Verbosity ( silent )
import           System.Console.GetOpt ( ArgDescr (..), OptDescr (..) )
import           System.Directory ( removeFile )
import           System.IO.Error ( isDoesNotExistError )
import           System.Process ( readProcess )

-- | Entrypoint.
main :: IO ()
main = shakeArgsWith
  shakeOptions { shakeFiles = releaseDir
               , shakeVerbosity = Chatty
               , shakeChange = ChangeModtimeAndDigestInput
               }
  options $
  \flags args -> do
  -- build the default value of type Global, with predefined constants

    -- 'stack build --dry-run' just ensures that 'stack.cabal' is generated from
    -- 'package.yaml'.
    _ <- readProcess "stack" ["build", "--dry-run"] ""
    gStackPackageDescription <-
      packageDescription <$> readGenericPackageDescription silent "stack.cabal"

    let Platform arch _ = buildPlatform
        gArch = arch
        gBuildArgs = ["--flag", "stack:-developer-mode"]
        global = foldl
          (flip id)
          Global
            { gStackPackageDescription
            , gArch
            , gBuildArgs
            }
          flags

    pure $ Just $ rules global args

-- | Additional command-line options.
options :: [OptDescr (Either String (Global -> Global))]
options =
  [ Option "" [alpineOptName]
      ( NoArg $ Right $ \g ->
          g { gBuildArgs =
                   gBuildArgs g
                ++ [ "--flag=stack:static"
                   , "--docker"
                   , "--system-ghc"
                   , "--no-install-ghc"
                   ]
            }
      )
      "Build a statically linked binary using an Alpine Docker image."
  , Option "" [buildArgsOptName]
      ( ReqArg
          (\v -> Right $ \g -> g{gBuildArgs = gBuildArgs g ++ words v})
          "\"ARG1 ARG2 ...\""
      )
      "Additional arguments to pass to 'stack build'."
  ]

-- | Shake rules.
rules :: Global -> [String] -> Rules ()
rules global args = do
  case args of
    [] -> error "No wanted target(s) specified."
    _ -> want args

  phony buildPhony $
    mapM_ (\f -> need [releaseDir </> f]) binaryPkgFileNames

  releaseDir </> binaryPkgTarGzFileName %> \out -> do
    stageFiles <- getBinaryPkgStageFiles
    writeTarGz id out releaseStageDir stageFiles

  releaseStageDir </> binaryName </> stackExeFileName %> \out -> do
    copyFileChanged (releaseDir </> binaryExeFileName) out

  releaseStageDir </> (binaryName ++ "//*") %> \out -> do
    copyFileChanged
      (dropDirectoryPrefix (releaseStageDir </> binaryName) out)
      out

  releaseDir </> binaryExeFileName %> \out -> do
    need [releaseBinDir </> binaryName </> stackExeFileName]
    case platformOS of
      OSX ->
        cmd "strip -o"
          [out, releaseBinDir </> binaryName </> stackExeFileName]
      _ -> undefined

  releaseBinDir </> binaryName </> stackExeFileName %> \out -> do
    alwaysRerun
    actionOnException
      ( cmd stackProgName
          (stackArgs global)
          ["--local-bin-path=" ++ takeDirectory out]
          "install"
          global.gBuildArgs
          integrationTestFlagArgs
          "--pedantic"
          "stack"
      )
      (tryJust (guard . isDoesNotExistError) (removeFile out))

 where
  integrationTestFlagArgs =
    -- Explicitly enabling 'hide-dependency-versions' and 'supported-build' to
    -- work around https://github.com/commercialhaskell/stack/issues/4960
    [ "--flag=stack:hide-dependency-versions"
    , "--flag=stack:supported-build"
    ]

  getBinaryPkgStageFiles = do
    docFiles <- getDocFiles
    let stageFiles = concat
          [ [releaseStageDir </> binaryName </> stackExeFileName]
          , map ((releaseStageDir </> binaryName) </>) docFiles
          ]
    need stageFiles
    pure stageFiles

  getDocFiles = getDirectoryFiles "." ["LICENSE", "*.md", "doc//*.md"]

  buildPhony = "build"

  releaseStageDir = releaseDir </> "stage"
  releaseBinDir = releaseDir </> "bin"

  binaryPkgFileNames =
    case platformOS of
      OSX -> [binaryExeFileName, binaryPkgTarGzFileName]
      _ -> undefined
  binaryPkgTarGzFileName = binaryName <.> tarGzExt
  binaryExeFileName = binaryName ++ "-bin" <.> exe
  binaryName = concat
    [ stackProgName
    , "-"
    , stackVersionStr global
    , "-"
    , display targetPlatformOS
    , "-"
    , display global.gArch
    ]
  stackExeFileName = stackProgName <.> exe

  tarGzExt = tarExt <.> gzExt
  gzExt = ".gz"
  tarExt = ".tar"

-- | Create a .tar.gz files from files.  The paths should be absolute, and will
-- be made relative to the base directory in the tarball.
writeTarGz ::
     (FilePath -> FilePath)
  -> FilePath
  -> FilePath
  -> [FilePath]
  -> Action ()
writeTarGz fixPath out baseDir inputFiles = liftIO $ do
  content <- Tar.pack baseDir $ map (dropDirectoryPrefix baseDir) inputFiles
  L8.writeFile out $ GZip.compress $ Tar.write $ map fixPath' content
 where
  fixPath' :: Tar.Entry -> Tar.Entry
  fixPath' entry =
    case TarEntry.toTarPath isDir $ fixPath $ TarEntry.entryPath entry of
      Left e -> error $ show (Tar.entryPath entry, e)
      Right tarPath -> entry { TarEntry.entryTarPath = tarPath }
   where
    isDir =
      case TarEntry.entryContent entry of
        TarEntry.Directory -> True
        _ -> False

-- | Drops a directory prefix from a path. The prefix automatically has a path
-- separator character appended. Fails if the path does not begin with the
-- prefix.
dropDirectoryPrefix :: FilePath -> FilePath -> FilePath
dropDirectoryPrefix prefix path =
  case stripPrefix (toStandard prefix ++ "/") (toStandard path) of
    Nothing -> error
      (  "dropDirectoryPrefix: cannot drop "
      ++ show prefix
      ++ " from "
      ++ show path
      )
    Just stripped -> stripped

-- | String representation of Stack package version.
stackVersionStr :: Global -> String
stackVersionStr =
  display . pkgVersion . package . gStackPackageDescription

-- | Current operating system.
platformOS :: OS
platformOS =
  let Platform _ os = buildPlatform
  in  os

-- | Target operating system
targetPlatformOS :: OS
targetPlatformOS = Linux

-- | Directory in which to store build and intermediate files.
releaseDir :: FilePath
releaseDir = "_release"

-- | @--build-args@ command-line option name.
buildArgsOptName :: String
buildArgsOptName = "build-args"

-- | @--alpine@ command-line option name.
alpineOptName :: String
alpineOptName = "alpine"

-- | Arguments to pass to all 'stack' invocations.
stackArgs :: Global -> [String]
stackArgs global = [ "--arch=" ++ display global.gArch
                   , "--interleaved-output"
                   ]

-- | Name of the 'stack' program.
stackProgName :: FilePath
stackProgName = "stack"

-- | Global values and options.
data Global = Global
  { gStackPackageDescription :: !PackageDescription
  , gArch :: !Arch
  , gBuildArgs :: [String]
  }
  deriving Show
