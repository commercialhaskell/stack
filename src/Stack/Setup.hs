{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-|
Module      : Stack.Setup
License     : BSD-3-Clause
-}

module Stack.Setup
  ( setupEnv
  , ensureCompilerAndMsys
  , ensureDockerStackExe
  , SetupOpts (..)
  , defaultSetupInfoYaml
  , withNewLocalBuildTargets

  -- * Stack binary download
  , StackReleaseInfo
  , getDownloadVersion
  , stackVersion
  , preferredPlatforms
  , downloadStackReleaseInfo
  , downloadStackExe
  ) where

import qualified Codec.Archive.Tar as Tar
import           Conduit
                   ( ConduitT, await, concatMapMC, filterCE, foldMC, yield )
import           Control.Applicative ( empty )
import           Crypto.Hash ( SHA1 (..), SHA256 (..) )
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types ( Value (..) )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings (..), logJSONWarnings )
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LBS
import           Data.Char ( isDigit )
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Lazy ( lazyConsume )
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed ( createSource )
import           Data.Conduit.Zlib ( ungzip )
import qualified Data.Either.Extra as EE
import           Data.List.Split ( splitOn )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding.Error as T
import qualified Data.Yaml as Yaml
import           Distribution.System ( Arch (..), OS, Platform (..) )
import qualified Distribution.System as Cabal
import           Distribution.Text ( simpleParse )
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Version ( mkVersion )
import           Network.HTTP.Client ( redirectCount )
import           Network.HTTP.StackClient
                   ( CheckHexDigest (..), HashCheck (..), getResponseBody
                   , getResponseStatusCode, httpLbs, httpJSON, mkDownloadRequest
                   , parseRequest, parseUrlThrow, setGitHubHeaders
                   , setHashChecks, setLengthCheck, setRequestMethod
                   , verifiedDownloadWithProgress, withResponse
                   )
import           Network.HTTP.Simple ( getResponseHeader )
import           Path
                   ( (</>), addExtension, fileExtension, filename, parent
                   , parseAbsDir, parseAbsFile, parseRelDir, parseRelFile
                   , takeDrive, toFilePath
                   )
import           Path.CheckInstall ( warnInstallSearchPathIssues )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO
                   ( canonicalizePath, doesFileExist, ensureDir, executable
                   , getPermissions, getTempDir, ignoringAbsence, listDir
                   , removeDirRecur, removeFile, renameDir, renameFile
                   , resolveFile', withTempDir
                   )
import           RIO.List
                   ( headMaybe, intercalate, intersperse, isPrefixOf
                   , maximumByMaybe, sort, sortOn, stripPrefix )
import           RIO.Process
                   ( EnvVars, HasProcessContext (..), ProcessContext
                   , augmentPath, augmentPathMap, doesExecutableExist, envVarsL
                   , exeSearchPathL, getStdout, mkProcessContext, modifyEnvVars
                   , proc, readProcess_, readProcessStdout, runProcess
                   , runProcess_, setStdout, waitExitCode, withModifyEnvVars
                   , withProcessWait, withWorkingDir, workingDirL
                   )
import           Stack.Build.Haddock ( shouldHaddockDeps )
import           Stack.Build.Source ( hashSourceMapData, loadSourceMap )
import           Stack.Build.Target ( NeedTargets (..), parseTargets )
import           Stack.Config.ConfigureScript ( ensureConfigureScript )
import           Stack.Constants
                   ( cabalPackageName, ghcBootScript,ghcConfigureMacOS
                   , ghcConfigurePosix, ghcConfigureWindows, hadrianScriptsPosix
                   , hadrianScriptsWindows, libDirs, osIsMacOS, osIsWindows
                   , relDirBin, relDirUsr, relFile7zdll, relFile7zexe
                   , relFileConfigure, relFileHadrianStackDotYaml
                   , relFileLibcMuslx86_64So1, relFileLibgmpSo10
                   , relFileLibgmpSo3, relFileLibncurseswSo6, relFileLibtinfoSo5
                   , relFileLibtinfoSo6, relFileMainHs, relFileStack
                   , relFileStackDotExe, relFileStackDotTmp
                   , relFileStackDotTmpDotExe, stackProgName, usrLibDirs
                   )
import           Stack.Constants.Config ( distRelativeDir )
import           Stack.GhcPkg
                   ( createDatabase, getGlobalDB, ghcPkgPathEnvVar
                   , mkGhcPackagePath )
import           Stack.Prelude
import           Stack.Setup.Installed
                   ( Tool (..), filterTools, getCompilerVersion, installDir
                   , listInstalled, markInstalled, tempInstallDir,toolExtraDirs
                   , toolString, unmarkInstalled
                   )
import           Stack.SourceMap
                   ( actualFromGhc, globalsFromDump, pruneGlobals )
import           Stack.Storage.User ( loadCompilerPaths, saveCompilerPaths )
import           Stack.Types.Build.Exception ( BuildPrettyException (..) )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), configFileRootL
                   , wantedCompilerVersionL
                   )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI (..) )
import           Stack.Types.Compiler
                   ( ActualCompiler (..), CompilerException (..)
                   , CompilerRepository (..), WhichCompiler (..)
                   , compilerVersionText, getGhcVersion, isWantedCompiler
                   , wantedToActual, whichCompiler, whichCompilerL
                   )
import           Stack.Types.CompilerBuild
                   ( CompilerBuild (..), compilerBuildName, compilerBuildSuffix
                   )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), GhcPkgExe (..), HasCompiler (..) )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), envOverrideSettingsL
                   , ghcInstallHook
                   )
import           Stack.Types.DownloadInfo ( DownloadInfo (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), extraBinDirs
                   , packageDatabaseDeps, packageDatabaseExtra
                   , packageDatabaseLocal
                   )
import           Stack.Types.EnvSettings
                   ( EnvSettings (..), minimalEnvSettings )
import           Stack.Types.ExtraDirs ( ExtraDirs (..) )
import           Stack.Types.FileDigestCache ( newFileDigestCache )
import           Stack.Types.GHCDownloadInfo ( GHCDownloadInfo (..) )
import           Stack.Types.GHCVariant
                   ( GHCVariant (..), HasGHCVariant (..), ghcVariantName
                   , ghcVariantSuffix
                   )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Platform
                   ( HasPlatform (..), PlatformVariant (..)
                   , platformOnlyRelDir )
import           Stack.Types.Runner
                   ( HasRunner (..), Runner (..), mExecutablePathL
                   , viewExecutablePath
                   )
import           Stack.Types.SetupInfo ( SetupInfo (..) )
import           Stack.Types.SourceMap
                   ( SMActual (..), SMWanted (..), SourceMap (..) )
import           Stack.Types.Version
                   ( VersionCheck, stackMinorVersion, stackVersion )
import           Stack.Types.VersionedDownloadInfo
                   ( VersionedDownloadInfo (..) )
import           Stack.Types.WantedCompilerSetter ( WantedCompilerSetter (..) )
import qualified System.Directory as D
import           System.Environment ( lookupEnv )
import           System.IO.Error ( isPermissionError )
import           System.FilePath ( searchPathSeparator )
import qualified System.FilePath as FP
import           System.Permissions ( setFileExecutable )
import           System.Uname ( getRelease )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Setup" module
data SetupException
  = WorkingDirectoryInvalidBug
  | StackBinaryArchiveZipUnsupportedBug
  deriving (Show, Typeable)

instance Exception SetupException where
  displayException WorkingDirectoryInvalidBug = bugReport "[S-2076]"
    "Invalid working directory."
  displayException StackBinaryArchiveZipUnsupportedBug = bugReport "[S-3967]"
    "FIXME: Handle zip files."

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Setup" module
data SetupPrettyException
  = GHCInstallFailed
      !SomeException
      !String
      !String
      ![String]
      !(Path Abs Dir)
      !(Path Abs Dir)
      !(Path Abs Dir)
  | InvalidGhcAt !(Path Abs File) !SomeException
  | ExecutableNotFound ![Path Abs File]
  | SandboxedCompilerNotFound ![String] ![Path Abs Dir]
  | UnsupportedSetupCombo !OS !Arch !StyleDoc !StyleDoc !(Path Abs Dir)
  | MissingDependencies ![String]
  | UnknownCompilerVersion
      !(Set.Set Text)
      !WantedCompiler
      !(Set.Set ActualCompiler)
  | UnknownOSKey !Text
  | GHCSanityCheckCompileFailed !SomeException !(Path Abs File)
  | RequireCustomGHCVariant
  | ProblemWhileDecompressing !(Path Abs File)
  | SetupInfoMissingSevenz
  | UnsupportedSetupConfiguration
  | MSYS2NotFound !Text
  | UnwantedCompilerVersion
  | UnwantedArchitecture
  | GHCInfoNotValidUTF8 !UnicodeException
  | GHCInfoNotListOfPairs
  | GHCInfoMissingGlobalPackageDB
  | GHCInfoMissingTargetPlatform
  | GHCInfoTargetPlatformInvalid !String
  | CabalNotFound !(Path Abs File)
  | GhcBootScriptNotFound
  | HadrianScriptNotFound
  | URLInvalid !String
  | UnknownArchiveExtension !String
  | Unsupported7z
  | TarballInvalid !String
  | TarballFileInvalid !String !(Path Abs File)
  | UnknownArchiveStructure !(Path Abs File)
  | StackReleaseInfoNotFound !String
  | StackBinaryArchiveNotFound ![String]
  | HadrianBindistNotFound
  | DownloadAndInstallCompilerError
  | StackBinaryArchiveUnsupported !Text
  | StackBinaryNotInArchive !String !Text
  | FileTypeInArchiveInvalid !Tar.Entry !Text
  | BinaryUpgradeOnOSUnsupported !Cabal.OS
  | BinaryUpgradeOnArchUnsupported !Cabal.Arch
  | ExistingMSYS2NotDeleted !(Path Abs Dir) !IOException
  deriving (Show, Typeable)

instance Pretty SetupPrettyException where
  pretty (GHCInstallFailed ex step cmd args wd tempDir destDir) =
    "[S-7441]"
    <> line
    <> string (displayException ex)
    <> line
    <> hang 2 (  fillSep
                   [ flow "Error encountered while"
                   , fromString step
                   , flow "GHC with"
                   ]
              <> line
              <> style Shell (fromString (unwords (cmd : args)))
              <> line
              -- TODO: Figure out how to insert \ in the appropriate spots
              -- hang 2 (shellColor (fillSep (fromString cmd : map fromString args))) <> line <>
              <> fillSep
                   [ flow "run in"
                   , pretty wd
                   ]
              )
    <> blankLine
    <> flow "The following directories may now contain files, but won't be \
            \used by Stack:"
    <> line
    <> bulletedList [pretty tempDir, pretty destDir]
    <> blankLine
    <> fillSep
         [ flow "For more information consider rerunning with"
         , style Shell "--verbose"
         , "flag."
         ]
    <> line
  pretty (InvalidGhcAt compiler e) =
    "[S-2476]"
    <> line
    <> fillSep
         [ flow "Stack considers the compiler at"
         , pretty compiler
         , flow "to be invalid."
         ]
    <> blankLine
    <> flow "While assessing that compiler, Stack encountered the error:"
    <> blankLine
    <> ppException e
  pretty (ExecutableNotFound toTry) =
    "[S-4764]"
    <> line
    <> flow "Stack could not find any of the following executables:"
    <> line
    <> bulletedList (map pretty toTry)
  pretty (SandboxedCompilerNotFound names fps) =
    "[S-9953]"
    <> line
    <> fillSep
         ( ( flow "Stack could not find the sandboxed compiler. It looked for \
                   \one named one of:"
            : mkNarrativeList Nothing False
                ( map fromString names :: [StyleDoc] )
            )
         <> ( flow "However, it could not find any on one of the paths:"
            : mkNarrativeList Nothing False fps
            )
         )
    <> blankLine
    <> fillSep
         [ flow "Perhaps a previously-installed compiler was not completely \
                \uninstalled. For further information about uninstalling \
                \tools, see the output of"
         , style Shell (flow "stack uninstall") <> "."
         ]
  pretty (UnsupportedSetupCombo os arch tool toolDirAdvice programsDir) =
    "[S-1852]"
    <> line
    <> fillSep
         [ flow "Stack does not know how to install"
         , tool
         , flow "for the combination of operating system"
         , style Shell (pretty os)
         , "and architecture"
         , style Shell (pretty arch) <> "."
         , flow "Please install manually."
         ]
    <> blankLine
    <> fillSep
         [ flow "To install manually the version of"
         , tool <> ","
         , flow "its root directory should be named"
         , toolDirAdvice
         , flow "and the directory should be accompanied by a file with the \
                \same name and extension"
         , style File ".installed"
         , flow "(which marks the"
         , tool
         , flow "version as installed). Both items should be located in the \
                \subdirectory for the specified platform in Stack's directory \
                \for local tools"
         , parens (pretty programsDir) <> "."
         ]
  pretty (MissingDependencies tools) =
    "[S-2126]"
    <> line
    <> fillSep
         ( flow "The following executables are missing and must be installed:"
         : mkNarrativeList Nothing False (map fromString tools :: [StyleDoc])
         )
  pretty (UnknownCompilerVersion oskeys wanted known) =
    "[S-9443]"
    <> line
    <> fillSep
         (  ( flow "No setup information found for"
            : style Current wanted'
            : flow "on your platform. This probably means a GHC binary \
                   \distribution has not yet been added for OS key"
            : mkNarrativeList (Just Shell) False
                (map (fromString . T.unpack) (sort $ Set.toList oskeys) :: [StyleDoc])
            )
         <> ( flow "Supported versions:"
            : mkNarrativeList Nothing False
                ( map
                    (fromString . T.unpack . compilerVersionText)
                    (sort $ Set.toList known)
                    :: [StyleDoc]
                )
            )
         )
   where
    wanted' = fromString . T.unpack . utf8BuilderToText $ display wanted
  pretty (UnknownOSKey oskey) =
    "[S-6810]"
    <> line
    <> fillSep
         [ flow "Unable to find installation URLs for OS key:"
         , fromString $ T.unpack oskey <> "."
         ]
  pretty (GHCSanityCheckCompileFailed e ghc) =
    "[S-5159]"
    <> line
    <> fillSep
         [ flow "The GHC located at"
         , pretty ghc
         , flow "failed to compile a sanity check. Please see:"
         , style Url "http://docs.haskellstack.org/en/stable/install_and_upgrade/"
         , flow "for more information. Stack encountered the following \
                \error:"
         ]
    <> blankLine
    <> string (displayException e)
  pretty RequireCustomGHCVariant =
    "[S-8948]"
    <> line
    <> fillSep
         [ flow "A custom"
         , style Shell "--ghc-variant"
         , flow "must be specified to use"
         , style Shell "--ghc-bindist" <> "."
         ]
  pretty (ProblemWhileDecompressing archive) =
    "[S-2905]"
    <> line
    <> fillSep
         [ flow "Problem while decompressing"
         , pretty archive <> "."
         ]
  pretty SetupInfoMissingSevenz =
    "[S-9561]"
    <> line
    <> flow "SetupInfo missing Sevenz EXE/DLL."
  pretty UnsupportedSetupConfiguration =
    "[S-7748]"
    <> line
    <> flow "Stack does not know how to install GHC on your system \
            \configuration. Please install manually."
  pretty (MSYS2NotFound osKey) =
    "[S-5308]"
    <> line
    <> fillSep
         [ flow "MSYS2 not found for"
         , fromString $ T.unpack osKey <> "."
         ]
  pretty UnwantedCompilerVersion =
    "[S-5127]"
    <> line
    <> flow "Not the compiler version we want."
  pretty UnwantedArchitecture =
    "[S-1540]"
    <> line
    <> flow "Not the architecture we want."
  pretty (GHCInfoNotValidUTF8 e) =
    "[S-8668]"
    <> line
    <> flow "GHC info is not valid UTF-8. Stack encountered the following \
            \error:"
    <> blankLine
    <> string (displayException e)
  pretty GHCInfoNotListOfPairs =
    "[S-4878]"
    <> line
    <> flow "GHC info does not parse as a list of pairs."
  pretty GHCInfoMissingGlobalPackageDB =
    "[S-2965]"
    <> line
    <> flow "Key 'Global Package DB' not found in GHC info."
  pretty GHCInfoMissingTargetPlatform =
    "[S-5219]"
    <> line
    <> flow "Key 'Target platform' not found in GHC info."
  pretty (GHCInfoTargetPlatformInvalid targetPlatform) =
    "[S-8299]"
    <> line
    <> fillSep
         [ flow "Invalid target platform in GHC info:"
         , fromString targetPlatform <> "."
         ]
  pretty (CabalNotFound compiler) =
    "[S-2574]"
    <> line
    <> fillSep
         [ flow "Cabal library not found in global package database for"
         , pretty compiler <> "."
         ]
  pretty GhcBootScriptNotFound =
    "[S-8488]"
    <> line
    <> flow "No GHC boot script found."
  pretty HadrianScriptNotFound =
    "[S-1128]"
    <> line
    <> flow "No Hadrian build script found."
  pretty (URLInvalid url) =
    "[S-1906]"
    <> line
    <> fillSep
         [ flow "`url` must be either an HTTP URL or a file path:"
         , fromString url <> "."
         ]
  pretty (UnknownArchiveExtension url) =
    "[S-1648]"
    <> line
    <> fillSep
         [ flow "Unknown extension for url:"
         , style Url (fromString url) <> "."
         ]
  pretty Unsupported7z =
    "[S-4509]"
    <> line
    <> fillSep
         [ flow "Stack does not know how to deal with"
         , style File ".7z"
         , flow "files on non-Windows operating systems."
         ]
  pretty (TarballInvalid name) =
    "[S-3158]"
    <> line
    <> fillSep
         [ style File (fromString name)
         , flow "must be a tarball file."
         ]
  pretty (TarballFileInvalid name archiveFile) =
    "[S-5252]"
    <> line
    <> fillSep
         [ "Invalid"
         , style File (fromString name)
         , "filename:"
         , pretty archiveFile <> "."
         ]
  pretty (UnknownArchiveStructure archiveFile) =
    "[S-1827]"
    <> line
    <> fillSep
         [ flow "Expected a single directory within unpacked"
         , pretty archiveFile <> "."
         ]
  pretty (StackReleaseInfoNotFound url) =
    "[S-9476]"
    <> line
    <> fillSep
         [ flow "Could not get release information for Stack from:"
         , style Url (fromString url) <> "."
         ]
  pretty (StackBinaryArchiveNotFound platforms) =
    "[S-4461]"
    <> line
    <> fillSep
         ( flow "Unable to find binary Stack archive for platforms:"
         : mkNarrativeList Nothing False
             (map fromString platforms :: [StyleDoc])
         )
  pretty HadrianBindistNotFound =
    "[S-6617]"
    <> line
    <> flow "Can't find Hadrian-generated binary distribution."
  pretty DownloadAndInstallCompilerError =
    "[S-7227]"
    <> line
    <> flow "'downloadAndInstallCompiler' should not be reached with ghc-git."
  pretty (StackBinaryArchiveUnsupported archiveURL) =
    "[S-6636]"
    <> line
    <> fillSep
         [ flow "Unknown archive format for Stack archive:"
         , style Url (fromString $ T.unpack archiveURL) <> "."
         ]
  pretty (StackBinaryNotInArchive exeName url) =
    "[S-7871]"
    <> line
    <> fillSep
         [ flow "Stack executable"
         , style File (fromString exeName)
         , flow "not found in archive from"
         , style Url (fromString $ T.unpack url) <> "."
         ]
  pretty (FileTypeInArchiveInvalid e url) =
    "[S-5046]"
    <> line
    <> fillSep
         [ flow "Invalid file type for tar entry named"
         , fromString (Tar.entryPath e)
         , flow "downloaded from"
         , style Url (fromString $ T.unpack url) <> "."
         ]
  pretty (BinaryUpgradeOnOSUnsupported os) =
    "[S-4132]"
    <> line
    <> fillSep
         [ flow "Binary upgrade not yet supported on OS:"
         , pretty os <> "."
         ]
  pretty (BinaryUpgradeOnArchUnsupported arch) =
    "[S-3249]"
    <> line
    <> fillSep
         [ flow "Binary upgrade not yet supported on architecture:"
         , pretty arch <> "."
         ]
  pretty (ExistingMSYS2NotDeleted destDir e) =
    "[S-4230]"
    <> line
    <> fillSep
         [ flow "Could not delete existing MSYS2 directory:"
         , pretty destDir <> "."
         , flow "Stack encountered the following error:"
         ]
    <> blankLine
    <> string (displayException e)

instance Exception SetupPrettyException

-- | Type representing exceptions thrown by 'performPathChecking'
data PerformPathCheckingException
  = ProcessExited ExitCode String [String]
  deriving (Show, Typeable)

instance Exception PerformPathCheckingException where
  displayException (ProcessExited ec cmd args) = concat
    [ "Error: [S-1991]\n"
    , "Process exited with "
    , displayException ec
    , ": "
    , unwords (cmd:args)
    ]

-- | Default location of the stack-setup.yaml file
defaultSetupInfoYaml :: String
defaultSetupInfoYaml =
  "https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/stack-setup-2.yaml"

-- | Type representing setup configurations.
data SetupOpts = SetupOpts
  { installGhcIfMissing :: !Bool
  , installMsysIfMissing :: !Bool
  , useSystem :: !Bool
    -- ^ Should we use a system compiler installation, if available?
  , wantedCompiler :: !WantedCompiler
  , compilerCheck :: !VersionCheck
  , configFile :: !(Maybe (Path Abs File))
    -- ^ If we got the desired GHC version from that configuration file, which
    -- may be either a user-specific global or a project-level one.
  , forceReinstall :: !Bool
  , sanityCheck :: !Bool
    -- ^ Run a sanity check on the selected GHC
  , skipGhcCheck :: !Bool
    -- ^ Don't check for a compatible GHC version/architecture
  , skipMsys :: !Bool
    -- ^ Do not use a custom msys installation on Windows
  , resolveMissingGHC :: !(Maybe StyleDoc)
    -- ^ Message shown to user for how to resolve the missing GHC
  , ghcBindistURL :: !(Maybe String)
    -- ^ Alternate GHC binary distribution (requires custom GHCVariant)
  }
  deriving Show

-- | Modify the environment variables (like PATH) appropriately, possibly doing
-- installation too
setupEnv ::
     NeedTargets
  -> BuildOptsCLI
  -> Maybe StyleDoc
     -- ^ Message to give user when necessary GHC is not available.
  -> RIO BuildConfig EnvConfig
setupEnv needTargets buildOptsCLI mResolveMissingGHC = do
  config <- view configL
  bc <- view buildConfigL
  -- We are indifferent as to whether the configuration file is a
  -- user-specific global or a project-level one.
  let eitherConfigFile = EE.fromEither bc.configFile
  platform <- view platformL
  wcVersion <- view wantedCompilerVersionL
  actual <- either throwIO pure $ wantedToActual wcVersion
  let wc = actual^.whichCompilerL
      sopts = SetupOpts
        { installGhcIfMissing = config.installGHC
        , installMsysIfMissing = config.installMsys
        , useSystem = config.systemGHC
        , wantedCompiler = wcVersion
        , compilerCheck = config.compilerCheck
        , configFile = Just eitherConfigFile
        , forceReinstall = False
        , sanityCheck = False
        , skipGhcCheck = config.skipGHCCheck
        , skipMsys = config.skipMsys
        , resolveMissingGHC = mResolveMissingGHC
        , ghcBindistURL = Nothing
        }

  (compilerPaths, ghcBin) <- ensureCompilerAndMsys sopts
  let compilerVer = compilerPaths.compilerVersion

  -- Modify the initial environment to include the GHC path, if a local GHC
  -- is being used
  menv0 <- view processContextL
  env <- either throwM (pure . removeHaskellEnvVars)
           $ augmentPathMap
               (map toFilePath ghcBin.bins)
               (view envVarsL menv0)
  menv <- mkProcessContext env

  logDebug "Resolving package entries"

  (sourceMap, sourceMapHash) <- runWithGHC menv compilerPaths $ do
    smActual <- actualFromGhc bc.smWanted compilerVer
    let actualPkgs = Map.keysSet smActual.deps <>
                     Map.keysSet smActual.project
        prunedActual = smActual
          { globals = pruneGlobals smActual.globals actualPkgs }
        haddockDeps = shouldHaddockDeps config.build
    targets <- parseTargets needTargets haddockDeps buildOptsCLI prunedActual
    sourceMap <- loadSourceMap targets buildOptsCLI smActual
    sourceMapHash <- hashSourceMapData buildOptsCLI sourceMap
    pure (sourceMap, sourceMapHash)

  fileDigestCache <- newFileDigestCache

  let envConfig0 = EnvConfig
        { buildConfig = bc
        , buildOptsCLI
        , fileDigestCache
        , sourceMap
        , sourceMapHash
        , compilerPaths
        }

  -- extra installation bin directories
  mkDirs <- runRIO envConfig0 extraBinDirs
  let mpath = Map.lookup "PATH" env
  depsPath <-
    either throwM pure $ augmentPath (toFilePath <$> mkDirs False) mpath
  localsPath <-
    either throwM pure $ augmentPath (toFilePath <$> mkDirs True) mpath

  deps <- runRIO envConfig0 packageDatabaseDeps
  runWithGHC menv compilerPaths $ createDatabase compilerPaths.pkg deps
  localdb <- runRIO envConfig0 packageDatabaseLocal
  runWithGHC menv compilerPaths $ createDatabase compilerPaths.pkg localdb
  extras <- runReaderT packageDatabaseExtra envConfig0
  let mkGPP locals =
        mkGhcPackagePath locals localdb deps extras compilerPaths.globalDB

  distDir <- runReaderT distRelativeDir envConfig0 >>= canonicalizePath

  mExecutablePath <- view mExecutablePathL

  mGhcRtsEnvVar <- liftIO $ lookupEnv "GHCRTS"

  envRef <- liftIO $ newIORef Map.empty
  let msysEnv = maybe "" (T.pack . show) config.msysEnvironment
      getProcessContext' es = do
        m <- readIORef envRef
        case Map.lookup es m of
          Just eo -> pure eo
          Nothing -> do
            eo <- mkProcessContext
              $ Map.insert
                  "PATH"
                  (if es.includeLocals then localsPath else depsPath)
              $ (if es.includeGhcPackagePath
                   then
                     Map.insert
                       (ghcPkgPathEnvVar wc)
                       (mkGPP es.includeLocals)
                   else id)

              $ (if es.stackExe
                   then maybe
                     -- We don't throw an exception if there is no Stack
                     -- executable path, so that buildConfigCompleter does not
                     -- need to specify a path.
                     id
                     ( \executablePath -> Map.insert
                         "STACK_EXE"
                         (T.pack $ toFilePath executablePath)
                     )
                     mExecutablePath
                   else id)

              $ (if es.localeUtf8
                   then Map.union utf8EnvVars
                   else id)

              $ case (sopts.skipMsys, platform) of
                  (False, Platform Cabal.I386   Cabal.Windows) ->
                    Map.insert "MSYSTEM" msysEnv
                  (False, Platform Cabal.X86_64 Cabal.Windows) ->
                    Map.insert "MSYSTEM" msysEnv
                  _ -> id

              -- See https://github.com/commercialhaskell/stack/issues/3444
              $ case (es.keepGhcRts, mGhcRtsEnvVar) of
                  (True, Just ghcRts) -> Map.insert "GHCRTS" (T.pack ghcRts)
                  _ -> id

              -- For reasoning and duplication, see:
              -- https://github.com/commercialhaskell/stack/issues/70
              $ Map.insert
                  "HASKELL_PACKAGE_SANDBOX"
                  (T.pack $ toFilePathNoTrailingSep deps)
              $ Map.insert "HASKELL_PACKAGE_SANDBOXES"
                  (T.pack $ if es.includeLocals
                    then intercalate [searchPathSeparator]
                           [ toFilePathNoTrailingSep localdb
                           , toFilePathNoTrailingSep deps
                           , ""
                           ]
                    else intercalate [searchPathSeparator]
                           [ toFilePathNoTrailingSep deps
                           , ""
                           ])
              $ Map.insert
                  "HASKELL_DIST_DIR"
                  (T.pack $ toFilePathNoTrailingSep distDir)

                -- Make sure that any .ghc.environment files
                -- are ignored, since we're setting up our
                -- own package databases. See
                -- https://github.com/commercialhaskell/stack/issues/4706
              $ (case compilerPaths.compilerVersion of
                  ACGhc version | version >= mkVersion [8, 4, 4] ->
                    Map.insert "GHC_ENVIRONMENT" "-"
                  _ -> id)

                env

            () <- atomicModifyIORef envRef $ \m' ->
                (Map.insert es eo m', ())
            pure eo

  envOverride <- liftIO $ getProcessContext' minimalEnvSettings
  pure EnvConfig
    { buildConfig = bc
        { config = addIncludeLib ghcBin
                 $ set processContextL envOverride
                    (view configL bc)
            { processContextSettings = getProcessContext'
            }
        }
    , buildOptsCLI
    , fileDigestCache
    , sourceMap
    , sourceMapHash
    , compilerPaths
    }

-- | A modified env which we know has an installed compiler on the PATH.
data WithGHC env = WithGHC !CompilerPaths !env

insideL :: Lens' (WithGHC env) env
insideL = lens (\(WithGHC _ x) -> x) (\(WithGHC cp _) -> WithGHC cp)

instance HasLogFunc env => HasLogFunc (WithGHC env) where
  logFuncL = insideL . logFuncL

instance HasRunner env => HasRunner (WithGHC env) where
  runnerL = insideL . runnerL

instance HasProcessContext env => HasProcessContext (WithGHC env) where
  processContextL = insideL . processContextL

instance HasStylesUpdate env => HasStylesUpdate (WithGHC env) where
  stylesUpdateL = insideL . stylesUpdateL

instance HasTerm env => HasTerm (WithGHC env) where
  useColorL = insideL . useColorL
  termWidthL = insideL . termWidthL

instance HasPantryConfig env => HasPantryConfig (WithGHC env) where
  pantryConfigL = insideL . pantryConfigL

instance HasConfig env => HasPlatform (WithGHC env) where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasConfig env => HasGHCVariant (WithGHC env) where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasConfig env => HasConfig (WithGHC env) where
  configL = insideL . configL

instance HasBuildConfig env => HasBuildConfig (WithGHC env) where
  buildConfigL = insideL . buildConfigL

instance HasCompiler (WithGHC env) where
  compilerPathsL = to (\(WithGHC cp _) -> cp)

-- | Set up a modified environment which includes the modified PATH that GHC can
-- be found on. This is needed for looking up global package information and ghc
-- fingerprint (result from 'ghc --info').
runWithGHC ::
     HasConfig env
  => ProcessContext
  -> CompilerPaths
  -> RIO (WithGHC env) a
  -> RIO env a
runWithGHC pc cp inner = do
  env <- ask
  let envg
        = WithGHC cp $
          set envOverrideSettingsL (\_ -> pure pc) $
          set processContextL pc env
  runRIO envg inner

-- | A modified environment which we know has MSYS2 on the PATH.
newtype WithMSYS env = WithMSYS env

insideMSYSL :: Lens' (WithMSYS env) env
insideMSYSL = lens (\(WithMSYS x) -> x) (\(WithMSYS _) -> WithMSYS)

instance HasLogFunc env => HasLogFunc (WithMSYS env) where
  logFuncL = insideMSYSL . logFuncL

instance HasRunner env => HasRunner (WithMSYS env) where
  runnerL = insideMSYSL . runnerL

instance HasProcessContext env => HasProcessContext (WithMSYS env) where
  processContextL = insideMSYSL . processContextL

instance HasStylesUpdate env => HasStylesUpdate (WithMSYS env) where
  stylesUpdateL = insideMSYSL . stylesUpdateL

instance HasTerm env => HasTerm (WithMSYS env) where
  useColorL = insideMSYSL . useColorL
  termWidthL = insideMSYSL . termWidthL

instance HasPantryConfig env => HasPantryConfig (WithMSYS env) where
  pantryConfigL = insideMSYSL . pantryConfigL

instance HasConfig env => HasPlatform (WithMSYS env) where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasConfig env => HasGHCVariant (WithMSYS env) where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasConfig env => HasConfig (WithMSYS env) where
  configL = insideMSYSL . configL

instance HasBuildConfig env => HasBuildConfig (WithMSYS env) where
  buildConfigL = insideMSYSL . buildConfigL

-- | Set up a modified environment which includes the modified PATH that MSYS2
-- can be found on.
runWithMSYS ::
     HasConfig env
  => Maybe ExtraDirs
  -> RIO (WithMSYS env) a
  -> RIO env a
runWithMSYS mmsysPaths inner = do
  env <- ask
  pc0 <- view processContextL
  pc <- case mmsysPaths of
    Nothing -> pure pc0
    Just msysPaths -> do
      envars <- either throwM pure $
        augmentPathMap
          (map toFilePath msysPaths.bins)
          (view envVarsL pc0)
      mkProcessContext envars
  let envMsys
        = WithMSYS $
          set envOverrideSettingsL (\_ -> pure pc) $
          set processContextL pc env
  runRIO envMsys inner

-- | special helper for GHCJS which needs an updated source map
-- only project dependencies should get included otherwise source map hash will
-- get changed and EnvConfig will become inconsistent
rebuildEnv ::
     EnvConfig
  -> NeedTargets
  -> Bool
  -> BuildOptsCLI
  -> RIO env EnvConfig
rebuildEnv envConfig needTargets haddockDeps boptsCLI = do
  let bc = envConfig.buildConfig
      cp = envConfig.compilerPaths
      compilerVer = envConfig.sourceMap.compiler
  runRIO (WithGHC cp bc) $ do
    smActual <- actualFromGhc bc.smWanted compilerVer
    let actualPkgs =
          Map.keysSet smActual.deps <> Map.keysSet smActual.project
        prunedActual = smActual
          { globals = pruneGlobals smActual.globals actualPkgs }
    targets <- parseTargets needTargets haddockDeps boptsCLI prunedActual
    sourceMap <- loadSourceMap targets boptsCLI smActual
    pure $ envConfig
      { sourceMap = sourceMap
      , buildOptsCLI = boptsCLI
      }

-- | Some commands (script, ghci and exec) set targets dynamically
-- see also the note about only local targets for rebuildEnv
withNewLocalBuildTargets ::
     HasEnvConfig  env
  => [Text]
  -> RIO env a
  -> RIO env a
withNewLocalBuildTargets targets f = do
  envConfig <- view envConfigL
  haddockDeps <- view $ configL . to (.build) . to shouldHaddockDeps
  let boptsCLI = envConfig.buildOptsCLI
  envConfig' <-
    rebuildEnv envConfig NeedTargets haddockDeps $ boptsCLI
      { targetsCLI = targets}
  local (set envConfigL envConfig') f

-- | Add the include and lib paths to the given Config
addIncludeLib :: ExtraDirs -> Config -> Config
addIncludeLib extraDirs config = config
  { extraIncludeDirs =
      config.extraIncludeDirs ++ map toFilePathNoTrailingSep extraDirs.includes
  , extraLibDirs =
      config.extraLibDirs ++ map toFilePathNoTrailingSep extraDirs.libs
  }

-- | Ensure both the compiler and the msys toolchain are installed and
-- provide the PATHs to add if necessary
ensureCompilerAndMsys ::
     (HasBuildConfig env, HasGHCVariant env)
  => SetupOpts
  -> RIO env (CompilerPaths, ExtraDirs)
ensureCompilerAndMsys sopts = do
  getSetupInfo' <- memoizeRef getSetupInfo
  mmsys2Tool <- ensureMsys sopts getSetupInfo'
  mmsysPaths <- maybe (pure Nothing) (fmap Just . toolExtraDirs) mmsys2Tool
  actual <- either throwIO pure $ wantedToActual sopts.wantedCompiler
  didWarn <- warnUnsupportedCompiler $ getGhcVersion actual
  -- Modify the initial environment to include the MSYS2 path, if MSYS2 is being
  -- used
  (cp, ghcPaths) <- runWithMSYS mmsysPaths $ ensureCompiler sopts getSetupInfo'

  warnUnsupportedCompilerCabal cp didWarn

  let paths = maybe ghcPaths (ghcPaths <>) mmsysPaths
  pure (cp, paths)

-- | See <https://github.com/commercialhaskell/stack/issues/4246>
warnUnsupportedCompiler ::
     (HasConfig env, HasTerm env)
  => Version
  -> RIO env Bool
warnUnsupportedCompiler ghcVersion = do
  notifyIfGhcUntested <- view $ configL . to (.notifyIfGhcUntested)
  if
    | ghcVersion < mkVersion [8, 4] -> do
        prettyWarnL
          [ flow "Stack will almost certainly fail with GHC below version 8.4, \
                 \requested"
          , fromString (versionString ghcVersion) <> "."
          , flow "Valiantly attempting to run anyway, but this is doomed."
          ]
        pure True
    | ghcVersion >= mkVersion [9, 11] && notifyIfGhcUntested -> do
        prettyWarnL
          [ flow "Stack has not been tested with GHC versions 9.12 and above, \
                 \and using"
          , fromString (versionString ghcVersion) <> ","
          , flow "this may fail."
          ]
        pure True
    | otherwise -> do
        logDebug "Asking for a supported GHC version"
        pure False

-- | See <https://github.com/commercialhaskell/stack/issues/4246>
warnUnsupportedCompilerCabal ::
     (HasConfig env, HasTerm env)
  => CompilerPaths
  -> Bool -- ^ already warned about GHC?
  -> RIO env ()
warnUnsupportedCompilerCabal cp didWarn = do
  unless didWarn $
    void $ warnUnsupportedCompiler $ getGhcVersion cp.compilerVersion
  let cabalVersion = cp.cabalVersion
  notifyIfCabalUntested <- view $ configL . to (.notifyIfCabalUntested)
  if
    | cabalVersion < mkVersion [2, 2] -> do
            -- Due to a bug, Stack 2.15.1 does not support Cabal < 2.
        let downgradeRecommendation = fillSep $
                 [ flow "Stack 2.15.5 or earlier" ]
              <> [ flow "(except Stack 2.15.1)" | cabalVersion < mkVersion [2] ]
        prettyWarnL
          [ flow "Stack uses the version of the Cabal package that comes with \
                 \the specified version of GHC. However, Stack no longer \
                 \supports such Cabal versions before 2.2. Version"
          , fromString (versionString cabalVersion)
          , flow "was found. This invocation of Stack may fail. To fix this, \
                 \either use"
          , downgradeRecommendation
          , flow "or use a snapshot that specifies a version of GHC that is \
                 \8.4 or later. Stackage LTS Haskell 12.0"
          , parens (style Shell "lts-12.0")
          , flow "or later or Nightly 2018-03-13"
          , parens (style Shell "nightly-2018-03-13")
          , flow "or later specify such GHC versions."
          ]
    | cabalVersion >= mkVersion [3, 13] && notifyIfCabalUntested ->
        prettyWarnL
          [ flow "Stack has not been tested with Cabal versions 3.14 and \
                 \above, but version"
          , fromString (versionString cabalVersion)
          , flow "was found, this may fail."
          ]
    | otherwise -> pure ()

-- | Ensure that the msys toolchain is installed if necessary and provide the
-- PATHs to add if necessary
ensureMsys ::
     HasBuildConfig env
  => SetupOpts
  -> Memoized SetupInfo
  -> RIO env (Maybe Tool)
ensureMsys sopts getSetupInfo' = do
  platform <- view platformL
  localPrograms <- view $ configL . to (.localPrograms)
  installed <- listInstalled localPrograms

  case platform of
    Platform _ Cabal.Windows | not sopts.skipMsys ->
      case getInstalledTool installed (mkPackageName "msys2") (const True) of
        Just tool -> pure (Just tool)
        Nothing
          | sopts.installMsysIfMissing -> do
              si <- runMemoized getSetupInfo'
              let msysDir = fillSep
                    [ style Dir "msys2-yyyymmdd"
                    , flow "(where yyyymmdd is the date-based version)"
                    ]
              osKey <- getOSKey "MSYS2" msysDir
              config <- view configL
              VersionedDownloadInfo version info <-
                case Map.lookup osKey si.msys2 of
                  Just x -> pure x
                  Nothing -> prettyThrowIO $ MSYS2NotFound osKey
              let tool = Tool (PackageIdentifier (mkPackageName "msys2") version)
              Just <$> downloadAndInstallTool
                         config.localPrograms
                         info
                         tool
                         (installMsys2Windows si)
          | otherwise -> do
              prettyWarnS "Stack is not using a Stack-supplied MSYS2."
              pure Nothing
    _ -> pure Nothing

installGhcBindist ::
     HasBuildConfig env
  => SetupOpts
  -> Memoized SetupInfo
  -> [Tool]
  -> RIO env (Tool, CompilerBuild)
installGhcBindist sopts getSetupInfo' installed = do
  Platform expectedArch _ <- view platformL
  let wanted = sopts.wantedCompiler
      isWanted =
        isWantedCompiler sopts.compilerCheck sopts.wantedCompiler
  config <- view configL
  ghcVariant <- view ghcVariantL
  wc <- either throwIO (pure . whichCompiler) $ wantedToActual wanted
  possibleCompilers <-
    case wc of
      Ghc -> do
        ghcBuilds <- getGhcBuilds
        forM ghcBuilds $ \ghcBuild -> do
          ghcPkgName <- parsePackageNameThrowing
            (  "ghc"
            ++ ghcVariantSuffix ghcVariant
            ++ compilerBuildSuffix ghcBuild
            )
          pure (getInstalledTool installed ghcPkgName (isWanted . ACGhc), ghcBuild)
  let existingCompilers = concatMap
        (\(installedCompiler, compilerBuild) ->
          case (installedCompiler, sopts.forceReinstall) of
            (Just tool, False) -> [(tool, compilerBuild)]
            _ -> [])
        possibleCompilers
      globalOpts = config.runner.globalOpts
      wantedCompilerSetter
        | isJust globalOpts.compiler = CompilerAtCommandLine
        | isJust globalOpts.snapshot = SnapshotAtCommandLine
        | otherwise = YamlConfiguration sopts.configFile
  logDebug $
       "Found already installed GHC builds: "
    <> mconcat (intersperse ", " (map (fromString . compilerBuildName . snd) existingCompilers))
  case existingCompilers of
    (tool, build_):_ -> pure (tool, build_)
    []
      | sopts.installGhcIfMissing -> do
          si <- runMemoized getSetupInfo'
          downloadAndInstallPossibleCompilers
            (map snd possibleCompilers)
            si
            sopts.wantedCompiler
            sopts.compilerCheck
            sopts.ghcBindistURL
      | otherwise -> do
          let suggestion =
                fromMaybe defaultSuggestion sopts.resolveMissingGHC
              defaultSuggestion = fillSep
                [ flow "To install the correct version of GHC into the \
                       \subdirectory for the specified platform in Stack's \
                       \directory for local tools"
                , parens (pretty config.localPrograms) <> ","
                , flow "try running"
                , style Shell (flow "stack setup")
                , flow "or use the"
                , style Shell "--install-ghc"
                , flow "flag. To use your system GHC installation, run"
                ,    style
                       Shell
                       (flow "stack config set system-ghc --global true")
                  <> ","
                , flow "or use the"
                , style Shell "--system-ghc"
                , "flag."
                ]

          prettyThrowM $ CompilerVersionMismatch
            Nothing -- FIXME ((\(x, y, _) -> (x, y)) <$> msystem)
            (sopts.wantedCompiler, expectedArch)
            ghcVariant
            (case possibleCompilers of
              [] -> CompilerBuildStandard
              (_, compilerBuild):_ -> compilerBuild)
            sopts.compilerCheck
            wantedCompilerSetter
            suggestion

-- | Ensure compiler is installed.
ensureCompiler ::
     forall env. (HasConfig env, HasBuildConfig env, HasGHCVariant env)
  => SetupOpts
  -> Memoized SetupInfo
  -> RIO (WithMSYS env) (CompilerPaths, ExtraDirs)
ensureCompiler sopts getSetupInfo' = do
  let wanted = sopts.wantedCompiler
  wc <- either throwIO (pure . whichCompiler) $ wantedToActual wanted

  hook <- ghcInstallHook
  hookIsExecutable <- handleIO (\_ -> pure False) $ if osIsWindows
    then doesFileExist hook  -- can't really detect executable on windows, only
                             -- file extension
    else executable <$> getPermissions hook

  Platform expectedArch _ <- view platformL

  let canUseCompiler cp
        | sopts.skipGhcCheck = pure cp
        | not $ isWanted cp.compilerVersion =
            prettyThrowIO UnwantedCompilerVersion
        | cp.arch /= expectedArch = prettyThrowIO UnwantedArchitecture
        | otherwise = pure cp
      isWanted =
        isWantedCompiler sopts.compilerCheck sopts.wantedCompiler

  let checkCompiler :: Path Abs File -> RIO (WithMSYS env) (Maybe CompilerPaths)
      checkCompiler compiler = do
        eres <- tryAny $
          pathsFromCompiler wc CompilerBuildStandard False compiler >>= canUseCompiler
        case eres of
          Left e -> do
            logDebug $
                 "Not using compiler at "
              <> displayShow (toFilePath compiler)
              <> ": "
              <> displayShow e
            pure Nothing
          Right cp -> pure $ Just cp

  mcp <-
    if | sopts.useSystem -> do
          logDebug "Getting system compiler version"
          runConduit $
            sourceSystemCompilers wanted .|
            concatMapMC checkCompiler .|
            await
       | hookIsExecutable -> do
        -- if the hook fails, we fall through to stacks sandboxed installation
          hookGHC <- runGHCInstallHook sopts hook
          maybe (pure Nothing) checkCompiler hookGHC
       | otherwise -> pure Nothing
  case mcp of
    Nothing -> ensureSandboxedCompiler sopts getSetupInfo'
    Just cp -> do
      let paths = ExtraDirs
            { bins = [parent cp.compiler]
            , includes = []
            , libs = []
            }
      pure (cp, paths)

-- | Runs @STACK_ROOT\/hooks\/ghc-install.sh@.
--
-- Reads and possibly validates the output of the process as the GHC binary and
-- returns it.
runGHCInstallHook ::
     HasBuildConfig env
  => SetupOpts
  -> Path Abs File
  -> RIO env (Maybe (Path Abs File))
runGHCInstallHook sopts hook = do
  logDebug "Getting hook installed compiler version"
  let wanted = sopts.wantedCompiler
  menv0 <- view processContextL
  menv <- mkProcessContext (Map.union (wantedCompilerToEnv wanted) $
    removeHaskellEnvVars (view envVarsL menv0))
  (exit, out) <- withProcessContext menv $ proc "sh" [toFilePath hook] readProcessStdout
  case exit of
    ExitSuccess -> do
      let ghcPath = stripNewline . TL.unpack . TL.decodeUtf8With T.lenientDecode $ out
      case parseAbsFile ghcPath of
        Just compiler -> do
          when sopts.sanityCheck $ sanityCheck compiler
          logDebug ("Using GHC compiler at: " <> fromString (toFilePath compiler))
          pure (Just compiler)
        Nothing -> do
          prettyWarnL
            [ flow "Path to GHC binary is not a valid path:"
            , style Dir (fromString ghcPath) <> "."
            ]
          pure Nothing
    ExitFailure i -> do
      prettyWarnL
        [ flow "GHC install hook exited with code:"
        , style Error (fromString $ show i) <> "."
        ]
      pure Nothing
 where
  wantedCompilerToEnv :: WantedCompiler -> EnvVars
  wantedCompilerToEnv (WCGhc ver) =
    Map.fromList [ ("HOOK_GHC_TYPE", "bindist")
                 , ("HOOK_GHC_VERSION", T.pack (versionString ver))
                 ]
  wantedCompilerToEnv (WCGhcGit commit flavor) =
    Map.fromList [ ("HOOK_GHC_TYPE", "git")
                 , ("HOOK_GHC_COMMIT", commit)
                 , ("HOOK_GHC_FLAVOR", flavor)
                 , ("HOOK_GHC_FLAVOUR", flavor)
                 ]
  wantedCompilerToEnv (WCGhcjs ghcjs_ver ghc_ver) =
    Map.fromList [ ("HOOK_GHC_TYPE", "ghcjs")
                 , ("HOOK_GHC_VERSION", T.pack (versionString ghc_ver))
                 , ("HOOK_GHCJS_VERSION", T.pack (versionString ghcjs_ver))
                 ]
  newlines :: [Char]
  newlines = ['\n', '\r']

  stripNewline :: String -> String
  stripNewline = filter (`notElem` newlines)

ensureSandboxedCompiler ::
     HasBuildConfig env
  => SetupOpts
  -> Memoized SetupInfo
  -> RIO (WithMSYS env) (CompilerPaths, ExtraDirs)
ensureSandboxedCompiler sopts getSetupInfo' = do
  let wanted = sopts.wantedCompiler
  -- List installed tools
  config <- view configL
  let localPrograms = config.localPrograms
  installed <- listInstalled localPrograms
  logDebug $
       "Installed tools: \n - "
    <> mconcat (intersperse "\n - " (map (fromString . toolString) installed))
  (compilerTool, compilerBuild) <-
    case sopts.wantedCompiler of
     -- shall we build GHC from source?
     WCGhcGit commitId flavour ->
       buildGhcFromSource
         getSetupInfo'
         installed
         config.compilerRepository
         commitId
         flavour
     _ -> installGhcBindist sopts getSetupInfo' installed
  paths <- toolExtraDirs compilerTool

  wc <- either throwIO (pure . whichCompiler) $ wantedToActual wanted
  menv0 <- view processContextL
  m <- either throwM pure
     $ augmentPathMap (toFilePath <$> paths.bins) (view envVarsL menv0)
  menv <- mkProcessContext (removeHaskellEnvVars m)

  names <-
    case wanted of
      WCGhc version -> pure ["ghc-" ++ versionString version, "ghc"]
      WCGhcGit{} -> pure ["ghc"]
      WCGhcjs{} -> throwIO GhcjsNotSupported

  -- Previously, we used findExecutable to locate these executables. This was
  -- actually somewhat sloppy, as it could discover executables outside of the
  -- sandbox. This led to a specific issue on Windows with GHC 9.0.1. See
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/20074. Instead, now, we look
  -- on the paths specified only.
  let loop [] = prettyThrowIO $ SandboxedCompilerNotFound names paths.bins
      loop (x:xs) = do
        res <- liftIO $
          D.findExecutablesInDirectories (map toFilePath paths.bins) x
        case res of
          [] -> loop xs
          compiler:rest -> do
            unless (null rest) $ do
              prettyWarn $
                   flow "Found multiple candidate compilers:"
                <> line
                <> bulletedList (map fromString res)
                <> blankLine
                <> fillSep
                     [ flow "This usually indicates a failed installation. \
                        \Trying anyway with"
                     , fromString compiler
                     ]
            parseAbsFile compiler
  compiler <- withProcessContext menv $ do
    compiler <- loop names

    -- Run this here to ensure that the sanity check uses the modified
    -- environment, otherwise we may infect GHC_PACKAGE_PATH and break sanity
    -- checks.
    when sopts.sanityCheck $ sanityCheck compiler

    pure compiler

  cp <- pathsFromCompiler wc compilerBuild True compiler
  pure (cp, paths)

pathsFromCompiler ::
     forall env. HasConfig env
  => WhichCompiler
  -> CompilerBuild
  -> Bool
  -> Path Abs File -- ^ executable filepath
  -> RIO env CompilerPaths
pathsFromCompiler wc build sandboxed compiler =
  withCache $ handleAny onErr $ do
    let dir = toFilePath $ parent compiler

        suffixNoVersion
          | osIsWindows = ".exe"
          | otherwise = ""
        msuffixWithVersion = do
          let prefix =
                case wc of
                  Ghc -> "ghc-"
          fmap ("-" ++) $ stripPrefix prefix $ toFilePath $ filename compiler
        suffixes = maybe id (:) msuffixWithVersion [suffixNoVersion]
        findHelper :: (WhichCompiler -> [String]) -> RIO env (Path Abs File)
        findHelper getNames = do
          toTry <- mapM
                     parseAbsFile
                     [ dir ++ name ++ suffix
                     | suffix <- suffixes, name <- getNames wc
                     ]
          let loop [] = throwIO $ PrettyException $ ExecutableNotFound toTry
              loop (guessedPath:rest) = do
                exists <- doesFileExist guessedPath
                if exists
                  then pure guessedPath
                  else loop rest
          prettyDebug $
               flow "Looking for executable(s):"
            <> line
            <> bulletedList (map pretty toTry)
          loop toTry
    pkg <- fmap GhcPkgExe $ findHelper $ \case
                               Ghc -> ["ghc-pkg"]

    menv0 <- view processContextL
    menv <- mkProcessContext (removeHaskellEnvVars (view envVarsL menv0))

    interpreter <- findHelper $
                   \case
                      Ghc -> ["runghc"]
    haddock <- findHelper $
               \case
                  Ghc -> ["haddock", "haddock-ghc"]
    ghcInfo <- proc (toFilePath compiler) ["--info"]
            $ fmap (toStrictBytes . fst) . readProcess_
    infotext <-
      case decodeUtf8' ghcInfo of
        Left e -> prettyThrowIO $ GHCInfoNotValidUTF8 e
        Right info -> pure info
    infoPairs :: [(String, String)] <-
      case readMaybe $ T.unpack infotext of
        Nothing -> prettyThrowIO GHCInfoNotListOfPairs
        Just infoPairs -> pure infoPairs
    let infoMap = Map.fromList infoPairs

    eglobaldb <- tryAny $
      case Map.lookup "Global Package DB" infoMap of
        Nothing -> prettyThrowIO GHCInfoMissingGlobalPackageDB
        Just db -> parseAbsDir db

    arch <-
      case Map.lookup "Target platform" infoMap of
        Nothing -> prettyThrowIO GHCInfoMissingTargetPlatform
        Just targetPlatform ->
          case simpleParse $ takeWhile (/= '-') targetPlatform of
            Nothing ->
              prettyThrowIO $ GHCInfoTargetPlatformInvalid targetPlatform
            Just arch -> pure arch
    compilerVersion <-
      case wc of
        Ghc ->
          case Map.lookup "Project version" infoMap of
            Nothing -> do
              prettyWarnS "Key 'Project version' not found in GHC info."
              getCompilerVersion wc compiler
            Just versionString' -> ACGhc <$> parseVersionThrowing versionString'
    globalDB <-
      case eglobaldb of
        Left e -> do
          prettyWarn $
               flow "Stack failed to parse the global DB from GHC info."
            <> blankLine
            <> flow "While parsing, Stack encountered the error:"
            <> blankLine
            <> string (show e)
            <> blankLine
            <> flow "Asking ghc-pkg directly."
          withProcessContext menv $ getGlobalDB pkg
        Right x -> pure x

    globalDump <- withProcessContext menv $ globalsFromDump pkg
    cabalVersion <-
      case Map.lookup cabalPackageName globalDump of
        Nothing -> prettyThrowIO $ CabalNotFound compiler
        Just dp -> pure $ pkgVersion dp.packageIdent

    pure CompilerPaths
      { build
      , arch
      , sandboxed
      , compilerVersion
      , compiler
      , pkg
      , interpreter
      , haddock
      , cabalVersion
      , globalDB
      , ghcInfo
      , globalDump
      }
 where
  onErr = throwIO . PrettyException . InvalidGhcAt compiler

  withCache inner = do
    eres <- tryAny $ loadCompilerPaths compiler build sandboxed
    mres <-
      case eres of
        Left e -> do
          prettyWarn $
               flow "Trouble loading CompilerPaths cache:"
            <> blankLine
            <> string (displayException e)
          pure Nothing
        Right x -> pure x
    case mres of
      Just cp -> cp <$ logDebug "Loaded compiler information from cache"
      Nothing -> do
        cp <- inner
        saveCompilerPaths cp `catchAny` \e ->
          prettyWarn $
               flow "Unable to save CompilerPaths cache:"
            <> blankLine
            <> string (displayException e)
        pure cp

buildGhcFromSource ::
     forall env. ( HasTerm env, HasProcessContext env, HasBuildConfig env)
  => Memoized SetupInfo
  -> [Tool]
  -> CompilerRepository
  -> Text
     -- ^ Commit ID.
  -> Text
     -- ^ Hadrain flavour.
  -> RIO (WithMSYS env) (Tool, CompilerBuild)
buildGhcFromSource getSetupInfo' installed (CompilerRepository url) commitId flavour = do
  config <- view configL
  let compilerTool = ToolGhcGit commitId flavour
  -- detect when the correct GHC is already installed
  if compilerTool `elem` installed
    then pure (compilerTool, CompilerBuildStandard)
    else
      -- clone the repository and execute the given commands
      withRepo (SimpleRepo url commitId RepoGit) $ do
        -- withRepo is guaranteed to set workingDirL, so let's get it
        mcwd <- traverse parseAbsDir =<< view workingDirL
        cwd <- maybe (throwIO WorkingDirectoryInvalidBug) pure mcwd
        let threads = config.jobs
            relFileHadrianStackDotYaml' = toFilePath relFileHadrianStackDotYaml
            ghcBootScriptPath = cwd </> ghcBootScript
            boot = if osIsWindows
              then proc "python3" ["boot"] runProcess_
              else
                proc (toFilePath ghcBootScriptPath) [] runProcess_
            stack args = proc "stack" args'' runProcess_
             where
              args'' = "--stack-yaml=" <> relFileHadrianStackDotYaml' : args'
              -- If a snapshot is specified on the command line, Stack will
              -- apply it. This allows the snapshot specified in Hadrian's
              -- stack.yaml file to be overridden.
              args' = maybe args addSnapshot config.snapshot
              addSnapshot snapshot = "--snapshot=" <> show snapshot : args
            happy = stack ["install", "happy"]
            alex = stack ["install", "alex"]
            -- Executed in the Stack environment, because GHC is required.
            configure = stack ("exec" : "--" : ghcConfigure)
            ghcConfigure
              | osIsWindows = ghcConfigureWindows
              | osIsMacOS = ghcConfigureMacOS
              | otherwise   = ghcConfigurePosix
            hadrianScripts
              | osIsWindows = hadrianScriptsWindows
              | otherwise   = hadrianScriptsPosix
            hadrianArgs = fmap T.unpack
              [ "-j" <> tshow threads   -- parallel build
              , "--flavour=" <> flavour -- selected flavour
              , "binary-dist"
              ]
        foundHadrianPaths <-
          filterM doesFileExist $ (cwd </>) <$> hadrianScripts
        hadrianPath <- maybe (prettyThrowIO HadrianScriptNotFound) pure $
          listToMaybe foundHadrianPaths
        exists <- doesFileExist ghcBootScriptPath
        unless exists $ prettyThrowIO GhcBootScriptNotFound
        ensureConfigureScript cwd
        logInfo "Running GHC boot script..."
        boot
        doesExecutableExist "happy" >>= \case
          True -> logInfo "happy executable installed on the PATH."
          False -> do
            logInfo "Installing happy executable..."
            happy
        doesExecutableExist "alex" >>= \case
          True -> logInfo "alex executable installed on the PATH."
          False -> do
            logInfo "Installing alex executable..."
            alex
        logInfo "Running GHC configure script..."
        configure
        logSticky $
             "Building GHC from source with `"
          <> display flavour
          <> "` flavour. It can take a long time (more than one hour)..."
        -- We need to provide an absolute path to the script since the process
        -- package only sets working directory _after_ discovering the
        -- executable.
        proc (toFilePath hadrianPath) hadrianArgs runProcess_

        -- find the bindist and install it
        bindistPath <- parseRelDir "_build/bindist"
        (_,files) <- listDir (cwd </> bindistPath)
        let isBindist p = do
              extension <- fileExtension (filename p)

              pure $
                   "ghc-" `isPrefixOf` toFilePath (filename p)
                && extension == ".xz"

        mbindist <- filterM isBindist files
        case mbindist of
          [bindist] -> do
            let bindist' = T.pack (toFilePath bindist)
                dlinfo = DownloadInfo
                  { url = bindist'
                    -- we can specify a filepath instead of a URL
                  , contentLength = Nothing
                  , sha1 = Nothing
                  , sha256 = Nothing
                  }
                ghcdlinfo = GHCDownloadInfo mempty mempty dlinfo
                installer
                   | osIsWindows = installGHCWindows
                   | otherwise   = installGHCPosix ghcdlinfo
            si <- runMemoized getSetupInfo'
            _ <- downloadAndInstallTool
              config.localPrograms
              dlinfo
              compilerTool
              (installer si)
            pure (compilerTool, CompilerBuildStandard)
          _ -> do
            forM_ files (logDebug . fromString . (" - " ++) . toFilePath)
            prettyThrowIO HadrianBindistNotFound

-- | Determine which GHC builds to use depending on which shared libraries are
-- available on the system.
getGhcBuilds :: HasConfig env => RIO env [CompilerBuild]
getGhcBuilds = do
  config <- view configL
  case config.ghcBuild of
    Just ghcBuild -> pure [ghcBuild]
    Nothing -> determineGhcBuild
 where
  -- The GHCup project is also interested in the algorithm below, as it copies
  -- it at GHCup.Platform.getStackGhcBuilds. If you change this algorithm, it
  -- would be a courtesy to bring that to the attention of the GHCup project
  -- maintainers.
  determineGhcBuild = do
    -- TODO: a more reliable, flexible, and data driven approach would be to
    -- actually download small "test" executables (from setup-info) that link to
    -- the same gmp/tinfo versions that GHC does (i.e. built in same environment
    -- as the GHC bindist). The algorithm would go something like this:
    --
    -- check for previous 'uname -a'/`ldconfig -p` plus compiler version/variant
    -- in cache.
    -- if cached, then use that as suffix
    -- otherwise:
    --     download setup-info
    --     go through all with right prefix for os/version/variant
    --     first try "standard" (no extra suffix), then the rest
    --         download "compatibility check" exe if not already downloaded
    --         try running it
    --         if successful, then choose that
    --             cache compiler suffix with the uname -a and
    --               ldconfig -p output hash plus compiler version
    --
    -- Of course, could also try to make a static GHC bindist instead of all
    -- this rigamarole.

    platform <- view platformL
    case platform of
      Platform _ Cabal.Linux -> do
        -- Some systems don't have ldconfig in the PATH, so make sure to look in
        -- /sbin and /usr/sbin as well
        let sbinEnv m = Map.insert
              "PATH"
              ("/sbin:/usr/sbin" <> maybe "" (":" <>) (Map.lookup "PATH" m))
              m
        eldconfigOut <- withModifyEnvVars sbinEnv
          $ proc "ldconfig" ["-p"]
          $ tryAny . fmap fst . readProcess_
        let firstWords = case eldconfigOut of
              Right ldconfigOut -> mapMaybe (listToMaybe . T.words) $
                T.lines $ T.decodeUtf8With T.lenientDecode
                        $ LBS.toStrict ldconfigOut
              Left _ -> []
            checkLib lib
              | libT `elem` firstWords = do
                  logDebug $
                       "Found shared library "
                    <> libD
                    <> " in 'ldconfig -p' output"
                  pure True
              | osIsWindows =
                  -- Cannot parse /usr/lib on Windows
                  pure False
              | otherwise = hasMatches lib usrLibDirs
              -- This is a workaround for the fact that libtinfo.so.x doesn't
              -- appear in the 'ldconfig -p' output on Arch or Slackware even
              -- when it exists. There doesn't seem to be an easy way to get the
              -- true list of directories to scan for shared libs, but this
              -- works for our particular cases.
             where
              libD = fromString (toFilePath lib)
              libT = T.pack (toFilePath lib)
            hasMatches lib dirs = do
              matches <- filterM (doesFileExist . (</> lib)) dirs
              case matches of
                [] ->
                     logDebug
                       (  "Did not find shared library "
                       <> libD
                       )
                  >> pure False
                (path:_) ->
                     logDebug
                       (  "Found shared library "
                       <> libD
                       <> " in "
                       <> fromString (Path.toFilePath path)
                       )
                  >> pure True
             where
              libD = fromString (toFilePath lib)
            getLibc6Version = do
              elddOut <-
                -- On Alpine Linux, 'ldd --version' will send output to stderr,
                -- which we wish to smother.
                proc "ldd" ["--version"] $ tryAny . readProcess_
              pure $ case elddOut of
                Right (lddOut, _) ->
                  let lddOut' =
                        decodeUtf8Lenient (LBS.toStrict lddOut)
                  in  case P.parse lddVersion lddOut' of
                        P.Done _ result -> Just result
                        _ -> Nothing
                Left _ -> Nothing
            -- Assumes the first line of ldd has the format:
            --
            -- ldd (...) nn.nn
            --
            -- where nn.nn corresponds to the version of libc6.
            lddVersion :: P.Parser Version
            lddVersion = do
              P.skipWhile (/= ')')
              P.skip (== ')')
              P.skipSpace
              lddMajorVersion <- P.decimal
              P.skip (== '.')
              lddMinorVersion <- P.decimal
              P.skip (not . isDigit)
              pure $ mkVersion [ lddMajorVersion, lddMinorVersion ]
        hasMusl <- hasMatches relFileLibcMuslx86_64So1 libDirs
        mLibc6Version <- getLibc6Version
        case mLibc6Version of
          Just libc6Version -> logDebug $
               "Found shared library libc6 in version: "
            <> fromString (versionString libc6Version)
          Nothing -> logDebug
            "Did not find a version of shared library libc6."
        let hasLibc6_2_32 =
              maybe False (>= mkVersion [2 , 32]) mLibc6Version
        hastinfo5 <- checkLib relFileLibtinfoSo5
        hastinfo6 <- checkLib relFileLibtinfoSo6
        hasncurses6 <- checkLib relFileLibncurseswSo6
        hasgmp5 <- checkLib relFileLibgmpSo10
        hasgmp4 <- checkLib relFileLibgmpSo3
        let libComponents = if hasMusl
              then
                [ ["musl"] ]
              else
                concat
                  [ if hastinfo6 && hasgmp5
                      then
                        if hasLibc6_2_32
                          then [["tinfo6"]]
                          else [["tinfo6-libc6-pre232"]]
                      else [[]]
                  , [ [] | hastinfo5 && hasgmp5 ]
                  , [ ["ncurses6"] | hasncurses6 && hasgmp5 ]
                  , [ ["gmp4"] | hasgmp4 ]
                  ]
        useBuilds $ map
          (\c -> case c of
            [] -> CompilerBuildStandard
            _ -> CompilerBuildSpecialized (intercalate "-" c))
          libComponents
      Platform _ Cabal.FreeBSD -> do
        let getMajorVer = readMaybe <=< headMaybe . splitOn "."
        majorVer <- getMajorVer <$> sysRelease
        if majorVer >= Just (12 :: Int)
          then
            useBuilds [CompilerBuildSpecialized "ino64"]
          else
            useBuilds [CompilerBuildStandard]
      Platform _ Cabal.OpenBSD -> do
        releaseStr <- mungeRelease <$> sysRelease
        useBuilds [CompilerBuildSpecialized releaseStr]
      _ -> useBuilds [CompilerBuildStandard]
  useBuilds builds = do
    logDebug $
         "Potential GHC builds: "
      <> mconcat (intersperse ", " (map (fromString . compilerBuildName) builds))
    pure builds

-- | Encode an OpenBSD version (like "6.1") into a valid argument for
-- CompilerBuildSpecialized, so "maj6-min1". Later version numbers are prefixed
-- with "r".
-- The result r must be such that "ghc-" ++ r is a valid package name,
-- as recognized by parsePackageNameFromString.
mungeRelease :: String -> String
mungeRelease = intercalate "-" . prefixMaj . splitOn "."
 where
  prefixFst pfx k (rev : revs) = (pfx ++ rev) : k revs
  prefixFst _ _ [] = []
  prefixMaj = prefixFst "maj" prefixMin
  prefixMin = prefixFst "min" (map ('r':))

sysRelease :: HasTerm env => RIO env String
sysRelease =
  handleIO
    ( \e -> do
        prettyWarn $
             flow "Could not query OS version:"
          <> blankLine
          <> string (displayException e)
        pure ""
    )
    (liftIO getRelease)

-- | Ensure Docker container-compatible \'stack\' executable is downloaded
ensureDockerStackExe :: HasConfig env => Platform -> RIO env (Path Abs File)
ensureDockerStackExe containerPlatform = do
  config <- view configL
  containerPlatformDir <-
    runReaderT platformOnlyRelDir (containerPlatform,PlatformVariantNone)
  let programsPath = config.localProgramsBase </> containerPlatformDir
      tool = Tool (PackageIdentifier (mkPackageName "stack") stackVersion)
  stackExeDir <- installDir programsPath tool
  let stackExePath = stackExeDir </> relFileStack
  stackExeExists <- doesFileExist stackExePath
  unless stackExeExists $ do
    prettyInfoL
      [ flow "Downloading Docker-compatible"
      , fromString stackProgName
      , "executable."
      ]
    sri <-
      downloadStackReleaseInfo
        Nothing
        Nothing
        (Just (versionString stackMinorVersion))
    platforms <-
      runReaderT preferredPlatforms (containerPlatform, PlatformVariantNone)
    downloadStackExe platforms sri stackExeDir False (const $ pure ())
  pure stackExePath

-- | Get all executables on the path that might match the wanted compiler
sourceSystemCompilers ::
     (HasLogFunc env, HasProcessContext env)
  => WantedCompiler
  -> ConduitT i (Path Abs File) (RIO env) ()
sourceSystemCompilers wanted = do
  searchPath <- view exeSearchPathL
  names <-
    case wanted of
      WCGhc version -> pure
        [ "ghc-" ++ versionString version
        , "ghc"
        ]
      WCGhcjs{} -> throwIO GhcjsNotSupported
      WCGhcGit{} -> pure [] -- only use sandboxed versions
  for_ names $ \name -> for_ searchPath $ \dir -> do
    fp <- resolveFile' $ addExe $ dir FP.</> name
    exists <- doesFileExist fp
    when exists $ yield fp
 where
  addExe
    | osIsWindows = (++ ".exe")
    | otherwise = id

-- | Download the most recent SetupInfo
getSetupInfo :: HasConfig env => RIO env SetupInfo
getSetupInfo = do
  config <- view configL
  let inlineSetupInfo = config.setupInfoInline
      locations' = config.setupInfoLocations
      locations = if null locations' then [defaultSetupInfoYaml] else locations'

  resolvedSetupInfos <- mapM loadSetupInfo locations
  pure (inlineSetupInfo <> mconcat resolvedSetupInfos)
 where
  loadSetupInfo urlOrFile = do
    bs <- case parseUrlThrow urlOrFile of
            Just req -> LBS.toStrict . getResponseBody <$> httpLbs req
            Nothing -> liftIO $ S.readFile urlOrFile
    WithJSONWarnings si warnings <- either throwM pure (Yaml.decodeEither' bs)
    when (urlOrFile /= defaultSetupInfoYaml) $
      logJSONWarnings urlOrFile warnings
    pure si

getInstalledTool ::
     [Tool]            -- ^ already installed
  -> PackageName       -- ^ package to find
  -> (Version -> Bool) -- ^ which versions are acceptable
  -> Maybe Tool
getInstalledTool installed name goodVersion = Tool <$>
  maximumByMaybe (comparing pkgVersion) (filterTools name goodVersion installed)

-- | Obtain and install the specified tool, using the specified download
-- information and installer. Also deletes the archive file for the tool after
-- installation.
downloadAndInstallTool ::
     (HasTerm env, HasBuildConfig env)
  => Path Abs Dir
     -- ^ Location of the directory for tools.
  -> DownloadInfo
     -- ^ Information about the file to obtain.
  -> Tool
     -- ^ The tool in question.
  -> (  Path Abs File
        -- Location of archive file.
     -> ArchiveType
        -- Type of archive file.
     -> Path Abs Dir
        -- Tempory directory to use.
     -> Path Abs Dir
        -- Destination directory for installed tool.
     -> RIO env ()
     )
     -- ^ Installer.
  -> RIO env Tool
downloadAndInstallTool programsDir downloadInfo tool installer = do
  ensureDir programsDir
  (file, at) <- downloadFromInfo programsDir downloadInfo tool
  dir <- installDir programsDir tool
  tempDir <- tempInstallDir programsDir tool
  liftIO $ ignoringAbsence (removeDirRecur tempDir)
  ensureDir tempDir
  unmarkInstalled programsDir tool
  installer file at tempDir dir
  markInstalled programsDir tool
  liftIO $ ignoringAbsence (removeDirRecur tempDir)
  liftIO $ ignoringAbsence (removeFile file)
  pure tool

-- Exceptions thrown by this function are caught by
-- 'downloadAndInstallPossibleCompilers'.
downloadAndInstallCompiler ::
     (HasBuildConfig env, HasGHCVariant env)
  => CompilerBuild
  -> SetupInfo
  -> WantedCompiler
  -> VersionCheck
  -> Maybe String
  -> RIO env Tool
downloadAndInstallCompiler ghcBuild si wanted@(WCGhc version) versionCheck mbindistURL = do
  ghcVariant <- view ghcVariantL
  (selectedVersion, downloadInfo) <- case mbindistURL of
    Just bindistURL -> do
      case ghcVariant of
        GHCCustom _ -> pure ()
        _ -> throwM RequireCustomGHCVariant
      pure
        ( version
        , GHCDownloadInfo mempty mempty DownloadInfo
            { url = T.pack bindistURL
            , contentLength = Nothing
            , sha1 = Nothing
            , sha256 = Nothing
            }
        )
    _ -> do
      ghcKey <- getGhcKey ghcBuild
      case Map.lookup ghcKey si.ghcByVersion of
        Nothing -> throwM $ UnknownOSKey ghcKey
        Just pairs_ ->
          getWantedCompilerInfo ghcKey versionCheck wanted ACGhc pairs_
  config <- view configL
  let installer =
        case config.platform of
          Platform _ Cabal.Windows -> installGHCWindows
          _ -> installGHCPosix downloadInfo
  prettyInfo $
    fillSep $
         flow "Preparing to install GHC"
      :  case ghcVariant of
           GHCStandard -> []
           v -> ["(" <> fromString (ghcVariantName v) <> ")"]
      <> case ghcBuild of
           CompilerBuildStandard -> []
           b -> ["(" <> fromString (compilerBuildName b) <> ")"]
      <> [ flow "to an isolated location. This will not interfere with any \
                \system-level installation."
         ]
  ghcPkgName <- parsePackageNameThrowing
    ("ghc" ++ ghcVariantSuffix ghcVariant ++ compilerBuildSuffix ghcBuild)
  let tool = Tool $ PackageIdentifier ghcPkgName selectedVersion
  downloadAndInstallTool
    config.localPrograms
    downloadInfo.downloadInfo
    tool
    (installer si)

downloadAndInstallCompiler _ _ WCGhcjs{} _ _ = throwIO GhcjsNotSupported

downloadAndInstallCompiler _ _ WCGhcGit{} _ _ =
  throwIO DownloadAndInstallCompilerError

-- Exceptions thrown by this function are caught by
-- 'downloadAndInstallPossibleCompilers'.
getWantedCompilerInfo ::
     (Ord k, MonadThrow m)
  => Text
  -> VersionCheck
  -> WantedCompiler
  -> (k -> ActualCompiler)
  -> Map k a
  -> m (k, a)
getWantedCompilerInfo key versionCheck wanted toCV pairs_ =
  case mpair of
    Just pair -> pure pair
    Nothing -> throwM $
      UnknownCompilerVersion
        (Set.singleton key)
        wanted
        (Set.fromList $ map toCV (Map.keys pairs_))
 where
  mpair =
    listToMaybe $
    sortOn (Down . fst) $
    filter
      (isWantedCompiler versionCheck wanted . toCV . fst)
      (Map.toList pairs_)

-- | Download and install the first available compiler build.
downloadAndInstallPossibleCompilers ::
     (HasGHCVariant env, HasBuildConfig env)
  => [CompilerBuild]
  -> SetupInfo
  -> WantedCompiler
  -> VersionCheck
  -> Maybe String
  -> RIO env (Tool, CompilerBuild)
downloadAndInstallPossibleCompilers possibleCompilers si wanted versionCheck mbindistURL =
  go possibleCompilers Nothing
 where
  -- This will stop as soon as one of the builds doesn't throw an @UnknownOSKey@
  -- or @UnknownCompilerVersion@ exception (so it will only try subsequent
  -- builds if one is nonexistent, not if the download or install fails for some
  -- other reason). The @Unknown*@ exceptions thrown by each attempt are
  -- combined into a single exception (if only @UnknownOSKey@ is thrown, then
  -- the first of those is rethrown, but if any @UnknownCompilerVersion@s are
  -- thrown then the attempted OS keys and available versions are unioned).
  go [] Nothing = prettyThrowM UnsupportedSetupConfiguration
  go [] (Just e) = prettyThrowM e
  go (b:bs) e = do
    logDebug $ "Trying to setup GHC build: " <> fromString (compilerBuildName b)
    er <- try $ downloadAndInstallCompiler b si wanted versionCheck mbindistURL
    case er of
      Left e'@(UnknownCompilerVersion ks' w' vs') ->
        case e of
          Nothing -> go bs (Just e')
          Just (UnknownOSKey k) ->
            go bs $ Just $ UnknownCompilerVersion (Set.insert k ks') w' vs'
          Just (UnknownCompilerVersion ks _ vs) ->
            go bs $ Just $
              UnknownCompilerVersion (Set.union ks' ks) w' (Set.union vs' vs)
          Just x -> prettyThrowM x
      Left e'@(UnknownOSKey k') ->
        case e of
          Nothing -> go bs (Just e')
          Just (UnknownOSKey _) -> go bs e
          Just (UnknownCompilerVersion ks w vs) ->
            go bs $ Just $ UnknownCompilerVersion (Set.insert k' ks) w vs
          Just x -> prettyThrowM x
      Left e' -> prettyThrowM e'
      Right r -> pure (r, b)

getGhcKey ::
     (HasBuildConfig env, HasGHCVariant env)
  => CompilerBuild
  -> RIO env Text
getGhcKey ghcBuild = do
  ghcVariant <- view ghcVariantL
  wantedComiler <- view $ buildConfigL . to (.smWanted.compiler)
  ghcVersion <- case wantedComiler of
        WCGhc version -> pure version
        WCGhcjs _ _ ->  throwIO GhcjsNotSupported
        WCGhcGit _ _ -> throwIO DownloadAndInstallCompilerError
  let variantSuffix = ghcVariantSuffix ghcVariant
      buildSuffix = compilerBuildSuffix ghcBuild
      ghcDir = style Dir $ mconcat
        [ "ghc"
        , fromString variantSuffix
        , fromString buildSuffix
        , "-"
        , fromString $ versionString ghcVersion
        ]
  osKey <- getOSKey "GHC" ghcDir
  pure $ osKey <> T.pack variantSuffix <> T.pack buildSuffix

getOSKey ::
     (HasConfig env, HasPlatform env)
  => StyleDoc
     -- ^ Description of the tool that is being set up.
  -> StyleDoc
     -- ^ Description of the root directory of the tool.
  -> RIO env Text
getOSKey tool toolDir = do
  programsDir <- view $ configL . to (.localPrograms)
  platform <- view platformL
  case platform of
    Platform I386                  Cabal.Linux   -> pure "linux32"
    Platform X86_64                Cabal.Linux   -> pure "linux64"
    Platform I386                  Cabal.OSX     -> pure "macosx"
    Platform X86_64                Cabal.OSX     -> pure "macosx"
    Platform I386                  Cabal.FreeBSD -> pure "freebsd32"
    Platform X86_64                Cabal.FreeBSD -> pure "freebsd64"
    Platform I386                  Cabal.OpenBSD -> pure "openbsd32"
    Platform X86_64                Cabal.OpenBSD -> pure "openbsd64"
    Platform I386                  Cabal.Windows -> pure "windows32"
    Platform X86_64                Cabal.Windows -> pure "windows64"
    Platform Arm                   Cabal.Linux   -> pure "linux-armv7"
    Platform AArch64               Cabal.Linux   -> pure "linux-aarch64"
    Platform Sparc                 Cabal.Linux   -> pure "linux-sparc"
    Platform AArch64               Cabal.OSX     -> pure "macosx-aarch64"
    Platform AArch64               Cabal.FreeBSD -> pure "freebsd-aarch64"
    Platform arch os ->
      prettyThrowM $ UnsupportedSetupCombo os arch tool toolDir programsDir

downloadOrUseLocal ::
     (HasTerm env, HasBuildConfig env)
  => Text
  -> DownloadInfo
  -> Path Abs File
  -> RIO env (Path Abs File)
downloadOrUseLocal downloadLabel downloadInfo destination =
  case url of
    (parseUrlThrow -> Just _) -> do
      ensureDir (parent destination)
      chattyDownload downloadLabel downloadInfo destination
      pure destination
    (parseAbsFile -> Just path) -> do
      warnOnIgnoredChecks
      pure path
    (parseRelFile -> Just path) -> do
      warnOnIgnoredChecks
      configFileRoot <- view configFileRootL
      pure (configFileRoot </> path)
    _ -> prettyThrowIO $ URLInvalid url
 where
  url = T.unpack downloadInfo.url
  warnOnIgnoredChecks = do
    let DownloadInfo
          { contentLength
          , sha1
          , sha256
          } = downloadInfo
    when (isJust contentLength) $
      prettyWarnS
        "`content-length` is not checked and should not be specified when \
        \`url` is a file path."
    when (isJust sha1) $
      prettyWarnS
        "`sha1` is not checked and should not be specified when `url` is a \
        \file path."
    when (isJust sha256) $
      prettyWarn
        "`sha256` is not checked and should not be specified when `url` is a \
        \file path"

downloadFromInfo ::
     (HasTerm env, HasBuildConfig env)
  => Path Abs Dir
  -> DownloadInfo
  -> Tool
  -> RIO env (Path Abs File, ArchiveType)
downloadFromInfo programsDir downloadInfo tool = do
  archiveType <-
    case extension of
      ".tar.xz" -> pure TarXz
      ".tar.bz2" -> pure TarBz2
      ".tar.gz" -> pure TarGz
      ".7z.exe" -> pure SevenZ
      _ -> prettyThrowIO $ UnknownArchiveExtension url

  relativeFile <- parseRelFile $ toolString tool ++ extension
  let destinationPath = programsDir </> relativeFile
  localPath <-
    downloadOrUseLocal (T.pack (toolString tool)) downloadInfo destinationPath
  pure (localPath, archiveType)

 where
  url = T.unpack downloadInfo.url
  extension = loop url
   where
    loop fp
      | ext `elem` [".tar", ".bz2", ".xz", ".exe", ".7z", ".gz"] = loop fp' ++ ext
      | otherwise = ""
     where
      (fp', ext) = FP.splitExtension fp


data ArchiveType
  = TarBz2
  | TarXz
  | TarGz
  | SevenZ

installGHCPosix ::
     HasConfig env
  => GHCDownloadInfo
  -> SetupInfo
  -> Path Abs File
  -> ArchiveType
  -> Path Abs Dir
  -> Path Abs Dir
  -> RIO env ()
installGHCPosix downloadInfo _ archiveFile archiveType tempDir destDir = do
  platform <- view platformL
  menv0 <- view processContextL
  menv <- mkProcessContext (removeHaskellEnvVars (view envVarsL menv0))
  logDebug $ "menv = " <> displayShow (view envVarsL menv)
  (zipTool', compOpt) <-
    case archiveType of
      TarXz -> pure ("xz", 'J')
      TarBz2 -> pure ("bzip2", 'j')
      TarGz -> pure ("gzip", 'z')
      SevenZ -> prettyThrowIO Unsupported7z
  -- Slight hack: OpenBSD's tar doesn't support xz.
  -- https://github.com/commercialhaskell/stack/issues/2283#issuecomment-237980986
  let tarDep =
        case (platform, archiveType) of
          (Platform _ Cabal.OpenBSD, TarXz) -> checkDependency "gtar"
          _ -> checkDependency "tar"
  (zipTool, makeTool, tarTool) <- checkDependencies $ (,,)
    <$> checkDependency zipTool'
    <*> (checkDependency "gmake" <|> checkDependency "make")
    <*> tarDep

  logDebug $ "ziptool: " <> fromString zipTool
  logDebug $ "make: " <> fromString makeTool
  logDebug $ "tar: " <> fromString tarTool

  let runStep step wd env cmd args = do
        menv' <- modifyEnvVars menv (Map.union env)
        let logLines lvl = CB.lines .| CL.mapM_ (lvl . displayBytesUtf8)
            logStdout = logLines logDebug
            logStderr = logLines logError
        void $ withWorkingDir (toFilePath wd) $
          withProcessContext menv' $
          sinkProcessStderrStdout cmd args logStderr logStdout
          `catchAny` \ex ->
            prettyThrowIO (GHCInstallFailed ex step cmd args wd tempDir destDir)

  logSticky $
       "Unpacking GHC into "
    <> fromString (toFilePath tempDir)
    <> " ..."
  logDebug $ "Unpacking " <> fromString (toFilePath archiveFile)
  runStep "unpacking" tempDir
    mempty
    tarTool
    [compOpt : "xf", toFilePath archiveFile]

  dir <- expectSingleUnpackedDir archiveFile tempDir

  mOverrideGccPath <- view $ configL . to (.overrideGccPath)

  -- The make application uses the CC environment variable to configure the
  -- program for compiling C programs
  let mGccEnv = let gccEnvFromPath p =
                      Map.singleton "CC" $ T.pack (toFilePath p)
                in  gccEnvFromPath <$> mOverrideGccPath

  -- Data.Map.union provides a left-biased union, so mGccEnv will prevail
  let ghcConfigureEnv =
        fromMaybe Map.empty mGccEnv `Map.union` downloadInfo.configureEnv

  logSticky "Configuring GHC ..."
  runStep "configuring" dir
    ghcConfigureEnv
    (toFilePath $ dir </> relFileConfigure)
    ( ("--prefix=" ++ toFilePath destDir)
    : map T.unpack downloadInfo.configureOpts
    )

  logSticky "Installing GHC ..."
  runStep "installing" dir mempty makeTool ["install"]

  logStickyDone "Installed GHC."
  logDebug $ "GHC installed to " <> fromString (toFilePath destDir)

-- | Check if given processes appear to be present, throwing an exception if
-- missing.
checkDependencies :: CheckDependency env a -> RIO env a
checkDependencies (CheckDependency f) =
  f >>= either (prettyThrowIO . MissingDependencies) pure

checkDependency :: HasProcessContext env => String -> CheckDependency env String
checkDependency tool = CheckDependency $ do
  exists <- doesExecutableExist tool
  pure $ if exists then Right tool else Left [tool]

newtype CheckDependency env a
  = CheckDependency (RIO env (Either [String] a))
  deriving Functor

instance Applicative (CheckDependency env) where
  pure x = CheckDependency $ pure (Right x)
  CheckDependency f <*> CheckDependency x = CheckDependency $ do
    f' <- f
    x' <- x
    pure $
      case (f', x') of
        (Left e1, Left e2) -> Left $ e1 ++ e2
        (Left e, Right _) -> Left e
        (Right _, Left e) -> Left e
        (Right f'', Right x'') -> Right $ f'' x''

instance Alternative (CheckDependency env) where
  empty = CheckDependency $ pure $ Left []
  CheckDependency x <|> CheckDependency y = CheckDependency $ do
    res1 <- x
    case res1 of
      Left _ -> y
      Right x' -> pure $ Right x'

installGHCWindows ::
     HasBuildConfig env
  => SetupInfo
  -> Path Abs File
  -> ArchiveType
  -> Path Abs Dir
  -> Path Abs Dir
  -> RIO env ()
installGHCWindows si archiveFile archiveType _tempDir destDir = do
  withUnpackedTarball7z "GHC" si archiveFile archiveType destDir
  prettyInfoL
    [ flow "GHC installed to"
    , pretty destDir <> "."
    ]

installMsys2Windows ::
     HasBuildConfig env
  => SetupInfo
  -> Path Abs File
  -> ArchiveType
  -> Path Abs Dir
  -> Path Abs Dir
  -> RIO env ()
installMsys2Windows si archiveFile archiveType _tempDir destDir = do
  exists <- liftIO $ D.doesDirectoryExist $ toFilePath destDir
  when exists $
    liftIO (D.removeDirectoryRecursive $ toFilePath destDir) `catchIO` \e ->
      prettyThrowM $ ExistingMSYS2NotDeleted destDir e

  withUnpackedTarball7z "MSYS2" si archiveFile archiveType destDir

  -- I couldn't find this officially documented anywhere, but you need to run
  -- the MSYS shell once in order to initialize some pacman stuff. Once that run
  -- happens, you can just run commands as usual.
  menv0 <- view processContextL
  newEnv0 <- modifyEnvVars menv0 $ Map.insert "MSYSTEM" "MSYS"
  newEnv <- either throwM pure $ augmentPathMap
              [toFilePath $ destDir </> relDirUsr </> relDirBin]
              (view envVarsL newEnv0)
  menv <- mkProcessContext newEnv
  withWorkingDir (toFilePath destDir) $ withProcessContext menv
    $ proc "sh" ["--login", "-c", "true"] runProcess_

  -- No longer installing git, it's unreliable
  -- (https://github.com/commercialhaskell/stack/issues/1046) and the
  -- MSYS2-installed version has bad CRLF defaults.
  --
  -- Install git. We could install other useful things in the future too.
  -- runCmd (Cmd (Just destDir) "pacman" menv ["-Sy", "--noconfirm", "git"]) Nothing

-- | Unpack a compressed tarball using 7zip. Expects a single directory in the
-- unpacked results, which is renamed to the destination directory.
withUnpackedTarball7z ::
     HasBuildConfig env
  => String -- ^ Name of tool, used in error messages
  -> SetupInfo
  -> Path Abs File -- ^ Path to archive file
  -> ArchiveType
  -> Path Abs Dir -- ^ Destination directory.
  -> RIO env ()
withUnpackedTarball7z name si archiveFile archiveType destDir = do
  suffix <-
    case archiveType of
      TarXz -> pure ".xz"
      TarBz2 -> pure ".bz2"
      TarGz -> pure ".gz"
      _ -> prettyThrowIO $ TarballInvalid name
  tarFile <-
    case T.stripSuffix suffix $ T.pack $ toFilePath (filename archiveFile) of
      Nothing -> prettyThrowIO $ TarballFileInvalid name archiveFile
      Just x -> parseRelFile $ T.unpack x
  run7z <- setup7z si
  -- We aim to reduce the risk of a filepath length of more than 260 characters,
  -- which can be problematic for 7-Zip if Windows is not 'long file paths'
  -- enabled. We use a short name for the temporary directory ...
  let tmpName = "stack-tmp"
      destDrive = takeDrive destDir
  ensureDir (parent destDir)
  tempDrive <- takeDrive <$> getTempDir
  -- We use a temporary directory with likely a short absolute path ...
  let withTempDir' = if tempDrive == destDrive
        then
          -- We use the system temporary directory if we can, as a Standard user
          -- may well not have permission to create a directory in the root of
          -- the system drive.
          withSystemTempDir
        else
          -- Otherwise we use a temporary directory in the root of the
          -- destination drive.
          withTempDir destDrive
  withRunInIO $ \run ->
    withTempDir' tmpName $ \tmpDir ->
      run $ do
        liftIO $ ignoringAbsence (removeDirRecur destDir)
        run7z tmpDir archiveFile
        run7z tmpDir (tmpDir </> tarFile)
        absSrcDir <- expectSingleUnpackedDir archiveFile tmpDir
        -- On Windows, 'renameDir' does not work across drives. However, we have
        -- ensured that 'tmpDir' has the same drive as 'destDir'.
        renameDir absSrcDir destDir

expectSingleUnpackedDir ::
     (MonadIO m, MonadThrow m)
  => Path Abs File
  -> Path Abs Dir
  -> m (Path Abs Dir)
expectSingleUnpackedDir archiveFile unpackDir = do
  contents <- listDir unpackDir
  case contents of
    ([dir], _ ) -> pure dir
    _ -> prettyThrowIO $ UnknownArchiveStructure archiveFile

-- | Download 7z as necessary, and get a function for unpacking things.
--
-- Returned function takes an unpack directory and archive.
setup7z ::
     (HasBuildConfig env, MonadIO m)
  => SetupInfo
  -> RIO env (Path Abs Dir -> Path Abs File -> m ())
setup7z si = do
  dir <- view $ configL . to (.localPrograms)
  ensureDir dir
  let exeDestination = dir </> relFile7zexe
      dllDestination = dir </> relFile7zdll
  case (si.sevenzDll, si.sevenzExe) of
    (Just sevenzDll, Just sevenzExe) -> do
      _ <- downloadOrUseLocal "7z.dll" sevenzDll dllDestination
      exePath <- downloadOrUseLocal "7z.exe" sevenzExe exeDestination
      withRunInIO $ \run -> pure $ \outdir archive -> liftIO $ run $ do
        let cmd = toFilePath exePath
            args =
              [ "x"
              , "-o" ++ toFilePath outdir
              , "-y"
              , archiveFP
              ]
            archiveFP = toFilePath archive
            archiveFileName = filename archive
            archiveDisplay = fromString $ toFilePath archiveFileName
            isExtract = FP.takeExtension archiveFP == ".tar"
        prettyInfoL
          [ if isExtract then "Extracting" else "Decompressing"
          , pretty archiveFileName <> "..."
          ]
        ec <-
          proc cmd args $ \pc ->
          if isExtract
            then withProcessWait (setStdout createSource pc) $ \p -> do
              total <- runConduit
                 $ getStdout p
                .| filterCE (== 10) -- newline characters
                .| foldMC
                     (\count bs -> do
                         let count' = count + S.length bs
                         logSticky $ "Extracted " <> display count' <> " files"
                         pure count'
                     )
                     0
              logStickyDone $
                   "Extracted total of "
                <> display total
                <> " files from "
                <> archiveDisplay
              waitExitCode p
            else runProcess pc
        when (ec /= ExitSuccess) $
          liftIO $ prettyThrowM (ProblemWhileDecompressing archive)
    _ -> prettyThrowM SetupInfoMissingSevenz

chattyDownload ::
     HasTerm env
  => Text          -- ^ label
  -> DownloadInfo  -- ^ URL, content-length, sha1, and sha256
  -> Path Abs File -- ^ destination
  -> RIO env ()
chattyDownload label downloadInfo path = do
  let url = downloadInfo.url
  req <- parseUrlThrow $ T.unpack url
  logSticky $
       "Preparing to download "
    <> display label
    <> " ..."
  logDebug $
       "Downloading from "
    <> display url
    <> " to "
    <> fromString (toFilePath path)
    <> " ..."
  hashChecks <- fmap catMaybes $ forM
    [ ("sha1", HashCheck SHA1, (.sha1))
    , ("sha256", HashCheck SHA256, (.sha256))
    ]
    $ \(name, constr, getter) ->
      case getter downloadInfo of
        Just bs -> do
          logDebug $
               "Will check against "
            <> name
            <> " hash: "
            <> displayBytesUtf8 bs
          pure $ Just $ constr $ CheckHexDigestByteString bs
        Nothing -> pure Nothing
  when (null hashChecks) $
    prettyWarnS
      "No sha1 or sha256 found in metadata, download hash won't be checked."
  let dReq = setHashChecks hashChecks $
             setLengthCheck mtotalSize $
             mkDownloadRequest req
  x <- verifiedDownloadWithProgress dReq path label mtotalSize
  if x
    then logStickyDone ("Downloaded " <> display label <> ".")
    else logStickyDone ("Already downloaded " <> display label <> ".")
 where
  mtotalSize = downloadInfo.contentLength

-- | Perform a basic sanity check of GHC
sanityCheck ::
     (HasLogFunc env, HasProcessContext env)
  => Path Abs File
  -> RIO env ()
sanityCheck ghc = withSystemTempDir "stack-sanity-check" $ \dir -> do
  let fp = toFilePath $ dir </> relFileMainHs
  liftIO $ S.writeFile fp $ T.encodeUtf8 $ T.pack $ unlines
    [ "import Distribution.Simple" -- ensure Cabal library is present
    , "main = putStrLn \"Hello World\""
    ]
  logDebug $ "Performing a sanity check on: " <> fromString (toFilePath ghc)
  eres <- withWorkingDir (toFilePath dir) $ proc (toFilePath ghc)
    [ fp
    , "-no-user-package-db"
    -- Required to stop GHC looking for a package environment in default
    -- locations.
    , "-hide-all-packages"
    -- Required because GHC flag -hide-all-packages is passed.
    , "-package base"
    , "-package Cabal" -- required for "import Distribution.Simple"
    ] $ try . readProcess_
  case eres of
    Left e -> prettyThrowIO $ GHCSanityCheckCompileFailed e ghc
    Right _ -> pure () -- TODO check that the output of running the command is
                       -- correct

-- Remove potentially confusing environment variables
removeHaskellEnvVars :: Map Text Text -> Map Text Text
removeHaskellEnvVars =
  Map.delete "GHC_PACKAGE_PATH" .
  Map.delete "GHC_ENVIRONMENT" .
  Map.delete "HASKELL_PACKAGE_SANDBOX" .
  Map.delete "HASKELL_PACKAGE_SANDBOXES" .
  Map.delete "HASKELL_DIST_DIR" .
  -- https://github.com/commercialhaskell/stack/issues/1460
  Map.delete "DESTDIR" .
  -- https://github.com/commercialhaskell/stack/issues/3444
  Map.delete "GHCRTS"

-- | Map of environment variables to set to change the GHC's encoding to UTF-8.
utf8EnvVars :: Map Text Text
utf8EnvVars =
  -- GHC_CHARENC supported by GHC >=7.10.3
  Map.singleton "GHC_CHARENC" "UTF-8"

-- Binary Stack upgrades

-- | Information on a binary release of Stack.
data StackReleaseInfo
  = SRIGitHub !Value
    -- ^ Metadata downloaded from GitHub releases about available binaries.
  | SRIHaskellStackOrg !HaskellStackOrg
    -- ^ Information on the latest available binary for the current platforms.

data HaskellStackOrg = HaskellStackOrg
  { url :: !Text
  , version :: !Version
  }
  deriving Show

-- | Download information on a binary release of Stack. If there is no given
-- GitHub user, GitHub repository and version, then first tries
-- @haskellstack.org@.
downloadStackReleaseInfo ::
     (HasLogFunc env, HasPlatform env)
  => Maybe String -- ^ Optional GitHub user.
  -> Maybe String -- ^ Optional GitHub repository.
  -> Maybe String -- ^ Optional version.
  -> RIO env StackReleaseInfo
downloadStackReleaseInfo Nothing Nothing Nothing = do
  platform <- view platformL
  -- Fallback list of URLs to try for upgrading.
  let urls0 =
        case platform of
          Platform X86_64 Cabal.Linux ->
            [ "https://get.haskellstack.org/upgrade/linux-x86_64-static.tar.gz"
            , "https://get.haskellstack.org/upgrade/linux-x86_64.tar.gz"
            ]
          Platform X86_64 Cabal.OSX ->
            [ "https://get.haskellstack.org/upgrade/osx-x86_64.tar.gz"
            ]
          Platform X86_64 Cabal.Windows ->
            [ "https://get.haskellstack.org/upgrade/windows-x86_64.tar.gz"
            ]
          _ -> []
      -- Helper function: extract the version from a GitHub releases URL.
  let extractVersion loc = do
        version0 <-
          case reverse $ splitOn "/" $ T.unpack loc of
            _final:version0:_ -> Right version0
            _ -> Left $ "Insufficient pieces in location: " ++ show loc
        version1 <-
          maybe (Left "no leading v on version") Right $ stripPrefix "v" version0
        maybe (Left $ "Invalid version: " ++ show version1) Right $ parseVersion version1

      -- Try out different URLs. If we've exhausted all of them, fall back to GitHub.
      loop [] = do
        logDebug "Could not get binary from haskellstack.org, trying GitHub"
        downloadStackReleaseInfoGitHub Nothing Nothing Nothing

      -- Try the next URL
      loop (url:urls) = do
        -- Make a HEAD request without any redirects
        req <- setRequestMethod "HEAD" <$> parseRequest (T.unpack url)
        res <- httpLbs req { redirectCount = 0 }

        -- Look for a redirect. We're looking for a standard GitHub releases
        -- URL where we can extract version information from.
        case getResponseHeader "location" res of
          [] -> logDebug "No location header found, continuing" *> loop urls
          -- Exactly one location header.
          [locBS] ->
            case decodeUtf8' locBS of
              Left e ->
                   logDebug
                     (   "Invalid UTF8: "
                     <> displayShow (locBS, e)
                     )
                *> loop urls
              Right loc ->
                case extractVersion loc of
                  Left s ->
                       logDebug
                         (   "No version found: "
                         <> displayShow (url, loc, s)
                         )
                    *> loop (loc:urls)
                  -- We found a valid URL, let's use it!
                  Right version -> do
                    let hso = HaskellStackOrg
                                { url = loc
                                , version
                                }
                    logDebug $
                         "Downloading from haskellstack.org: "
                      <> displayShow hso
                    pure $ SRIHaskellStackOrg hso
          locs ->
               logDebug
                 (  "Multiple location headers found: "
                 <> displayShow locs
                 )
            *> loop urls
  loop urls0
downloadStackReleaseInfo morg mrepo mver =
  downloadStackReleaseInfoGitHub morg mrepo mver

-- | Same as above, but always uses GitHub
downloadStackReleaseInfoGitHub ::
     (MonadIO m, MonadThrow m)
  => Maybe String -- GitHub org
  -> Maybe String -- GitHub repo
  -> Maybe String -- ^ optional version
  -> m StackReleaseInfo
downloadStackReleaseInfoGitHub morg mrepo mver = liftIO $ do
  let org = fromMaybe "commercialhaskell" morg
      repo = fromMaybe "stack" mrepo
  let url = concat
        [ "https://api.github.com/repos/"
        , org
        , "/"
        , repo
        , "/releases/"
        , case mver of
            Nothing -> "latest"
            Just ver -> "tags/v" ++ ver
        ]
  req <- parseRequest url
  res <- httpJSON $ setGitHubHeaders req
  let code = getResponseStatusCode res
  if code >= 200 && code < 300
    then pure $ SRIGitHub $ getResponseBody res
    else prettyThrowIO $ StackReleaseInfoNotFound url

-- | Yield a list of the preferred GHC variants for the platform. The first item
-- of each pair indicates if the operating system is Windows. The second item
-- is the name of the GHC variant in Stack's @setup-info@ dictionary.
preferredPlatforms ::
     (MonadReader env m, HasPlatform env, MonadThrow m)
  => m [(Bool, String)]
preferredPlatforms = do
  Platform arch' os' <- view platformL
  (isWindows, os) <-
    case os' of
      Cabal.Linux -> pure (False, "linux")
      Cabal.Windows -> pure (True, "windows")
      Cabal.OSX -> pure (False, "osx")
      Cabal.FreeBSD -> pure (False, "freebsd")
      _ -> prettyThrowM $ BinaryUpgradeOnOSUnsupported os'
  arch <-
    case arch' of
      I386 -> pure "i386"
      X86_64 -> pure "x86_64"
      Arm -> pure "arm"
      AArch64 -> pure "aarch64"
      _ -> prettyThrowM $ BinaryUpgradeOnArchUnsupported arch'
  let hasgmp4 = False -- FIXME import relevant code from Stack.Setup?
                      -- checkLib $(mkRelFile "libgmp.so.3")
      suffixes
          -- 'gmp4' ceased to be relevant after Stack 1.9.3 (December 2018).
        | hasgmp4 = ["-static", "-gmp4", ""]
          -- 'static' will cease to be relevant after Stack 2.11.1 (May 2023).
        | otherwise = ["-static", ""]
  pure $ map (\suffix -> (isWindows, concat [os, "-", arch, suffix])) suffixes

-- | Download a Stack executable.
downloadStackExe ::
     HasConfig env
  => [(Bool, String)] -- ^ acceptable platforms
  -> StackReleaseInfo
  -> Path Abs Dir -- ^ destination directory
  -> Bool -- ^ perform PATH-aware checking, see #3232
  -> (Path Abs File -> IO ()) -- ^ test the temp exe before renaming
  -> RIO env ()
downloadStackExe platforms0 archiveInfo destDir checkPath testExe = do
  (isWindows, archiveURL) <-
    let loop [] =
          prettyThrowIO $ StackBinaryArchiveNotFound (map snd platforms0)
        loop ((isWindows, p'):ps) = do
          let p = T.pack p'
          prettyInfoL
            [ flow "Querying for archive location for platform:"
            , style Current (fromString p') <> "."
            ]
          case findArchive archiveInfo p of
            Just x -> pure (isWindows, x)
            Nothing -> loop ps
    in  loop platforms0

  let (destFile, tmpFile)
        | isWindows =
            ( destDir </> relFileStackDotExe
            , destDir </> relFileStackDotTmpDotExe
            )
        | otherwise =
            ( destDir </> relFileStack
            , destDir </> relFileStackDotTmp
            )

  prettyInfoL
    [ flow "Downloading from:"
    , style Url (fromString $ T.unpack archiveURL) <> "."
    ]

  liftIO $
    if | ".tar.gz" `T.isSuffixOf` archiveURL ->
           handleTarball tmpFile isWindows archiveURL
       | ".zip" `T.isSuffixOf` archiveURL ->
            throwIO StackBinaryArchiveZipUnsupportedBug
       | otherwise -> prettyThrowIO $ StackBinaryArchiveUnsupported archiveURL

  prettyInfoS "Download complete, testing executable."

  -- We need to preserve the name of the executable file before we overwrite the
  -- currently running binary: after that, Linux will append (deleted) to the
  -- filename.
  currExe <- viewExecutablePath

  liftIO $ do
    setFileExecutable (toFilePath tmpFile)
    testExe tmpFile

  relocateStackExeFile currExe tmpFile destFile

  prettyInfoL
    [ flow "New Stack executable available at:"
    , pretty destFile <> "."
    ]

  destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir
  warnInstallSearchPathIssues destDir' ["stack"]

  when checkPath $ performPathChecking destFile currExe
    `catchAny` (logError . displayShow)
 where

  findArchive (SRIGitHub val) platformPattern = do
    Object top <- pure val
    Array assets <- KeyMap.lookup "assets" top
    getFirst $ foldMap (First . findMatch pattern') assets
   where
    pattern' = mconcat ["-", platformPattern, "."]

    findMatch pattern'' (Object o) = do
        String name <- KeyMap.lookup "name" o
        guard $ not $ ".asc" `T.isSuffixOf` name
        guard $ pattern'' `T.isInfixOf` name
        String url <- KeyMap.lookup "browser_download_url" o
        Just url
    findMatch _ _ = Nothing
  findArchive (SRIHaskellStackOrg hso) _ = pure hso.url

  handleTarball :: Path Abs File -> Bool -> T.Text -> IO ()
  handleTarball tmpFile isWindows url = do
    req <- fmap setGitHubHeaders $ parseUrlThrow $ T.unpack url
    withResponse req $ \res -> do
      entries <- fmap (Tar.read . LBS.fromChunks)
        $ lazyConsume
        $ getResponseBody res .| ungzip
      let loop Tar.Done = prettyThrowIO $ StackBinaryNotInArchive exeName url
          loop (Tar.Fail e) = throwM e
          loop (Tar.Next e es) =
            case FP.splitPath (Tar.entryPath e) of
              -- Ignore the first component, see:
              -- https://github.com/commercialhaskell/stack/issues/5288
              [_ignored, name] | name == exeName -> do
                case Tar.entryContent e of
                  Tar.NormalFile lbs _ -> do
                    ensureDir destDir
                    LBS.writeFile (toFilePath tmpFile) lbs
                  _ -> prettyThrowIO $ FileTypeInArchiveInvalid e url
              _ -> loop es
      loop entries
   where
    exeName
      | isWindows = "stack.exe"
      | otherwise = "stack"

relocateStackExeFile ::
     HasTerm env
  => Path Abs File
     -- ^ Path to the currently running executable
  -> Path Abs File
     -- ^ Path to the executable file to be relocated
  -> Path Abs File
     -- ^ Path to the new location for the excutable file
  -> RIO env ()
relocateStackExeFile currExeFile newExeFile destExeFile = do
  when (osIsWindows && destExeFile == currExeFile) $ do
    -- Windows allows a running executable's file to be renamed, but not to be
    -- overwritten.
    old <- addExtension ".old" currExeFile
    prettyInfoL
      [ flow "Renaming existing:"
      , pretty currExeFile
      , "as:"
      , pretty old <> "."
      ]
    renameFile currExeFile old
  renameFile newExeFile destExeFile

-- | Ensure that the Stack executable download is in the same location as the
-- currently running executable. See:
-- https://github.com/commercialhaskell/stack/issues/3232
performPathChecking ::
     HasConfig env
  => Path Abs File
     -- ^ Path to the newly downloaded file
  -> Path Abs File
     -- ^ Path to the currently running executable
  -> RIO env ()
performPathChecking newExeFile currExeFile = do
  unless (newExeFile == currExeFile) $ do
    prettyInfoL
      [ flow "Also copying Stack executable to:"
      , pretty currExeFile <> "."
      ]
    tmpFile <- toFilePath <$> addExtension ".tmp" currExeFile
    eres <- tryIO $
      relocateStackExeFile currExeFile newExeFile currExeFile
    case eres of
      Right () -> prettyInfoS "Stack executable copied successfully!"
      Left e
        | isPermissionError e -> if osIsWindows
            then do
              prettyWarn $
                   flow "Permission error when trying to copy:"
                <> blankLine
                <> string (displayException e)
            else do
              prettyWarn $
                   flow "Permission error when trying to copy:"
                <> blankLine
                <> string (displayException e)
                <> blankLine
                <> fillSep
                     [ flow "Should I try to perform the file copy using"
                     , style Shell "sudo" <> "?"
                     , flow "This may fail."
                     ]
              toSudo <- promptBool "Try using sudo? (y/n) "
              when toSudo $ do
                let run cmd args = do
                      ec <- proc cmd args runProcess
                      when (ec /= ExitSuccess) $
                        throwIO $ ProcessExited ec cmd args
                    commands =
                      [ ("sudo",
                          [ "cp"
                          , toFilePath newExeFile
                          , tmpFile
                          ])
                      , ("sudo",
                          [ "mv"
                          , tmpFile
                          , toFilePath currExeFile
                          ])
                      ]
                prettyInfo $
                     flow "Going to run the following commands:"
                  <> blankLine
                  <> bulletedList
                       ( map
                         ( \(cmd, args) ->
                             style Shell $ fillSep
                               $ fromString cmd
                               : map fromString args
                         )
                         commands
                       )
                mapM_ (uncurry run) commands
                prettyInfo $
                     line
                  <> flow "sudo file copy worked!"
        | otherwise -> throwM e

-- | If available, yields the version of the given binary release of Stack.
getDownloadVersion :: StackReleaseInfo -> Maybe Version
getDownloadVersion (SRIGitHub val) = do
  Object o <- Just val
  String rawName <- KeyMap.lookup "name" o
  -- drop the "v" at the beginning of the name
  parseVersion $ T.unpack (T.drop 1 rawName)
getDownloadVersion (SRIHaskellStackOrg hso) = Just hso.version
