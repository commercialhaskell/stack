{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-} -- keep TH usage here

{-|
Module      : Stack.Constants
Description : Constants used throughout the project.
License     : BSD-3-Clause

Constants used throughout the project.
-}

module Stack.Constants
  ( buildPlanDir
  , buildPlanCacheDir
  , haskellFileExts
  , haskellDefaultPreprocessorExts
  , stackProgName
  , stackProgName'
  , nixProgName
  , stackDotYaml
  , stackWorkEnvVar
  , stackRootEnvVar
  , stackXdgEnvVar
  , stackRootOptionName
  , stackGlobalConfigOptionName
  , pantryRootEnvVar
  , inContainerEnvVar
  , inNixShellEnvVar
  , stackProgNameUpper
  , wiredInPackages
  , cabalPackageName
  , implicitGlobalProjectDir
  , defaultUserConfigPath
  , defaultGlobalConfigPath
  , platformVariantEnvVar
  , compilerOptionsCabalFlag
  , ghcColorForceFlag
  , minTerminalWidth
  , maxTerminalWidth
  , defaultTerminalWidth
  , osIsMacOS
  , osIsWindows
  , relFileSetupHs
  , relFileSetupLhs
  , relFileHpackPackageConfig
  , relDirGlobalAutogen
  , relDirAutogen
  , relDirLogs
  , relFileCabalMacrosH
  , relDirBuild
  , relDirBin
  , relDirGhci
  , relDirGhciScript
  , relDirPantry
  , relDirPrograms
  , relDirRoot
  , relDirUpperPrograms
  , relDirStackProgName
  , relDirStackWork
  , relFileReadmeTxt
  , relDirScript
  , relDirScripts
  , relFileConfigYaml
  , relDirSnapshots
  , relDirGlobalHints
  , relFileGlobalHintsYaml
  , relDirInstall
  , relDirCompilerTools
  , relDirHoogle
  , relFileDatabaseHoo
  , relDirPkgdb
  , relFileStorage
  , relDirLoadedSnapshotCache
  , bindirSuffix
  , docDirSuffix
  , htmlDirSuffix
  , relDirHpc
  , relDirLib
  , relDirShare
  , relDirLibexec
  , relDirEtc
  , setupGhciShimCode
  , relDirSetupExeCache
  , relDirSetupExeSrc
  , relFileConfigure
  , relDirDist
  , relFileSetupMacrosH
  , relDirSetup
  , relFileSetupLower
  , relDirMingw
  , relDirMingw32
  , relDirMingw64
  , relDirClang32
  , relDirClang64
  , relDirClangArm64
  , relDirUcrt64
  , relDirLocal
  , relDirUsr
  , relDirInclude
  , relFileIndexHtml
  , relDirAll
  , relFilePackageCache
  , relFileDockerfile
  , relFileGhciScript
  , relDirCombined
  , relFileHpcIndexHtml
  , relDirCustom
  , relDirPackageConfInplace
  , relDirExtraTixFiles
  , relDirInstalledPackages
  , backupUrlRelPath
  , relDirDotLocal
  , relDirDotSsh
  , relDirDotStackProgName
  , relDirUnderHome
  , relDirSrc
  , relFileLibcMuslx86_64So1
  , relFileLibtinfoSo5
  , relFileLibtinfoSo6
  , relFileLibncurseswSo6
  , relFileLibgmpSo10
  , relFileLibgmpSo3
  , relDirNewCabal
  , relFileSetupExe
  , relFileSetupUpper
  , relFile7zexe
  , relFile7zdll
  , relFileMainHs
  , relFileStack
  , relFileStackDotExe
  , relFileStackDotTmpDotExe
  , relFileStackDotTmp
  , ghcShowOptionsOutput
  , ghcBootScript
  , ghcConfigureScript
  , ghcConfigureWindows
  , ghcConfigureMacOS
  , ghcConfigurePosix
  , relDirHadrian
  , relFileHadrianStackDotYaml
  , hadrianScriptsWindows
  , hadrianScriptsPosix
  , libDirs
  , usrLibDirs
  , testGhcEnvRelFile
  , relFileBuildLock
  , stackDeveloperModeDefault
  , isStackUploadDisabled
  , globalFooter
  , gitHubBasicAuthType
  , gitHubTokenEnvVar
  , altGitHubTokenEnvVar
  , hackageBaseUrl
  ) where

import           Data.ByteString.Builder ( byteString )
import           Data.Char ( toUpper )
import           Data.FileEmbed ( embedFile, makeRelativeToProject )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Package ( mkPackageName )
import           Hpack.Config ( packageConfig )
import qualified Language.Haskell.TH.Syntax as TH ( runIO, lift )
import           Path ( (</>), mkRelDir, mkRelFile, parseAbsFile )
import           Stack.Constants.StackProgName ( stackProgName )
import           Stack.Constants.UsrLibDirs ( libDirs, usrLibDirs )
import           Stack.Prelude
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           System.Permissions ( osIsMacOS, osIsWindows )
import           System.Process ( readProcess )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Constants" module.
data ConstantsException
  = WiredInPackagesNotParsedBug
  deriving (Show, Typeable)

instance Exception ConstantsException where
  displayException WiredInPackagesNotParsedBug = bugReport "[S-6057]"
    "Parse error in wiredInPackages."

-- | Name of the Stack program.
stackProgName' :: Text
stackProgName' = T.pack stackProgName

-- | Name of the Nix package manager command
nixProgName :: String
nixProgName = "nix"

-- | Extensions used for Haskell modules. Excludes preprocessor ones.
haskellFileExts :: [Text]
haskellFileExts = ["hs", "hsc", "lhs"]

-- | Extensions for modules that are preprocessed by common preprocessors.
haskellDefaultPreprocessorExts :: [Text]
haskellDefaultPreprocessorExts = ["gc", "chs", "hsc", "x", "y", "ly", "cpphs"]

-- | Name of the \'stack\' program, uppercased
stackProgNameUpper :: String
stackProgNameUpper = map toUpper stackProgName

-- | The filename used for the Stack project-level configuration file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

-- | Environment variable used to override the '.stack-work' relative dir.
stackWorkEnvVar :: String
stackWorkEnvVar = "STACK_WORK"

-- | Environment variable used to override the '~/.stack' location.
stackRootEnvVar :: String
stackRootEnvVar = "STACK_ROOT"

-- | Environment variable used to indicate XDG directories should be used.
stackXdgEnvVar :: String
stackXdgEnvVar = "STACK_XDG"

-- | Option name for the global Stack root.
stackRootOptionName :: String
stackRootOptionName = "stack-root"

-- | Option name for the global Stack configuration file.
stackGlobalConfigOptionName :: String
stackGlobalConfigOptionName = "global-config"

-- | Environment variable used to override the location of the Pantry store
pantryRootEnvVar :: String
pantryRootEnvVar = "PANTRY_ROOT"

-- | Environment variable used to indicate Stack is running in container.
inContainerEnvVar :: String
inContainerEnvVar = stackProgNameUpper ++ "_IN_CONTAINER"

-- | Environment variable used to indicate Stack is running in container.
-- although we already have STACK_IN_NIX_EXTRA_ARGS that is set in the same conditions,
-- it can happen that STACK_IN_NIX_EXTRA_ARGS is set to empty.
inNixShellEnvVar :: String
inNixShellEnvVar = map toUpper stackProgName ++ "_IN_NIX_SHELL"

-- | The comment to \'see
-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/ghc/src/Module.html#integerPackageKey\'
-- appears to be out of date.
--
-- See \'Note [About units]\' and \'Wired-in units\' at
-- https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Unit.hs.
--
-- The \'wired-in packages\' appear to have been replaced by those that have (e.g)
--
-- > ghc-options: -this-unit-id ghc-prim
--
-- in their Cabal file because they are \'magic\'.
--
-- Cabal (the tool) also treats certain packages as non-reinstallable. See
-- @Distribution.Client.Dependency.nonReinstallablePackages@.
wiredInPackages :: Set PackageName
wiredInPackages = case mparsed of
  Just parsed -> Set.fromList parsed
  Nothing -> impureThrow WiredInPackagesNotParsedBug
 where
  mparsed = mapM parsePackageName
    [ "rts"
      -- Said to be not a \'real\' package.
    , "base"
      -- A magic package. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "ghc"
      -- A magic package. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "ghc-bignum"
      -- A magic package from GHC 9.0.1. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "ghc-internal"
      -- A magic package from GHC 9.10.1. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "ghc-prim"
      -- A magic package. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "integer-gmp"
      -- No longer magic > 1.0.3.0 (GHC >= 9.0) and deprecated in favour of
      -- ghc-bignum. With GHC 9.8.4 at least, there seems to be no problem in
      -- using it. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "integer-simple"
      -- A magic package. Also treated as non-reinstallable by
      -- cabal-install-3.14.2.0.
    , "template-haskell"
      -- No longer magic > 2.22.0.0 (GHC >= 9.12). Also treated as
      -- non-reinstallable by cabal-install-3.14.2.0.
    , "interactive"
      -- Type and class declarations at the GHCi command prompt are treated as
      -- if they were defined in modules all sharing a common package
      -- interactive. See 'Note [The interactive package]' at
      -- https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Runtime/Context.hs
      -- With GHC 9.8.4 at least, there seems to be no problem in using it.
    ]

-- | Just to avoid repetition and magic strings.
cabalPackageName :: PackageName
cabalPackageName =
    mkPackageName "Cabal"

-- | Implicit global project directory used when outside of a project.
-- Normally, @getImplicitGlobalProjectDir@ should be used instead.
implicitGlobalProjectDir ::
     Path Abs Dir -- ^ Stack root.
  -> Path Abs Dir
implicitGlobalProjectDir p = p </> $(mkRelDir "global-project")

-- | Default user global configuration path. Normally,
-- @getDefaultUserConfigPath@ should be used instead.
defaultUserConfigPath :: Path Abs Dir -> Path Abs File
defaultUserConfigPath = (</> $(mkRelFile "config.yaml"))

-- | Default global config path. On Windows, by design, this will be @Nothing@.
defaultGlobalConfigPath :: Maybe (Path Abs File)
defaultGlobalConfigPath = parseAbsFile "/etc/stack/config.yaml"

-- | Path where build plans are stored.
buildPlanDir ::
     Path Abs Dir -- ^ Stack root
  -> Path Abs Dir
buildPlanDir = (</> $(mkRelDir "build-plan"))

-- | Path where binary caches of the build plans are stored.
buildPlanCacheDir ::
     Path Abs Dir -- ^ Stack root
  -> Path Abs Dir
buildPlanCacheDir = (</> $(mkRelDir "build-plan-cache"))

-- | Environment variable that stores a variant to append to platform-specific directory
-- names.  Used to ensure incompatible binaries aren't shared between Docker builds and host
platformVariantEnvVar :: String
platformVariantEnvVar = stackProgNameUpper ++ "_PLATFORM_VARIANT"

-- | Provides --ghc-options for 'Ghc'
compilerOptionsCabalFlag :: WhichCompiler -> String
compilerOptionsCabalFlag Ghc = "--ghc-options"

-- | The flag to pass to GHC when we want to force its output to be
-- colorized.
ghcColorForceFlag :: String
ghcColorForceFlag = "-fdiagnostics-color=always"

-- | The minimum allowed terminal width. Used for pretty-printing.
minTerminalWidth :: Int
minTerminalWidth = 40

-- | The maximum allowed terminal width. Used for pretty-printing.
maxTerminalWidth :: Int
maxTerminalWidth = 200

-- | The default terminal width. Used for pretty-printing when we can't
-- automatically detect it and when the user doesn't supply one.
defaultTerminalWidth :: Int
defaultTerminalWidth = 100

relFileSetupHs :: Path Rel File
relFileSetupHs = $(mkRelFile "Setup.hs")

relFileSetupLhs :: Path Rel File
relFileSetupLhs = $(mkRelFile "Setup.lhs")

relFileHpackPackageConfig :: Path Rel File
relFileHpackPackageConfig = $(mkRelFile packageConfig)

relDirGlobalAutogen :: Path Rel Dir
relDirGlobalAutogen = $(mkRelDir "global-autogen")

relDirAutogen :: Path Rel Dir
relDirAutogen = $(mkRelDir "autogen")

relDirLogs :: Path Rel Dir
relDirLogs = $(mkRelDir "logs")

relFileCabalMacrosH :: Path Rel File
relFileCabalMacrosH = $(mkRelFile "cabal_macros.h")

relDirBuild :: Path Rel Dir
relDirBuild = $(mkRelDir "build")

relDirBin :: Path Rel Dir
relDirBin = $(mkRelDir "bin")

relDirGhci :: Path Rel Dir
relDirGhci = $(mkRelDir "ghci")

relDirGhciScript :: Path Rel Dir
relDirGhciScript = $(mkRelDir "ghci-script")

relDirPantry :: Path Rel Dir
relDirPantry = $(mkRelDir "pantry")

relDirPrograms :: Path Rel Dir
relDirPrograms = $(mkRelDir "programs")

relDirRoot :: Path Rel Dir
relDirRoot = $(mkRelDir ".")

relDirUpperPrograms :: Path Rel Dir
relDirUpperPrograms = $(mkRelDir "Programs")

relDirStackProgName :: Path Rel Dir
relDirStackProgName = $(mkRelDir stackProgName)

relDirStackWork :: Path Rel Dir
relDirStackWork = $(mkRelDir ".stack-work")

relFileReadmeTxt :: Path Rel File
relFileReadmeTxt = $(mkRelFile "README.txt")

relDirScript :: Path Rel Dir
relDirScript = $(mkRelDir "script")

relDirScripts :: Path Rel Dir
relDirScripts = $(mkRelDir "scripts")

relFileConfigYaml :: Path Rel File
relFileConfigYaml = $(mkRelFile "config.yaml")

relDirSnapshots :: Path Rel Dir
relDirSnapshots = $(mkRelDir "snapshots")

relDirGlobalHints :: Path Rel Dir
relDirGlobalHints = $(mkRelDir "global-hints")

relFileGlobalHintsYaml :: Path Rel File
relFileGlobalHintsYaml = $(mkRelFile "global-hints.yaml")

relDirInstall :: Path Rel Dir
relDirInstall = $(mkRelDir "install")

relDirCompilerTools :: Path Rel Dir
relDirCompilerTools = $(mkRelDir "compiler-tools")

relDirHoogle :: Path Rel Dir
relDirHoogle = $(mkRelDir "hoogle")

relFileDatabaseHoo :: Path Rel File
relFileDatabaseHoo = $(mkRelFile "database.hoo")

relDirPkgdb :: Path Rel Dir
relDirPkgdb = $(mkRelDir "pkgdb")

relFileStorage :: Path Rel File
relFileStorage = $(mkRelFile "stack.sqlite3")

relDirLoadedSnapshotCache :: Path Rel Dir
relDirLoadedSnapshotCache = $(mkRelDir "loaded-snapshot-cached")

-- | Suffix applied to an installation root to get the bin dir
bindirSuffix :: Path Rel Dir
bindirSuffix = relDirBin

-- | Suffix applied to an installation root to get the doc dir
docDirSuffix :: Path Rel Dir
docDirSuffix = $(mkRelDir "doc")

-- | Suffix applied to a path to get the @html@ directory.
htmlDirSuffix :: Path Rel Dir
htmlDirSuffix = $(mkRelDir "html")

relDirHpc :: Path Rel Dir
relDirHpc = $(mkRelDir "hpc")

relDirLib :: Path Rel Dir
relDirLib = $(mkRelDir "lib")

relDirShare :: Path Rel Dir
relDirShare = $(mkRelDir "share")

relDirLibexec :: Path Rel Dir
relDirLibexec = $(mkRelDir "libexec")

relDirEtc :: Path Rel Dir
relDirEtc = $(mkRelDir "etc")

setupGhciShimCode :: Builder
setupGhciShimCode = byteString $(do
    path <- makeRelativeToProject "src/setup-shim/StackSetupShim.hs"
    embedFile path)

relDirSetupExeCache :: Path Rel Dir
relDirSetupExeCache = $(mkRelDir "setup-exe-cache")

relDirSetupExeSrc :: Path Rel Dir
relDirSetupExeSrc = $(mkRelDir "setup-exe-src")

relFileConfigure :: Path Rel File
relFileConfigure = $(mkRelFile "configure")

relDirDist :: Path Rel Dir
relDirDist = $(mkRelDir "dist")

relFileSetupMacrosH :: Path Rel File
relFileSetupMacrosH = $(mkRelFile "setup_macros.h")

relDirSetup :: Path Rel Dir
relDirSetup = $(mkRelDir "setup")

relFileSetupLower :: Path Rel File
relFileSetupLower = $(mkRelFile "setup")

relDirMingw :: Path Rel Dir
relDirMingw = $(mkRelDir "mingw")

relDirMingw32 :: Path Rel Dir
relDirMingw32 = $(mkRelDir "mingw32")

relDirMingw64 :: Path Rel Dir
relDirMingw64 = $(mkRelDir "mingw64")

relDirClang32 :: Path Rel Dir
relDirClang32 = $(mkRelDir "clang32")

relDirClang64 :: Path Rel Dir
relDirClang64 = $(mkRelDir "clang64")

relDirClangArm64 :: Path Rel Dir
relDirClangArm64 = $(mkRelDir "clangarm64")

relDirUcrt64 :: Path Rel Dir
relDirUcrt64 = $(mkRelDir "ucrt64")

relDirLocal :: Path Rel Dir
relDirLocal = $(mkRelDir "local")

relDirUsr :: Path Rel Dir
relDirUsr = $(mkRelDir "usr")

relDirInclude :: Path Rel Dir
relDirInclude = $(mkRelDir "include")

relFileIndexHtml :: Path Rel File
relFileIndexHtml = $(mkRelFile "index.html")

relDirAll :: Path Rel Dir
relDirAll = $(mkRelDir "all")

relFilePackageCache :: Path Rel File
relFilePackageCache = $(mkRelFile "package.cache")

relFileDockerfile :: Path Rel File
relFileDockerfile = $(mkRelFile "Dockerfile")

relFileGhciScript :: Path Rel File
relFileGhciScript = $(mkRelFile "ghci-script")

relDirCombined :: Path Rel Dir
relDirCombined = $(mkRelDir "combined")

relFileHpcIndexHtml :: Path Rel File
relFileHpcIndexHtml = $(mkRelFile "hpc_index.html")

relDirCustom :: Path Rel Dir
relDirCustom = $(mkRelDir "custom")

relDirPackageConfInplace :: Path Rel Dir
relDirPackageConfInplace = $(mkRelDir "package.conf.inplace")

relDirExtraTixFiles :: Path Rel Dir
relDirExtraTixFiles = $(mkRelDir "extra-tix-files")

relDirInstalledPackages :: Path Rel Dir
relDirInstalledPackages = $(mkRelDir "installed-packages")

backupUrlRelPath :: Path Rel File
backupUrlRelPath = $(mkRelFile "downloaded.template.file.hsfiles")

relDirDotLocal :: Path Rel Dir
relDirDotLocal = $(mkRelDir ".local")

relDirDotSsh :: Path Rel Dir
relDirDotSsh = $(mkRelDir ".ssh")

relDirDotStackProgName :: Path Rel Dir
relDirDotStackProgName = $(mkRelDir ('.' : stackProgName))

relDirUnderHome :: Path Rel Dir
relDirUnderHome = $(mkRelDir "_home")

relDirSrc :: Path Rel Dir
relDirSrc = $(mkRelDir "src")

relFileLibcMuslx86_64So1 :: Path Rel File
relFileLibcMuslx86_64So1 = $(mkRelFile "libc.musl-x86_64.so.1")

relFileLibtinfoSo5 :: Path Rel File
relFileLibtinfoSo5 = $(mkRelFile "libtinfo.so.5")

relFileLibtinfoSo6 :: Path Rel File
relFileLibtinfoSo6 = $(mkRelFile "libtinfo.so.6")

relFileLibncurseswSo6 :: Path Rel File
relFileLibncurseswSo6 = $(mkRelFile "libncursesw.so.6")

relFileLibgmpSo10 :: Path Rel File
relFileLibgmpSo10 = $(mkRelFile "libgmp.so.10")

relFileLibgmpSo3 :: Path Rel File
relFileLibgmpSo3 = $(mkRelFile "libgmp.so.3")

relDirNewCabal :: Path Rel Dir
relDirNewCabal = $(mkRelDir "new-cabal")

relFileSetupExe :: Path Rel File
relFileSetupExe = $(mkRelFile "Setup.exe")

relFileSetupUpper :: Path Rel File
relFileSetupUpper = $(mkRelFile "Setup")

relFile7zexe :: Path Rel File
relFile7zexe = $(mkRelFile "7z.exe")

relFile7zdll :: Path Rel File
relFile7zdll = $(mkRelFile "7z.dll")

relFileMainHs :: Path Rel File
relFileMainHs = $(mkRelFile "Main.hs")

relFileStackDotExe :: Path Rel File
relFileStackDotExe = $(mkRelFile "stack.exe")

relFileStackDotTmpDotExe :: Path Rel File
relFileStackDotTmpDotExe = $(mkRelFile "stack.tmp.exe")

relFileStackDotTmp :: Path Rel File
relFileStackDotTmp = $(mkRelFile "stack.tmp")

relFileStack :: Path Rel File
relFileStack = $(mkRelFile "stack")

-- Technically, we should be consulting the user's current ghc,
-- but that would require loading up a BuildConfig.
ghcShowOptionsOutput :: [String]
ghcShowOptionsOutput =
  $(TH.runIO (readProcess "ghc" ["--show-options"] "") >>= TH.lift . lines)

-- | Relative paths inside a GHC repo to the boot script.
ghcBootScript :: Path Rel File
ghcBootScript = $(mkRelFile "boot")

-- | Relative paths inside a GHC repo to the configure script.
ghcConfigureScript :: Path Rel File
ghcConfigureScript = $(mkRelFile "configure")

-- | Command applicable to GHC's configure script on Windows. See:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/master/hadrian/README.md
ghcConfigureWindows :: [String]
ghcConfigureWindows = ["sh", "configure", "--enable-tarballs-autodownload"]

-- | Command applicable to GHC's configure script on macOS. See:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/master/hadrian/README.md
ghcConfigureMacOS :: [String]
ghcConfigureMacOS = ["./configure", "--with-intree-gmp"]

-- | Command applicable to GHC's configure script on non-Windows, non-macOS.
-- See: https://gitlab.haskell.org/ghc/ghc/-/blob/master/hadrian/README.md
ghcConfigurePosix :: [String]
ghcConfigurePosix = ["./configure"]

relDirHadrian :: Path Rel Dir
relDirHadrian = $(mkRelDir "hadrian")

relFileHadrianStackDotYaml :: Path Rel File
relFileHadrianStackDotYaml = relDirHadrian </> stackDotYaml

-- | Relative paths inside a GHC repo to the Hadrian build batch script.
-- The second path is maintained for compatibility with older GHC versions.
hadrianScriptsWindows :: [Path Rel File]
hadrianScriptsWindows =
  [ $(mkRelFile "hadrian/build-stack.bat")
  , $(mkRelFile "hadrian/build.stack.bat")
  ]

-- | Relative paths inside a GHC repo to the Hadrian build shell script
-- The second path is maintained for compatibility with older GHC versions.
hadrianScriptsPosix :: [Path Rel File]
hadrianScriptsPosix =
  [$(mkRelFile "hadrian/build-stack"), $(mkRelFile "hadrian/build.stack.sh")]

-- | Relative file path for a temporary GHC environment file for tests
testGhcEnvRelFile :: Path Rel File
testGhcEnvRelFile = $(mkRelFile "test-ghc-env")

-- | File inside a dist directory to use for locking
relFileBuildLock :: Path Rel File
relFileBuildLock = $(mkRelFile "build-lock")

-- | What should the default be for stack-developer-mode
stackDeveloperModeDefault :: Bool
stackDeveloperModeDefault = STACK_DEVELOPER_MODE_DEFAULT

-- | What should the default be for stack-developer-mode
isStackUploadDisabled :: Bool
isStackUploadDisabled = STACK_DISABLE_STACK_UPLOAD

-- | The footer to the help for Stack's subcommands
globalFooter :: String
globalFooter =
     "Command 'stack --help' (or '-h') for global options that apply to all "
  <> "subcommands."

-- | The type for GitHub REST API HTTP \'Basic\' authentication.
gitHubBasicAuthType :: ByteString
gitHubBasicAuthType = "Bearer"

-- | Environment variable to hold credentials for GitHub REST API HTTP \'Basic\'
-- authentication.
gitHubTokenEnvVar :: String
gitHubTokenEnvVar = "GH_TOKEN"

-- | Alternate environment variable to hold credentials for GitHub REST API HTTP
-- \'Basic\' authentication.
altGitHubTokenEnvVar :: String
altGitHubTokenEnvVar = "GITHUB_TOKEN"

hackageBaseUrl :: Text
hackageBaseUrl = "https://hackage.haskell.org/"
