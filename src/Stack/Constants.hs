{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} -- keep TH usage here

-- | Constants used throughout the project.

module Stack.Constants
    (buildPlanDir
    ,buildPlanCacheDir
    ,haskellFileExts
    ,haskellPreprocessorExts
    ,stackDotYaml
    ,stackWorkEnvVar
    ,stackRootEnvVar
    ,stackRootOptionName
    ,deprecatedStackRootOptionName
    ,inContainerEnvVar
    ,inNixShellEnvVar
    ,stackProgNameUpper
    ,wiredInPackages
    ,cabalPackageName
    ,implicitGlobalProjectDirDeprecated
    ,implicitGlobalProjectDir
    ,defaultUserConfigPathDeprecated
    ,defaultUserConfigPath
    ,defaultGlobalConfigPathDeprecated
    ,defaultGlobalConfigPath
    ,platformVariantEnvVar
    ,compilerOptionsCabalFlag
    ,ghcColorForceFlag
    ,minTerminalWidth
    ,maxTerminalWidth
    ,defaultTerminalWidth
    ,osIsWindows
    ,relFileSetupHs
    ,relFileSetupLhs
    ,relFileHpackPackageConfig
    ,relDirGlobalAutogen
    ,relDirAutogen
    ,relDirLogs
    ,relFileCabalMacrosH
    ,relDirBuild
    ,relDirBin
    ,relDirPantry
    ,relDirPrograms
    ,relDirUpperPrograms
    ,relDirStackProgName
    ,relDirStackWork
    ,relFileReadmeTxt
    ,relDirScript
    ,relFileConfigYaml
    ,relDirSnapshots
    ,relDirGlobalHints
    ,relFileGlobalHintsYaml
    ,relDirInstall
    ,relDirCompilerTools
    ,relDirHoogle
    ,relFileDatabaseHoo
    ,relDirPkgdb
    ,relFileStorage
    ,relDirLoadedSnapshotCache
    ,bindirSuffix
    ,docDirSuffix
    ,relDirHpc
    ,relDirLib
    ,relDirShare
    ,relDirLibexec
    ,relDirEtc
    ,setupGhciShimCode
    ,relDirSetupExeCache
    ,relDirSetupExeSrc
    ,relFileConfigure
    ,relDirDist
    ,relFileSetupMacrosH
    ,relDirSetup
    ,relFileSetupLower
    ,relDirMingw
    ,relDirMingw32
    ,relDirMingw64
    ,relDirLocal
    ,relDirUsr
    ,relDirInclude
    ,relFileIndexHtml
    ,relDirAll
    ,relFilePackageCache
    ,relFileDockerfile
    ,relDirHaskellStackGhci
    ,relFileGhciScript
    ,relDirCombined
    ,relFileHpcIndexHtml
    ,relDirCustom
    ,relDirPackageConfInplace
    ,relDirExtraTixFiles
    ,relDirInstalledPackages
    ,backupUrlRelPath
    ,relDirDotLocal
    ,relDirDotSsh
    ,relDirDotStackProgName
    ,relDirUnderHome
    ,relDirSrc
    ,relFileLibtinfoSo5
    ,relFileLibtinfoSo6
    ,relFileLibncurseswSo6
    ,relFileLibgmpSo10
    ,relFileLibgmpSo3
    ,relDirNewCabal
    ,relFileSetupExe
    ,relFileSetupUpper
    ,relFile7zexe
    ,relFile7zdll
    ,relFileMainHs
    ,relFileStack
    ,relFileStackDotExe
    ,relFileStackDotTmpDotExe
    ,relFileStackDotTmp
    ,ghcShowOptionsOutput
    ,hadrianCmdWindows
    ,hadrianCmdPosix
    ,usrLibDirs
    ,testGhcEnvRelFile
    ,relFileBuildLock
    )
    where

import           Data.ByteString.Builder (byteString)
import           Data.Char (toUpper)
import           Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.Set as Set
import           Distribution.Package (mkPackageName)
import qualified Hpack.Config as Hpack
import qualified Language.Haskell.TH.Syntax as TH (runIO, lift)
import           Path as FL
import           Stack.Prelude
import           Stack.Types.Compiler
import           System.Permissions (osIsWindows)
import           System.Process (readProcess)

-- | Extensions used for Haskell modules. Excludes preprocessor ones.
haskellFileExts :: [Text]
haskellFileExts = ["hs", "hsc", "lhs"]

-- | Extensions for modules that are preprocessed by common preprocessors.
haskellPreprocessorExts :: [Text]
haskellPreprocessorExts = ["gc", "chs", "hsc", "x", "y", "ly", "cpphs"]

-- | Name of the 'stack' program, uppercased
stackProgNameUpper :: String
stackProgNameUpper = map toUpper stackProgName

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

-- | Environment variable used to override the '.stack-work' relative dir.
stackWorkEnvVar :: String
stackWorkEnvVar = "STACK_WORK"

-- | Environment variable used to override the '~/.stack' location.
stackRootEnvVar :: String
stackRootEnvVar = "STACK_ROOT"

-- | Option name for the global stack root.
stackRootOptionName :: String
stackRootOptionName = "stack-root"

-- | Deprecated option name for the global stack root.
--
-- Deprecated since stack-1.1.0.
--
-- TODO: Remove occurrences of this variable and use 'stackRootOptionName' only
-- after an appropriate deprecation period.
deprecatedStackRootOptionName :: String
deprecatedStackRootOptionName = "global-stack-root"

-- | Environment variable used to indicate stack is running in container.
inContainerEnvVar :: String
inContainerEnvVar = stackProgNameUpper ++ "_IN_CONTAINER"

-- | Environment variable used to indicate stack is running in container.
-- although we already have STACK_IN_NIX_EXTRA_ARGS that is set in the same conditions,
-- it can happen that STACK_IN_NIX_EXTRA_ARGS is set to empty.
inNixShellEnvVar :: String
inNixShellEnvVar = map toUpper stackProgName ++ "_IN_NIX_SHELL"

-- See https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/ghc/src/Module.html#integerPackageKey
wiredInPackages :: Set PackageName
wiredInPackages =
    maybe (error "Parse error in wiredInPackages") Set.fromList mparsed
  where
    mparsed = mapM parsePackageName
      [ "ghc-prim"
      , "integer-gmp"
      , "integer-simple"
      , "base"
      , "rts"
      , "template-haskell"
      , "dph-seq"
      , "dph-par"
      , "ghc"
      , "interactive"
      ]

-- | Just to avoid repetition and magic strings.
cabalPackageName :: PackageName
cabalPackageName =
    mkPackageName "Cabal"

-- | Deprecated implicit global project directory used when outside of a project.
implicitGlobalProjectDirDeprecated :: Path Abs Dir -- ^ Stack root.
                                   -> Path Abs Dir
implicitGlobalProjectDirDeprecated p =
    p </>
    $(mkRelDir "global")

-- | Implicit global project directory used when outside of a project.
-- Normally, @getImplicitGlobalProjectDir@ should be used instead.
implicitGlobalProjectDir :: Path Abs Dir -- ^ Stack root.
                         -> Path Abs Dir
implicitGlobalProjectDir p =
    p </>
    $(mkRelDir "global-project")

-- | Deprecated default global config path.
defaultUserConfigPathDeprecated :: Path Abs Dir -> Path Abs File
defaultUserConfigPathDeprecated = (</> $(mkRelFile "stack.yaml"))

-- | Default global config path.
-- Normally, @getDefaultUserConfigPath@ should be used instead.
defaultUserConfigPath :: Path Abs Dir -> Path Abs File
defaultUserConfigPath = (</> $(mkRelFile "config.yaml"))

-- | Deprecated default global config path.
-- Note that this will be @Nothing@ on Windows, which is by design.
defaultGlobalConfigPathDeprecated :: Maybe (Path Abs File)
defaultGlobalConfigPathDeprecated = parseAbsFile "/etc/stack/config"

-- | Default global config path.
-- Normally, @getDefaultGlobalConfigPath@ should be used instead.
-- Note that this will be @Nothing@ on Windows, which is by design.
defaultGlobalConfigPath :: Maybe (Path Abs File)
defaultGlobalConfigPath = parseAbsFile "/etc/stack/config.yaml"

-- | Path where build plans are stored.
buildPlanDir :: Path Abs Dir -- ^ Stack root
             -> Path Abs Dir
buildPlanDir = (</> $(mkRelDir "build-plan"))

-- | Path where binary caches of the build plans are stored.
buildPlanCacheDir
  :: Path Abs Dir -- ^ Stack root
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
relFileHpackPackageConfig = $(mkRelFile Hpack.packageConfig)

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

relDirPantry :: Path Rel Dir
relDirPantry = $(mkRelDir "pantry")

relDirPrograms :: Path Rel Dir
relDirPrograms = $(mkRelDir "programs")

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

relDirHaskellStackGhci :: Path Rel Dir
relDirHaskellStackGhci = $(mkRelDir "haskell-stack-ghci")

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

-- | Relative path inside a GHC repo to the Hadrian build batch script
hadrianCmdWindows :: Path Rel File
hadrianCmdWindows = $(mkRelFile "hadrian/build.stack.bat")

-- | Relative path inside a GHC repo to the Hadrian build shell script
hadrianCmdPosix :: Path Rel File
hadrianCmdPosix = $(mkRelFile "hadrian/build.stack.sh")

-- | Used in Stack.Setup for detecting libtinfo, see comments at use site
usrLibDirs :: [Path Abs Dir]
#if WINDOWS
usrLibDirs = []
#else
usrLibDirs = [$(mkAbsDir "/usr/lib"),$(mkAbsDir "/usr/lib64")]
#endif

-- | Relative file path for a temporary GHC environment file for tests
testGhcEnvRelFile :: Path Rel File
testGhcEnvRelFile = $(mkRelFile "test-ghc-env")

-- | File inside a dist directory to use for locking
relFileBuildLock :: Path Rel File
relFileBuildLock = $(mkRelFile "build-lock")
