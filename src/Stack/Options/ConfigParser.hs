module Stack.Options.ConfigParser where

import           Data.Char
import           Data.Either.Combinators
import           Data.Monoid.Extra
import qualified Data.Set                          as Set
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Path
import           Stack.Constants
import           Stack.Options.BuildMonoidParser
import           Stack.Options.DockerParser
import           Stack.Options.GhcBuildParser
import           Stack.Options.GhcVariantParser
import           Stack.Options.NixParser
import           Stack.Options.Utils
import           Stack.Types.Config
import qualified System.FilePath as FilePath

-- | Command-line arguments parser for configuration.
configOptsParser :: FilePath -> GlobalOptsContext -> Parser ConfigMonoid
configOptsParser currentDir hide0 =
    (\stackRoot workDir buildOpts dockerOpts nixOpts systemGHC installGHC arch ghcVariant ghcBuild jobs includes libs
    progAlexPath progArPath progC2hsPath progCpphsPath progGccPath progGhcPath progGhcPkgPath progGhcjsPath progGhcjsPkgPath progGreencardPath progHaddockPath progHappyPath progHaskellSuitePath progHaskellSuitePkgPath progHmakePath progHpcPath progHsc2hsPath progHscolourPath progJhcPath progLdPath progLhcPath progLhcPkgPath progPkgConfigPath progStripPath progTarPath progUhcPath
    progAlexOptions progArOptions progC2hsOptions progCpphsOptions progGccOptions progGhcOptions progGhcPkgOptions progGhcjsOptions progGhcjsPkgOptions progGreencardOptions progHaddockOptions progHappyOptions progHaskellSuiteOptions progHaskellSuitePkgOptions progHmakeOptions progHpcOptions progHsc2hsOptions progHscolourOptions progJhcOptions progLdOptions progLhcOptions progLhcPkgOptions progPkgConfigOptions progStripOptions progTarOptions progUhcOptions
    skipGHCCheck skipMsys localBin modifyCodePage allowDifferentUser dumpLogs -> mempty
        { configMonoidStackRoot = stackRoot
        , configMonoidWorkDir = workDir
        , configMonoidBuildOpts = buildOpts
        , configMonoidDockerOpts = dockerOpts
        , configMonoidNixOpts = nixOpts
        , configMonoidSystemGHC = systemGHC
        , configMonoidInstallGHC = installGHC
        , configMonoidSkipGHCCheck = skipGHCCheck
        , configMonoidArch = arch
        , configMonoidGHCVariant = ghcVariant
        , configMonoidGHCBuild = ghcBuild
        , configMonoidJobs = jobs
        , configMonoidExtraIncludeDirs = includes
        , configMonoidExtraLibDirs = libs
        , configMonoidProgAlexPath            = progAlexPath
        , configMonoidProgArPath              = progArPath
        , configMonoidProgC2hsPath            = progC2hsPath
        , configMonoidProgCpphsPath           = progCpphsPath
        , configMonoidProgGccPath             = progGccPath
        , configMonoidProgGhcPath             = progGhcPath
        , configMonoidProgGhcPkgPath          = progGhcPkgPath
        , configMonoidProgGhcjsPath           = progGhcjsPath
        , configMonoidProgGhcjsPkgPath        = progGhcjsPkgPath
        , configMonoidProgGreencardPath       = progGreencardPath
        , configMonoidProgHaddockPath         = progHaddockPath
        , configMonoidProgHappyPath           = progHappyPath
        , configMonoidProgHaskellSuitePath    = progHaskellSuitePath
        , configMonoidProgHaskellSuitePkgPath = progHaskellSuitePkgPath
        , configMonoidProgHmakePath           = progHmakePath
        , configMonoidProgHpcPath             = progHpcPath
        , configMonoidProgHsc2hsPath          = progHsc2hsPath
        , configMonoidProgHscolourPath        = progHscolourPath
        , configMonoidProgJhcPath             = progJhcPath
        , configMonoidProgLdPath              = progLdPath
        , configMonoidProgLhcPath             = progLhcPath
        , configMonoidProgLhcPkgPath          = progLhcPkgPath
        , configMonoidProgPkgConfigPath       = progPkgConfigPath
        , configMonoidProgStripPath           = progStripPath
        , configMonoidProgTarPath             = progTarPath
        , configMonoidProgUhcPath             = progUhcPath
        , configMonoidProgAlexOptions            = progAlexOptions
        , configMonoidProgArOptions              = progArOptions
        , configMonoidProgC2hsOptions            = progC2hsOptions
        , configMonoidProgCpphsOptions           = progCpphsOptions
        , configMonoidProgGccOptions             = progGccOptions
        , configMonoidProgGhcOptions             = progGhcOptions
        , configMonoidProgGhcPkgOptions          = progGhcPkgOptions
        , configMonoidProgGhcjsOptions           = progGhcjsOptions
        , configMonoidProgGhcjsPkgOptions        = progGhcjsPkgOptions
        , configMonoidProgGreencardOptions       = progGreencardOptions
        , configMonoidProgHaddockOptions         = progHaddockOptions
        , configMonoidProgHappyOptions           = progHappyOptions
        , configMonoidProgHaskellSuiteOptions    = progHaskellSuiteOptions
        , configMonoidProgHaskellSuitePkgOptions = progHaskellSuitePkgOptions
        , configMonoidProgHmakeOptions           = progHmakeOptions
        , configMonoidProgHpcOptions             = progHpcOptions
        , configMonoidProgHsc2hsOptions          = progHsc2hsOptions
        , configMonoidProgHscolourOptions        = progHscolourOptions
        , configMonoidProgJhcOptions             = progJhcOptions
        , configMonoidProgLdOptions              = progLdOptions
        , configMonoidProgLhcOptions             = progLhcOptions
        , configMonoidProgLhcPkgOptions          = progLhcPkgOptions
        , configMonoidProgPkgConfigOptions       = progPkgConfigOptions
        , configMonoidProgStripOptions           = progStripOptions
        , configMonoidProgTarOptions             = progTarOptions
        , configMonoidProgUhcOptions             = progUhcOptions
        , configMonoidSkipMsys = skipMsys
        , configMonoidLocalBinPath = localBin
        , configMonoidModifyCodePage = modifyCodePage
        , configMonoidAllowDifferentUser = allowDifferentUser
        , configMonoidDumpLogs = dumpLogs
        })
    <$> optionalFirst (absDirOption
            ( long stackRootOptionName
            <> metavar (map toUpper stackRootOptionName)
            <> help ("Absolute path to the global stack root directory " ++
                     "(Overrides any STACK_ROOT environment variable)")
            <> hide
            ))
    <*> optionalFirst (option (eitherReader (mapLeft showWorkDirError . parseRelDir))
            ( long "work-dir"
            <> metavar "WORK-DIR"
            <> completer (pathCompleterWith (defaultPathCompleterOpts { pcoAbsolute = False, pcoFileFilter = const False }))
            <> help ("Relative path of work directory " ++
                     "(Overrides any STACK_WORK environment variable, default is '.stack-work')")
            <> hide
            ))
    <*> buildOptsMonoidParser hide0
    <*> dockerOptsParser True
    <*> nixOptsParser True
    <*> firstBoolFlags
            "system-ghc"
            "using the system installed GHC (on the PATH) if available and a matching version. Disabled by default."
            hide
    <*> firstBoolFlags
            "install-ghc"
            "downloading and installing GHC if necessary (can be done manually with stack setup)"
            hide
    <*> optionalFirst (strOption
            ( long "arch"
           <> metavar "ARCH"
           <> help "System architecture, e.g. i386, x86_64"
           <> hide
            ))
    <*> optionalFirst (ghcVariantParser (hide0 /= OuterGlobalOpts))
    <*> optionalFirst (ghcBuildParser (hide0 /= OuterGlobalOpts))
    <*> optionalFirst (option auto
            ( long "jobs"
           <> short 'j'
           <> metavar "JOBS"
           <> help "Number of concurrent jobs to run"
           <> hide
            ))
    <*> fmap Set.fromList (many ((currentDir FilePath.</>) <$> strOption
            ( long "extra-include-dirs"
           <> metavar "DIR"
           <> completer dirCompleter
           <> help "Extra directories to check for C header files"
           <> hide
            )))
    <*> fmap Set.fromList (many ((currentDir FilePath.</>) <$> strOption
            ( long "extra-lib-dirs"
           <> metavar "DIR"
           <> completer dirCompleter
           <> help "Extra directories to check for libraries"
           <> hide
            )))
    <*> optionalFirst (absFileOption
                ( long "with-alex"
               <> metavar "PATH-TO-ALEX"
               <> help "Use alex found at PATH-TO-ALEX"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-ar"
               <> metavar "PATH-TO-AR"
               <> help "Use ar found at PATH-TO-AR"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-c2hs"
               <> metavar "PATH-TO-C2HS"
               <> help "Use c2hs found at PATH-TO-C2HS"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-cpphs"
               <> metavar "PATH-TO-CPPHS"
               <> help "Use cpphs found at PATH-TO-CPPHS"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-gcc"
               <> metavar "PATH-TO-GCC"
               <> help "Use gcc found at PATH-TO-GCC"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-ghc"
               <> metavar "PATH-TO-GHC"
               <> help "Use ghc found at PATH-TO-GHC"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-ghc-pkg"
               <> metavar "PATH-TO-GHC-PKG"
               <> help "Use ghc-pkg found at PATH-TO-GHC-PKG"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-ghcjs"
               <> metavar "PATH-TO-GHCJS"
               <> help "Use ghcjs found at PATH-TO-GHCJS"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-ghcjs-pkg"
               <> metavar "PATH-TO-GHCJS-PKG"
               <> help "Use ghcjs-pkg found at PATH-TO-GHCJS-PKG"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-greencard"
               <> metavar "PATH-TO-GREENCARD"
               <> help "Use greencard found at PATH-TO-GREENCARD"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-haddock"
               <> metavar "PATH-TO-HADDOCK"
               <> help "Use haddock found at PATH-TO-HADDOCK"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-happy"
               <> metavar "PATH-TO-HAPPY"
               <> help "Use happy found at PATH-TO-HAPPY"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-haskell-suite"
               <> metavar "PATH-TO-HASKELL-SUITE"
               <> help "Use haskell-suite found at PATH-TO-HASKELL-SUITE"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-haskell-suite-pkg"
               <> metavar "PATH-TO-HASKELL-SUITE-PKG"
               <> help "Use haskell-suite-pkg found at PATH-TO-HASKELL-SUITE-PKG"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-hmake"
               <> metavar "PATH-TO-HMAKE"
               <> help "Use hmake found at PATH-TO-HMAKE"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-hpc"
               <> metavar "PATH-TO-HPC"
               <> help "Use hpc found at PATH-TO-HPC"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-hsc2hs"
               <> metavar "PATH-TO-HSC2HS"
               <> help "Use hsc2hs found at PATH-TO-HSC2HS"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-hscolour"
               <> metavar "PATH-TO-HSCOLOUR"
               <> help "Use hscolour found at PATH-TO-HSCOLOUR"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-jhc"
               <> metavar "PATH-TO-JHC"
               <> help "Use jhc found at PATH-TO-JHC"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-ld"
               <> metavar "PATH-TO-LD"
               <> help "Use ld found at PATH-TO-LD"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-lhc"
               <> metavar "PATH-TO-LHC"
               <> help "Use lhc found at PATH-TO-LHC"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-lhc-pkg"
               <> metavar "PATH-TO-LHC-PKG"
               <> help "Use lhc-pkg found at PATH-TO-LHC-PKG"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-pkg-config"
               <> metavar "PATH-TO-PKG-CONFIG"
               <> help "Use pkg-config found at PATH-TO-PKG-CONFIG"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-strip"
               <> metavar "PATH-TO-STRIP"
               <> help "Use strip found at PATH-TO-STRIP"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-tar"
               <> metavar "PATH-TO-TAR"
               <> help "Use tar found at PATH-TO-TAR"
               <> hide
                ))
    <*> optionalFirst (absFileOption
                ( long "with-uhc"
               <> metavar "PATH-TO-UHC"
               <> help "Use uhc found at PATH-TO-UHC"
               <> hide
                ))
    <*> many (textOption
                ( long "alex-option"
               <> metavar "ALEX-OPTIONS"
               <> help "Pass ALEX-OPTIONS to the alex"
               <> hide
                ))
    <*> many (textOption
                ( long "ar-option"
               <> metavar "AR-OPTIONS"
               <> help "Pass AR-OPTIONS to the ar"
               <> hide
                ))
    <*> many (textOption
                ( long "c2hs-option"
               <> metavar "C2HS-OPTIONS"
               <> help "Pass C2HS-OPTIONS to the c2hs"
               <> hide
                ))
    <*> many (textOption
                ( long "cpphs-option"
               <> metavar "CPPHS-OPTIONS"
               <> help "Pass CPPHS-OPTIONS to the cpphs"
               <> hide
                ))
    <*> many (textOption
                ( long "gcc-option"
               <> metavar "GCC-OPTIONS"
               <> help "Pass GCC-OPTIONS to the gcc"
               <> hide
                ))
    <*> many (textOption
                ( long "ghc-option"
               <> metavar "GHC-OPTIONS"
               <> help "Pass GHC-OPTIONS to the ghc"
               <> hide
                ))
    <*> many (textOption
                ( long "ghc-pkg-option"
               <> metavar "GHC-PKG-OPTIONS"
               <> help "Pass GHC-PKG-OPTIONS to the ghc-pkg"
               <> hide
                ))
    <*> many (textOption
                ( long "ghcjs-option"
               <> metavar "GHCJS-OPTIONS"
               <> help "Pass GHCJS-OPTIONS to the ghcjs"
               <> hide
                ))
    <*> many (textOption
                ( long "ghcjs-pkg-option"
               <> metavar "GHCJS-PKG-OPTIONS"
               <> help "Pass GHCJS-PKG-OPTIONS to the ghcjs-pkg"
               <> hide
                ))
    <*> many (textOption
                ( long "greencard-option"
               <> metavar "GREENCARD-OPTIONS"
               <> help "Pass GREENCARD-OPTIONS to the greencard"
               <> hide
                ))
    <*> many (textOption
                ( long "haddock-option"
               <> metavar "HADDOCK-OPTIONS"
               <> help "Pass HADDOCK-OPTIONS to the haddock"
               <> hide
                ))
    <*> many (textOption
                ( long "happy-option"
               <> metavar "HAPPY-OPTIONS"
               <> help "Pass HAPPY-OPTIONS to the happy"
               <> hide
                ))
    <*> many (textOption
                ( long "haskell-suite-option"
               <> metavar "HASKELL-SUITE-OPTIONS"
               <> help "Pass HASKELL-SUITE-OPTIONS to the haskell-suite"
               <> hide
                ))
    <*> many (textOption
                ( long "haskell-suite-pkg-option"
               <> metavar "HASKELL-SUITE-PKG-OPTIONS"
               <> help "Pass HASKELL-SUITE-PKG-OPTIONS to the haskell-suite-pkg"
               <> hide
                ))
    <*> many (textOption
                ( long "hmake-option"
               <> metavar "HMAKE-OPTIONS"
               <> help "Pass HMAKE-OPTIONS to the hmake"
               <> hide
                ))
    <*> many (textOption
                ( long "hpc-option"
               <> metavar "HPC-OPTIONS"
               <> help "Pass HPC-OPTIONS to the hpc"
               <> hide
                ))
    <*> many (textOption
                ( long "hsc2hs-option"
               <> metavar "HSC2HS-OPTIONS"
               <> help "Pass HSC2HS-OPTIONS to the hsc2hs"
               <> hide
                ))
    <*> many (textOption
                ( long "hscolour-option"
               <> metavar "HSCOLOUR-OPTIONS"
               <> help "Pass HSCOLOUR-OPTIONS to the hscolour"
               <> hide
                ))
    <*> many (textOption
                ( long "jhc-option"
               <> metavar "JHC-OPTIONS"
               <> help "Pass JHC-OPTIONS to the jhc"
               <> hide
                ))
    <*> many (textOption
                ( long "ld-option"
               <> metavar "LD-OPTIONS"
               <> help "Pass LD-OPTIONS to the ld"
               <> hide
                ))
    <*> many (textOption
                ( long "lhc-option"
               <> metavar "LHC-OPTIONS"
               <> help "Pass LHC-OPTIONS to the lhc"
               <> hide
                ))
    <*> many (textOption
                ( long "lhc-pkg-option"
               <> metavar "LHC-PKG-OPTIONS"
               <> help "Pass LHC-PKG-OPTIONS to the lhc-pkg"
               <> hide
                ))
    <*> many (textOption
                ( long "pkg-config-option"
               <> metavar "PKG-CONFIG-OPTIONS"
               <> help "Pass PKG-CONFIG-OPTIONS to the pkg-config"
               <> hide
                ))
    <*> many (textOption
                ( long "strip-option"
               <> metavar "STRIP-OPTIONS"
               <> help "Pass STRIP-OPTIONS to the strip"
               <> hide
                ))
    <*> many (textOption
                ( long "tar-option"
               <> metavar "TAR-OPTIONS"
               <> help "Pass TAR-OPTIONS to the tar"
               <> hide
                ))
    <*> many (textOption
                ( long "uhc-option"
               <> metavar "UHC-OPTIONS"
               <> help "Pass UHC-OPTIONS to the uhc"
               <> hide
                ))
    <*> firstBoolFlags
            "skip-ghc-check"
            "skipping the GHC version and architecture check"
            hide
    <*> firstBoolFlags
            "skip-msys"
            "skipping the local MSYS installation (Windows only)"
            hide
    <*> optionalFirst (strOption
             ( long "local-bin-path"
            <> metavar "DIR"
            <> completer dirCompleter
            <> help "Install binaries to DIR"
            <> hide
             ))
    <*> firstBoolFlags
            "modify-code-page"
            "setting the codepage to support UTF-8 (Windows only)"
            hide
    <*> firstBoolFlags
            "allow-different-user"
            ("permission for users other than the owner of the stack root " ++
                "directory to use a stack installation (POSIX only)")
            hide
    <*> fmap toDumpLogs
            (firstBoolFlags
             "dump-logs"
             "dump the build output logs for local packages to the console"
             hide)
  where
    hide = hideMods (hide0 /= OuterGlobalOpts)
    toDumpLogs (First (Just True)) = First (Just DumpAllLogs)
    toDumpLogs (First (Just False)) = First (Just DumpNoLogs)
    toDumpLogs (First Nothing) = First Nothing
    showWorkDirError err = show err ++
        "\nNote that --work-dir must be a relative child directory, because work-dirs outside of the package are not supported by Cabal." ++
        "\nSee https://github.com/commercialhaskell/stack/issues/2954"
