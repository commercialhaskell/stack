{-# LANGUAGE NoImplicitPrelude #-}
module Stack.Options.ConfigParser where

import           Data.Char
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
import           Stack.Prelude
import           Stack.Types.Config
import qualified System.FilePath as FilePath

-- | Command-line arguments parser for configuration.
configOptsParser :: FilePath -> GlobalOptsContext -> Parser ConfigMonoid
configOptsParser currentDir hide0 =
    (\stackRoot workDir buildOpts dockerOpts nixOpts systemGHC installGHC arch
        ghcVariant ghcBuild jobs includes libs overrideGccPath overrideHpack
        skipGHCCheck skipMsys localBin setupInfoLocations modifyCodePage
        allowDifferentUser dumpLogs colorWhen -> mempty
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
            , configMonoidOverrideGccPath = overrideGccPath
            , configMonoidOverrideHpack = overrideHpack
            , configMonoidSkipMsys = skipMsys
            , configMonoidLocalBinPath = localBin
            , configMonoidSetupInfoLocations = setupInfoLocations
            , configMonoidModifyCodePage = modifyCodePage
            , configMonoidAllowDifferentUser = allowDifferentUser
            , configMonoidDumpLogs = dumpLogs
            , configMonoidColorWhen = colorWhen
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
    <*> firstBoolFlagsNoDefault
            "system-ghc"
            "using the system installed GHC (on the PATH) if it is available and its version matches. Disabled by default."
            hide
    <*> firstBoolFlagsTrue
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
    <*> many ((currentDir FilePath.</>) <$> strOption
            ( long "extra-include-dirs"
           <> metavar "DIR"
           <> completer dirCompleter
           <> help "Extra directories to check for C header files"
           <> hide
            ))
    <*> many ((currentDir FilePath.</>) <$> strOption
            ( long "extra-lib-dirs"
           <> metavar "DIR"
           <> completer dirCompleter
           <> help "Extra directories to check for libraries"
           <> hide
            ))
    <*> optionalFirst (absFileOption
             ( long "with-gcc"
            <> metavar "PATH-TO-GCC"
            <> help "Use gcc found at PATH-TO-GCC"
            <> hide
             ))
    <*> optionalFirst (strOption
             ( long "with-hpack"
            <> metavar "HPACK"
            <> help "Use HPACK executable (overrides bundled Hpack)"
            <> hide
             ))
    <*> firstBoolFlagsFalse
            "skip-ghc-check"
            "skipping the GHC version and architecture check"
            hide
    <*> firstBoolFlagsFalse
            "skip-msys"
            "skipping the local MSYS installation (Windows only)"
            hide
    <*> optionalFirst ((currentDir FilePath.</>) <$> strOption
             ( long "local-bin-path"
            <> metavar "DIR"
            <> completer dirCompleter
            <> help "Install binaries to DIR"
            <> hide
             ))
    <*> many (
        strOption
            ( long "setup-info-yaml"
           <> help "Alternate URL or relative / absolute path for stack dependencies"
           <> metavar "URL" ))
    <*> firstBoolFlagsTrue
            "modify-code-page"
            "setting the codepage to support UTF-8 (Windows only)"
            hide
    <*> firstBoolFlagsNoDefault
            "allow-different-user"
            ("permission for users other than the owner of the stack root " ++
                "directory to use a stack installation (POSIX only) " ++
                "(default: true inside Docker, otherwise false)")
            hide
    <*> fmap toDumpLogs
            (firstBoolFlagsNoDefault
             "dump-logs"
             "dump the build output logs for local packages to the console (default: dump warning logs)"
             hide)
    <*> optionalFirst (option readColorWhen
             ( long "color"
            <> long "colour"
            <> metavar "WHEN"
            <> completeWith ["always", "never", "auto"]
            <> help "Specify when to use color in output; WHEN is 'always', \
                    \'never', or 'auto'. On Windows versions before Windows \
                    \10, for terminals that do not support color codes, the \
                    \default is 'never'; color may work on terminals that \
                    \support color codes"
            <> hide
             ))
  where
    hide = hideMods (hide0 /= OuterGlobalOpts)
    toDumpLogs (First (Just True)) = First (Just DumpAllLogs)
    toDumpLogs (First (Just False)) = First (Just DumpNoLogs)
    toDumpLogs (First Nothing) = First Nothing
    showWorkDirError err = show err ++
        "\nNote that --work-dir must be a relative child directory, because work-dirs outside of the package are not supported by Cabal." ++
        "\nSee https://github.com/commercialhaskell/stack/issues/2954"
