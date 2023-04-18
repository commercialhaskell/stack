{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.ConfigParser
  ( configOptsParser
  ) where

import           Data.Char ( toUpper )
import           Options.Applicative
                   ( Parser, auto, completer, completeWith, eitherReader, help
                   , long, metavar, option, short, strOption
                   )
import           Options.Applicative.Builder.Extra
                   ( PathCompleterOpts (..), absDirOption, absFileOption
                   , defaultPathCompleterOpts, dirCompleter, firstBoolFlagsFalse
                   , firstBoolFlagsNoDefault, firstBoolFlagsTrue, optionalFirst
                   , pathCompleterWith
                   )
import           Path ( PathException (..), parseRelDir )
import           Stack.Constants ( stackRootOptionName )
import           Stack.Options.BuildMonoidParser ( buildOptsMonoidParser )
import           Stack.Options.DockerParser ( dockerOptsParser )
import           Stack.Options.GhcBuildParser ( ghcBuildParser )
import           Stack.Options.GhcVariantParser ( ghcVariantParser )
import           Stack.Options.NixParser ( nixOptsParser )
import           Stack.Options.Utils ( GlobalOptsContext (..), hideMods )
import           Stack.Prelude
import           Stack.Types.ColorWhen ( readColorWhen )
import           Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import           Stack.Types.DumpLogs ( DumpLogs (..) )
import qualified System.FilePath as FilePath

-- | Command-line arguments parser for configuration.
configOptsParser :: FilePath -> GlobalOptsContext -> Parser ConfigMonoid
configOptsParser currentDir hide0 =
  ( \stackRoot workDir buildOpts dockerOpts nixOpts systemGHC installGHC arch
     ghcVariant ghcBuild jobs includes libs preprocs overrideGccPath overrideHpack
     skipGHCCheck skipMsys localBin setupInfoLocations modifyCodePage
     allowDifferentUser dumpLogs colorWhen snapLoc noRunCompile -> mempty
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
       , configMonoidCustomPreprocessorExts = preprocs
       , configMonoidOverrideGccPath = overrideGccPath
       , configMonoidOverrideHpack = overrideHpack
       , configMonoidSkipMsys = skipMsys
       , configMonoidLocalBinPath = localBin
       , configMonoidSetupInfoLocations = setupInfoLocations
       , configMonoidModifyCodePage = modifyCodePage
       , configMonoidAllowDifferentUser = allowDifferentUser
       , configMonoidDumpLogs = dumpLogs
       , configMonoidColorWhen = colorWhen
       , configMonoidSnapshotLocation = snapLoc
       , configMonoidNoRunCompile = noRunCompile
       }
  )
  <$> optionalFirst (absDirOption
        ( long stackRootOptionName
        <> metavar (map toUpper stackRootOptionName)
        <> help "Absolute path to the global Stack root directory. Overrides \
                \any STACK_ROOT environment variable."
        <> hide
        ))
  <*> optionalFirst (option (eitherReader (mapLeft showWorkDirError . parseRelDir))
        ( long "work-dir"
        <> metavar "WORK-DIR"
        <> completer
             ( pathCompleterWith
               ( defaultPathCompleterOpts
                   { pcoAbsolute = False, pcoFileFilter = const False }
               )
             )
        <> help "Relative path to Stack's work directory. Overrides any \
                \STACK_WORK environment variable. (default: '.stack-work')"
        <> hide
        ))
  <*> buildOptsMonoidParser hide0
  <*> dockerOptsParser True
  <*> nixOptsParser True
  <*> firstBoolFlagsNoDefault
        "system-ghc"
        "using the system installed GHC (on the PATH) if it is available and \
        \its version matches. (default: disabled)"
        hide
  <*> firstBoolFlagsTrue
        "install-ghc"
        "downloading and installing GHC if necessary. (Can be done manually \
        \with 'stack setup'.)"
        hide
  <*> optionalFirst (strOption
        (  long "arch"
        <> metavar "ARCH"
        <> help "System architecture, e.g. i386, x86_64."
        <> hide
        ))
  <*> optionalFirst (ghcVariantParser (hide0 /= OuterGlobalOpts))
  <*> optionalFirst (ghcBuildParser (hide0 /= OuterGlobalOpts))
  <*> optionalFirst (option auto
        (  long "jobs"
        <> short 'j'
        <> metavar "JOBS"
        <> help "Number of concurrent jobs to run."
        <> hide
        ))
  <*> many ((currentDir FilePath.</>) <$> strOption
        (  long "extra-include-dirs"
        <> metavar "DIR"
        <> completer dirCompleter
        <> help "Extra directories to check for C header files."
        <> hide
        ))
  <*> many ((currentDir FilePath.</>) <$> strOption
        (  long "extra-lib-dirs"
        <> metavar "DIR"
        <> completer dirCompleter
        <> help "Extra directories to check for libraries."
        <> hide
        ))
  <*> many (strOption
        (  long "custom-preprocessor-extensions"
        <> metavar "EXT"
        <> help "Extensions used for custom preprocessors."
        <> hide
        ))
  <*> optionalFirst (absFileOption
        (  long "with-gcc"
        <> metavar "PATH-TO-GCC"
        <> help "Use gcc found at PATH-TO-GCC."
        <> hide
        ))
  <*> optionalFirst (strOption
        (  long "with-hpack"
        <> metavar "HPACK"
        <> help "Use HPACK executable (overrides bundled Hpack)."
        <> hide
        ))
  <*> firstBoolFlagsFalse
        "skip-ghc-check"
        "skipping the GHC version and architecture check."
        hide
  <*> firstBoolFlagsFalse
        "skip-msys"
        "skipping the local MSYS installation (Windows only)."
        hide
  <*> optionalFirst ((currentDir FilePath.</>) <$> strOption
        ( long "local-bin-path"
        <> metavar "DIR"
        <> completer dirCompleter
        <> help "Override the target directory for 'stack build --copy-bins' \
                \and 'stack install'. DIR can be an absolute path or one \
                \relative to the current directory."
        <> hide
        ))
  <*> many (strOption
        (  long "setup-info-yaml"
        <> help "Alternate URL or path (relative or absolute) for Stack \
                \dependencies."
        <> metavar "URL"
        ))
  <*> firstBoolFlagsTrue
        "modify-code-page"
        "setting the codepage to support UTF-8 (Windows only)."
        hide
  <*> firstBoolFlagsNoDefault
        "allow-different-user"
        "permission for users other than the owner of the Stack root directory \
        \to use a Stack installation (POSIX only). (default: inside Docker, \
        \ true; otherwise, false)"
        hide
  <*> fmap toDumpLogs (firstBoolFlagsNoDefault
        "dump-logs"
        "dump the build output logs for local packages to the console. \
        \(default: dump warning logs)"
        hide)
  <*> optionalFirst (option readColorWhen
        (  long "color"
        <> long "colour"
        <> metavar "WHEN"
        <> completeWith ["always", "never", "auto"]
        <> help "Specify when to use color in output; WHEN is 'always', \
                \'never', or 'auto'. On Windows versions before Windows \
                \10, for terminals that do not support color codes, the \
                \default is 'never'; color may work on terminals that \
                \support color codes."
        <> hide
        ))
  <*> optionalFirst (strOption
        (  long "snapshot-location-base"
        <> help "The base location of LTS/Nightly snapshots."
        <> metavar "URL"
        ))
  <*> firstBoolFlagsFalse
        "script-no-run-compile"
        "the use of options `--no-run --compile` with `stack script`."
        hide
 where
  hide = hideMods (hide0 /= OuterGlobalOpts)
  toDumpLogs (First (Just True)) = First (Just DumpAllLogs)
  toDumpLogs (First (Just False)) = First (Just DumpNoLogs)
  toDumpLogs (First Nothing) = First Nothing
  showWorkDirError err = case fromException err of
    Just (InvalidRelDir x) ->
      "Stack failed to interpret the value of the option as a valid\n\
      \relative path to a directory. Stack will not accept an absolute path. A \
      \path\n\
      \containing a .. (parent directory) component is not valid.\n\n\
      \If set, Stack expects the value to identify the location of Stack's \
      \work\n\
      \directory, relative to the root directory of the project or package. \
      \Stack\n\
      \encountered the value:\n"
      ++ x
    _ -> displayException err
