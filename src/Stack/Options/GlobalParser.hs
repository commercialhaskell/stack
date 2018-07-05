{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Stack.Options.GlobalParser where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import qualified Stack.Docker                      as Docker
import           Stack.Init
import           Stack.Prelude
import           Stack.Options.ConfigParser
import           Stack.Options.LogLevelParser
import           Stack.Options.ResolverParser
import           Stack.Options.Utils
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Runner

-- | Parser for global command-line options.
globalOptsParser :: FilePath -> GlobalOptsContext -> Maybe LogLevel -> Parser GlobalOptsMonoid
globalOptsParser currentDir kind defLogLevel =
    GlobalOptsMonoid <$>
    optionalFirst (strOption (long Docker.reExecArgName <> hidden <> internal)) <*>
    optionalFirst (option auto (long dockerEntrypointArgName <> hidden <> internal)) <*>
    (First <$> logLevelOptsParser hide0 defLogLevel) <*>
    firstBoolFlags
        "time-in-log"
        "inclusion of timings in logs, for the purposes of using diff with logs"
        hide <*>
    configOptsParser currentDir kind <*>
    optionalFirst (abstractResolverOptsParser hide0) <*>
    optionalFirst (compilerOptsParser hide0) <*>
    firstBoolFlags
        "terminal"
        "overriding terminal detection in the case of running in a false terminal"
        hide <*>
    optionalFirst (option readColorWhen
        (long "color" <>
         metavar "WHEN" <>
         completeWith ["always", "never", "auto"] <>
         help "Specify when to use color in output; WHEN is 'always', 'never', \
              \or 'auto'. On Windows versions before Windows 10, for terminals \
              \that do not support color codes, the default is 'never'; color \
              \may work on terminals that support color codes" <>
         hide)) <*>
    optionalFirst (option auto
        (long "terminal-width" <>
         metavar "INT" <>
         help "Specify the width of the terminal, used for pretty-print messages" <>
         hide)) <*>
    optionalFirst
        (strOption
            (long "stack-yaml" <>
             metavar "STACK-YAML" <>
             completer (fileExtCompleter [".yaml"]) <>
             help ("Override project stack.yaml file " <>
                   "(overrides any STACK_YAML environment variable)") <>
             hide))
  where
    hide = hideMods hide0
    hide0 = kind /= OuterGlobalOpts

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid :: Bool -> ColorWhen -> GlobalOptsMonoid -> GlobalOpts
globalOptsFromMonoid defaultTerminal defaultColorWhen GlobalOptsMonoid{..} = GlobalOpts
    { globalReExecVersion = getFirst globalMonoidReExecVersion
    , globalDockerEntrypoint = getFirst globalMonoidDockerEntrypoint
    , globalLogLevel = fromFirst defaultLogLevel globalMonoidLogLevel
    , globalTimeInLog = fromFirst True globalMonoidTimeInLog
    , globalConfigMonoid = globalMonoidConfigMonoid
    , globalResolver = getFirst globalMonoidResolver
    , globalCompiler = getFirst globalMonoidCompiler
    , globalTerminal = fromFirst defaultTerminal globalMonoidTerminal
    , globalColorWhen = fromFirst defaultColorWhen globalMonoidColorWhen
    , globalTermWidth = getFirst globalMonoidTermWidth
    , globalStackYaml = maybe SYLDefault SYLOverride $ getFirst globalMonoidStackYaml }

initOptsParser :: Parser InitOpts
initOptsParser =
    InitOpts <$> searchDirs
             <*> solver <*> omitPackages
             <*> overwrite <*> fmap not ignoreSubDirs
  where
    searchDirs =
      many (textArgument
              (metavar "DIR" <>
               completer dirCompleter <>
               help "Directories to include, default is current directory."))
    ignoreSubDirs = switch (long "ignore-subdirs" <>
                           help "Do not search for .cabal files in sub directories")
    overwrite = switch (long "force" <>
                       help "Force overwriting an existing stack.yaml")
    omitPackages = switch (long "omit-packages" <>
                           help "Exclude conflicting or incompatible user packages")
    solver = switch (long "solver" <>
             help "Use a dependency solver to determine extra dependencies")
