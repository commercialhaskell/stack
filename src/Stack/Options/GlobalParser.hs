{-# LANGUAGE RecordWildCards #-}

module Stack.Options.GlobalParser where

import           Control.Monad.Logger              (LogLevel (..))
import           Data.Monoid.Extra
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import qualified Stack.Docker                      as Docker
import           Stack.Init
import           Stack.Options.ConfigParser
import           Stack.Options.LogLevelParser
import           Stack.Options.ResolverParser
import           Stack.Options.Utils
import           Stack.Types.Config
import           Stack.Types.Docker

-- | Parser for global command-line options.
globalOptsParser :: GlobalOptsContext -> Maybe LogLevel -> Parser GlobalOptsMonoid
globalOptsParser kind defLogLevel =
    GlobalOptsMonoid <$>
    optionalFirst (strOption (long Docker.reExecArgName <> hidden <> internal)) <*>
    optionalFirst (option auto (long dockerEntrypointArgName <> hidden <> internal)) <*>
    (First <$> logLevelOptsParser hide0 defLogLevel) <*>
    firstBoolFlags
        "time-in-log"
        "inclusion of timings in logs, for the purposes of using diff with logs"
        hide <*>
    configOptsParser kind <*>
    optionalFirst (abstractResolverOptsParser hide0) <*>
    optionalFirst (compilerOptsParser hide0) <*>
    firstBoolFlags
        "terminal"
        "overriding terminal detection in the case of running in a false terminal"
        hide <*>
    optionalFirst (option readColorWhen
        (long "color" <>
         metavar "WHEN" <>
         help "Specify when to use color in output; WHEN is 'always', 'never', or 'auto'" <>
         hide)) <*>
    optionalFirst
        (strOption
            (long "stack-yaml" <>
             metavar "STACK-YAML" <>
             help ("Override project stack.yaml file " <>
                   "(overrides any STACK_YAML environment variable)") <>
             hide))
  where
    hide = hideMods hide0
    hide0 = kind /= OuterGlobalOpts

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid :: Bool -> GlobalOptsMonoid -> GlobalOpts
globalOptsFromMonoid defaultTerminal GlobalOptsMonoid{..} = GlobalOpts
    { globalReExecVersion = getFirst globalMonoidReExecVersion
    , globalDockerEntrypoint = getFirst globalMonoidDockerEntrypoint
    , globalLogLevel = fromFirst defaultLogLevel globalMonoidLogLevel
    , globalTimeInLog = fromFirst True globalMonoidTimeInLog
    , globalConfigMonoid = globalMonoidConfigMonoid
    , globalResolver = getFirst globalMonoidResolver
    , globalCompiler = getFirst globalMonoidCompiler
    , globalTerminal = fromFirst defaultTerminal globalMonoidTerminal
    , globalColorWhen = fromFirst ColorAuto globalMonoidColorWhen
    , globalStackYaml = getFirst globalMonoidStackYaml }

initOptsParser :: Parser InitOpts
initOptsParser =
    InitOpts <$> searchDirs
             <*> solver <*> omitPackages
             <*> overwrite <*> fmap not ignoreSubDirs
  where
    searchDirs =
      many (textArgument
              (metavar "DIRS" <>
               help "Directories to include, default is current directory."))
    ignoreSubDirs = switch (long "ignore-subdirs" <>
                           help "Do not search for .cabal files in sub directories")
    overwrite = switch (long "force" <>
                       help "Force overwriting an existing stack.yaml")
    omitPackages = switch (long "omit-packages" <>
                           help "Exclude conflicting or incompatible user packages")
    solver = switch (long "solver" <>
             help "Use a dependency solver to determine extra dependencies")
