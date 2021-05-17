{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Stack.Options.GlobalParser where

import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Path.IO (getCurrentDir, resolveDir', resolveFile')
import qualified Stack.Docker                      as Docker
import           Stack.Init
import           Stack.Prelude
import           Stack.Options.ConfigParser
import           Stack.Options.LogLevelParser
import           Stack.Options.ResolverParser
import           Stack.Options.Utils
import           Stack.Types.Config
import           Stack.Types.Docker

-- | Parser for global command-line options.
globalOptsParser :: FilePath -> GlobalOptsContext -> Maybe LogLevel -> Parser GlobalOptsMonoid
globalOptsParser currentDir kind defLogLevel =
    GlobalOptsMonoid <$>
    optionalFirst (strOption (long Docker.reExecArgName <> hidden <> internal)) <*>
    optionalFirst (option auto (long dockerEntrypointArgName <> hidden <> internal)) <*>
    (First <$> logLevelOptsParser hide0 defLogLevel) <*>
    firstBoolFlagsTrue
        "time-in-log"
        "inclusion of timings in logs, for the purposes of using diff with logs"
        hide <*>
    configOptsParser currentDir kind <*>
    optionalFirst (abstractResolverOptsParser hide0) <*>
    pure (First Nothing) <*> -- resolver root is only set via the script command
    optionalFirst (compilerOptsParser hide0) <*>
    firstBoolFlagsNoDefault
        "terminal"
        "overriding terminal detection in the case of running in a false terminal"
        hide <*>
    option readStyles
         (long "stack-colors" <>
          long "stack-colours" <>
          metavar "STYLES" <>
          value mempty <>
          help "Specify stack's output styles; STYLES is a colon-delimited \
               \sequence of key=value, where 'key' is a style name and 'value' \
               \is a semicolon-delimited list of 'ANSI' SGR (Select Graphic \
               \Rendition) control codes (in decimal). Use 'stack ls \
               \stack-colors --basic' to see the current sequence. In shells \
               \where a semicolon is a command separator, enclose STYLES in \
               \quotes." <>
          hide) <*>
    optionalFirst (option auto
        (long "terminal-width" <>
         metavar "INT" <>
         help "Specify the width of the terminal, used for pretty-print messages" <>
         hide)) <*>
    many
        (strOption
            (long "stack-yaml" <>
             metavar "STACK-YAML" <>
             completer (fileExtCompleter [".yaml"]) <>
             help ("Override project stack.yaml file " <>
                   "(overrides any STACK_YAML environment variable)") <>
             hide)) <*>
    optionalFirst (option readLockFileBehavior
        (long "lock-file" <>
         help "Specify how to interact with lock files. Default: read/write. If resolver is overridden: read-only" <>
         hide))
  where
    hide = hideMods hide0
    hide0 = kind /= OuterGlobalOpts

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid :: MonadIO m => Bool -> GlobalOptsMonoid -> m GlobalOpts
globalOptsFromMonoid defaultTerminal GlobalOptsMonoid{..} = do
  resolver <- for (getFirst globalMonoidResolver) $ \ur -> do
    root <-
      case globalMonoidResolverRoot of
        First Nothing -> getCurrentDir
        First (Just dir) -> resolveDir' dir
    resolvePaths (Just root) ur
  stackYaml <-
    case globalMonoidStackYaml of
      [] -> pure SYLDefault
      mainFp:fps -> SYLOverride <$> mapM resolveFile' (mainFp :| fps)
  pure GlobalOpts
    { globalReExecVersion = getFirst globalMonoidReExecVersion
    , globalDockerEntrypoint = getFirst globalMonoidDockerEntrypoint
    , globalLogLevel = fromFirst defaultLogLevel globalMonoidLogLevel
    , globalTimeInLog = fromFirstTrue globalMonoidTimeInLog
    , globalConfigMonoid = globalMonoidConfigMonoid
    , globalResolver = resolver
    , globalCompiler = getFirst globalMonoidCompiler
    , globalTerminal = fromFirst defaultTerminal globalMonoidTerminal
    , globalStylesUpdate = globalMonoidStyles
    , globalTermWidth = getFirst globalMonoidTermWidth
    , globalStackYaml = stackYaml
    , globalLockFileBehavior =
        let defLFB =
              case getFirst globalMonoidResolver of
                Nothing -> LFBReadWrite
                _ -> LFBReadOnly
         in fromFirst defLFB globalMonoidLockFileBehavior
    }

initOptsParser :: Parser InitOpts
initOptsParser =
    InitOpts <$> searchDirs
             <*> omitPackages
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
