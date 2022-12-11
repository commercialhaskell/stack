{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack.Options.GlobalParser
  ( globalOptsFromMonoid
  , globalOptsParser
  , initOptsParser
  ) where

import           Options.Applicative
                   ( Parser, auto, completer, help, hidden, internal, long
                   , metavar, option, strOption, switch, value
                   )
import           Options.Applicative.Builder.Extra
                   ( dirCompleter, fileExtCompleter, firstBoolFlagsFalse
                   , firstBoolFlagsNoDefault, firstBoolFlagsTrue, optionalFirst
                   , textArgument
                   )
import           Path.IO ( getCurrentDir, resolveDir', resolveFile' )
import qualified Stack.Docker as Docker
import           Stack.Init ( InitOpts (..) )
import           Stack.Prelude
import           Stack.Options.ConfigParser ( configOptsParser )
import           Stack.Options.LogLevelParser ( logLevelOptsParser )
import           Stack.Options.ResolverParser
                   ( abstractResolverOptsParser, compilerOptsParser )
import           Stack.Options.Utils ( GlobalOptsContext (..), hideMods )
import           Stack.Types.Config
                   ( GlobalOpts (..), GlobalOptsMonoid (..)
                   , LockFileBehavior (..), StackYamlLoc (..), defaultLogLevel
                   , readLockFileBehavior, readStyles
                   )
import           Stack.Types.Docker ( dockerEntrypointArgName )

-- | Parser for global command-line options.
globalOptsParser ::
     FilePath
  -> GlobalOptsContext
  -> Maybe LogLevel
  -> Parser GlobalOptsMonoid
globalOptsParser currentDir kind defLogLevel = GlobalOptsMonoid
  <$> optionalFirst (strOption
        (  long Docker.reExecArgName
        <> hidden
        <> internal
        ))
  <*> optionalFirst (option auto
        (  long dockerEntrypointArgName
        <> hidden
        <> internal
        ))
  <*> (First <$> logLevelOptsParser hide0 defLogLevel)
  <*> firstBoolFlagsTrue
        "time-in-log"
        "inclusion of timings in logs, for the purposes of using diff with logs"
        hide
  <*> firstBoolFlagsFalse
        "rsl-in-log"
        "inclusion of raw snapshot layer (rsl) in logs"
        hide
  <*> configOptsParser currentDir kind
  <*> optionalFirst (abstractResolverOptsParser hide0)
  <*> pure (First Nothing)
  <*> optionalFirst (compilerOptsParser hide0)
      -- resolver root is only set via the script command
  <*> firstBoolFlagsNoDefault
        "terminal"
        "overriding terminal detection in the case of running in a false terminal"
        hide
  <*> option readStyles
        (  long "stack-colors"
        <> long "stack-colours"
        <> metavar "STYLES"
        <> value mempty
        <> help "Specify Stack's output styles; STYLES is a colon-delimited \
                \sequence of key=value, where 'key' is a style name and 'value' \
                \is a semicolon-delimited list of 'ANSI' SGR (Select Graphic \
                \Rendition) control codes (in decimal). Use 'stack ls \
                \stack-colors --basic' to see the current sequence. In shells \
                \where a semicolon is a command separator, enclose STYLES in \
                \quotes."
        <> hide
        )
  <*> optionalFirst (option auto
        (  long "terminal-width"
        <> metavar "INT"
        <> help "Specify the width of the terminal, used for pretty-print \
                \messages"
        <> hide
        ))
  <*> optionalFirst (strOption
        (  long "stack-yaml"
        <> metavar "STACK-YAML"
        <> completer (fileExtCompleter [".yaml"])
        <> help
             (  "Override project stack.yaml file "
             <> "(overrides any STACK_YAML environment variable)"
             )
        <> hide
        ))
  <*> optionalFirst (option readLockFileBehavior
        (  long "lock-file"
        <> help "Specify how to interact with lock files. Default: read/write. \
                \If resolver is overridden: read-only"
        <> hide
        ))
 where
  hide = hideMods hide0
  hide0 = kind /= OuterGlobalOpts

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid ::
     MonadIO m
  => Bool
  -> GlobalOptsMonoid
  -> m GlobalOpts
globalOptsFromMonoid defaultTerminal GlobalOptsMonoid{..} = do
  resolver <- for (getFirst globalMonoidResolver) $ \ur -> do
    root <-
      case globalMonoidResolverRoot of
        First Nothing -> getCurrentDir
        First (Just dir) -> resolveDir' dir
    resolvePaths (Just root) ur
  stackYaml <-
    case getFirst globalMonoidStackYaml of
      Nothing -> pure SYLDefault
      Just fp -> SYLOverride <$> resolveFile' fp
  pure GlobalOpts
    { globalReExecVersion = getFirst globalMonoidReExecVersion
    , globalDockerEntrypoint = getFirst globalMonoidDockerEntrypoint
    , globalLogLevel = fromFirst defaultLogLevel globalMonoidLogLevel
    , globalTimeInLog = fromFirstTrue globalMonoidTimeInLog
    , globalRSLInLog = fromFirstFalse globalMonoidRSLInLog
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
initOptsParser = InitOpts
  <$> searchDirs
  <*> omitPackages
  <*> overwrite
  <*> fmap not ignoreSubDirs
 where
  searchDirs = many (textArgument
    (  metavar "DIR(S)"
    <> completer dirCompleter
    <> help "Directory, or directories, to include in the search for .cabal \
            \files, when initialising. The default is the current directory."
    ))
  ignoreSubDirs = switch
    (  long "ignore-subdirs"
    <> help "Do not search for .cabal files in subdirectories, when \
            \initialising."
    )
  overwrite = switch
    (  long "force"
    <> help "Force an initialisation that overwrites any existing stack.yaml \
            \file."
    )
  omitPackages = switch
    (  long "omit-packages"
    <> help "Exclude conflicting or incompatible user packages, when \
            \initialising."
    )
