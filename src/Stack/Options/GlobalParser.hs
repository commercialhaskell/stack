{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Functions to parse Stack's \'global\' command line arguments.
module Stack.Options.GlobalParser
  ( globalOptsFromMonoid
  , globalOptsParser
  ) where

import           Options.Applicative
                   ( Parser, ReadM, auto, completer, help, hidden, internal
                   , long, metavar, option, strOption, value
                   )
import           Options.Applicative.Builder.Extra
                   ( fileExtCompleter, firstBoolFlagsFalse
                   , firstBoolFlagsNoDefault, firstBoolFlagsTrue, optionalFirst
                   )
import           Options.Applicative.Types ( readerAsk )
import           Path.IO ( getCurrentDir, resolveDir', resolveFile' )
import qualified Stack.Docker as Docker
import           Stack.Prelude
import           Stack.Options.ConfigParser ( configOptsParser )
import           Stack.Options.LogLevelParser ( logLevelOptsParser )
import           Stack.Options.SnapshotParser
                   ( abstractSnapshotOptsParser, compilerOptsParser )
import           Stack.Options.Utils ( GlobalOptsContext (..), hideMods )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid (..) )
import           Stack.Types.LockFileBehavior
                   ( LockFileBehavior (..), readLockFileBehavior )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           Stack.Types.Docker ( dockerEntrypointArgName )

-- | Parser for global command-line options.
globalOptsParser ::
     FilePath
  -> GlobalOptsContext
  -> Parser GlobalOptsMonoid
globalOptsParser currentDir kind = GlobalOptsMonoid
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
  <*> (First <$> logLevelOptsParser hide0)
  <*> firstBoolFlagsTrue
        "time-in-log"
        "inclusion of timings in logs, for the purposes of using diff with \
        \logs."
        hide
  <*> firstBoolFlagsFalse
        "rsl-in-log"
        "inclusion of raw snapshot layer (rsl) in logs."
        hide
  <*> firstBoolFlagsFalse
        "plan-in-log"
        "inclusion of information about build plan construction in logs."
        hide
  <*> configOptsParser currentDir kind
  <*> optionalFirst (abstractSnapshotOptsParser hide0)
  <*> pure (First Nothing)
  <*> optionalFirst (compilerOptsParser hide0)
      -- snapshot root is only set via the script command
  <*> firstBoolFlagsNoDefault
        "terminal"
        "overriding terminal detection in the case of running in a false \
        \terminal."
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
                \messages."
        <> hide
        ))
  <*> optionalFirst (strOption
        (  long "stack-yaml"
        <> metavar "STACK-YAML"
        <> completer (fileExtCompleter [".yaml"])
        <> help "Override project stack.yaml file (overrides any STACK_YAML \
                \environment variable)."
        <> hide
        ))
  <*> optionalFirst (option readLockFileBehavior
        (  long "lock-file"
        <> help "Specify how to interact with lock files. (default: if \
                \snapshot is overridden: read-only; otherwise: read/write)"
        <> hide
        ))
 where
  hide = hideMods hide0
  hide0 = kind /= OuterGlobalOpts

-- | Create GlobalOpts from GlobalOptsMonoid.
globalOptsFromMonoid ::
     MonadIO m
  => String
     -- ^ The name of the current Stack executable, as it was invoked.
  -> Maybe (Path Abs File)
     -- ^ The path to the current Stack executable, if the operating system
     -- provides a reliable way to determine it and where a result was
     -- available.
  -> Bool
  -> GlobalOptsMonoid
  -> m GlobalOpts
globalOptsFromMonoid progName mExecutablePath defaultTerminal globalMonoid = do
  snapshot <- for (getFirst globalMonoid.snapshot) $ \us -> do
    root <-
      case globalMonoid.snapshotRoot of
        First Nothing -> getCurrentDir
        First (Just dir) -> resolveDir' dir
    resolvePaths (Just root) us
  stackYaml <-
    case getFirst globalMonoid.stackYaml of
      Nothing -> pure SYLDefault
      Just fp -> SYLOverride <$> resolveFile' fp
  let lockFileBehavior =
        let defLFB =
              case getFirst globalMonoid.snapshot of
                Nothing -> LFBReadWrite
                _ -> LFBReadOnly
        in  fromFirst defLFB globalMonoid.lockFileBehavior
  pure GlobalOpts
    { reExecVersion = getFirst globalMonoid.reExecVersion
    , dockerEntrypoint = getFirst globalMonoid.dockerEntrypoint
    , logLevel = fromFirst defaultLogLevel globalMonoid.logLevel
    , timeInLog = fromFirstTrue globalMonoid.timeInLog
    , rslInLog = fromFirstFalse globalMonoid.rslInLog
    , planInLog = fromFirstFalse globalMonoid.planInLog
    , configMonoid = globalMonoid.configMonoid
    , snapshot
    , compiler = getFirst globalMonoid.compiler
    , terminal = fromFirst defaultTerminal globalMonoid.terminal
    , stylesUpdate = globalMonoid.styles
    , termWidthOpt = getFirst globalMonoid.termWidthOpt
    , stackYaml
    , lockFileBehavior
    , progName
    , mExecutablePath
    }

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

readStyles :: ReadM StylesUpdate
readStyles = parseStylesUpdateFromString <$> readerAsk
