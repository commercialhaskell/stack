{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Make changes to project or global configuration.
module Stack.ConfigCmd
       (ConfigCmdSet(..)
       ,configCmdSetParser
       ,cfgCmdSet
       ,cfgCmdSetName
       ,configCmdEnvParser
       ,cfgCmdEnv
       ,cfgCmdEnvName
       ,cfgCmdName) where

import           Stack.Prelude
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Attoparsec.Text as P (Parser, parseOnly, skip, skipWhile,
                                           string, takeText, takeWhile)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Options.Applicative.Builder.Extra
import           Pantry (loadSnapshot)
import           Path
import qualified RIO.Map as Map
import           RIO.Process (envVarsL)
import           Stack.Config (makeConcreteResolver, getProjectConfig,
                              getImplicitGlobalProjectDir)
import           Stack.Constants
import           Stack.Types.Config
import           Stack.Types.Resolver
import           System.Environment (getEnvironment)

data ConfigCmdSet
    = ConfigCmdSetResolver (Unresolved AbstractResolver)
    | ConfigCmdSetSystemGhc CommandScope
                            Bool
    | ConfigCmdSetInstallGhc CommandScope
                             Bool

data CommandScope
    = CommandScopeGlobal
      -- ^ Apply changes to the global configuration,
      --   typically at @~/.stack/config.yaml@.
    | CommandScopeProject
      -- ^ Apply changes to the project @stack.yaml@.

configCmdSetScope :: ConfigCmdSet -> CommandScope
configCmdSetScope (ConfigCmdSetResolver _) = CommandScopeProject
configCmdSetScope (ConfigCmdSetSystemGhc scope _) = scope
configCmdSetScope (ConfigCmdSetInstallGhc scope _) = scope

cfgCmdSet
    :: (HasConfig env, HasGHCVariant env)
    => ConfigCmdSet -> RIO env ()
cfgCmdSet cmd = do
    conf <- view configL
    configFilePath <-
             case configCmdSetScope cmd of
                 CommandScopeProject -> do
                     mstackYamlOption <- view $ globalOptsL.to globalStackYaml
                     mstackYaml <- getProjectConfig mstackYamlOption
                     case mstackYaml of
                         PCProject stackYaml -> pure stackYaml
                         PCGlobalProject -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir conf)
                         PCNoProject _extraDeps -> throwString "config command used when no project configuration available" -- maybe modify the ~/.stack/config.yaml file instead?
                 CommandScopeGlobal -> pure (configUserConfigPath conf)
    rawConfig <- liftIO (readFileUtf8 (toFilePath configFilePath))
    config <- either throwM pure (Yaml.decodeEither' $ encodeUtf8 rawConfig)
    newValue <- cfgCmdSetValue (parent configFilePath) cmd
    let yamlLines = T.lines rawConfig
        cmdKey = cfgCmdSetOptionName cmd  -- Text
        cmdKey' = Key.fromText cmdKey     -- Data.Aeson.Key.Key
        newValue' = T.stripEnd $
            decodeUtf8With lenientDecode $ Yaml.encode newValue  -- Text
        file = toFilePath configFilePath  -- String
        file' = display $ T.pack file     -- Utf8Builder
    newYamlLines <- case KeyMap.lookup cmdKey' config of
        Nothing -> do
            logInfo $ file' <> " has been extended."
            pure $ yamlLines <> [cmdKey <> ": " <> newValue']
        Just oldValue -> if oldValue == newValue
            then do
                logInfo $ file' <> " already contained the intended \
                    \configuration and remains unchanged."
                pure yamlLines
            else switchLine file' cmdKey newValue' [] yamlLines
    liftIO $ writeFileUtf8 file (T.unlines newYamlLines)
  where
    switchLine file cmdKey _ searched [] = do
        logWarn $ display cmdKey <> " not found in YAML file " <> file <>
            " as a single line. Multi-line key:value formats are not supported."
        pure $ reverse searched
    switchLine file cmdKey newValue searched (oldLine:rest) =
        case parseOnly (parseLine cmdKey) oldLine of
            Left _ ->
                switchLine file cmdKey newValue (oldLine:searched) rest
            Right (kt, spaces1, spaces2, comment) -> do
                let newLine = renderKey cmdKey kt <> spaces1 <> ":" <>
                        spaces2 <> newValue <> comment
                logInfo $ file <> " has been updated."
                pure $ reverse searched <> (newLine:rest)

    -- This assumes that a top-level key will not be indented in the YAML file.
    parseLine :: Text -> Parser (KeyType, Text, Text, Text)
    parseLine key = do
        kt <- parseKey key
        spaces1 <- P.takeWhile (== ' ')
        skip (== ':')
        spaces2 <- P.takeWhile (== ' ')
        skipWhile (/= ' ')
        comment <- takeText
        pure (kt, spaces1, spaces2, comment)

    -- If the key is, for example, install-ghc, this recognises install-ghc,
    -- 'install-ghc' or "install-ghc".
    parseKey :: Text -> Parser KeyType
    parseKey k =   parsePlainKey k
               <|> parseSingleQuotedKey k
               <|> parseDoubleQuotedKey k

    parsePlainKey :: Text -> Parser KeyType
    parsePlainKey key = do
      _ <- string key
      pure PlainKey

    parseSingleQuotedKey :: Text -> Parser KeyType
    parseSingleQuotedKey = parseQuotedKey SingleQuotedKey '\''

    parseDoubleQuotedKey :: Text -> Parser KeyType
    parseDoubleQuotedKey = parseQuotedKey DoubleQuotedKey '"'

    parseQuotedKey :: KeyType -> Char -> Text -> Parser KeyType
    parseQuotedKey kt c key = do
        skip (==c)
        _ <- string key
        skip (==c)
        pure kt

    renderKey :: Text -> KeyType -> Text
    renderKey key kt = case kt of
        PlainKey -> key
        SingleQuotedKey -> '\'' `T.cons` key `T.snoc` '\''
        DoubleQuotedKey -> '"' `T.cons` key `T.snoc` '"'

-- |Type representing types of representations of keys in YAML files.
data KeyType
    = PlainKey  -- ^ For example: install-ghc
    | SingleQuotedKey  -- ^ For example: 'install-ghc'
    | DoubleQuotedKey  -- ^ For example: "install-ghc"
    deriving (Eq, Show)

cfgCmdSetValue
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir -- ^ root directory of project
    -> ConfigCmdSet -> RIO env Yaml.Value
cfgCmdSetValue root (ConfigCmdSetResolver newResolver) = do
    newResolver' <- resolvePaths (Just root) newResolver
    concreteResolver <- makeConcreteResolver newResolver'
    -- Check that the snapshot actually exists
    void $ loadSnapshot =<< completeSnapshotLocation concreteResolver
    pure (Yaml.toJSON concreteResolver)
cfgCmdSetValue _ (ConfigCmdSetSystemGhc _ bool') =
    pure (Yaml.Bool bool')
cfgCmdSetValue _ (ConfigCmdSetInstallGhc _ bool') =
    pure (Yaml.Bool bool')

cfgCmdSetOptionName :: ConfigCmdSet -> Text
cfgCmdSetOptionName (ConfigCmdSetResolver _) = "resolver"
cfgCmdSetOptionName (ConfigCmdSetSystemGhc _ _) = configMonoidSystemGHCName
cfgCmdSetOptionName (ConfigCmdSetInstallGhc _ _) = configMonoidInstallGHCName

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"

cfgCmdEnvName :: String
cfgCmdEnvName = "env"

configCmdSetParser :: OA.Parser ConfigCmdSet
configCmdSetParser = OA.hsubparser $
  mconcat
    [ OA.command "resolver"
        ( OA.info
            (ConfigCmdSetResolver <$>
             OA.argument
                 readAbstractResolver
                 (OA.metavar "SNAPSHOT" <>
                  OA.help "E.g. \"nightly\" or \"lts-7.2\""))
            (OA.progDesc
               "Change the resolver of the current project."))
    , OA.command (T.unpack configMonoidSystemGHCName)
        ( OA.info
            (ConfigCmdSetSystemGhc <$> scopeFlag <*> boolArgument)
            (OA.progDesc
               "Configure whether Stack should use a system GHC installation \
               \or not."))
    , OA.command (T.unpack configMonoidInstallGHCName)
        ( OA.info
            (ConfigCmdSetInstallGhc <$> scopeFlag <*> boolArgument)
            (OA.progDesc
               "Configure whether Stack should automatically install GHC when \
               \necessary."))
    ]

scopeFlag :: OA.Parser CommandScope
scopeFlag = OA.flag
  CommandScopeProject
  CommandScopeGlobal
  (  OA.long "global"
  <> OA.help
       "Modify the user-specific global configuration file ('config.yaml') \
       \instead of the project-level configuration file ('stack.yaml')."
  )

readBool :: OA.ReadM Bool
readBool = do
  s <- OA.readerAsk
  case s of
    "true" -> pure True
    "false" -> pure False
    _ -> OA.readerError ("Invalid value " ++ show s ++
           ": Expected \"true\" or \"false\"")

boolArgument :: OA.Parser Bool
boolArgument = OA.argument
  readBool
  (  OA.metavar "true|false"
  <> OA.completeWith ["true", "false"]
  )

configCmdEnvParser :: OA.Parser EnvSettings
configCmdEnvParser = EnvSettings
  <$> boolFlags True "locals" "include local package information" mempty
  <*> boolFlags True
        "ghc-package-path" "set GHC_PACKAGE_PATH environment variable" mempty
  <*> boolFlags True "stack-exe" "set STACK_EXE environment variable" mempty
  <*> boolFlags False
        "locale-utf8" "set the GHC_CHARENC environment variable to UTF-8" mempty
  <*> boolFlags False
        "keep-ghc-rts" "keep any GHCRTS environment variable" mempty

data EnvVarAction = EVASet !Text | EVAUnset
  deriving Show

cfgCmdEnv :: EnvSettings -> RIO EnvConfig ()
cfgCmdEnv es = do
  origEnv <- liftIO $ Map.fromList . map (first fromString) <$> getEnvironment
  mkPC <- view $ configL.to configProcessContextSettings
  pc <- liftIO $ mkPC es
  let newEnv = pc ^. envVarsL
      actions = Map.merge
        (pure EVAUnset)
        (Map.traverseMissing $ \_k new -> pure (EVASet new))
        (Map.zipWithMaybeAMatched $ \_k old new -> pure $
            if fromString old == new
              then Nothing
              else Just (EVASet new))
        origEnv
        newEnv
      toLine key EVAUnset = "unset " <> encodeUtf8Builder key <> ";\n"
      toLine key (EVASet value) =
        encodeUtf8Builder key <> "='" <>
        encodeUtf8Builder (T.concatMap escape value) <> -- TODO more efficient to use encodeUtf8BuilderEscaped
        "'; export " <>
        encodeUtf8Builder key <> ";\n"
      escape '\'' = "'\"'\"'"
      escape c = T.singleton c
  hPutBuilder stdout $ Map.foldMapWithKey toLine actions
