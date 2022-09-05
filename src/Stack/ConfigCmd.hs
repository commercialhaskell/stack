{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Make changes to project or global configuration.
module Stack.ConfigCmd
       (cfgCmdName

       -- * config dump project
       ,ConfigCmdDumpProject(..)
       ,configCmdDumpProjectParser
       ,cfgCmdDumpProject
       ,cfgCmdDumpProjectName

       -- * config dump stack
       ,ConfigCmdDumpStack(..)
       ,configCmdDumpStackParser
       ,cfgCmdDumpStack
       ,cfgCmdDumpStackName

       -- * config get
       ,ConfigCmdGet(..)
       ,configCmdGetParser
       ,cfgCmdGet
       ,cfgCmdGetName

       -- * config set
       ,ConfigCmdSet(..)
       ,configCmdSetParser
       ,cfgCmdSet
       ,cfgCmdSetName

       -- * config env
       ,configCmdEnvParser
       ,cfgCmdEnv
       ,cfgCmdEnvName
       ) where

import           Stack.Prelude
import           Data.Coerce (coerce)
import           Pantry.Internal.AesonExtended
                 (ToJSON(..), FromJSON, (.=), WithJSONWarnings (WithJSONWarnings), object)
import           Data.Aeson.Encode.Pretty (encodePretty, encodePretty', confCompare)
import qualified Data.Aeson.Encode.Pretty as Aeson (defConfig)
import qualified Data.Aeson.Key as Key
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.ByteString.Builder (byteString)
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
import           Stack.YamlUpdate

data ConfigDumpFormat = ConfigDumpYaml | ConfigDumpJson

-- | Dump project configuration settings.
newtype ConfigCmdDumpProject = ConfigCmdDumpProject ConfigDumpFormat

-- | Dump stack's own settings. Configuration related to its own opertion. This
-- can be defaulted or stored in a global location or project location or both,
-- in @~\/.stack\/config.yaml@ or @stack.yaml@.
data ConfigCmdDumpStack = ConfigCmdDumpStack DumpStackScope ConfigDumpFormat

-- | Get configuration items that can be individually set by `stack config set`.
data ConfigCmdGet
    = ConfigCmdGetResolver
    | ConfigCmdGetSystemGhc CommandScope
    | ConfigCmdGetInstallGhc CommandScope

-- | Set the resolver for the project or set compiler-related configuration at
-- project or global scope.
data ConfigCmdSet
    = ConfigCmdSetResolver (Unresolved AbstractResolver)
    | ConfigCmdSetSystemGhc CommandScope Bool
    | ConfigCmdSetInstallGhc CommandScope Bool

-- | Where to get the configuration settings from.
data CommandScope
    = CommandScopeGlobal
      -- ^ Apply changes to or get settings from the global configuration,
      -- typically at @~\/.stack\/config.yaml@.
    | CommandScopeProject
      -- ^ Apply changes to or get settings from the project @stack.yaml@.

-- | Where to get the configuration settings from.
data DumpStackScope
    = DumpStackScopeEffective
      -- ^ A view of settings where those settings in the project but related to
      -- stack's own operation override settings in the global location.
    | DumpStackScopeGlobal
      -- ^ Apply changes to or get settings from the global configuration,
      -- typically at @~\/.stack\/config.yaml@.
    | DumpStackScopeProject
      -- ^ Apply changes to or get settings from the project @stack.yaml@.

instance Display CommandScope where
    display CommandScopeProject = "project"
    display CommandScopeGlobal = "global"

configCmdGetScope :: ConfigCmdGet -> CommandScope
configCmdGetScope ConfigCmdGetResolver = CommandScopeProject
configCmdGetScope (ConfigCmdGetSystemGhc scope) = scope
configCmdGetScope (ConfigCmdGetInstallGhc scope) = scope

configCmdSetScope :: ConfigCmdSet -> CommandScope
configCmdSetScope (ConfigCmdSetResolver _) = CommandScopeProject
configCmdSetScope (ConfigCmdSetSystemGhc scope _) = scope
configCmdSetScope (ConfigCmdSetInstallGhc scope _) = scope

encodeDumpProject :: RawYaml -> ConfigDumpFormat -> Project -> ByteString
encodeDumpProject rawConfig format p
    | ConfigDumpYaml <- format = dumpProject (\e d ->
        either (const e) encodeUtf8 (cfgRedress rawConfig d ""))
    | ConfigDumpJson <- format = dumpProject (\_ d ->
        toStrictBytes $ encodePretty' (cfgPretty d) d)
    where
        -- REVIEW: Is there a way to encode straight to keymap?
        -- encode project to bytestring then decode to keymap.
        dumpProject f = let e = Yaml.encode p in Yaml.decodeEither' e &
            either (const e) (\(d :: KeyMap Yaml.Value) -> f e d)

        cfgPretty d = Aeson.defConfig{confCompare = cfgKeyCompare rawConfig d ""}

cfgKeyCompare :: RawYaml -> KeyMap Yaml.Value -> Text -> (Text -> Text -> Ordering)
cfgKeyCompare (yamlLines -> configLines) (fmap Key.toText . KeyMap.keys -> keys) cmdKey =
    compareInOrder configLines (coerce keys) (coerce cmdKey)

encodeDumpStackBy :: ToJSON a => (Config -> a) -> ConfigCmdDumpStack -> (Config -> ByteString)
encodeDumpStackBy f (ConfigCmdDumpStack _ ConfigDumpYaml) = Yaml.encode . f
encodeDumpStackBy f (ConfigCmdDumpStack _ ConfigDumpJson) = toStrictBytes . encodePretty . f

encodeDumpStack :: ConfigDumpFormat -> (DumpStack -> ByteString)
encodeDumpStack ConfigDumpYaml = Yaml.encode
encodeDumpStack ConfigDumpJson = toStrictBytes . encodePretty

cfgReadProject :: (HasConfig env, HasLogFunc env) => CommandScope -> RIO env (Maybe Project)
cfgReadProject scope = do
    (configFilePath, yamlConfig) <- cfgRead scope
    let parser = parseProjectAndConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> do
            logError . display $ T.pack err
            return Nothing
        Right (WithJSONWarnings res _warnings) -> do
            ProjectAndConfigMonoid project _ <- liftIO res
            return $ Just project

cfgCmdDumpProject :: (HasConfig env, HasLogFunc env) => ConfigCmdDumpProject -> RIO env ()
cfgCmdDumpProject (ConfigCmdDumpProject dumpFormat) = do
    configFilePath <- cfgLocation CommandScopeProject
    rawConfig <- mkRaw <$> liftIO (readFileUtf8 (toFilePath configFilePath))
    project <- cfgReadProject CommandScopeProject
    project & maybe (logError "Couldn't find project") (\p ->
        encodeDumpProject rawConfig dumpFormat p
        & decodeUtf8'
        & either throwM (logInfo . display))

data DumpStack =
    DumpStack
        { dsInstallGHC :: !(Maybe Bool)
        , dsSystemGHC  :: !(Maybe Bool)
        }

instance ToJSON DumpStack where
    toJSON DumpStack{..} = object
        [ "install-GHC" .= toJSON dsInstallGHC
        , "system-GHC" .= toJSON dsSystemGHC
        ]

cfgCmdDumpStack :: (HasConfig env, HasLogFunc env) => ConfigCmdDumpStack -> RIO env ()
cfgCmdDumpStack cmd@(ConfigCmdDumpStack scope dumpFormat)
    | DumpStackScopeEffective <- scope = cfgCmdDumpStackEffective cmd
    | DumpStackScopeProject <- scope = cfgDumpStack CommandScopeProject dumpFormat
    | DumpStackScopeGlobal <- scope = cfgDumpStack CommandScopeGlobal dumpFormat

cfgDumpStack
    :: (HasConfig env, HasLogFunc env)
    => CommandScope -> ConfigDumpFormat -> RIO env ()
cfgDumpStack scope dumpFormat = do
    (configFilePath, yamlConfig) <- cfgRead scope
    let parser = parseConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> logError . display $ T.pack err
        Right (WithJSONWarnings config _warnings) -> do
            let dsSystemGHC = getFirst $ configMonoidSystemGHC config
            let dsInstallGHC = getFirstTrue $ configMonoidInstallGHC config
            
            DumpStack{..}
                & encodeDumpStack dumpFormat
                & decodeUtf8'
                & either throwM (logInfo . display)

cfgCmdDumpStackEffective :: (HasConfig env, HasLogFunc env) => ConfigCmdDumpStack -> RIO env ()
cfgCmdDumpStackEffective cmd = do
    conf <- view configL
    let f Config{..} =
            DumpStack
                { dsInstallGHC = Just configInstallGHC
                , dsSystemGHC = Just configSystemGHC
                }
    conf
        & encodeDumpStackBy f cmd
        & decodeUtf8'
        & either throwM (logInfo . display)

cfgCmdGet :: (HasConfig env, HasLogFunc env) => ConfigCmdGet -> RIO env ()
cfgCmdGet cmd = do
    let logBool maybeValue = logInfo $
            maybe "default" (display . T.toLower . T.pack . show) maybeValue

    (configFilePath, yamlConfig) <- cfgRead (configCmdGetScope cmd)
    let parser = parseProjectAndConfigMonoid (parent configFilePath)
    case Yaml.parseEither parser yamlConfig of
        Left err -> logError . display $ T.pack err
        Right (WithJSONWarnings res _warnings) -> do
            ProjectAndConfigMonoid project config <- liftIO res
            cmd & \case
                ConfigCmdGetResolver ->
                    logInfo . display $ projectResolver project
                ConfigCmdGetSystemGhc{} ->
                    logBool (getFirst $ configMonoidSystemGHC config)
                ConfigCmdGetInstallGhc{} ->
                    logBool (getFirstTrue $ configMonoidInstallGHC config)

-- | Configuration location for a scope. Typically:
-- * at @~\/.stack\/config.yaml@ for global scope.
-- * at @.\/stack.yaml@ by default or from the @--stack-yaml@ option for project scope.
cfgLocation :: HasConfig s => CommandScope -> RIO s (Path Abs File)
cfgLocation scope = do
    conf <- view configL
    case scope of
        CommandScopeProject -> do
            mstackYamlOption <- view $ globalOptsL.to globalStackYaml
            mstackYaml <- getProjectConfig mstackYamlOption
            case mstackYaml of
                PCProject stackYaml -> return stackYaml
                PCGlobalProject -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir conf)
                PCNoProject _extraDeps ->
                    -- REVIEW: Maybe modify the ~/.stack/config.yaml file instead?
                    throwString "config command used when no project configuration available"
        CommandScopeGlobal -> return (configUserConfigPath conf)

cfgRead :: (HasConfig s, FromJSON a) => CommandScope -> RIO s (Path Abs File, a)
cfgRead scope = do
    configFilePath <- cfgLocation scope

    -- We don't need to worry about checking for a valid yaml here
    liftIO (Yaml.decodeFileEither (toFilePath configFilePath)) >>=
        either throwM (return . (configFilePath,))

cfgRedress :: RawYaml -> KeyMap Yaml.Value -> Text -> Either UnicodeException Text
cfgRedress (yamlLines -> configLines) config@(fmap Key.toText . KeyMap.keys -> keys) cmdKey =
    unmkRaw . redress configLines <$>
        encodeInOrder configLines (coerce keys) (coerce cmdKey) config

cfgRedressWrite :: RawYaml -> KeyMap Yaml.Value -> Text -> (Text -> RIO env ()) -> RIO env ()
cfgRedressWrite rawConfig config cmdKey write =
    either throwM write (cfgRedress rawConfig config cmdKey)

cfgCmdSet :: (HasConfig env, HasGHCVariant env) => ConfigCmdSet -> RIO env ()
cfgCmdSet cmd = do
    -- We don't need to worry about checking for a valid yaml here
    configFilePath <- cfgLocation $ configCmdSetScope cmd
    rawConfig <- mkRaw <$> liftIO (readFileUtf8 (toFilePath configFilePath))
    (config :: Yaml.Object) <- either throwM return (Yaml.decodeEither' . encodeUtf8 $ coerce rawConfig)
    newValue <- cfgCmdSetValue (parent configFilePath) cmd
    let cmdKey = cfgCmdSetOptionName cmd
        config' = KeyMap.insert (Key.fromText cmdKey) newValue config
    if config' == config
        then logInfo
                 (fromString (toFilePath configFilePath) <>
                  " already contained the intended configuration and remains \
                  \unchanged.")
        else cfgRedressWrite rawConfig config' cmdKey (\redressed -> do
            writeBinaryFileAtomic configFilePath . byteString $ encodeUtf8 redressed

            let file = fromString $ toFilePath configFilePath
            logInfo (file <> " has been updated."))

cfgCmdSetValue
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir -- ^ root directory of project
    -> ConfigCmdSet -> RIO env Yaml.Value
cfgCmdSetValue root (ConfigCmdSetResolver newResolver) = do
    newResolver' <- resolvePaths (Just root) newResolver
    concreteResolver <- makeConcreteResolver newResolver'
    -- Check that the snapshot actually exists
    void $ loadSnapshot =<< completeSnapshotLocation concreteResolver
    return (Yaml.toJSON concreteResolver)
cfgCmdSetValue _ (ConfigCmdSetSystemGhc _ bool') =
    return (Yaml.Bool bool')
cfgCmdSetValue _ (ConfigCmdSetInstallGhc _ bool') =
    return (Yaml.Bool bool')

cfgCmdSetOptionName :: ConfigCmdSet -> Text
cfgCmdSetOptionName (ConfigCmdSetResolver _) = "resolver"
cfgCmdSetOptionName (ConfigCmdSetSystemGhc _ _) = configMonoidSystemGHCName
cfgCmdSetOptionName (ConfigCmdSetInstallGhc _ _) = configMonoidInstallGHCName

cfgCmdName, cfgCmdGetName, cfgCmdSetName, cfgCmdEnvName :: String
cfgCmdDumpProjectName, cfgCmdDumpStackName :: String
cfgCmdName = "config"
cfgCmdDumpProjectName = "dump-project"
cfgCmdDumpStackName = "dump-stack"
cfgCmdGetName = "get"
cfgCmdSetName = "set"
cfgCmdEnvName = "env"

configCmdDumpProjectParser :: OA.Parser ConfigCmdDumpProject
configCmdDumpProjectParser = ConfigCmdDumpProject <$> dumpFormatFlag

configCmdDumpStackParser :: OA.Parser ConfigCmdDumpStack
configCmdDumpStackParser = ConfigCmdDumpStack <$> getDumpStackScope <*> dumpFormatFlag

dumpFormatFlag :: OA.Parser ConfigDumpFormat
dumpFormatFlag =
    OA.flag
        ConfigDumpYaml
        ConfigDumpJson
            (OA.long "json" <> OA.help "Dump the configuration as JSON instead of as YAML")

configCmdGetParser :: OA.Parser ConfigCmdGet
configCmdGetParser =
    OA.hsubparser $
    mconcat
        [ OA.command
              "resolver"
              (OA.info
                   (OA.pure ConfigCmdGetResolver)
                   (OA.progDesc "Gets the configured resolver."))
        , OA.command
              (T.unpack configMonoidSystemGHCName)
              (OA.info
                   (ConfigCmdGetSystemGhc <$> getScopeFlag)
                   (OA.progDesc
                        "Gets whether stack should use a system GHC installation or not."))
        , OA.command
              (T.unpack configMonoidInstallGHCName)
              (OA.info
                   (ConfigCmdGetInstallGhc <$> getScopeFlag)
                   (OA.progDesc
                        "Gets whether stack should automatically install GHC when necessary."))
        ]

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
            (ConfigCmdSetSystemGhc <$> setScopeFlag <*> boolArgument)
            (OA.progDesc
               "Configure whether Stack should use a system GHC installation \
               \or not."))
    , OA.command (T.unpack configMonoidInstallGHCName)
        ( OA.info
            (ConfigCmdSetInstallGhc <$> setScopeFlag <*> boolArgument)
            (OA.progDesc
               "Configure whether Stack should automatically install GHC when \
               \necessary."))
    ]

getScopeFlag, setScopeFlag :: OA.Parser CommandScope
getScopeFlag = scopeFlag "From"
setScopeFlag = scopeFlag "Modify"

getDumpStackScope :: OA.Parser DumpStackScope
getDumpStackScope = OA.option readDumpStackScope
    $ OA.long "lens"
    <> OA.help "Which configuration to look at, project or global or effective (global with project overrides)."
    <> OA.metavar "[project|global|effective]"

scopeFlag :: String -> OA.Parser CommandScope
scopeFlag action =
    OA.flag
        CommandScopeProject
        CommandScopeGlobal
        (OA.long "global" <>
         OA.help
             (action <>
                " the user-specific global configuration file ('config.yaml') \
                \instead of the project-level configuration file ('stack.yaml')."))

readDumpStackScope :: OA.ReadM DumpStackScope
readDumpStackScope = OA.str >>= \case
    ("effective" :: String) -> return DumpStackScopeEffective
    "project" -> return DumpStackScopeProject
    "global" -> return DumpStackScopeGlobal
    _ -> OA.readerError "Accepted scopes are 'effective', 'project' and 'global'."

readBool :: OA.ReadM Bool
readBool = OA.readerAsk >>= \case
    "true" -> return True
    "false" -> return False
    s -> OA.readerError ("Invalid value " ++ show s ++ ": Expected \"true\" or \"false\"")

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
