{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
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
import           Data.Coerce (coerce)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import           Data.ByteString.Builder (byteString)
import qualified Data.Map.Merge.Strict as Map
#if !MIN_VERSION_aeson(2,0,0)
import qualified Data.HashMap.Strict as HMap
#endif
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Options.Applicative.Builder.Extra
import           Pantry (loadSnapshot)
import           Path
import qualified RIO.Map as Map
import           RIO.Process (envVarsL)
import           Stack.Config (makeConcreteResolver, getProjectConfig, getImplicitGlobalProjectDir)
import           Stack.Constants
import           Stack.Types.Config
import           Stack.Types.Resolver
import           System.Environment (getEnvironment)
import           Stack.YamlUpdate

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
                         PCProject stackYaml -> return stackYaml
                         PCGlobalProject -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir conf)
                         PCNoProject _extraDeps -> throwString "config command used when no project configuration available" -- maybe modify the ~/.stack/config.yaml file instead?
                 CommandScopeGlobal -> return (configUserConfigPath conf)
    -- We don't need to worry about checking for a valid yaml here
    rawConfig <- mkRaw <$> liftIO (readFileUtf8 (toFilePath configFilePath))
    (config :: Yaml.Object) <- either throwM return (Yaml.decodeEither' . encodeUtf8 $ coerce rawConfig)
    newValue <- cfgCmdSetValue (parent configFilePath) cmd
    let cmdKey = cfgCmdSetOptionName cmd
#if MIN_VERSION_aeson(2,0,0)
        config' = KeyMap.insert (Key.fromText cmdKey) newValue config
        yamlKeys = Key.toText <$> KeyMap.keys config
#else
        config' = HMap.insert cmdKey newValue config
        yamlKeys = HMap.keys config
#endif
    if config' == config
        then logInfo
                 (fromString (toFilePath configFilePath) <>
                  " already contained the intended configuration and remains unchanged.")
        else do
            let configLines = yamlLines rawConfig
            either
                throwM
                (\updated -> do
                    let redressed = unmkRaw $ redress configLines updated
                    writeBinaryFileAtomic configFilePath . byteString $ encodeUtf8 redressed

                    let file = fromString $ toFilePath configFilePath
                    logInfo (file <> " has been updated."))
                (encodeInOrder configLines (coerce yamlKeys) (coerce cmdKey) config')

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

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"

cfgCmdEnvName :: String
cfgCmdEnvName = "env"

configCmdSetParser :: OA.Parser ConfigCmdSet
configCmdSetParser =
    OA.hsubparser $
    mconcat
        [ OA.command
              "resolver"
              (OA.info
                   (ConfigCmdSetResolver <$>
                    OA.argument
                        readAbstractResolver
                        (OA.metavar "RESOLVER" <>
                         OA.help "E.g. \"nightly\" or \"lts-7.2\""))
                   (OA.progDesc
                        "Change the resolver of the current project. See https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver for more info."))
        , OA.command
              (T.unpack configMonoidSystemGHCName)
              (OA.info
                   (ConfigCmdSetSystemGhc <$> scopeFlag <*> boolArgument)
                   (OA.progDesc
                        "Configure whether stack should use a system GHC installation or not."))
        , OA.command
              (T.unpack configMonoidInstallGHCName)
              (OA.info
                   (ConfigCmdSetInstallGhc <$> scopeFlag <*> boolArgument)
                   (OA.progDesc
                        "Configure whether stack should automatically install GHC when necessary."))
        ]

scopeFlag :: OA.Parser CommandScope
scopeFlag =
    OA.flag
        CommandScopeProject
        CommandScopeGlobal
        (OA.long "global" <>
         OA.help
             "Modify the global configuration (typically at \"~/.stack/config.yaml\") instead of the project stack.yaml.")

readBool :: OA.ReadM Bool
readBool = do
    s <- OA.readerAsk
    case s of
        "true" -> return True
        "false" -> return False
        _ -> OA.readerError ("Invalid value " ++ show s ++ ": Expected \"true\" or \"false\"")

boolArgument :: OA.Parser Bool
boolArgument = OA.argument readBool (OA.metavar "true|false" <> OA.completeWith ["true", "false"])

configCmdEnvParser :: OA.Parser EnvSettings
configCmdEnvParser = EnvSettings
  <$> boolFlags True "locals" "include local package information" mempty
  <*> boolFlags True "ghc-package-path" "set GHC_PACKAGE_PATH variable" mempty
  <*> boolFlags True "stack-exe" "set STACK_EXE environment variable" mempty
  <*> boolFlags False "locale-utf8" "set the GHC_CHARENC environment variable to UTF8" mempty
  <*> boolFlags False "keep-ghc-rts" "keep any GHC_RTS environment variables" mempty

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
