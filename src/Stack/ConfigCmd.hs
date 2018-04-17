{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | Make changes to project or global configuration.
module Stack.ConfigCmd
       (ConfigCmdSet(..)
       ,configCmdSetParser
       ,cfgCmdSet
       ,cfgCmdSetName
       ,cfgCmdName) where

import           Stack.Prelude
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Path
import           Path.IO
import           Stack.Config (makeConcreteResolver, getProjectConfig, getImplicitGlobalProjectDir, LocalConfigStatus(..))
import           Stack.Constants
import           Stack.Snapshot (loadResolver)
import           Stack.Types.Config
import           Stack.Types.Resolver

data ConfigCmdSet
    = ConfigCmdSetResolver AbstractResolver
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
    => GlobalOpts -> ConfigCmdSet -> RIO env ()
cfgCmdSet go cmd = do
    conf <- view configL
    configFilePath <-
             case configCmdSetScope cmd of
                 CommandScopeProject -> do
                     mstackYamlOption <- forM (globalStackYaml go) resolveFile'
                     mstackYaml <- getProjectConfig mstackYamlOption
                     case mstackYaml of
                         LCSProject stackYaml -> return stackYaml
                         LCSNoProject -> liftM (</> stackDotYaml) (getImplicitGlobalProjectDir conf)
                         LCSNoConfig _ -> throwString "config command used when no local configuration available"
                 CommandScopeGlobal -> return (configUserConfigPath conf)
    -- We don't need to worry about checking for a valid yaml here
    (config :: Yaml.Object) <-
        liftIO (Yaml.decodeFileEither (toFilePath configFilePath)) >>= either throwM return
    newValue <- cfgCmdSetValue (parent configFilePath) cmd
    let cmdKey = cfgCmdSetOptionName cmd
        config' = HMap.insert cmdKey newValue config
    if config' == config
        then logInfo
                 (fromString (toFilePath configFilePath) <>
                  " already contained the intended configuration and remains unchanged.")
        else do
            liftIO (S.writeFile (toFilePath configFilePath) (Yaml.encode config'))
            logInfo (fromString (toFilePath configFilePath) <> " has been updated.")

cfgCmdSetValue
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir -- ^ root directory of project
    -> ConfigCmdSet -> RIO env Yaml.Value
cfgCmdSetValue root (ConfigCmdSetResolver newResolver) = do
    concreteResolver <- makeConcreteResolver (Just root) newResolver
    -- Check that the snapshot actually exists
    void $ loadResolver concreteResolver
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
