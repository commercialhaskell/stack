{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

-- | Make changes to project or global configuration.
module Stack.ConfigCmd
       (ConfigCmdSet(..)
       ,configCmdSetParser
       ,cfgCmdSet
       ,cfgCmdSetName
       ,cfgCmdName) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch (throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HMap
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Path
import           Prelude -- Silence redundant import warnings
import           Stack.BuildPlan
import           Stack.Config (makeConcreteResolver, getStackYaml)
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
    :: (StackMiniM env m, HasConfig env, HasGHCVariant env)
    => ConfigCmdSet -> m ()
cfgCmdSet cmd = do
    configFilePath <-
        liftM
            toFilePath
            (case configCmdSetScope cmd of
                 CommandScopeProject -> getStackYaml
                 CommandScopeGlobal -> view $ configL.to configUserConfigPath)
    -- We don't need to worry about checking for a valid yaml here
    (config :: Yaml.Object) <-
        liftIO (Yaml.decodeFileEither configFilePath) >>= either throwM return
    newValue <- cfgCmdSetValue cmd
    let cmdKey = cfgCmdSetOptionName cmd
        config' = HMap.insert cmdKey newValue config
    if config' == config
        then $logInfo
                 (T.pack configFilePath <>
                  " already contained the intended configuration and remains unchanged.")
        else do
            liftIO (S.writeFile configFilePath (Yaml.encode config'))
            $logInfo (T.pack configFilePath <> " has been updated.")

cfgCmdSetValue
    :: (StackMiniM env m, HasConfig env, HasGHCVariant env)
    => ConfigCmdSet -> m Yaml.Value
cfgCmdSetValue (ConfigCmdSetResolver newResolver) = do
    concreteResolver <- makeConcreteResolver newResolver
    case concreteResolver of
        -- Check that the snapshot actually exists
        ResolverSnapshot snapName -> void $ loadMiniBuildPlan snapName
        ResolverCompiler _ -> return ()
        -- TODO: custom snapshot support?  Would need a way to specify on CLI
        ResolverCustom _ _ -> error "'stack config set resolver' does not support custom resolvers"
    return (Yaml.String (resolverName concreteResolver))
cfgCmdSetValue (ConfigCmdSetSystemGhc _ bool) =
    return (Yaml.Bool bool)
cfgCmdSetValue (ConfigCmdSetInstallGhc _ bool) =
    return (Yaml.Bool bool)

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
boolArgument = OA.argument readBool (OA.metavar "true/false")
