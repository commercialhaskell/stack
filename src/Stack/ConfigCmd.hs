{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Make changes to the stack yaml file

module Stack.ConfigCmd
       (ConfigCmdSet(..)
       ,configCmdSetParser
       ,cfgCmdSet
       ,cfgCmdSetName
       ,cfgCmdName) where

import           Control.Monad.Catch (MonadMask, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HMap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml.Extra as Yaml
import           Network.HTTP.Client.Conduit (HasHttpManager)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Path
import           Stack.BuildPlan
import           Stack.Config (makeConcreteResolver)
import           Stack.Types.BuildPlan
import           Stack.Types.Config

data ConfigCmdSet
    = ConfigCmdSetResolver AbstractResolver
    | ConfigCmdSetSystemGhc Bool
    | ConfigCmdSetInstallGhc Bool

cfgCmdSet :: ( MonadIO m
             , MonadBaseControl IO m
             , MonadMask m
             , MonadReader env m
             , HasBuildConfig env
             , HasHttpManager env
             , HasGHCVariant env
             , MonadLogger m)
             => ConfigCmdSet -> m ()
cfgCmdSet cmd = do
    stackYaml <- fmap bcStackYaml (asks getBuildConfig)
    let stackYamlFp =
            toFilePath stackYaml
    -- We don't need to worry about checking for a valid yaml here
    (projectYamlConfig :: Yaml.Object) <-
        liftIO (Yaml.decodeFileEither stackYamlFp) >>=
        either throwM return
    newValue <- cfgCmdSetValue cmd
    let cmdKey = cfgCmdSetOptionName cmd
        projectYamlConfig' =
            HMap.insert
                cmdKey
                newValue
                projectYamlConfig
    liftIO
        (S.writeFile
             stackYamlFp
             (Yaml.encode projectYamlConfig'))
    return ()

cfgCmdSetValue
    :: ( MonadIO m
       , MonadBaseControl IO m
       , MonadMask m
       , MonadReader env m
       , HasBuildConfig env
       , HasHttpManager env
       , HasGHCVariant env
       , MonadLogger m)
    => ConfigCmdSet
    -> m Yaml.Value
cfgCmdSetValue (ConfigCmdSetResolver newResolver) = do
    -- TODO: custom snapshot support?
    newResolverText <- fmap resolverName (makeConcreteResolver newResolver)
    -- We checking here that the snapshot actually exists
    snap <- parseSnapName newResolverText
    _ <- loadMiniBuildPlan snap
    return (Yaml.String newResolverText)
cfgCmdSetValue (ConfigCmdSetSystemGhc bool) = do
    return (Yaml.Bool bool)
cfgCmdSetValue (ConfigCmdSetInstallGhc bool) = do
    return (Yaml.Bool bool)

cfgCmdSetOptionName :: ConfigCmdSet -> Text
cfgCmdSetOptionName (ConfigCmdSetResolver _) = "resolver"
cfgCmdSetOptionName (ConfigCmdSetSystemGhc _) = configMonoidSystemGHCName
cfgCmdSetOptionName (ConfigCmdSetInstallGhc _) = configMonoidInstallGHCName

cfgCmdName :: String
cfgCmdName = "config"

cfgCmdSetName :: String
cfgCmdSetName = "set"

configCmdSetParser :: OA.Parser ConfigCmdSet
configCmdSetParser =
    OA.fromM
        (do field <-
                OA.oneM
                    (OA.strArgument
                         (OA.metavar "FIELD VALUE"))
            OA.oneM (fieldToValParser field))
  where
    fieldToValParser :: String -> OA.Parser ConfigCmdSet
    fieldToValParser s =
        Map.findWithDefault
            (error $ concat $
                 [ "Invalid field "
                 , show s
                 , ": Only the following fields are currently implemented:"
                 ] ++
                 map
                     (("\n  - " ++) . T.unpack)
                     (Map.keys fieldToValParser'))
            (T.pack s)
            fieldToValParser'
    fieldToValParser' :: Map Text (OA.Parser ConfigCmdSet)
    fieldToValParser' =
        Map.fromList
            [ ( "resolver"
              , ConfigCmdSetResolver <$>
                OA.argument
                    readAbstractResolver
                    OA.idm)
            , ( configMonoidSystemGHCName
              , ConfigCmdSetSystemGhc <$> boolArgument)
            , ( configMonoidInstallGHCName
              , ConfigCmdSetInstallGhc <$> boolArgument)
            ]

readBool :: OA.ReadM Bool
readBool = do
    s <- OA.readerAsk
    case s of
        "true" -> return True
        "false" -> return False
        _ -> OA.readerError ("Invalid value " ++ show s ++ ": Expected \"true\" or \"false\"")

boolArgument :: OA.Parser Bool
boolArgument = OA.argument readBool OA.idm
