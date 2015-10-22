{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards, TemplateHaskell #-}

-- | Docker configuration
module Stack.Config.ExecEnv where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Catch (throwM, MonadThrow)
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Path
import Stack.Types

-- | Interprets DockerOptsMonoid options.
execEnvOptsFromMonoid
    :: MonadThrow m
    => Maybe Project -> Path Abs Dir -> ExecEnvOptsMonoid -> m ExecEnvOpts
execEnvOptsFromMonoid mproject stackRoot ExecEnvOptsMonoid{..} = do
    let execEnvType =
            if fromMaybe execEnvMonoidDefaultEnable execEnvMonoidEnable
            then Just NixShellExecEnv
            else Nothing
        execEnvPackages = execEnvMonoidPackages
     {- dockerContainerName = emptyToNothing dockerMonoidContainerName
        dockerRunArgs = dockerMonoidRunArgs
        dockerMount = dockerMonoidMount
        dockerEnv = dockerMonoidEnv
    dockerDatabasePath <-
        case dockerMonoidDatabasePath of
            Nothing -> return $ stackRoot </> $(mkRelFile "docker.db")
            Just fp ->
                case parseAbsFile fp of
                    Left e -> throwM (InvalidDatabasePathException e)
                    Right p -> return p
    dockerStackExe <-
        case dockerMonoidStackExe of
            Just e -> liftM Just (parseDockerStackExe e)
            Nothing -> return Nothing -}
    return ExecEnvOpts{..}

{-  where emptyToNothing Nothing = Nothing
        emptyToNothing (Just s) | null s = Nothing
                                | otherwise = Just s

-- | Exceptions thrown by Stack.Docker.Config.
data StackNixConfigException
    = ResolverNotSupportedException String
    -- ^ Only LTS resolvers are supported for default image tag.
    | InvalidDatabasePathException SomeException
    -- ^ Invalid global database path.
    deriving (Typeable)

-- | Exception instance for StackDockerConfigException.
instance Exception StackDockerConfigException

-- | Show instance for StackDockerConfigException.
instance Show StackDockerConfigException where
    show (ResolverNotSupportedException resolver) =
        concat
            [ "Resolver not supported for Docker images:\n    "
            , resolver
            , "\nUse an LTS resolver, or set the '"
            , T.unpack dockerImageArgName
            , "' explicitly, in your configuration file."]
    show (InvalidDatabasePathException ex) =
        concat ["Invalid database path: ", show ex]
-}
