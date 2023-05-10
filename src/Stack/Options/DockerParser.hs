{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.DockerParser
  ( dockerOptsParser
  ) where

import           Data.List ( intercalate )
import qualified Data.Text as T
import           Distribution.Version ( anyVersion )
import           Options.Applicative
                   ( Parser, auto, completer, help, listCompleter, long, metavar
                   , option, str, value
                   )
import           Options.Applicative.Args ( argsOption )
import           Options.Applicative.Builder.Extra
                   ( dirCompleter, eitherReader', fileCompleter
                   , firstBoolFlagsFalse, firstBoolFlagsNoDefault
                   , firstBoolFlagsTrue, optionalFirst
                   )
import           Stack.Constants ( stackProgName )
import           Stack.Docker ( dockerCmdName )
import           Stack.Prelude
import           Stack.Options.Utils ( hideMods )
import           Stack.Types.Version ( IntersectingVersionRange (..) )
import           Stack.Types.Docker
                   ( DockerMonoidRepoOrImage (..), DockerOptsMonoid (..)
                   , dockerAutoPullArgName, dockerImageArgName
                   , dockerContainerNameArgName, dockerDetachArgName
                   , dockerEnvArgName, dockerPersistArgName
                   , dockerRegistryLoginArgName, dockerRegistryPasswordArgName
                   , dockerRegistryUsernameArgName, dockerRepoArgName
                   , dockerRunArgsArgName, dockerMountArgName
                   , dockerMountModeArgName, dockerNetworkArgName
                   , dockerSetUserArgName, dockerStackExeArgName
                   , dockerStackExeDownloadVal, dockerStackExeHostVal
                   , dockerStackExeImageVal, parseDockerStackExe
                   )

-- | Options parser configuration for Docker.
dockerOptsParser :: Bool -> Parser DockerOptsMonoid
dockerOptsParser hide0 = DockerOptsMonoid (Any False)
  <$> firstBoolFlagsNoDefault
        dockerCmdName
        "using a Docker container. --docker implies 'system-ghc: true'."
        hide
  <*> fmap First
        (   Just . DockerMonoidRepo <$> option str
              (  long (dockerOptName dockerRepoArgName)
              <> hide
              <> metavar "NAME"
              <> help "Docker repository name."
              )
        <|> Just . DockerMonoidImage <$> option str
              (  long (dockerOptName dockerImageArgName)
              <> hide
              <> metavar "IMAGE"
              <> help "Exact Docker image ID (overrides docker-repo)."
              )
        <|> pure Nothing
        )
  <*> firstBoolFlagsNoDefault
        (dockerOptName dockerRegistryLoginArgName)
        "registry requires login."
        hide
  <*> firstStrOption
        (  long (dockerOptName dockerRegistryUsernameArgName)
        <> hide
        <> metavar "USERNAME"
        <> help "Docker registry username."
        )
  <*> firstStrOption
        (  long (dockerOptName dockerRegistryPasswordArgName)
        <> hide
        <> metavar "PASSWORD"
        <> help "Docker registry password."
        )
  <*> firstBoolFlagsTrue
        (dockerOptName dockerAutoPullArgName)
        "automatic pulling latest version of image."
        hide
  <*> firstBoolFlagsFalse
        (dockerOptName dockerDetachArgName)
        "running a detached Docker container."
        hide
  <*> firstBoolFlagsFalse
        (dockerOptName dockerPersistArgName)
        "not deleting container after it exits."
        hide
  <*> firstStrOption
        (  long (dockerOptName dockerContainerNameArgName)
        <> hide
        <> metavar "NAME"
        <> help "Docker container name."
        )
  <*> firstStrOption
        (  long (dockerOptName dockerNetworkArgName)
        <> hide
        <> metavar "NETWORK"
        <> help "Docker network."
        )
  <*> argsOption
        (  long (dockerOptName dockerRunArgsArgName)
        <> hide
        <> value []
        <> metavar "'ARG1 [ARG2 ...]'"
        <> help "Additional options to pass to 'docker run'.")
  <*> many (option auto
        (  long (dockerOptName dockerMountArgName)
        <> hide
        <> metavar "(PATH | HOST-PATH:CONTAINER-PATH)"
        <> completer dirCompleter
        <> help "Mount volumes from host in container (can be specified \
                \multiple times)."
        ))
  <*> firstStrOption
        (  long (dockerOptName dockerMountModeArgName)
        <> hide
        <> metavar "SUFFIX"
        <> help "Volume mount mode suffix."
        )
  <*> many (option str
        (  long (dockerOptName dockerEnvArgName)
        <> hide
        <> metavar "NAME=VALUE"
        <> help "Set environment variable in container (can be specified \
                \multiple times)."
        ))
  <*> optionalFirst (option (eitherReader' parseDockerStackExe)
        ( let specialOpts = [ dockerStackExeDownloadVal
                            , dockerStackExeHostVal
                            , dockerStackExeImageVal
                            ]
          in     long (dockerOptName dockerStackExeArgName)
              <> hide
              <> metavar (intercalate "|" (specialOpts ++ ["PATH"]))
              <> completer (listCompleter specialOpts <> fileCompleter)
              <> help ( concat
                          [ "Location of "
                          , stackProgName
                          , " executable used in container."
                          ]
                      )
        ))
  <*> firstBoolFlagsNoDefault
        (dockerOptName dockerSetUserArgName)
        "setting user in container to match host."
        hide
  <*> pure (IntersectingVersionRange anyVersion)
 where
  dockerOptName optName = dockerCmdName ++ "-" ++ T.unpack optName
  firstStrOption = optionalFirst . option str
  hide = hideMods hide0
