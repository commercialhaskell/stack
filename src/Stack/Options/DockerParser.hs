module Stack.Options.DockerParser where

import           Data.Char
import           Data.List                         (intercalate)
import           Data.Monoid.Extra
import qualified Data.Text                         as T
import           Distribution.Version              (anyVersion)
import           Options.Applicative
import           Options.Applicative.Args
import           Options.Applicative.Builder.Extra
import           Stack.Constants
import           Stack.Docker
import qualified Stack.Docker                      as Docker
import           Stack.Options.Utils
import           Stack.Types.Version
import           Stack.Types.Docker

-- | Options parser configuration for Docker.
dockerOptsParser :: Bool -> Parser DockerOptsMonoid
dockerOptsParser hide0 =
    DockerOptsMonoid
    <$> pure (Any False)
    <*> firstBoolFlags dockerCmdName
                       "using a Docker container. Implies 'system-ghc: true'"
                       hide
    <*> fmap First
           ((Just . DockerMonoidRepo) <$> option str (long (dockerOptName dockerRepoArgName) <>
                                                     hide <>
                                                     metavar "NAME" <>
                                                     help "Docker repository name") <|>
             (Just . DockerMonoidImage) <$> option str (long (dockerOptName dockerImageArgName) <>
                                                      hide <>
                                                      metavar "IMAGE" <>
                                                      help "Exact Docker image ID (overrides docker-repo)") <|>
         pure Nothing)
    <*> firstBoolFlags (dockerOptName dockerRegistryLoginArgName)
                       "registry requires login"
                       hide
    <*> firstStrOption (long (dockerOptName dockerRegistryUsernameArgName) <>
                        hide <>
                        metavar "USERNAME" <>
                        help "Docker registry username")
    <*> firstStrOption (long (dockerOptName dockerRegistryPasswordArgName) <>
                        hide <>
                        metavar "PASSWORD" <>
                        help "Docker registry password")
    <*> firstBoolFlags (dockerOptName dockerAutoPullArgName)
                       "automatic pulling latest version of image"
                       hide
    <*> firstBoolFlags (dockerOptName dockerDetachArgName)
                       "running a detached Docker container"
                       hide
    <*> firstBoolFlags (dockerOptName dockerPersistArgName)
                       "not deleting container after it exits"
                       hide
    <*> firstStrOption (long (dockerOptName dockerContainerNameArgName) <>
                        hide <>
                        metavar "NAME" <>
                        help "Docker container name")
    <*> argsOption (long (dockerOptName dockerRunArgsArgName) <>
                    hide <>
                    value [] <>
                    metavar "'ARG1 [ARG2 ...]'" <>
                    help "Additional options to pass to 'docker run'")
    <*> many (option auto (long (dockerOptName dockerMountArgName) <>
                           hide <>
                           metavar "(PATH | HOST-PATH:CONTAINER-PATH)" <>
                           help ("Mount volumes from host in container " ++
                                 "(may specify multiple times)")))
    <*> many (option str (long (dockerOptName dockerEnvArgName) <>
                                hide <>
                                metavar "NAME=VALUE" <>
                                help ("Set environment variable in container " ++
                                      "(may specify multiple times)")))
    <*> optionalFirst (absFileOption
            (long (dockerOptName dockerDatabasePathArgName) <>
             hide <>
             metavar "PATH" <>
             help "Location of image usage tracking database"))
    <*> optionalFirst (option (eitherReader' parseDockerStackExe)
            (long(dockerOptName dockerStackExeArgName) <>
             hide <>
             metavar (intercalate "|"
                          [ dockerStackExeDownloadVal
                          , dockerStackExeHostVal
                          , dockerStackExeImageVal
                          , "PATH" ]) <>
             help (concat [ "Location of "
                          , stackProgName
                          , " executable used in container" ])))
    <*> firstBoolFlags (dockerOptName dockerSetUserArgName)
                       "setting user in container to match host"
                       hide
    <*> pure (IntersectingVersionRange anyVersion)
  where
    dockerOptName optName = dockerCmdName ++ "-" ++ T.unpack optName
    firstStrOption = optionalFirst . option str
    hide = hideMods hide0

-- | Parser for docker cleanup arguments.
dockerCleanupOptsParser :: Parser Docker.CleanupOpts
dockerCleanupOptsParser =
  Docker.CleanupOpts <$>
  (flag' Docker.CleanupInteractive
         (short 'i' <>
          long "interactive" <>
          help "Show cleanup plan in editor and allow changes (default)") <|>
   flag' Docker.CleanupImmediate
         (short 'y' <>
          long "immediate" <>
          help "Immediately execute cleanup plan") <|>
   flag' Docker.CleanupDryRun
         (short 'n' <>
          long "dry-run" <>
          help "Display cleanup plan but do not execute") <|>
   pure Docker.CleanupInteractive) <*>
  opt (Just 14) "known-images" "LAST-USED" <*>
  opt Nothing "unknown-images" "CREATED" <*>
  opt (Just 0) "dangling-images" "CREATED" <*>
  opt Nothing "stopped-containers" "CREATED" <*>
  opt Nothing "running-containers" "CREATED"
  where opt def' name mv =
          fmap Just
               (option auto
                       (long name <>
                        metavar (mv ++ "-DAYS-AGO") <>
                        help ("Remove " ++
                              toDescr name ++
                              " " ++
                              map toLower (toDescr mv) ++
                              " N days ago" ++
                              case def' of
                                Just n -> " (default " ++ show n ++ ")"
                                Nothing -> ""))) <|>
          flag' Nothing
                (long ("no-" ++ name) <>
                 help ("Do not remove " ++
                       toDescr name ++
                       case def' of
                         Just _ -> ""
                         Nothing -> " (default)")) <|>
          pure def'
        toDescr = map (\c -> if c == '-' then ' ' else c)
