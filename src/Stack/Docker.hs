{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Run commands in Docker containers
module Stack.Docker
  ( dockerCmdName
  , dockerHelpOptName
  , dockerPullCmdName
  , entrypoint
  , preventInContainer
  , pull
  , reset
  , reExecArgName
  , DockerException (..)
  , getProjectRoot
  , runContainerAndExit
  ) where

import qualified Crypto.Hash as Hash ( Digest, MD5, hash )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char ( isAscii, isDigit )
import           Data.Conduit.List ( sinkNull )
import           Data.Conduit.Process.Typed hiding ( proc )
import           Data.List ( dropWhileEnd, isInfixOf, isPrefixOf )
import           Data.List.Extra ( trim )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time ( UTCTime )
import qualified Data.Version ( parseVersion )
import           Distribution.Version ( mkVersion, mkVersion' )
import           Pantry.Internal.AesonExtended
                   ( FromJSON (..), (.:), (.:?), (.!=), eitherDecode )
import           Path
                   ( (</>), dirname, filename, parent, parseAbsDir
                   , splitExtension
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.IO hiding ( canonicalizePath )
import qualified RIO.Directory ( makeAbsolute )
import           RIO.Process
                   ( HasProcessContext, augmentPath, doesExecutableExist, proc
                   , processContextL, withWorkingDir
                   )
import           Stack.Config ( getInContainer )
import           Stack.Constants
                   ( buildPlanDir, inContainerEnvVar, platformVariantEnvVar
                   , relDirBin, relDirDotLocal, relDirDotSsh
                   , relDirDotStackProgName, relDirUnderHome, stackRootEnvVar
                   )
import           Stack.Constants.Config ( projectDockerSandboxDir )
import           Stack.Docker.Handlers ( handleSetGroups, handleSignals )
import           Stack.Prelude
import           Stack.Setup ( ensureDockerStackExe )
import           Stack.Storage.User
                   ( loadDockerImageExeCache, saveDockerImageExeCache )
import           Stack.Types.Config
                   ( Config (..), DockerEntrypoint (..), DockerUser (..)
                   , HasConfig (..), configProjectRoot, stackRootL, terminalL
                   )
import           Stack.Types.Docker
                  ( DockerException (..), DockerOpts (..), DockerStackExe (..)
                  , Mount (..), dockerCmdName, dockerContainerPlatform
                  , dockerEntrypointArgName, dockerHelpOptName
                  , dockerPullCmdName, reExecArgName
                  )
import           Stack.Types.Version ( showStackVersion, withinRange )
import           System.Environment
                   ( getArgs, getEnv, getEnvironment, getExecutablePath
                   , getProgName
                   )
import qualified System.FilePath as FP
import           System.IO.Error ( isDoesNotExistError )
import           System.IO.Unsafe ( unsafePerformIO )
import qualified System.PosixCompat.User as User
import qualified System.PosixCompat.Files as Files
import           System.Terminal ( hIsTerminalDeviceOrMinTTY )
import           Text.ParserCombinators.ReadP ( readP_to_S )

-- | Function to get command and arguments to run in Docker container
getCmdArgs ::
     HasConfig env
  => DockerOpts
  -> Inspect
  -> Bool
  -> RIO env (FilePath,[String],[(String,String)],[Mount])
getCmdArgs docker imageInfo isRemoteDocker = do
    config <- view configL
    deUser <-
        if fromMaybe (not isRemoteDocker) (dockerSetUser docker)
            then liftIO $ do
              duUid <- User.getEffectiveUserID
              duGid <- User.getEffectiveGroupID
              duGroups <- nubOrd <$> User.getGroups
              duUmask <- Files.setFileCreationMask 0o022
              -- Only way to get old umask seems to be to change it, so set it back afterward
              _ <- Files.setFileCreationMask duUmask
              pure (Just DockerUser{..})
            else pure Nothing
    args <-
        fmap
            (["--" ++ reExecArgName ++ "=" ++ showStackVersion
             ,"--" ++ dockerEntrypointArgName
             ,show DockerEntrypoint{..}] ++)
            (liftIO getArgs)
    case dockerStackExe (configDocker config) of
        Just DockerStackExeHost
          | configPlatform config == dockerContainerPlatform -> do
              exePath <- resolveFile' =<< liftIO getExecutablePath
              cmdArgs args exePath
          | otherwise -> throwIO UnsupportedStackExeHostPlatformException
        Just DockerStackExeImage -> do
            progName <- liftIO getProgName
            pure (FP.takeBaseName progName, args, [], [])
        Just (DockerStackExePath path) -> cmdArgs args path
        Just DockerStackExeDownload -> exeDownload args
        Nothing
          | configPlatform config == dockerContainerPlatform -> do
              (exePath,exeTimestamp,misCompatible) <-
                  do exePath <- resolveFile' =<< liftIO getExecutablePath
                     exeTimestamp <- getModificationTime exePath
                     isKnown <-
                         loadDockerImageExeCache
                             (iiId imageInfo)
                             exePath
                             exeTimestamp
                     pure (exePath, exeTimestamp, isKnown)
              case misCompatible of
                  Just True -> cmdArgs args exePath
                  Just False -> exeDownload args
                  Nothing -> do
                      e <-
                          try $
                          sinkProcessStderrStdout
                              "docker"
                              [ "run"
                              , "-v"
                              , toFilePath exePath ++ ":" ++ "/tmp/stack"
                              , T.unpack (iiId imageInfo)
                              , "/tmp/stack"
                              , "--version"]
                              sinkNull
                              sinkNull
                      let compatible =
                              case e of
                                  Left ExitCodeException{} -> False
                                  Right _ -> True
                      saveDockerImageExeCache
                          (iiId imageInfo)
                          exePath
                          exeTimestamp
                          compatible
                      if compatible
                          then cmdArgs args exePath
                          else exeDownload args
        Nothing -> exeDownload args
  where
    exeDownload args = do
        exePath <- ensureDockerStackExe dockerContainerPlatform
        cmdArgs args exePath
    cmdArgs args exePath = do
        -- MSS 2020-04-21 previously used replaceExtension, but semantics changed in path 0.7
        -- In any event, I'm not even sure _why_ we need to drop a file extension here
        -- Originally introduced here: https://github.com/commercialhaskell/stack/commit/6218dadaf5fd7bf312bb1bd0db63b4784ba78cb2
        let exeBase =
              case splitExtension exePath of
                Left _ -> exePath
                Right (x, _) -> x
        let mountPath = hostBinDir FP.</> toFilePath (filename exeBase)
        pure (mountPath, args, [], [Mount (toFilePath exePath) mountPath])

-- | Error if running in a container.
preventInContainer :: MonadIO m => m () -> m ()
preventInContainer inner =
  do inContainer <- getInContainer
     if inContainer
        then throwIO OnlyOnHostException
        else inner

-- | Run a command in a new Docker container, then exit the process.
runContainerAndExit :: HasConfig env => RIO env void
runContainerAndExit = do
  config <- view configL
  let docker = configDocker config
  checkDockerVersion docker
  (env,isStdinTerminal,isStderrTerminal,homeDir) <- liftIO $
    (,,,)
    <$> getEnvironment
    <*> hIsTerminalDeviceOrMinTTY stdin
    <*> hIsTerminalDeviceOrMinTTY stderr
    <*> getHomeDir
  isStdoutTerminal <- view terminalL
  let dockerHost = lookup "DOCKER_HOST" env
      dockerCertPath = lookup "DOCKER_CERT_PATH" env
      bamboo = lookup "bamboo_buildKey" env
      jenkins = lookup "JENKINS_HOME" env
      msshAuthSock = lookup "SSH_AUTH_SOCK" env
      muserEnv = lookup "USER" env
      isRemoteDocker = maybe False (isPrefixOf "tcp://") dockerHost
  mstackYaml <- for (lookup "STACK_YAML" env) RIO.Directory.makeAbsolute
  image <- either throwIO pure (dockerImage docker)
  when
    ( isRemoteDocker && maybe False (isInfixOf "boot2docker") dockerCertPath )
    ( prettyWarnS
        "Using boot2docker is NOT supported, and not likely to perform well."
    )
  maybeImageInfo <- inspect image
  imageInfo@Inspect{..} <- case maybeImageInfo of
    Just ii -> pure ii
    Nothing
      | dockerAutoPull docker -> do
          pullImage docker image
          mii2 <- inspect image
          case mii2 of
            Just ii2 -> pure ii2
            Nothing -> throwM (InspectFailedException image)
      | otherwise -> throwM (NotPulledException image)
  projectRoot <- getProjectRoot
  sandboxDir <- projectDockerSandboxDir projectRoot
  let ImageConfig {..} = iiConfig
      imageEnvVars = map (break (== '=')) icEnv
      platformVariant = show $ hashRepoName image
      stackRoot = view stackRootL config
      sandboxHomeDir = sandboxDir </> homeDirName
      isTerm = not (dockerDetach docker) &&
               isStdinTerminal &&
               isStdoutTerminal &&
               isStderrTerminal
      keepStdinOpen = not (dockerDetach docker) &&
                      -- Workaround for https://github.com/docker/docker/issues/12319
                      -- This is fixed in Docker 1.9.1, but will leave the workaround
                      -- in place for now, for users who haven't upgraded yet.
                      (isTerm || (isNothing bamboo && isNothing jenkins))
  let mpath = T.pack <$> lookupImageEnv "PATH" imageEnvVars
  when (isNothing mpath) $ do
    prettyWarnL
      [ flow "The Docker image does not set the PATH environment variable. \
             \This will likely fail. For further information, see"
      , style Url "https://github.com/commercialhaskell/stack/issues/2742" <> "."
      ]
  newPathEnv <- either throwM pure $ augmentPath
    [ hostBinDir
    , toFilePath (sandboxHomeDir </> relDirDotLocal </> relDirBin)
    ]
    mpath
  (cmnd,args,envVars,extraMount) <- getCmdArgs docker imageInfo isRemoteDocker
  pwd <- getCurrentDir
  liftIO $ mapM_ ensureDir [sandboxHomeDir, stackRoot]
  -- Since $HOME is now mounted in the same place in the container we can
  -- just symlink $HOME/.ssh to the right place for the stack docker user
  let sshDir = homeDir </> sshRelDir
  sshDirExists <- doesDirExist sshDir
  sshSandboxDirExists <-
    liftIO
      (Files.fileExist
        (toFilePathNoTrailingSep (sandboxHomeDir </> sshRelDir)))
  when (sshDirExists && not sshSandboxDirExists)
    (liftIO
      (Files.createSymbolicLink
        (toFilePathNoTrailingSep sshDir)
        (toFilePathNoTrailingSep (sandboxHomeDir </> sshRelDir))))
  let mountSuffix = maybe "" (":" ++) (dockerMountMode docker)
  containerID <- withWorkingDir (toFilePath projectRoot) $
    trim . decodeUtf8 <$> readDockerProcess
      ( concat
        [ [ "create"
          , "-e", inContainerEnvVar ++ "=1"
          , "-e", stackRootEnvVar ++ "=" ++ toFilePathNoTrailingSep stackRoot
          , "-e", platformVariantEnvVar ++ "=dk" ++ platformVariant
          , "-e", "HOME=" ++ toFilePathNoTrailingSep sandboxHomeDir
          , "-e", "PATH=" ++ T.unpack newPathEnv
          , "-e", "PWD=" ++ toFilePathNoTrailingSep pwd
          , "-v"
          , toFilePathNoTrailingSep homeDir ++ ":" ++
              toFilePathNoTrailingSep homeDir ++ mountSuffix
          , "-v"
          , toFilePathNoTrailingSep stackRoot ++ ":" ++
              toFilePathNoTrailingSep stackRoot ++ mountSuffix
          , "-v"
          , toFilePathNoTrailingSep projectRoot ++ ":" ++
              toFilePathNoTrailingSep projectRoot ++ mountSuffix
          , "-v"
          , toFilePathNoTrailingSep sandboxHomeDir ++ ":" ++
              toFilePathNoTrailingSep sandboxHomeDir ++ mountSuffix
          , "-w", toFilePathNoTrailingSep pwd
          ]
        , case dockerNetwork docker of
            Nothing -> ["--net=host"]
            Just name -> ["--net=" ++ name]
        , case muserEnv of
            Nothing -> []
            Just userEnv -> ["-e","USER=" ++ userEnv]
        , case msshAuthSock of
            Nothing -> []
            Just sshAuthSock ->
              [ "-e","SSH_AUTH_SOCK=" ++ sshAuthSock
              , "-v",sshAuthSock ++ ":" ++ sshAuthSock
              ]
        , case mstackYaml of
            Nothing -> []
            Just stackYaml ->
              [ "-e","STACK_YAML=" ++ stackYaml
              , "-v",stackYaml++ ":" ++ stackYaml ++ ":ro"
              ]
           -- Disable the deprecated entrypoint in FP Complete-generated images
        , [ "--entrypoint=/usr/bin/env"
          |  isJust (lookupImageEnv oldSandboxIdEnvVar imageEnvVars)
          && (  icEntrypoint == ["/usr/local/sbin/docker-entrypoint"]
             || icEntrypoint == ["/root/entrypoint.sh"]
             )
          ]
        , concatMap (\(k,v) -> ["-e", k ++ "=" ++ v]) envVars
        , concatMap (mountArg mountSuffix) (extraMount ++ dockerMount docker)
        , concatMap (\nv -> ["-e", nv]) (dockerEnv docker)
        , case dockerContainerName docker of
            Just name -> ["--name=" ++ name]
            Nothing -> []
        , ["-t" | isTerm]
        , ["-i" | keepStdinOpen]
        , dockerRunArgs docker
        , [image]
        , [cmnd]
        , args
        ]
      )
  e <- handleSignals docker keepStdinOpen containerID
  case e of
    Left ExitCodeException{eceExitCode} -> exitWith eceExitCode
    Right () -> exitSuccess
  where
    -- This is using a hash of the Docker repository (without tag or digest) to
    -- ensure binaries/libraries aren't shared between Docker and host (or
    -- incompatible Docker images)
    hashRepoName :: String -> Hash.Digest Hash.MD5
    hashRepoName = Hash.hash . BS.pack . takeWhile (\c -> c /= ':' && c /= '@')
    lookupImageEnv name vars =
      case lookup name vars of
        Just ('=':val) -> Just val
        _ -> Nothing
    mountArg mountSuffix (Mount host container) =
      ["-v",host ++ ":" ++ container ++ mountSuffix]
    sshRelDir = relDirDotSsh

-- | Inspect Docker image or container.
inspect :: (HasProcessContext env, HasLogFunc env)
        => String
        -> RIO env (Maybe Inspect)
inspect image = do
  results <- inspects [image]
  case Map.toList results of
    [] -> pure Nothing
    [(_,i)] -> pure (Just i)
    _ -> throwIO (InvalidInspectOutputException "expect a single result")

-- | Inspect multiple Docker images and/or containers.
inspects :: (HasProcessContext env, HasLogFunc env)
         => [String]
         -> RIO env (Map Text Inspect)
inspects [] = pure Map.empty
inspects images = do
  maybeInspectOut <-
    -- not using 'readDockerProcess' as the error from a missing image
    -- needs to be recovered.
    try (BL.toStrict . fst <$> proc "docker" ("inspect" : images) readProcess_)
  case maybeInspectOut of
    Right inspectOut ->
      -- filtering with 'isAscii' to workaround @docker inspect@ output
      -- containing invalid UTF-8
      case eitherDecode (LBS.pack (filter isAscii (decodeUtf8 inspectOut))) of
        Left msg -> throwIO (InvalidInspectOutputException msg)
        Right results -> pure (Map.fromList (map (\r -> (iiId r,r)) results))
    Left ece
      | any (`LBS.isPrefixOf` eceStderr ece) missingImagePrefixes ->
          pure Map.empty
    Left e -> throwIO e
 where
  missingImagePrefixes = ["Error: No such image", "Error: No such object:"]

-- | Pull latest version of configured Docker image from registry.
pull :: HasConfig env => RIO env ()
pull = do
  config <- view configL
  let docker = configDocker config
  checkDockerVersion docker
  either throwIO (pullImage docker) (dockerImage docker)

-- | Pull Docker image from registry.
pullImage :: (HasProcessContext env, HasLogFunc env)
          => DockerOpts
          -> String
          -> RIO env ()
pullImage docker image = do
  logInfo ("Pulling image from registry: '" <> fromString image <> "'")
  when (dockerRegistryLogin docker) $ do
    logInfo "You may need to log in."
    proc
      "docker"
      ( concat
          [ ["login"]
          , maybe [] (\n -> ["--username=" ++ n]) (dockerRegistryUsername docker)
          , maybe [] (\p -> ["--password=" ++ p]) (dockerRegistryPassword docker)
          , [takeWhile (/= '/') image]
          ]
      )
      runProcess_
  -- We redirect the stdout of the process to stderr so that the output
  -- of @docker pull@ will not interfere with the output of other
  -- commands when using --auto-docker-pull. See issue #2733.
  ec <- proc "docker" ["pull", image] $ \pc0 -> do
    let pc = setStdout (useHandleOpen stderr)
           $ setStderr (useHandleOpen stderr)
           $ setStdin closed
             pc0
    runProcess pc
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwIO (PullFailedException image)

-- | Check docker version (throws exception if incorrect)
checkDockerVersion ::
     (HasProcessContext env, HasLogFunc env)
  => DockerOpts
  -> RIO env ()
checkDockerVersion docker = do
  dockerExists <- doesExecutableExist "docker"
  unless dockerExists (throwIO DockerNotInstalledException)
  dockerVersionOut <- readDockerProcess ["--version"]
  case words (decodeUtf8 dockerVersionOut) of
    (_:_:v:_) ->
      case fmap mkVersion' $ parseVersion' $ stripVersion v of
        Just v'
          | v' < minimumDockerVersion ->
            throwIO (DockerTooOldException minimumDockerVersion v')
          | v' `elem` prohibitedDockerVersions ->
            throwIO (DockerVersionProhibitedException prohibitedDockerVersions v')
          | not (v' `withinRange` dockerRequireDockerVersion docker) ->
            throwIO (BadDockerVersionException (dockerRequireDockerVersion docker) v')
          | otherwise ->
            pure ()
        _ -> throwIO InvalidVersionOutputException
    _ -> throwIO InvalidVersionOutputException
 where
  minimumDockerVersion = mkVersion [1, 6, 0]
  prohibitedDockerVersions = []
  stripVersion v = takeWhile (/= '-') (dropWhileEnd (not . isDigit) v)
  -- version is parsed by Data.Version provided code to avoid
  -- Cabal's Distribution.Version lack of support for leading zeros in version
  parseVersion' =
    fmap fst . listToMaybe . reverse . readP_to_S Data.Version.parseVersion

-- | Remove the project's Docker sandbox.
reset :: HasConfig env => Bool -> RIO env ()
reset keepHome = do
  projectRoot <- getProjectRoot
  dockerSandboxDir <- projectDockerSandboxDir projectRoot
  liftIO (removeDirectoryContents
            dockerSandboxDir
            [homeDirName | keepHome]
            [])

-- | The Docker container "entrypoint": special actions performed when first
-- entering a container, such as switching the UID/GID to the "outside-Docker"
-- user's.
entrypoint :: (HasProcessContext env, HasLogFunc env)
           => Config
           -> DockerEntrypoint
           -> RIO env ()
entrypoint config@Config{} DockerEntrypoint{..} =
  modifyMVar_ entrypointMVar $ \alreadyRan -> do
    -- Only run the entrypoint once
    unless alreadyRan $ do
      envOverride <- view processContextL
      homeDir <- liftIO $ parseAbsDir =<< getEnv "HOME"
      -- Get the UserEntry for the 'stack' user in the image, if it exists
      estackUserEntry0 <- liftIO $ tryJust (guard . isDoesNotExistError) $
        User.getUserEntryForName stackUserName
      -- Switch UID/GID if needed, and update user's home directory
      case deUser of
        Nothing -> pure ()
        Just (DockerUser 0 _ _ _) -> pure ()
        Just du -> withProcessContext envOverride $
          updateOrCreateStackUser estackUserEntry0 homeDir du
      case estackUserEntry0 of
        Left _ -> pure ()
        Right ue -> do
          -- If the 'stack' user exists in the image, copy any build plans and
          -- package indices from its original home directory to the host's
          -- Stack root, to avoid needing to download them
          origStackHomeDir <- liftIO $ parseAbsDir (User.homeDirectory ue)
          let origStackRoot = origStackHomeDir </> relDirDotStackProgName
          buildPlanDirExists <- doesDirExist (buildPlanDir origStackRoot)
          when buildPlanDirExists $ do
            (_, buildPlans) <- listDir (buildPlanDir origStackRoot)
            forM_ buildPlans $ \srcBuildPlan -> do
              let destBuildPlan =
                    buildPlanDir (view stackRootL config) </> filename srcBuildPlan
              exists <- doesFileExist destBuildPlan
              unless exists $ do
                ensureDir (parent destBuildPlan)
                copyFile srcBuildPlan destBuildPlan
    pure True
 where
  updateOrCreateStackUser estackUserEntry homeDir DockerUser{..} = do
    case estackUserEntry of
      Left _ -> do
        -- If no 'stack' user in image, create one with correct UID/GID and home
        -- directory
        readProcessNull "groupadd"
          ["-o"
          ,"--gid",show duGid
          ,stackUserName]
        readProcessNull "useradd"
          ["-oN"
          ,"--uid",show duUid
          ,"--gid",show duGid
          ,"--home",toFilePathNoTrailingSep homeDir
          ,stackUserName]
      Right _ -> do
        -- If there is already a 'stack' user in the image, adjust its UID/GID
        -- and home directory
        readProcessNull "usermod"
          ["-o"
          ,"--uid",show duUid
          ,"--home",toFilePathNoTrailingSep homeDir
          ,stackUserName]
        readProcessNull "groupmod"
          ["-o"
          ,"--gid",show duGid
          ,stackUserName]
    forM_ duGroups $ \gid ->
      readProcessNull "groupadd"
        ["-o"
        ,"--gid",show gid
        ,"group" ++ show gid]
    -- 'setuid' to the wanted UID and GID
    liftIO $ do
      User.setGroupID duGid
      handleSetGroups duGroups
      User.setUserID duUid
      _ <- Files.setFileCreationMask duUmask
      pure ()
  stackUserName = "stack" :: String

-- | MVar used to ensure the Docker entrypoint is performed exactly once
entrypointMVar :: MVar Bool
{-# NOINLINE entrypointMVar #-}
entrypointMVar = unsafePerformIO (newMVar False)

-- | Remove the contents of a directory, without removing the directory itself.
-- This is used instead of 'FS.removeTree' to clear bind-mounted directories,
-- since removing the root of the bind-mount won't work.
removeDirectoryContents ::
     Path Abs Dir -- ^ Directory to remove contents of
  -> [Path Rel Dir] -- ^ Top-level directory names to exclude from removal
  -> [Path Rel File] -- ^ Top-level file names to exclude from removal
  -> IO ()
removeDirectoryContents path excludeDirs excludeFiles = do
  isRootDir <- doesDirExist path
  when isRootDir $ do
    (lsd,lsf) <- listDir path
    forM_ lsd
          (\d -> unless (dirname d `elem` excludeDirs)
                        (removeDirRecur d))
    forM_ lsf
          (\f -> unless (filename f `elem` excludeFiles)
                        (removeFile f))

-- | Produce a strict 'S.ByteString' from the stdout of a process. Throws a
-- 'ReadProcessException' exception if the process fails.
--
-- The stderr output is passed straight through, which is desirable for some
-- cases e.g. docker pull, in which docker uses stderr for progress output.
--
-- Use 'readProcess_' directly to customize this.
readDockerProcess ::
     (HasProcessContext env, HasLogFunc env)
  => [String] -> RIO env BS.ByteString
readDockerProcess args = BL.toStrict <$> proc "docker" args readProcessStdout_

-- | Name of home directory within docker sandbox.
homeDirName :: Path Rel Dir
homeDirName = relDirUnderHome

-- | Directory where 'stack' executable is bind-mounted in Docker container
-- This refers to a path in the Linux *container*, and so should remain a
-- 'FilePath' (not 'Path Abs Dir') so that it works when the host runs Windows.
hostBinDir :: FilePath
hostBinDir = "/opt/host/bin"

-- | Convenience function to decode ByteString to String.
decodeUtf8 :: BS.ByteString -> String
decodeUtf8 bs = T.unpack (T.decodeUtf8 bs)

-- | Fail with friendly error if project root not set.
getProjectRoot :: HasConfig env => RIO env (Path Abs Dir)
getProjectRoot = do
  mroot <- view $ configL.to configProjectRoot
  maybe (throwIO CannotDetermineProjectRootException) pure mroot

-- | Environment variable that contained the old sandbox ID.
-- | Use of this variable is deprecated, and only used to detect old images.
oldSandboxIdEnvVar :: String
oldSandboxIdEnvVar = "DOCKER_SANDBOX_ID"

-- | Parsed result of @docker inspect@.
data Inspect = Inspect
  { iiConfig      :: ImageConfig
  , iiCreated     :: UTCTime
  , iiId          :: Text
  , iiVirtualSize :: Maybe Integer
  }
  deriving Show

-- | Parse @docker inspect@ output.
instance FromJSON Inspect where
  parseJSON v = do
    o <- parseJSON v
    Inspect
      <$> o .: "Config"
      <*> o .: "Created"
      <*> o .: "Id"
      <*> o .:? "VirtualSize"

-- | Parsed @Config@ section of @docker inspect@ output.
data ImageConfig = ImageConfig
  { icEnv :: [String]
  , icEntrypoint :: [String]
  }
  deriving Show

-- | Parse @Config@ section of @docker inspect@ output.
instance FromJSON ImageConfig where
  parseJSON v = do
    o <- parseJSON v
    ImageConfig
      <$> fmap join (o .:? "Env") .!= []
      <*> fmap join (o .:? "Entrypoint") .!= []
