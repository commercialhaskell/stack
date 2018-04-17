{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, ConstraintKinds, DeriveDataTypeable, FlexibleContexts, MultiWayIf, NamedFieldPuns,
             OverloadedStrings, PackageImports, RankNTypes, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell, TupleSections #-}

-- | Run commands in Docker containers
module Stack.Docker
  (cleanup
  ,CleanupOpts(..)
  ,CleanupAction(..)
  ,dockerCleanupCmdName
  ,dockerCmdName
  ,dockerHelpOptName
  ,dockerPullCmdName
  ,entrypoint
  ,preventInContainer
  ,pull
  ,reexecWithOptionalContainer
  ,reset
  ,reExecArgName
  ,StackDockerException(..)
  ) where

import           Stack.Prelude
import           Control.Monad.Writer (execWriter,runWriter,tell)
import qualified Crypto.Hash as Hash (Digest, MD5, hash)
import           Data.Aeson.Extended (FromJSON(..),(.:),(.:?),(.!=),eitherDecode)
import           Data.ByteString.Builder (stringUtf8,charUtf8,toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isSpace,toUpper,isAscii,isDigit)
import           Data.Conduit.List (sinkNull)
import           Data.Conduit.Process.Typed hiding (proc)
import           Data.List (dropWhileEnd,intercalate,isPrefixOf,isInfixOf)
import           Data.List.Extra (trim)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down(..))
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime,LocalTime(..),diffDays,utcToLocalTime,getZonedTime,ZonedTime(..))
import           Data.Version (showVersion)
import           GHC.Exts (sortWith)
import           Lens.Micro (set)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO hiding (canonicalizePath)
import qualified Paths_stack as Meta
import           Stack.Config (getInContainer)
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Docker.GlobalDB
import           Stack.PackageIndex
import           Stack.Types.PackageIndex
import           Stack.Types.Runner
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Setup (ensureDockerStackExe)
import           System.Directory (canonicalizePath,getHomeDirectory)
import           System.Environment (getEnv,getEnvironment,getProgName,getArgs,getExecutablePath)
import           System.Exit (exitSuccess, exitWith, ExitCode(..))
import qualified System.FilePath as FP
import           System.IO (stderr,stdin,stdout)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.PosixCompat.User as User
import qualified System.PosixCompat.Files as Files
import           System.Process.PagerEditor (editByteString)
import           RIO.Process
import           Text.Printf (printf)

#ifndef WINDOWS
import           System.Posix.Signals
import qualified System.Posix.User as PosixUser
#endif

-- | If Docker is enabled, re-runs the currently running OS command in a Docker container.
-- Otherwise, runs the inner action.
--
-- This takes an optional release action which should be taken IFF control is
-- transferring away from the current process to the intra-container one.  The main use
-- for this is releasing a lock.  After launching reexecution, the host process becomes
-- nothing but an manager for the call into docker and thus may not hold the lock.
reexecWithOptionalContainer
    :: HasConfig env
    => Maybe (Path Abs Dir)
    -> Maybe (RIO env ())
    -> IO ()
    -> Maybe (RIO env ())
    -> Maybe (RIO env ())
    -> RIO env ()
reexecWithOptionalContainer mprojectRoot =
    execWithOptionalContainer mprojectRoot getCmdArgs
  where
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
                  return (Just DockerUser{..})
                else return Nothing
        args <-
            fmap
                (["--" ++ reExecArgName ++ "=" ++ showVersion Meta.version
                 ,"--" ++ dockerEntrypointArgName
                 ,show DockerEntrypoint{..}] ++)
                (liftIO getArgs)
        case dockerStackExe (configDocker config) of
            Just DockerStackExeHost
              | configPlatform config == dockerContainerPlatform -> do
                  exePath <- liftIO getExecutablePath
                  cmdArgs args exePath
              | otherwise -> throwIO UnsupportedStackExeHostPlatformException
            Just DockerStackExeImage -> do
                progName <- liftIO getProgName
                return (FP.takeBaseName progName, args, [], [])
            Just (DockerStackExePath path) -> do
                exePath <- liftIO $ canonicalizePath (toFilePath path)
                cmdArgs args exePath
            Just DockerStackExeDownload -> exeDownload args
            Nothing
              | configPlatform config == dockerContainerPlatform -> do
                  (exePath,exeTimestamp,misCompatible) <-
                      liftIO $
                      do exePath <- liftIO getExecutablePath
                         exeTimestamp <- resolveFile' exePath >>= getModificationTime
                         isKnown <-
                             liftIO $
                             getDockerImageExe
                                 config
                                 (iiId imageInfo)
                                 exePath
                                 exeTimestamp
                         return (exePath, exeTimestamp, isKnown)
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
                                  , exePath ++ ":" ++ "/tmp/stack"
                                  , iiId imageInfo
                                  , "/tmp/stack"
                                  , "--version"]
                                  sinkNull
                                  sinkNull
                          let compatible =
                                  case e of
                                      Left (ProcessExitedUnsuccessfully _ _) ->
                                          False
                                      Right _ -> True
                          liftIO $
                              setDockerImageExe
                                  config
                                  (iiId imageInfo)
                                  exePath
                                  exeTimestamp
                                  compatible
                          if compatible
                              then cmdArgs args exePath
                              else exeDownload args
            Nothing -> exeDownload args
    exeDownload args = do
        exePath <- ensureDockerStackExe dockerContainerPlatform
        cmdArgs args (toFilePath exePath)
    cmdArgs args exePath = do
        let mountPath = hostBinDir FP.</> FP.takeBaseName exePath
        return (mountPath, args, [], [Mount exePath mountPath])

-- | If Docker is enabled, re-runs the OS command returned by the second argument in a
-- Docker container.  Otherwise, runs the inner action.
--
-- This takes an optional release action just like `reexecWithOptionalContainer`.
execWithOptionalContainer
    :: HasConfig env
    => Maybe (Path Abs Dir)
    -> GetCmdArgs env
    -> Maybe (RIO env ())
    -> IO ()
    -> Maybe (RIO env ())
    -> Maybe (RIO env ())
    -> RIO env ()
execWithOptionalContainer mprojectRoot getCmdArgs mbefore inner mafter mrelease =
  do config <- view configL
     inContainer <- getInContainer
     isReExec <- view reExecL
     if | inContainer && not isReExec && (isJust mbefore || isJust mafter) ->
            throwIO OnlyOnHostException
        | inContainer ->
            liftIO (do inner
                       exitSuccess)
        | not (dockerEnable (configDocker config)) ->
            do fromMaybeAction mbefore
               liftIO inner
               fromMaybeAction mafter
               liftIO exitSuccess
        | otherwise ->
            do fromMaybeAction mrelease
               runContainerAndExit
                 getCmdArgs
                 mprojectRoot
                 (fromMaybeAction mbefore)
                 (fromMaybeAction mafter)
  where
    fromMaybeAction Nothing = return ()
    fromMaybeAction (Just hook) = hook

-- | Error if running in a container.
preventInContainer :: MonadIO m => m () -> m ()
preventInContainer inner =
  do inContainer <- getInContainer
     if inContainer
        then throwIO OnlyOnHostException
        else inner

-- | Run a command in a new Docker container, then exit the process.
runContainerAndExit
  :: HasConfig env
  => GetCmdArgs env
  -> Maybe (Path Abs Dir) -- ^ Project root (maybe)
  -> RIO env ()  -- ^ Action to run before
  -> RIO env ()  -- ^ Action to run after
  -> RIO env ()
runContainerAndExit getCmdArgs
                    mprojectRoot
                    before
                    after = do
     config <- view configL
     let docker = configDocker config
     checkDockerVersion docker
     (env,isStdinTerminal,isStderrTerminal,homeDir) <- liftIO $
       (,,,)
       <$> getEnvironment
       <*> hIsTerminalDevice stdin
       <*> hIsTerminalDevice stderr
       <*> (parseAbsDir =<< getHomeDirectory)
     isStdoutTerminal <- view terminalL
     let dockerHost = lookup "DOCKER_HOST" env
         dockerCertPath = lookup "DOCKER_CERT_PATH" env
         bamboo = lookup "bamboo_buildKey" env
         jenkins = lookup "JENKINS_HOME" env
         msshAuthSock = lookup "SSH_AUTH_SOCK" env
         muserEnv = lookup "USER" env
         isRemoteDocker = maybe False (isPrefixOf "tcp://") dockerHost
         image = dockerImage docker
     when (isRemoteDocker &&
           maybe False (isInfixOf "boot2docker") dockerCertPath)
          (logWarn "Warning: Using boot2docker is NOT supported, and not likely to perform well.")
     maybeImageInfo <- inspect image
     imageInfo@Inspect{..} <- case maybeImageInfo of
       Just ii -> return ii
       Nothing
         | dockerAutoPull docker ->
             do pullImage docker image
                mii2 <- inspect image
                case mii2 of
                  Just ii2 -> return ii2
                  Nothing -> throwM (InspectFailedException image)
         | otherwise -> throwM (NotPulledException image)
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
     hostBinDirPath <- parseAbsDir hostBinDir
     newPathEnv <- either throwM return $ augmentPath
                      ( toFilePath <$>
                      [ hostBinDirPath
                      , sandboxHomeDir </> $(mkRelDir ".local/bin")])
                      (T.pack <$> lookupImageEnv "PATH" imageEnvVars)
     (cmnd,args,envVars,extraMount) <- getCmdArgs docker imageInfo isRemoteDocker
     pwd <- getCurrentDir
     liftIO
       (do updateDockerImageLastUsed config iiId (toFilePath projectRoot)
           mapM_ ensureDir [sandboxHomeDir, stackRoot])
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
     containerID <- withWorkingDir (toFilePath projectRoot) $ trim . decodeUtf8 <$> readDockerProcess
       (concat
         [["create"
          ,"--net=host"
          ,"-e",inContainerEnvVar ++ "=1"
          ,"-e",stackRootEnvVar ++ "=" ++ toFilePathNoTrailingSep stackRoot
          ,"-e",platformVariantEnvVar ++ "=dk" ++ platformVariant
          ,"-e","HOME=" ++ toFilePathNoTrailingSep sandboxHomeDir
          ,"-e","PATH=" ++ T.unpack newPathEnv
          ,"-e","PWD=" ++ toFilePathNoTrailingSep pwd
          ,"-v",toFilePathNoTrailingSep homeDir ++ ":" ++ toFilePathNoTrailingSep homeDir
          ,"-v",toFilePathNoTrailingSep stackRoot ++ ":" ++ toFilePathNoTrailingSep stackRoot
          ,"-v",toFilePathNoTrailingSep projectRoot ++ ":" ++ toFilePathNoTrailingSep projectRoot
          ,"-v",toFilePathNoTrailingSep sandboxHomeDir ++ ":" ++ toFilePathNoTrailingSep sandboxHomeDir
          ,"-w",toFilePathNoTrailingSep pwd]
         ,case muserEnv of
            Nothing -> []
            Just userEnv -> ["-e","USER=" ++ userEnv]
         ,case msshAuthSock of
            Nothing -> []
            Just sshAuthSock ->
              ["-e","SSH_AUTH_SOCK=" ++ sshAuthSock
              ,"-v",sshAuthSock ++ ":" ++ sshAuthSock]
           -- Disable the deprecated entrypoint in FP Complete-generated images
         ,["--entrypoint=/usr/bin/env"
             | isJust (lookupImageEnv oldSandboxIdEnvVar imageEnvVars) &&
               (icEntrypoint == ["/usr/local/sbin/docker-entrypoint"] ||
                 icEntrypoint == ["/root/entrypoint.sh"])]
         ,concatMap (\(k,v) -> ["-e", k ++ "=" ++ v]) envVars
         ,concatMap mountArg (extraMount ++ dockerMount docker)
         ,concatMap (\nv -> ["-e", nv]) (dockerEnv docker)
         ,case dockerContainerName docker of
            Just name -> ["--name=" ++ name]
            Nothing -> []
         ,["-t" | isTerm]
         ,["-i" | keepStdinOpen]
         ,dockerRunArgs docker
         ,[image]
         ,[cmnd]
         ,args])
     before
#ifndef WINDOWS
     run <- askRunInIO
     oldHandlers <- forM [sigINT,sigABRT,sigHUP,sigPIPE,sigTERM,sigUSR1,sigUSR2] $ \sig -> do
       let sigHandler = run $ do
             readProcessNull "docker" ["kill","--signal=" ++ show sig,containerID]
             when (sig `elem` [sigTERM,sigABRT]) $ do
               -- Give the container 30 seconds to exit gracefully, then send a sigKILL to force it
               threadDelay 30000000
               readProcessNull "docker" ["kill",containerID]
       oldHandler <- liftIO $ installHandler sig (Catch sigHandler) Nothing
       return (sig, oldHandler)
#endif
     let args' = concat [["start"]
                        ,["-a" | not (dockerDetach docker)]
                        ,["-i" | keepStdinOpen]
                        ,[containerID]]
     e <- try (proc "docker" args' $ runProcess_ . setDelegateCtlc False)
         `finally`
         (do unless (dockerPersist docker || dockerDetach docker) $
                 readProcessNull "docker" ["rm","-f",containerID]
                 `catch` (\(_::ExitCodeException) -> return ())
#ifndef WINDOWS
             forM_ oldHandlers $ \(sig,oldHandler) ->
               liftIO $ installHandler sig oldHandler Nothing
#endif
         )
     case e of
       Left (ProcessExitedUnsuccessfully _ ec) -> liftIO (exitWith ec)
       Right () -> do after
                      liftIO exitSuccess
  where
    -- This is using a hash of the Docker repository (without tag or digest) to ensure
    -- binaries/libraries aren't shared between Docker and host (or incompatible Docker images)
    hashRepoName :: String -> Hash.Digest Hash.MD5
    hashRepoName = Hash.hash . BS.pack . takeWhile (\c -> c /= ':' && c /= '@')
    lookupImageEnv name vars =
      case lookup name vars of
        Just ('=':val) -> Just val
        _ -> Nothing
    mountArg (Mount host container) = ["-v",host ++ ":" ++ container]
    projectRoot = fromMaybeProjectRoot mprojectRoot
    sshRelDir = $(mkRelDir ".ssh/")

-- | Clean-up old docker images and containers.
cleanup :: HasConfig env => CleanupOpts -> RIO env ()
cleanup opts = do
     config <- view configL
     let docker = configDocker config
     checkDockerVersion docker
     imagesOut <- readDockerProcess ["images","--no-trunc","-f","dangling=false"]
     danglingImagesOut <- readDockerProcess ["images","--no-trunc","-f","dangling=true"]
     runningContainersOut <- readDockerProcess ["ps","-a","--no-trunc","-f","status=running"]
     restartingContainersOut <- readDockerProcess ["ps","-a","--no-trunc","-f","status=restarting"]
     exitedContainersOut <- readDockerProcess ["ps","-a","--no-trunc","-f","status=exited"]
     pausedContainersOut <- readDockerProcess ["ps","-a","--no-trunc","-f","status=paused"]
     let imageRepos = parseImagesOut imagesOut
         danglingImageHashes = Map.keys (parseImagesOut danglingImagesOut)
         runningContainers = parseContainersOut runningContainersOut ++
                             parseContainersOut restartingContainersOut
         stoppedContainers = parseContainersOut exitedContainersOut ++
                             parseContainersOut pausedContainersOut
     inspectMap <- inspects (Map.keys imageRepos ++
                             danglingImageHashes ++
                             map fst stoppedContainers ++
                             map fst runningContainers)
     (imagesLastUsed,curTime) <-
       liftIO ((,) <$> getDockerImagesLastUsed config
                   <*> getZonedTime)
     let planWriter = buildPlan curTime
                                imagesLastUsed
                                imageRepos
                                danglingImageHashes
                                stoppedContainers
                                runningContainers
                                inspectMap
         plan = toLazyByteString (execWriter planWriter)
     plan' <- case dcAction opts of
                CleanupInteractive ->
                  liftIO (editByteString (intercalate "-" [stackProgName
                                                          ,dockerCmdName
                                                          ,dockerCleanupCmdName
                                                          ,"plan"])
                                         plan)
                CleanupImmediate -> return plan
                CleanupDryRun -> do liftIO (LBS.hPut stdout plan)
                                    return LBS.empty
     mapM_ performPlanLine
           (reverse (filter filterPlanLine (lines (LBS.unpack plan'))))
     allImageHashesOut <- readDockerProcess ["images","-aq","--no-trunc"]
     liftIO (pruneDockerImagesLastUsed config (lines (decodeUtf8 allImageHashesOut)))
  where
    filterPlanLine line =
      case line of
        c:_ | isSpace c -> False
        _ -> True
    performPlanLine line =
      case filter (not . null) (words (takeWhile (/= '#') line)) of
        [] -> return ()
        (c:_):t:v:_ ->
          do args <- if | toUpper c == 'R' && t == imageStr ->
                            do logInfo $
                                 "Removing image: '" <>
                                 fromString v <>
                                 "'"
                               return ["rmi",v]
                        | toUpper c == 'R' && t == containerStr ->
                            do logInfo $
                                 "Removing container: '" <>
                                 fromString v <>
                                 "'"
                               return ["rm","-f",v]
                        | otherwise -> throwM (InvalidCleanupCommandException line)
             e <- try (readDockerProcess args)
             case e of
               Left ex ->
                 logError $
                   "Could not remove: '" <>
                   fromString v <>
                   "': " <>
                   displayShow (ex :: ExitCodeException)
               Right _ -> return ()
        _ -> throwM (InvalidCleanupCommandException line)
    parseImagesOut = Map.fromListWith (++) . map parseImageRepo . drop 1 . lines . decodeUtf8
      where parseImageRepo :: String -> (String, [String])
            parseImageRepo line =
              case words line of
                repo:tag:hash:_
                  | repo == "<none>" -> (hash,[])
                  | tag == "<none>" -> (hash,[repo])
                  | otherwise -> (hash,[repo ++ ":" ++ tag])
                _ -> impureThrow (InvalidImagesOutputException line)
    parseContainersOut = map parseContainer . drop 1 . lines . decodeUtf8
      where parseContainer line =
              case words line of
                hash:image:rest | last:_ <- reverse rest -> (hash,(image,last))
                _ -> impureThrow (InvalidPSOutputException line)
    buildPlan curTime
              imagesLastUsed
              imageRepos
              danglingImageHashes
              stoppedContainers
              runningContainers
              inspectMap =
      do case dcAction opts of
           CleanupInteractive ->
             do buildStrLn
                  (concat
                     ["# STACK DOCKER CLEANUP PLAN"
                     ,"\n#"
                     ,"\n# When you leave the editor, the lines in this plan will be processed."
                     ,"\n#"
                     ,"\n# Lines that begin with 'R' denote an image or container that will be."
                     ,"\n# removed.  You may change the first character to/from 'R' to remove/keep"
                     ,"\n# and image or container that would otherwise be kept/removed."
                     ,"\n#"
                     ,"\n# To cancel the cleanup, delete all lines in this file."
                     ,"\n#"
                     ,"\n# By default, the following images/containers will be removed:"
                     ,"\n#"])
                buildDefault dcRemoveKnownImagesLastUsedDaysAgo "Known images last used"
                buildDefault dcRemoveUnknownImagesCreatedDaysAgo "Unknown images created"
                buildDefault dcRemoveDanglingImagesCreatedDaysAgo "Dangling images created"
                buildDefault dcRemoveStoppedContainersCreatedDaysAgo "Stopped containers created"
                buildDefault dcRemoveRunningContainersCreatedDaysAgo "Running containers created"
                buildStrLn
                  (concat
                     ["#"
                     ,"\n# The default plan can be adjusted using command-line arguments."
                     ,"\n# Run '" ++ unwords [stackProgName, dockerCmdName, dockerCleanupCmdName] ++
                      " --help' for details."
                     ,"\n#"])
           _ -> buildStrLn
                  (unlines
                    ["# Lines that begin with 'R' denote an image or container that will be."
                    ,"# removed."])
         buildSection "KNOWN IMAGES (pulled/used by stack)"
                      imagesLastUsed
                      buildKnownImage
         buildSection "UNKNOWN IMAGES (not managed by stack)"
                      (sortCreated (Map.toList (foldl' (\m (h,_) -> Map.delete h m)
                                                       imageRepos
                                                       imagesLastUsed)))
                      buildUnknownImage
         buildSection "DANGLING IMAGES (no named references and not depended on by other images)"
                      (sortCreated (map (,()) danglingImageHashes))
                      buildDanglingImage
         buildSection "STOPPED CONTAINERS"
                      (sortCreated stoppedContainers)
                      (buildContainer (dcRemoveStoppedContainersCreatedDaysAgo opts))
         buildSection "RUNNING CONTAINERS"
                      (sortCreated runningContainers)
                      (buildContainer (dcRemoveRunningContainersCreatedDaysAgo opts))
      where
        buildDefault accessor description =
          case accessor opts of
            Just days -> buildStrLn ("#   - " ++ description ++ " at least " ++ showDays days ++ ".")
            Nothing -> return ()
        sortCreated =
            sortWith (\(_,_,x) -> Down x) .
             mapMaybe (\(h,r) ->
                case Map.lookup h inspectMap of
                    Nothing -> Nothing
                    Just ii -> Just (h,r,iiCreated ii))
        buildSection sectionHead items itemBuilder =
          do let (anyWrote,b) = runWriter (forM items itemBuilder)
             when (or anyWrote) $
                do buildSectionHead sectionHead
                   tell b
        buildKnownImage (imageHash,lastUsedProjects) =
          case Map.lookup imageHash imageRepos of
            Just repos@(_:_) ->
              do case lastUsedProjects of
                   (l,_):_ -> forM_ repos (buildImageTime (dcRemoveKnownImagesLastUsedDaysAgo opts) l)
                   _ -> forM_ repos buildKeepImage
                 forM_ lastUsedProjects buildProject
                 buildInspect imageHash
                 return True
            _ -> return False
        buildUnknownImage (hash, repos, created) =
          case repos of
            [] -> return False
            _ -> do forM_ repos (buildImageTime (dcRemoveUnknownImagesCreatedDaysAgo opts) created)
                    buildInspect hash
                    return True
        buildDanglingImage (hash, (), created) =
          do buildImageTime (dcRemoveDanglingImagesCreatedDaysAgo opts) created hash
             buildInspect hash
             return True
        buildContainer removeAge (hash,(image,name),created) =
          do let disp = name ++ " (image: " ++ image ++ ")"
             buildTime containerStr removeAge created disp
             buildInspect hash
             return True
        buildProject (lastUsedTime, projectPath) =
          buildInfo ("Last used " ++
                     showDaysAgo lastUsedTime ++
                     " in " ++
                     projectPath)
        buildInspect hash =
          case Map.lookup hash inspectMap of
            Just Inspect{iiCreated,iiVirtualSize} ->
              buildInfo ("Created " ++
                         showDaysAgo iiCreated ++
                         maybe ""
                               (\s -> " (size: " ++
                                      printf "%g" (fromIntegral s / 1024.0 / 1024.0 :: Float) ++
                                      "M)")
                               iiVirtualSize)
            Nothing -> return ()
        showDays days =
          case days of
            0 -> "today"
            1 -> "yesterday"
            n -> show n ++ " days ago"
        showDaysAgo oldTime = showDays (daysAgo oldTime)
        daysAgo oldTime =
          let ZonedTime (LocalTime today _) zone = curTime
              LocalTime oldDay _ = utcToLocalTime zone oldTime
          in diffDays today oldDay
        buildImageTime = buildTime imageStr
        buildTime t removeAge time disp =
          case removeAge of
            Just d | daysAgo time >= d -> buildStrLn ("R " ++ t ++ " " ++ disp)
            _ -> buildKeep t disp
        buildKeep t d = buildStrLn ("  " ++ t ++ " " ++ d)
        buildKeepImage = buildKeep imageStr
        buildSectionHead s = buildStrLn ("\n#\n# " ++ s ++ "\n#\n")
        buildInfo = buildStrLn . ("        # " ++)
        buildStrLn l = do buildStr l
                          tell (charUtf8 '\n')
        buildStr = tell . stringUtf8

    imageStr = "image"
    containerStr = "container"

-- | Inspect Docker image or container.
inspect :: (HasProcessContext env, HasLogFunc env)
        => String -> RIO env (Maybe Inspect)
inspect image =
  do results <- inspects [image]
     case Map.toList results of
       [] -> return Nothing
       [(_,i)] -> return (Just i)
       _ -> throwIO (InvalidInspectOutputException "expect a single result")

-- | Inspect multiple Docker images and/or containers.
inspects :: (HasProcessContext env, HasLogFunc env)
         => [String] -> RIO env (Map String Inspect)
inspects [] = return Map.empty
inspects images =
  do maybeInspectOut <- try (readDockerProcess ("inspect" : images))
     case maybeInspectOut of
       Right inspectOut ->
         -- filtering with 'isAscii' to workaround @docker inspect@ output containing invalid UTF-8
         case eitherDecode (LBS.pack (filter isAscii (decodeUtf8 inspectOut))) of
           Left msg -> throwIO (InvalidInspectOutputException msg)
           Right results -> return (Map.fromList (map (\r -> (iiId r,r)) results))
       Left ece
         |  any (`LBS.isPrefixOf` eceStderr ece) missingImagePrefixes -> return Map.empty
       Left e -> throwIO e
  where missingImagePrefixes = ["Error: No such image", "Error: No such object:"]

-- | Pull latest version of configured Docker image from registry.
pull :: HasConfig env => RIO env ()
pull =
  do config <- view configL
     let docker = configDocker config
     checkDockerVersion docker
     pullImage docker (dockerImage docker)

-- | Pull Docker image from registry.
pullImage :: (HasProcessContext env, HasLogFunc env)
          => DockerOpts -> String -> RIO env ()
pullImage docker image =
  do logInfo ("Pulling image from registry: '" <> fromString image <> "'")
     when (dockerRegistryLogin docker)
          (do logInfo "You may need to log in."
              proc
                "docker"
                (concat
                   [["login"]
                   ,maybe [] (\n -> ["--username=" ++ n]) (dockerRegistryUsername docker)
                   ,maybe [] (\p -> ["--password=" ++ p]) (dockerRegistryPassword docker)
                   ,[takeWhile (/= '/') image]])
                runProcess_)
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
       ExitSuccess -> return ()
       ExitFailure _ -> throwIO (PullFailedException image)

-- | Check docker version (throws exception if incorrect)
checkDockerVersion
    :: (HasProcessContext env, HasLogFunc env)
    => DockerOpts -> RIO env ()
checkDockerVersion docker =
  do dockerExists <- doesExecutableExist "docker"
     unless dockerExists (throwIO DockerNotInstalledException)
     dockerVersionOut <- readDockerProcess ["--version"]
     case words (decodeUtf8 dockerVersionOut) of
       (_:_:v:_) ->
         case parseVersionFromString (stripVersion v) of
           Just v'
             | v' < minimumDockerVersion ->
               throwIO (DockerTooOldException minimumDockerVersion v')
             | v' `elem` prohibitedDockerVersions ->
               throwIO (DockerVersionProhibitedException prohibitedDockerVersions v')
             | not (v' `withinRange` dockerRequireDockerVersion docker) ->
               throwIO (BadDockerVersionException (dockerRequireDockerVersion docker) v')
             | otherwise ->
               return ()
           _ -> throwIO InvalidVersionOutputException
       _ -> throwIO InvalidVersionOutputException
  where minimumDockerVersion = $(mkVersion "1.6.0")
        prohibitedDockerVersions = []
        stripVersion v = takeWhile (/= '-') (dropWhileEnd (not . isDigit) v)

-- | Remove the project's Docker sandbox.
reset :: (MonadIO m, MonadReader env m, HasConfig env)
  => Maybe (Path Abs Dir) -> Bool -> m ()
reset maybeProjectRoot keepHome = do
  dockerSandboxDir <- projectDockerSandboxDir projectRoot
  liftIO (removeDirectoryContents
            dockerSandboxDir
            [homeDirName | keepHome]
            [])
  where projectRoot = fromMaybeProjectRoot maybeProjectRoot

-- | The Docker container "entrypoint": special actions performed when first entering
-- a container, such as switching the UID/GID to the "outside-Docker" user's.
entrypoint :: (HasProcessContext env, HasLogFunc env)
           => Config -> DockerEntrypoint -> RIO env ()
entrypoint config@Config{..} DockerEntrypoint{..} =
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
        Nothing -> return ()
        Just (DockerUser 0 _ _ _) -> return ()
        Just du -> withProcessContext envOverride $ updateOrCreateStackUser estackUserEntry0 homeDir du
      case estackUserEntry0 of
        Left _ -> return ()
        Right ue -> do
          -- If the 'stack' user exists in the image, copy any build plans and package indices from
          -- its original home directory to the host's stack root, to avoid needing to download them
          origStackHomeDir <- liftIO $ parseAbsDir (User.homeDirectory ue)
          let origStackRoot = origStackHomeDir </> $(mkRelDir ("." ++ stackProgName))
          buildPlanDirExists <- doesDirExist (buildPlanDir origStackRoot)
          when buildPlanDirExists $ do
            (_, buildPlans) <- listDir (buildPlanDir origStackRoot)
            forM_ buildPlans $ \srcBuildPlan -> do
              let destBuildPlan = buildPlanDir (view stackRootL config) </> filename srcBuildPlan
              exists <- doesFileExist destBuildPlan
              unless exists $ do
                ensureDir (parent destBuildPlan)
                copyFile srcBuildPlan destBuildPlan
          forM_ clIndices $ \pkgIdx -> do
            msrcIndex <- runRIO (set stackRootL origStackRoot config) $ do
               srcIndex <- configPackageIndex (indexName pkgIdx)
               exists <- doesFileExist srcIndex
               return $ if exists
                 then Just srcIndex
                 else Nothing
            case msrcIndex of
              Nothing -> return ()
              Just srcIndex ->
                runRIO config $ do
                  destIndex <- configPackageIndex (indexName pkgIdx)
                  exists <- doesFileExist destIndex
                  unless exists $ do
                    ensureDir (parent destIndex)
                    copyFile srcIndex destIndex
    return True
  where
    CabalLoader {..} = configCabalLoader
    updateOrCreateStackUser estackUserEntry homeDir DockerUser{..} = do
      case estackUserEntry of
        Left _ -> do
          -- If no 'stack' user in image, create one with correct UID/GID and home directory
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
          -- If there is already a 'stack' user in the image, adjust its UID/GID and home directory
          readProcessNull "usermod"
            ["-o"
            ,"--uid",show duUid
            ,"--home",toFilePathNoTrailingSep homeDir
            ,stackUserName]
          readProcessNull "groupmod"
            ["-o"
            ,"--gid",show duGid
            ,stackUserName]
      forM_ duGroups $ \gid -> do
        readProcessNull "groupadd"
          ["-o"
          ,"--gid",show gid
          ,"group" ++ show gid]
      -- 'setuid' to the wanted UID and GID
      liftIO $ do
        User.setGroupID duGid
#ifndef WINDOWS
        PosixUser.setGroups duGroups
#endif
        User.setUserID duUid
        _ <- Files.setFileCreationMask duUmask
        return ()
    stackUserName = "stack"::String

-- | MVar used to ensure the Docker entrypoint is performed exactly once
entrypointMVar :: MVar Bool
{-# NOINLINE entrypointMVar #-}
entrypointMVar = unsafePerformIO (newMVar False)

-- | Remove the contents of a directory, without removing the directory itself.
-- This is used instead of 'FS.removeTree' to clear bind-mounted directories, since
-- removing the root of the bind-mount won't work.
removeDirectoryContents :: Path Abs Dir -- ^ Directory to remove contents of
                        -> [Path Rel Dir] -- ^ Top-level directory names to exclude from removal
                        -> [Path Rel File] -- ^ Top-level file names to exclude from removal
                        -> IO ()
removeDirectoryContents path excludeDirs excludeFiles =
  do isRootDir <- doesDirExist path
     when isRootDir
          (do (lsd,lsf) <- listDir path
              forM_ lsd
                    (\d -> unless (dirname d `elem` excludeDirs)
                                  (removeDirRecur d))
              forM_ lsf
                    (\f -> unless (filename f `elem` excludeFiles)
                                  (removeFile f)))

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ReadProcessException' exception if the
-- process fails.  Logs process's stderr using @logError@.
readDockerProcess
    :: (HasProcessContext env, HasLogFunc env)
    => [String] -> RIO env BS.ByteString
readDockerProcess args = BL.toStrict <$> proc "docker" args readProcessStdout_ -- FIXME stderr isn't logged with logError, should it be?

-- | Name of home directory within docker sandbox.
homeDirName :: Path Rel Dir
homeDirName = $(mkRelDir "_home/")

-- | Directory where 'stack' executable is bind-mounted in Docker container
hostBinDir :: FilePath
hostBinDir = "/opt/host/bin"

-- | Convenience function to decode ByteString to String.
decodeUtf8 :: BS.ByteString -> String
decodeUtf8 bs = T.unpack (T.decodeUtf8 bs)

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot = fromMaybe (impureThrow CannotDetermineProjectRootException)

-- | Environment variable that contained the old sandbox ID.
-- | Use of this variable is deprecated, and only used to detect old images.
oldSandboxIdEnvVar :: String
oldSandboxIdEnvVar = "DOCKER_SANDBOX_ID"

-- | Options for 'cleanup'.
data CleanupOpts = CleanupOpts
  { dcAction                                :: !CleanupAction
  , dcRemoveKnownImagesLastUsedDaysAgo      :: !(Maybe Integer)
  , dcRemoveUnknownImagesCreatedDaysAgo     :: !(Maybe Integer)
  , dcRemoveDanglingImagesCreatedDaysAgo    :: !(Maybe Integer)
  , dcRemoveStoppedContainersCreatedDaysAgo :: !(Maybe Integer)
  , dcRemoveRunningContainersCreatedDaysAgo :: !(Maybe Integer) }
  deriving (Show)

-- | Cleanup action.
data CleanupAction = CleanupInteractive
                   | CleanupImmediate
                   | CleanupDryRun
  deriving (Show)

-- | Parsed result of @docker inspect@.
data Inspect = Inspect
  {iiConfig      :: ImageConfig
  ,iiCreated     :: UTCTime
  ,iiId          :: String
  ,iiVirtualSize :: Maybe Integer}
  deriving (Show)

-- | Parse @docker inspect@ output.
instance FromJSON Inspect where
  parseJSON v =
    do o <- parseJSON v
       Inspect <$> o .: "Config"
               <*> o .: "Created"
               <*> o .: "Id"
               <*> o .:? "VirtualSize"

-- | Parsed @Config@ section of @docker inspect@ output.
data ImageConfig = ImageConfig
  {icEnv :: [String]
  ,icEntrypoint :: [String]}
  deriving (Show)

-- | Parse @Config@ section of @docker inspect@ output.
instance FromJSON ImageConfig where
  parseJSON v =
    do o <- parseJSON v
       ImageConfig
         <$> fmap join (o .:? "Env") .!= []
         <*> fmap join (o .:? "Entrypoint") .!= []

-- | Function to get command and arguments to run in Docker container
type GetCmdArgs env
   = DockerOpts
  -> Inspect
  -> Bool
  -> RIO env (FilePath,[String],[(String,String)],[Mount])
