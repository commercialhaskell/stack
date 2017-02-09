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

import           Control.Applicative
import           Control.Concurrent.MVar.Lifted (MVar,modifyMVar_,newMVar)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadThrow,throwM,MonadCatch)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logError,logInfo,logWarn)
import           Control.Monad.Reader (MonadReader,runReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Writer (execWriter,runWriter,tell)
import qualified "cryptohash" Crypto.Hash as Hash
import           Data.Aeson.Extended (FromJSON(..),(.:),(.:?),(.!=),eitherDecode)
import           Data.ByteString.Builder (stringUtf8,charUtf8,toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isSpace,toUpper,isAscii,isDigit)
import           Data.Conduit.List (sinkNull)
import           Data.List (dropWhileEnd,intercalate,isPrefixOf,isInfixOf,foldl')
import           Data.List.Extra (trim)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord (Down(..))
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime,LocalTime(..),diffDays,utcToLocalTime,getZonedTime,ZonedTime(..))
import           Data.Version (showVersion)
import           GHC.Exts (sortWith)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO hiding (canonicalizePath)
import qualified Paths_stack as Meta
import           Prelude -- Fix redundant import warnings
import           Stack.Config (getInContainer)
import           Stack.Constants
import           Stack.Docker.GlobalDB
import           Stack.Types.PackageIndex
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Internal
import           Stack.Types.StackT
import           Stack.Setup (ensureDockerStackExe)
import           System.Directory (canonicalizePath,getHomeDirectory)
import           System.Environment (getEnv,getEnvironment,getProgName,getArgs,getExecutablePath)
import           System.Exit (exitSuccess, exitWith, ExitCode(..))
import qualified System.FilePath as FP
import           System.IO (stderr,stdin,stdout,hIsTerminalDevice, hClose)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.PosixCompat.User as User
import qualified System.PosixCompat.Files as Files
import           System.Process (CreateProcess(..), StdStream(..), waitForProcess)
import           System.Process.PagerEditor (editByteString)
import           System.Process.Read
import           System.Process.Run
import           Text.Printf (printf)

#ifndef WINDOWS
import           Control.Concurrent (threadDelay)
import qualified Control.Monad.Trans.Control as Control
import           System.Posix.Signals
import qualified System.Posix.User as PosixUser
#endif

-- | If Docker is enabled, re-runs the currently running OS command in a Docker container.
-- Otherwise, runs the inner action.
--
-- This takes an optional release action which should be taken IFF control is
-- transfering away from the current process to the intra-container one.  The main use
-- for this is releasing a lock.  After launching reexecution, the host process becomes
-- nothing but an manager for the call into docker and thus may not hold the lock.
reexecWithOptionalContainer
    :: (StackM env m, HasConfig env)
    => Maybe (Path Abs Dir)
    -> Maybe (m ())
    -> IO ()
    -> Maybe (m ())
    -> Maybe (m ())
    -> m ()
reexecWithOptionalContainer mprojectRoot =
    execWithOptionalContainer mprojectRoot getCmdArgs
  where
    getCmdArgs docker envOverride imageInfo isRemoteDocker = do
        config <- view configL
        deUser <-
            if fromMaybe (not isRemoteDocker) (dockerSetUser docker)
                then liftIO $ do
                  duUid <- User.getEffectiveUserID
                  duGid <- User.getEffectiveGroupID
                  duGroups <- User.getGroups
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
              | otherwise -> throwM UnsupportedStackExeHostPlatformException
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
                                  Nothing
                                  envOverride
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
    :: (StackM env m, HasConfig env)
    => Maybe (Path Abs Dir)
    -> GetCmdArgs env m
    -> Maybe (m ())
    -> IO ()
    -> Maybe (m ())
    -> Maybe (m ())
    -> m ()
execWithOptionalContainer mprojectRoot getCmdArgs mbefore inner mafter mrelease =
  do config <- view configL
     inContainer <- getInContainer
     isReExec <- view reExecL
     if | inContainer && not isReExec && (isJust mbefore || isJust mafter) ->
            throwM OnlyOnHostException
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
preventInContainer :: (MonadIO m,MonadThrow m) => m () -> m ()
preventInContainer inner =
  do inContainer <- getInContainer
     if inContainer
        then throwM OnlyOnHostException
        else inner

-- | Run a command in a new Docker container, then exit the process.
runContainerAndExit :: (StackM env m, HasConfig env)
  => GetCmdArgs env m
  -> Maybe (Path Abs Dir) -- ^ Project root (maybe)
  -> m ()              -- ^ Action to run before
  -> m ()              -- ^ Action to run after
  -> m ()
runContainerAndExit getCmdArgs
                    mprojectRoot
                    before
                    after =
  do config <- view configL
     let docker = configDocker config
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride docker
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
          ($logWarn "Warning: Using boot2docker is NOT supported, and not likely to perform well.")
     maybeImageInfo <- inspect envOverride image
     imageInfo@Inspect{..} <- case maybeImageInfo of
       Just ii -> return ii
       Nothing
         | dockerAutoPull docker ->
             do pullImage envOverride docker image
                mii2 <- inspect envOverride image
                case mii2 of
                  Just ii2 -> return ii2
                  Nothing -> throwM (InspectFailedException image)
         | otherwise -> throwM (NotPulledException image)
     sandboxDir <- projectDockerSandboxDir projectRoot
     let ImageConfig {..} = iiConfig
         imageEnvVars = map (break (== '=')) icEnv
         platformVariant = BS.unpack $ Hash.digestToHexByteString $ hashRepoName image
         stackRoot = configStackRoot config
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
     newPathEnv <- augmentPath
                      [ hostBinDirPath
                      , sandboxHomeDir </> $(mkRelDir ".local/bin")]
                      (T.pack <$> lookupImageEnv "PATH" imageEnvVars)
     (cmnd,args,envVars,extraMount) <- getCmdArgs docker envOverride imageInfo isRemoteDocker
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
     containerID <- (trim . decodeUtf8) <$> readDockerProcess
       envOverride
       (Just projectRoot)
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
     runInBase <- Control.liftBaseWith $ \run -> return (void . run)
     oldHandlers <- forM [sigINT,sigABRT,sigHUP,sigPIPE,sigTERM,sigUSR1,sigUSR2] $ \sig -> do
       let sigHandler = runInBase $ do
             readProcessNull Nothing envOverride "docker"
                             ["kill","--signal=" ++ show sig,containerID]
             when (sig `elem` [sigTERM,sigABRT]) $ do
               -- Give the container 30 seconds to exit gracefully, then send a sigKILL to force it
               liftIO $ threadDelay 30000000
               readProcessNull Nothing envOverride "docker" ["kill",containerID]
       oldHandler <- liftIO $ installHandler sig (Catch sigHandler) Nothing
       return (sig, oldHandler)
#endif
     let cmd = Cmd Nothing
                 "docker"
                 envOverride
                 (concat [["start"]
                         ,["-a" | not (dockerDetach docker)]
                         ,["-i" | keepStdinOpen]
                         ,[containerID]])
     e <- finally
         (try $ callProcess'
             (\cp -> cp { delegate_ctlc = False })
             cmd)
         (do unless (dockerPersist docker || dockerDetach docker) $
               catch
                 (readProcessNull Nothing envOverride "docker" ["rm","-f",containerID])
                 (\(_::ReadProcessException) -> return ())
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
cleanup :: (StackM env m, HasConfig env)
        => CleanupOpts -> m ()
cleanup opts =
  do config <- view configL
     let docker = configDocker config
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride docker
     let runDocker = readDockerProcess envOverride Nothing
     imagesOut <- runDocker ["images","--no-trunc","-f","dangling=false"]
     danglingImagesOut <- runDocker ["images","--no-trunc","-f","dangling=true"]
     runningContainersOut <- runDocker ["ps","-a","--no-trunc","-f","status=running"]
     restartingContainersOut <- runDocker ["ps","-a","--no-trunc","-f","status=restarting"]
     exitedContainersOut <- runDocker ["ps","-a","--no-trunc","-f","status=exited"]
     pausedContainersOut <- runDocker ["ps","-a","--no-trunc","-f","status=paused"]
     let imageRepos = parseImagesOut imagesOut
         danglingImageHashes = Map.keys (parseImagesOut danglingImagesOut)
         runningContainers = parseContainersOut runningContainersOut ++
                             parseContainersOut restartingContainersOut
         stoppedContainers = parseContainersOut exitedContainersOut ++
                             parseContainersOut pausedContainersOut
     inspectMap <- inspects envOverride
                            (Map.keys imageRepos ++
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
     mapM_ (performPlanLine envOverride)
           (reverse (filter filterPlanLine (lines (LBS.unpack plan'))))
     allImageHashesOut <- runDocker ["images","-aq","--no-trunc"]
     liftIO (pruneDockerImagesLastUsed config (lines (decodeUtf8 allImageHashesOut)))
  where
    filterPlanLine line =
      case line of
        c:_ | isSpace c -> False
        _ -> True
    performPlanLine envOverride line =
      case filter (not . null) (words (takeWhile (/= '#') line)) of
        [] -> return ()
        (c:_):t:v:_ ->
          do args <- if | toUpper c == 'R' && t == imageStr ->
                            do $logInfo (concatT ["Removing image: '",v,"'"])
                               return ["rmi",v]
                        | toUpper c == 'R' && t == containerStr ->
                            do $logInfo (concatT ["Removing container: '",v,"'"])
                               return ["rm","-f",v]
                        | otherwise -> throwM (InvalidCleanupCommandException line)
             e <- try (readDockerProcess envOverride Nothing args)
             case e of
               Left ex@ProcessFailed{} ->
                 $logError (concatT ["Could not remove: '",v,"': ", show ex])
               Left e' -> throwM e'
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
                _ -> throw (InvalidImagesOutputException line)
    parseContainersOut = map parseContainer . drop 1 . lines . decodeUtf8
      where parseContainer line =
              case words line of
                hash:image:rest -> (hash,(image,last rest))
                _ -> throw (InvalidPSOutputException line)
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
inspect :: (MonadIO m,MonadLogger m,MonadBaseControl IO m,MonadCatch m)
        => EnvOverride -> String -> m (Maybe Inspect)
inspect envOverride image =
  do results <- inspects envOverride [image]
     case Map.toList results of
       [] -> return Nothing
       [(_,i)] -> return (Just i)
       _ -> throwM (InvalidInspectOutputException "expect a single result")

-- | Inspect multiple Docker images and/or containers.
inspects :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
         => EnvOverride -> [String] -> m (Map String Inspect)
inspects _ [] = return Map.empty
inspects envOverride images =
  do maybeInspectOut <-
       try (readDockerProcess envOverride Nothing ("inspect" : images))
     case maybeInspectOut of
       Right inspectOut ->
         -- filtering with 'isAscii' to workaround @docker inspect@ output containing invalid UTF-8
         case eitherDecode (LBS.pack (filter isAscii (decodeUtf8 inspectOut))) of
           Left msg -> throwM (InvalidInspectOutputException msg)
           Right results -> return (Map.fromList (map (\r -> (iiId r,r)) results))
       Left (ProcessFailed _ _ _ err)
         | "Error: No such image" `LBS.isPrefixOf` err -> return Map.empty
       Left e -> throwM e

-- | Pull latest version of configured Docker image from registry.
pull :: (StackM env m, HasConfig env) => m ()
pull =
  do config <- view configL
     let docker = configDocker config
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride docker
     pullImage envOverride docker (dockerImage docker)

-- | Pull Docker image from registry.
pullImage :: (MonadLogger m,MonadIO m,MonadThrow m)
          => EnvOverride -> DockerOpts -> String -> m ()
pullImage envOverride docker image =
  do $logInfo (concatT ["Pulling image from registry: '",image,"'"])
     when (dockerRegistryLogin docker)
          (do $logInfo "You may need to log in."
              callProcess $ Cmd
                Nothing
                "docker"
                envOverride
                (concat
                   [["login"]
                   ,maybe [] (\n -> ["--username=" ++ n]) (dockerRegistryUsername docker)
                   ,maybe [] (\p -> ["--password=" ++ p]) (dockerRegistryPassword docker)
                   ,[takeWhile (/= '/') image]]))
     -- We redirect the stdout of the process to stderr so that the output
     -- of @docker pull@ will not interfere with the output of other
     -- commands when using --auto-docker-pull. See issue #2733.
     let stdoutToStderr cp = cp
           { std_out = UseHandle stderr
           , std_err = UseHandle stderr
           , std_in = CreatePipe
           }
     (Just hin, _, _, ph) <- createProcess' "pullImage" stdoutToStderr $
       Cmd Nothing "docker" envOverride ["pull",image]
     liftIO (hClose hin)
     ec <- liftIO (waitForProcess ph)
     case ec of
       ExitSuccess -> return ()
       ExitFailure _ -> throwM (PullFailedException image)

-- | Check docker version (throws exception if incorrect)
checkDockerVersion
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride -> DockerOpts -> m ()
checkDockerVersion envOverride docker =
  do dockerExists <- doesExecutableExist envOverride "docker"
     unless dockerExists (throwM DockerNotInstalledException)
     dockerVersionOut <- readDockerProcess envOverride Nothing ["--version"]
     case words (decodeUtf8 dockerVersionOut) of
       (_:_:v:_) ->
         case parseVersionFromString (stripVersion v) of
           Just v'
             | v' < minimumDockerVersion ->
               throwM (DockerTooOldException minimumDockerVersion v')
             | v' `elem` prohibitedDockerVersions ->
               throwM (DockerVersionProhibitedException prohibitedDockerVersions v')
             | not (v' `withinRange` dockerRequireDockerVersion docker) ->
               throwM (BadDockerVersionException (dockerRequireDockerVersion docker) v')
             | otherwise ->
               return ()
           _ -> throwM InvalidVersionOutputException
       _ -> throwM InvalidVersionOutputException
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
entrypoint :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadLogger m)
           => Config -> DockerEntrypoint -> m ()
entrypoint config@Config{..} DockerEntrypoint{..} =
  modifyMVar_ entrypointMVar $ \alreadyRan -> do
    -- Only run the entrypoint once
    unless alreadyRan $ do
      envOverride <- getEnvOverride configPlatform
      homeDir <- parseAbsDir =<< liftIO (getEnv "HOME")
      -- Get the UserEntry for the 'stack' user in the image, if it exists
      estackUserEntry0 <- liftIO $ tryJust (guard . isDoesNotExistError) $
        User.getUserEntryForName stackUserName
      -- Switch UID/GID if needed, and update user's home directory
      case deUser of
        Nothing -> return ()
        Just (DockerUser 0 _ _ _) -> return ()
        Just du -> updateOrCreateStackUser envOverride estackUserEntry0 homeDir du
      case estackUserEntry0 of
        Left _ -> return ()
        Right ue -> do
          -- If the 'stack' user exists in the image, copy any build plans and package indices from
          -- its original home directory to the host's stack root, to avoid needing to download them
          origStackHomeDir <- parseAbsDir (User.homeDirectory ue)
          let origStackRoot = origStackHomeDir </> $(mkRelDir ("." ++ stackProgName))
          buildPlanDirExists <- doesDirExist (buildPlanDir origStackRoot)
          when buildPlanDirExists $ do
            (_, buildPlans) <- listDir (buildPlanDir origStackRoot)
            forM_ buildPlans $ \srcBuildPlan -> do
              let destBuildPlan = buildPlanDir configStackRoot </> filename srcBuildPlan
              exists <- doesFileExist destBuildPlan
              unless exists $ do
                ensureDir (parent destBuildPlan)
                copyFile srcBuildPlan destBuildPlan
          forM_ configPackageIndices $ \pkgIdx -> do
            msrcIndex <- flip runReaderT (config{configStackRoot = origStackRoot}) $ do
               srcIndex <- configPackageIndex (indexName pkgIdx)
               exists <- doesFileExist srcIndex
               return $ if exists
                 then Just srcIndex
                 else Nothing
            case msrcIndex of
              Nothing -> return ()
              Just srcIndex -> do
                flip runReaderT config $ do
                  destIndex <- configPackageIndex (indexName pkgIdx)
                  exists <- doesFileExist destIndex
                  unless exists $ do
                    ensureDir (parent destIndex)
                    copyFile srcIndex destIndex
    return True
  where
    updateOrCreateStackUser envOverride estackUserEntry homeDir DockerUser{..} = do
      case estackUserEntry of
        Left _ -> do
          -- If no 'stack' user in image, create one with correct UID/GID and home directory
          readProcessNull Nothing envOverride "groupadd"
            ["-o"
            ,"--gid",show duGid
            ,stackUserName]
          readProcessNull Nothing envOverride "useradd"
            ["-oN"
            ,"--uid",show duUid
            ,"--gid",show duGid
            ,"--home",toFilePathNoTrailingSep homeDir
            ,stackUserName]
        Right _ -> do
          -- If there is already a 'stack' user in the image, adjust its UID/GID and home directory
          readProcessNull Nothing envOverride "usermod"
            ["-o"
            ,"--uid",show duUid
            ,"--home",toFilePathNoTrailingSep homeDir
            ,stackUserName]
          readProcessNull Nothing envOverride "groupmod"
            ["-o"
            ,"--gid",show duGid
            ,stackUserName]
      forM_ duGroups $ \gid -> do
        readProcessNull Nothing envOverride "groupadd"
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
-- process fails.  Logs process's stderr using @$logError@.
readDockerProcess
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride -> Maybe (Path Abs Dir) -> [String] -> m BS.ByteString
readDockerProcess envOverride mpwd = readProcessStdout mpwd envOverride "docker"

-- | Name of home directory within docker sandbox.
homeDirName :: Path Rel Dir
homeDirName = $(mkRelDir "_home/")

-- | Directory where 'stack' executable is bind-mounted in Docker container
hostBinDir :: FilePath
hostBinDir = "/opt/host/bin"

-- | Convenience function to decode ByteString to String.
decodeUtf8 :: BS.ByteString -> String
decodeUtf8 bs = T.unpack (T.decodeUtf8 bs)

-- | Convenience function constructing message for @$log*@.
concatT :: [String] -> Text
concatT = T.pack . concat

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot = fromMaybe (throw CannotDetermineProjectRootException)

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
type GetCmdArgs env m
   = (StackM env m, HasConfig env)
  => DockerOpts
  -> EnvOverride
  -> Inspect
  -> Bool
  -> m (FilePath,[String],[(String,String)],[Mount])
