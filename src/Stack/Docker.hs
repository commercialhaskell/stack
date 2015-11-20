{-# LANGUAGE CPP, ConstraintKinds, DeriveDataTypeable, FlexibleContexts, MultiWayIf, NamedFieldPuns,
             OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell,
             TupleSections #-}

-- | Run commands in Docker containers
module Stack.Docker
  (cleanup
  ,CleanupOpts(..)
  ,CleanupAction(..)
  ,dockerCleanupCmdName
  ,dockerCmdName
  ,dockerPullCmdName
  ,entrypoint
  ,preventInContainer
  ,pull
  ,reexecWithOptionalContainer
  ,reset
  ,reExecArgName
  ) where

import           Control.Applicative
import           Control.Concurrent.MVar.Lifted (MVar,modifyMVar_,newMVar)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadThrow,throwM,MonadCatch,MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logError,logInfo,logWarn)
import           Control.Monad.Reader (MonadReader,asks,runReaderT)
import           Control.Monad.Writer (execWriter,runWriter,tell)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson.Extended (FromJSON(..),(.:),(.:?),(.!=),eitherDecode)
import           Data.ByteString.Builder (stringUtf8,charUtf8,toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isSpace,toUpper,isAscii,isDigit)
import           Data.Conduit.List (sinkNull)
import           Data.List (dropWhileEnd,intercalate,isPrefixOf,isInfixOf,foldl')
import           Data.List.Extra (trim,nubOrd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord (Down(..))
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime,LocalTime(..),diffDays,utcToLocalTime,getZonedTime,ZonedTime(..))
import           Data.Typeable (Typeable)
import           Data.Version (showVersion)
import           Distribution.System (Platform (Platform),Arch (X86_64),OS (Linux))
import           Distribution.Text (display)
import           GHC.Exts (sortWith)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO
import qualified Paths_stack as Meta
import           Prelude -- Fix redundant import warnings
import           Stack.Constants (projectDockerSandboxDir,stackProgName,stackRootEnvVar,buildPlanDir)
import           Stack.Docker.GlobalDB
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Setup (ensureDockerStackExe)
import           System.Directory (canonicalizePath,getModificationTime)
import           System.Environment (getEnv,getProgName,getArgs,getExecutablePath,lookupEnv)
import           System.Exit (exitSuccess, exitWith)
import qualified System.FilePath as FP
import qualified System.FilePath.Posix as Posix
import           System.IO (stderr,stdin,stdout,hIsTerminalDevice)
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.PosixCompat.User as User
import           System.Process.PagerEditor (editByteString)
import           System.Process.Read
import           System.Process.Run
import           System.Process (CreateProcess(delegate_ctlc))
import           Text.Printf (printf)

#ifndef WINDOWS
import           Control.Monad.Trans.Control (liftBaseWith)
import           System.Posix.Signals
#endif

-- | If Docker is enabled, re-runs the currently running OS command in a Docker container.
-- Otherwise, runs the inner action.
--
-- This takes an optional release action which should be taken IFF control is
-- transfering away from the current process to the intra-container one.  The main use
-- for this is releasing a lock.  After launching reexecution, the host process becomes
-- nothing but an manager for the call into docker and thus may not hold the lock.
reexecWithOptionalContainer
    :: M env m
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
        config <- asks getConfig
        deUidGid <-
            if fromMaybe (not isRemoteDocker) (dockerSetUser docker)
                then liftIO $ Just <$>
                  ((,) <$> User.getEffectiveUserID <*> User.getEffectiveGroupID)
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
                         exeTimestamp <- liftIO (getModificationTime exePath)
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
    :: M env m
    => Maybe (Path Abs Dir)
    -> GetCmdArgs env m
    -> Maybe (m ())
    -> IO ()
    -> Maybe (m ())
    -> Maybe (m ())
    -> m ()
execWithOptionalContainer mprojectRoot getCmdArgs mbefore inner mafter mrelease =
  do config <- asks getConfig
     inContainer <- getInContainer
     isReExec <- asks getReExec
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

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: (MonadIO m) => m Bool
getInContainer = liftIO (isJust <$> lookupEnv inContainerEnvVar)

-- | Run a command in a new Docker container, then exit the process.
runContainerAndExit :: M env m
                    => GetCmdArgs env m
                    -> Maybe (Path Abs Dir)
                    -> m ()
                    -> m ()
                    -> m ()
runContainerAndExit getCmdArgs
                    mprojectRoot
                    before
                    after =
  do config <- asks getConfig
     let docker = configDocker config
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride docker
     (dockerHost,dockerCertPath,bamboo,jenkins) <-
       liftIO ((,,,) <$> lookupEnv "DOCKER_HOST"
                     <*> lookupEnv "DOCKER_CERT_PATH"
                     <*> lookupEnv "bamboo_buildKey"
                     <*> lookupEnv "JENKINS_HOME")
     let isRemoteDocker = maybe False (isPrefixOf "tcp://") dockerHost
     isStdoutTerminal <- asks getTerminal
     (isStdinTerminal,isStderrTerminal) <-
       liftIO ((,) <$> hIsTerminalDevice stdin
                   <*> hIsTerminalDevice stderr)
     when (isRemoteDocker &&
           maybe False (isInfixOf "boot2docker") dockerCertPath)
          ($logWarn "Warning: Using boot2docker is NOT supported, and not likely to perform well.")
     let image = dockerImage docker
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
     let ImageConfig {..} = iiConfig
         imageEnvVars = map (break (== '=')) icEnv
         msandboxID = lookupImageEnv sandboxIDEnvVar imageEnvVars
         sandboxID = fromMaybe "default" msandboxID
     sandboxIDDir <- parseRelDir (sandboxID ++ "/")
     let stackRoot = configStackRoot config
         sandboxDir = projectDockerSandboxDir projectRoot
         sandboxSandboxDir = sandboxDir </> $(mkRelDir "_sandbox/") </> sandboxIDDir
         sandboxHomeDir = sandboxDir </> homeDirName
         sandboxRepoDir = sandboxDir </> sandboxIDDir
         sandboxSubdirs = map (\d -> sandboxRepoDir </> d)
                              sandboxedHomeSubdirectories
         isTerm = not (dockerDetach docker) &&
                  isStdinTerminal &&
                  isStdoutTerminal &&
                  isStderrTerminal
         keepStdinOpen = not (dockerDetach docker) &&
                         -- Workaround for https://github.com/docker/docker/issues/12319
                         (isTerm || (isNothing bamboo && isNothing jenkins))
         newPathEnv = intercalate [Posix.searchPathSeparator] $
                      nubOrd $
                      [toFilePathNoTrailingSep $ sandboxRepoDir </> $(mkRelDir ".local/bin")
                      ,toFilePathNoTrailingSep $ sandboxRepoDir </> $(mkRelDir ".cabal/bin")
                      ,toFilePathNoTrailingSep $ sandboxRepoDir </> $(mkRelDir "bin")
                      ,hostBinDir] ++
                      maybe [] Posix.splitSearchPath (lookupImageEnv "PATH" imageEnvVars)
     (cmnd,args,envVars,extraMount) <- getCmdArgs docker envOverride imageInfo isRemoteDocker
     pwd <- getWorkingDir
     liftIO
       (do updateDockerImageLastUsed config iiId (toFilePath projectRoot)
           mapM_ createTree
                 ([sandboxHomeDir, sandboxSandboxDir, stackRoot] ++
                          sandboxSubdirs))
     containerID <- (trim . decodeUtf8) <$> readDockerProcess
       envOverride
       (concat
         [["create"
          ,"--net=host"
          ,"-e",inContainerEnvVar ++ "=1"
          ,"-e",stackRootEnvVar ++ "=" ++ toFilePathNoTrailingSep stackRoot
          ,"-e","HOME=" ++ toFilePathNoTrailingSep sandboxRepoDir
          ,"-e","PATH=" ++ newPathEnv
          ,"-v",toFilePathNoTrailingSep stackRoot ++ ":" ++ toFilePathNoTrailingSep stackRoot
          ,"-v",toFilePathNoTrailingSep projectRoot ++ ":" ++ toFilePathNoTrailingSep projectRoot
          ,"-v",toFilePathNoTrailingSep sandboxSandboxDir ++ ":" ++ toFilePathNoTrailingSep sandboxDir
          ,"-v",toFilePathNoTrailingSep sandboxHomeDir ++ ":" ++ toFilePathNoTrailingSep sandboxRepoDir
          ,"-v",toFilePathNoTrailingSep stackRoot ++ ":" ++
                toFilePathNoTrailingSep (sandboxRepoDir </> $(mkRelDir ("." ++ stackProgName ++ "/")))
          ,"-w",toFilePathNoTrailingSep pwd]
           -- Disable the deprecated entrypoint in FP Complete-generated images
         ,["--entrypoint=/usr/bin/env"
             | isJust msandboxID &&
               (icEntrypoint == ["/usr/local/sbin/docker-entrypoint"] ||
                 icEntrypoint == ["/root/entrypoint.sh"])]
         ,concatMap (\(k,v) -> ["-e", k ++ "=" ++ v]) envVars
         ,concatMap sandboxSubdirArg sandboxSubdirs
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
     runInBase <- liftBaseWith $ \run -> return (void . run)
     oldHandlers <- forM ([sigINT | not keepStdinOpen] ++ [sigTERM]) $ \sig -> do
       let sigHandler = do
             runInBase (readProcessNull Nothing envOverride "docker"
                                        ["kill","--signal=" ++ show sig,containerID])
       oldHandler <- liftIO $ installHandler sig (Catch sigHandler) Nothing
       return (sig, oldHandler)
#endif
     e <- try (callProcess'
                 (if keepStdinOpen then id else (\cp -> cp { delegate_ctlc = False }))
                 Nothing
                 envOverride
                 "docker"
                 (concat [["start"]
                         ,["-a" | not (dockerDetach docker)]
                         ,["-i" | keepStdinOpen]
                         ,[containerID]]))
#ifndef WINDOWS
     forM_ oldHandlers $ \(sig,oldHandler) ->
       liftIO $ installHandler sig oldHandler Nothing
#endif
     unless (dockerPersist docker || dockerDetach docker)
            (readProcessNull Nothing envOverride "docker" ["rm","-f",containerID])
     case e of
       Left (ProcessExitedUnsuccessfully _ ec) -> liftIO (exitWith ec)
       Right () -> do after
                      liftIO exitSuccess
  where
    lookupImageEnv name vars =
      case lookup name vars of
        Just ('=':val) -> Just val
        _ -> Nothing
    mountArg (Mount host container) = ["-v",host ++ ":" ++ container]
    sandboxSubdirArg subdir = ["-v",toFilePathNoTrailingSep subdir++ ":" ++ toFilePathNoTrailingSep subdir]
    projectRoot = fromMaybeProjectRoot mprojectRoot

-- | Clean-up old docker images and containers.
cleanup :: M env m
        => CleanupOpts -> m ()
cleanup opts =
  do config <- asks getConfig
     let docker = configDocker config
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride docker
     let runDocker = readDockerProcess envOverride
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
             e <- try (readDockerProcess envOverride args)
             case e of
               Left (ReadProcessException{}) ->
                 $logError (concatT ["Could not remove: '",v,"'"])
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
            (mapMaybe (\(h,r) ->
                case Map.lookup h inspectMap of
                    Nothing -> Nothing
                    Just ii -> Just (h,r,iiCreated ii)))
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
            Just (Inspect{iiCreated,iiVirtualSize}) ->
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
inspect :: (MonadIO m,MonadThrow m,MonadLogger m,MonadBaseControl IO m,MonadCatch m)
        => EnvOverride -> String -> m (Maybe Inspect)
inspect envOverride image =
  do results <- inspects envOverride [image]
     case Map.toList results of
       [] -> return Nothing
       [(_,i)] -> return (Just i)
       _ -> throwM (InvalidInspectOutputException "expect a single result")

-- | Inspect multiple Docker images and/or containers.
inspects :: (MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
         => EnvOverride -> [String] -> m (Map String Inspect)
inspects _ [] = return Map.empty
inspects envOverride images =
  do maybeInspectOut <-
       try (readDockerProcess envOverride ("inspect" : images))
     case maybeInspectOut of
       Right inspectOut ->
         -- filtering with 'isAscii' to workaround @docker inspect@ output containing invalid UTF-8
         case eitherDecode (LBS.pack (filter isAscii (decodeUtf8 inspectOut))) of
           Left msg -> throwM (InvalidInspectOutputException msg)
           Right results -> return (Map.fromList (map (\r -> (iiId r,r)) results))
       Left (ReadProcessException{}) -> return Map.empty
       Left e -> throwM e

-- | Pull latest version of configured Docker image from registry.
pull :: M env m => m ()
pull =
  do config <- asks getConfig
     let docker = configDocker config
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride docker
     pullImage envOverride docker (dockerImage docker)

-- | Pull Docker image from registry.
pullImage :: (MonadLogger m,MonadIO m,MonadThrow m,MonadBaseControl IO m)
          => EnvOverride -> DockerOpts -> String -> m ()
pullImage envOverride docker image =
  do $logInfo (concatT ["Pulling image from registry: '",image,"'"])
     when (dockerRegistryLogin docker)
          (do $logInfo "You may need to log in."
              callProcess
                Nothing
                envOverride
                "docker"
                (concat
                   [["login"]
                   ,maybe [] (\n -> ["--username=" ++ n]) (dockerRegistryUsername docker)
                   ,maybe [] (\p -> ["--password=" ++ p]) (dockerRegistryPassword docker)
                   ,[takeWhile (/= '/') image]]))
     e <- try (callProcess Nothing envOverride "docker" ["pull",image])
     case e of
       Left (ProcessExitedUnsuccessfully _ _) -> throwM (PullFailedException image)
       Right () -> return ()

-- | Check docker version (throws exception if incorrect)
checkDockerVersion
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride -> DockerOpts -> m ()
checkDockerVersion envOverride docker =
  do dockerExists <- doesExecutableExist envOverride "docker"
     unless dockerExists (throwM DockerNotInstalledException)
     dockerVersionOut <- readDockerProcess envOverride ["--version"]
     case words (decodeUtf8 dockerVersionOut) of
       (_:_:v:_) ->
         case parseVersionFromString (dropWhileEnd (not . isDigit) v) of
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

-- | Remove the project's Docker sandbox.
reset :: (MonadIO m) => Maybe (Path Abs Dir) -> Bool -> m ()
reset maybeProjectRoot keepHome =
  liftIO (removeDirectoryContents
            (projectDockerSandboxDir projectRoot)
            [homeDirName | keepHome]
            [])
  where projectRoot = fromMaybeProjectRoot maybeProjectRoot

-- | The Docker container "entrypoint": special actions performed when first entering
-- a container, such as switching the UID/GID to the "outside-Docker" user's.
entrypoint :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadLogger m)
           => Config -> DockerEntrypoint -> m ()
entrypoint config@Config{..} DockerEntrypoint{..} = do
  modifyMVar_ entrypointMVar $ \alreadyRan -> do
    -- Only run the entrypoint once
    unless alreadyRan $ do
      envOverride <- getEnvOverride configPlatform
      homeDir <- parseAbsDir =<< liftIO (getEnv "HOME")
      -- Get the UserEntry for the 'stack' user in the image, if it exists
      estackUserEntry0 <- liftIO $ tryJust (guard . isDoesNotExistError) $
        User.getUserEntryForName stackUserName
      -- Switch UID/GID if needed, and update user's home directory
      case deUidGid of
        Nothing -> return ()
        Just (0,_) -> return ()
        Just (uid,gid) -> updateOrCreateStackUser envOverride estackUserEntry0 homeDir uid gid
      case estackUserEntry0 of
        Left _ -> return ()
        Right ue -> do
          -- If the 'stack' user exists in the image, copy any build plans and package indices from
          -- its original home directory to the host's stack root, to avoid needing to download them
          origStackHomeDir <- parseAbsDir (User.homeDirectory ue)
          let origStackRoot = origStackHomeDir </> $(mkRelDir ("." ++ stackProgName))
          buildPlanDirExists <- dirExists (buildPlanDir origStackRoot)
          when buildPlanDirExists $ do
            (_, buildPlans) <- listDirectory (buildPlanDir origStackRoot)
            forM_ buildPlans $ \srcBuildPlan -> do
              let destBuildPlan = buildPlanDir configStackRoot </> filename srcBuildPlan
              exists <- fileExists destBuildPlan
              unless exists $ do
                createTree (parent destBuildPlan)
                copyFile srcBuildPlan destBuildPlan
          forM_ configPackageIndices $ \pkgIdx -> do
            msrcIndex <- flip runReaderT (config{configStackRoot = origStackRoot}) $ do
               srcIndex <- configPackageIndex (indexName pkgIdx)
               exists <- fileExists srcIndex
               return $ if exists
                 then Just srcIndex
                 else Nothing
            case msrcIndex of
              Nothing -> return ()
              Just srcIndex -> do
                flip runReaderT config $ do
                  destIndex <- configPackageIndex (indexName pkgIdx)
                  exists <- fileExists destIndex
                  unless exists $ do
                    createTree (parent destIndex)
                    copyFile srcIndex destIndex
    return True
  where
    updateOrCreateStackUser envOverride estackUserEntry homeDir uid gid = do
      case estackUserEntry of
        Left _ -> do
          -- If no 'stack' user in image, create one with correct UID/GID and home directory
          readProcessNull Nothing envOverride "groupadd"
            ["-o"
            ,"--gid",show gid
            ,stackUserName]
          readProcessNull Nothing envOverride "useradd"
            ["-oN"
            ,"--uid",show uid
            ,"--gid",show gid
            ,"--home",toFilePathNoTrailingSep homeDir
            ,stackUserName]
        Right _ -> do
          -- If there is already a 'stack' user in thr image, adjust its UID/GID and home directory
          readProcessNull Nothing envOverride "usermod"
            ["-o"
            ,"--uid",show uid
            ,"--home",toFilePathNoTrailingSep homeDir
            ,stackUserName]
          readProcessNull Nothing envOverride "groupmod"
            ["-o"
            ,"--gid",show gid
            ,stackUserName]
      -- 'setuid' to the wanted UID and GID
      liftIO $ do
        User.setGroupID gid
        User.setUserID uid
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
  do isRootDir <- dirExists path
     when isRootDir
          (do (lsd,lsf) <- listDirectory path
              forM_ lsd
                    (\d -> unless (dirname d `elem` excludeDirs)
                                  (removeTree d))
              forM_ lsf
                    (\f -> unless (filename f `elem` excludeFiles)
                                  (removeFile f)))

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ReadProcessException' exception if the
-- process fails.  Logs process's stderr using @$logError@.
readDockerProcess
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => EnvOverride -> [String] -> m BS.ByteString
readDockerProcess envOverride = readProcessStdout Nothing envOverride "docker"

-- | Subdirectories of the home directory to sandbox between GHC/Stackage versions.
sandboxedHomeSubdirectories :: [Path Rel Dir]
sandboxedHomeSubdirectories =
  [$(mkRelDir ".ghc/")
  ,$(mkRelDir ".cabal/")
  ,$(mkRelDir ".ghcjs/")]

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

-- | Environment variable that contains the sandbox ID.
sandboxIDEnvVar :: String
sandboxIDEnvVar = "DOCKER_SANDBOX_ID"

-- | Environment variable used to indicate stack is running in container.
inContainerEnvVar :: String
inContainerEnvVar = fmap toUpper stackProgName ++ "_IN_CONTAINER"

-- | Command-line argument for "docker"
dockerCmdName :: String
dockerCmdName = "docker"

-- | Command-line argument for @docker pull@.
dockerPullCmdName :: String
dockerPullCmdName = "pull"

-- | Command-line argument for @docker cleanup@.
dockerCleanupCmdName :: String
dockerCleanupCmdName = "cleanup"

-- | Command-line option for @--internal-re-exec-version@.
reExecArgName :: String
reExecArgName = "internal-re-exec-version"

-- | Platform that Docker containers run
dockerContainerPlatform :: Platform
dockerContainerPlatform = Platform X86_64 Linux

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
         <$> o .:? "Env" .!= []
         <*> o .:? "Entrypoint" .!= []

-- | Exceptions thrown by Stack.Docker.
data StackDockerException
  = DockerMustBeEnabledException
    -- ^ Docker must be enabled to use the command.
  | OnlyOnHostException
    -- ^ Command must be run on host OS (not in a container).
  | InspectFailedException String
    -- ^ @docker inspect@ failed.
  | NotPulledException String
    -- ^ Image does not exist.
  | InvalidCleanupCommandException String
    -- ^ Input to @docker cleanup@ has invalid command.
  | InvalidImagesOutputException String
    -- ^ Invalid output from @docker images@.
  | InvalidPSOutputException String
    -- ^ Invalid output from @docker ps@.
  | InvalidInspectOutputException String
    -- ^ Invalid output from @docker inspect@.
  | PullFailedException String
    -- ^ Could not pull a Docker image.
  | DockerTooOldException Version Version
    -- ^ Installed version of @docker@ below minimum version.
  | DockerVersionProhibitedException [Version] Version
    -- ^ Installed version of @docker@ is prohibited.
  | BadDockerVersionException VersionRange Version
    -- ^ Installed version of @docker@ is out of range specified in config file.
  | InvalidVersionOutputException
    -- ^ Invalid output from @docker --version@.
  | HostStackTooOldException Version (Maybe Version)
    -- ^ Version of @stack@ on host is too old for version in image.
  | ContainerStackTooOldException Version Version
    -- ^ Version of @stack@ in container/image is too old for version on host.
  | CannotDetermineProjectRootException
    -- ^ Can't determine the project root (where to put docker sandbox).
  | DockerNotInstalledException
    -- ^ @docker --version@ failed.
  | UnsupportedStackExeHostPlatformException
    -- ^ Using host stack-exe on unsupported platform.
  deriving (Typeable)

-- | Exception instance for StackDockerException.
instance Exception StackDockerException

-- | Show instance for StackDockerException.
instance Show StackDockerException where
  show DockerMustBeEnabledException =
    "Docker must be enabled in your configuration file to use this command."
  show OnlyOnHostException =
    "This command must be run on host OS (not in a Docker container)."
  show (InspectFailedException image) =
    concat ["'docker inspect' failed for image after pull: ",image,"."]
  show (NotPulledException image) =
    concat ["The Docker image referenced by your configuration file"
           ," has not\nbeen downloaded:\n    "
           ,image
           ,"\n\nRun '"
           ,unwords [stackProgName, dockerCmdName, dockerPullCmdName]
           ,"' to download it, then try again."]
  show (InvalidCleanupCommandException line) =
    concat ["Invalid line in cleanup commands: '",line,"'."]
  show (InvalidImagesOutputException line) =
    concat ["Invalid 'docker images' output line: '",line,"'."]
  show (InvalidPSOutputException line) =
    concat ["Invalid 'docker ps' output line: '",line,"'."]
  show (InvalidInspectOutputException msg) =
    concat ["Invalid 'docker inspect' output: ",msg,"."]
  show (PullFailedException image) =
    concat ["Could not pull Docker image:\n    "
           ,image
           ,"\nThere may not be an image on the registry for your resolver's LTS version in\n"
           ,"your configuration file."]
  show (DockerTooOldException minVersion haveVersion) =
    concat ["Minimum docker version '"
           ,versionString minVersion
           ,"' is required by "
           ,stackProgName
           ," (you have '"
           ,versionString haveVersion
           ,"')."]
  show (DockerVersionProhibitedException prohibitedVersions haveVersion) =
    concat ["These Docker versions are incompatible with "
           ,stackProgName
           ," (you have '"
           ,versionString haveVersion
           ,"'): "
           ,intercalate ", " (map versionString prohibitedVersions)
           ,"."]
  show (BadDockerVersionException requiredRange haveVersion) =
    concat ["The version of 'docker' you are using ("
           ,show haveVersion
           ,") is outside the required\n"
           ,"version range specified in stack.yaml ("
           ,T.unpack (versionRangeText requiredRange)
           ,")."]
  show InvalidVersionOutputException =
    "Cannot get Docker version (invalid 'docker --version' output)."
  show (HostStackTooOldException minVersion (Just hostVersion)) =
    concat ["The host's version of '"
           ,stackProgName
           ,"' is too old for this Docker image.\nVersion "
           ,versionString minVersion
           ," is required; you have "
           ,versionString hostVersion
           ,"."]
  show (HostStackTooOldException minVersion Nothing) =
    concat ["The host's version of '"
           ,stackProgName
           ,"' is too old.\nVersion "
           ,versionString minVersion
           ," is required."]
  show (ContainerStackTooOldException requiredVersion containerVersion) =
    concat ["The Docker container's version of '"
           ,stackProgName
           ,"' is too old.\nVersion "
           ,versionString requiredVersion
           ," is required; the container has "
           ,versionString containerVersion
           ,"."]
  show CannotDetermineProjectRootException =
    "Cannot determine project root directory for Docker sandbox."
  show DockerNotInstalledException =
    "Cannot find 'docker' in PATH.  Is Docker installed?"
  show UnsupportedStackExeHostPlatformException = concat
    [ "Using host's "
    , stackProgName
    , " executable in Docker container is only supported on "
    , display dockerContainerPlatform
    , " platform" ]

-- | Function to get command and arguments to run in Docker container
type GetCmdArgs env m
   = M env m
  => DockerOpts
  -> EnvOverride
  -> Inspect
  -> Bool
  -> m (FilePath,[String],[(String,String)],[Mount])


type M env m = (MonadIO m,MonadReader env m,MonadLogger m,MonadBaseControl IO m,MonadCatch m
               ,HasConfig env,HasTerminal env,HasReExec env,HasHttpManager env,MonadMask m)
