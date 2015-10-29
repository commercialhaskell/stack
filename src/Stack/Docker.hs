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
  ,execWithOptionalContainer
  ,preventInContainer
  ,pull
  ,reexecWithOptionalContainer
  ,reset
  ,reExecArgName
  ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadThrow,throwM,MonadCatch,MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logError,logInfo,logWarn)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Writer (execWriter,runWriter,tell)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson.Extended (FromJSON(..),(.:),(.:?),(.!=),eitherDecode)
import           Data.ByteString.Builder (stringUtf8,charUtf8,toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isSpace,toUpper,isAscii,isDigit)
import           Data.Conduit.List (sinkNull)
import           Data.List (dropWhileEnd,intercalate,isPrefixOf,isInfixOf,foldl',sortBy)
import           Data.List.Extra (trim)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime,LocalTime(..),diffDays,utcToLocalTime,getZonedTime,ZonedTime(..))
import           Data.Typeable (Typeable)
import           Data.Version (showVersion)
import           Distribution.System (Platform (Platform), Arch (X86_64), OS (Linux))
import           Distribution.Text (display)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.IO (getWorkingDir,listDirectory,createTree,removeFile,removeTree,dirExists)
import qualified Paths_stack as Meta
import           Prelude -- Fix redundant import warnings
import           Stack.Constants (projectDockerSandboxDir,stackProgName,stackRootEnvVar)
import           Stack.Docker.GlobalDB
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Setup (ensureDockerStackExe)
import           System.Directory (canonicalizePath, getModificationTime)
import           System.Environment (lookupEnv,getProgName, getArgs,getExecutablePath)
import           System.Exit (exitSuccess, exitWith)
import           System.FilePath (takeBaseName)
import           System.IO (stderr,stdin,stdout,hIsTerminalDevice)
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
    getCmdArgs envOverride imageInfo = do
        config <- asks getConfig
        args <-
            fmap
                (("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) :)
                (liftIO getArgs)
        case dockerStackExe (configDocker config) of
            Just DockerStackExeHost
              | configPlatform config == dockerContainerPlatform ->
                  fmap (cmdArgs args) (liftIO getExecutablePath)
              | otherwise -> throwM UnsupportedStackExeHostPlatformException
            Just DockerStackExeImage -> do
                progName <- liftIO getProgName
                return (takeBaseName progName, args, [], [])
            Just (DockerStackExePath path) ->
                fmap
                    (cmdArgs args)
                    (liftIO $ canonicalizePath (toFilePath path))
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
                      Just True -> return (cmdArgs args exePath)
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
                              then return (cmdArgs args exePath)
                              else exeDownload args
            Nothing -> exeDownload args
    exeDownload args =
        fmap
            (cmdArgs args . toFilePath)
            (ensureDockerStackExe dockerContainerPlatform)
    cmdArgs args exePath =
        let mountPath = concat ["/opt/host/bin/", takeBaseName exePath]
        in (mountPath, args, [], [Mount exePath mountPath])

-- | If Docker is enabled, re-runs the OS command returned by the second argument in a
-- Docker container.  Otherwise, runs the inner action.
--
-- This takes an optional release action just like `reexecWithOptionalContainer`.
execWithOptionalContainer
    :: M env m
    => Maybe (Path Abs Dir)
    -> (EnvOverride -> Inspect -> m (FilePath,[String],[(String,String)],[Mount]))
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
                    => (EnvOverride -> Inspect -> m (FilePath,[String],[(String,String)],[Mount]))
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
     checkDockerVersion envOverride
     (dockerHost,dockerCertPath,bamboo,jenkins) <-
       liftIO ((,,,) <$> lookupEnv "DOCKER_HOST"
                     <*> lookupEnv "DOCKER_CERT_PATH"
                     <*> lookupEnv "bamboo_buildKey"
                     <*> lookupEnv "JENKINS_HOME")
     let isRemoteDocker = maybe False (isPrefixOf "tcp://") dockerHost
     userEnvVars <-
         if fromMaybe (not isRemoteDocker) (dockerSetUser docker)
             then do
                 uidOut <- readProcessStdout Nothing envOverride "id" ["-u"]
                 gidOut <- readProcessStdout Nothing envOverride "id" ["-g"]
                 return
                     [ "-e","WORK_UID=" ++ dropWhileEnd isSpace (decodeUtf8 uidOut)
                     , "-e","WORK_GID=" ++ dropWhileEnd isSpace (decodeUtf8 gidOut) ]
             else return []
     isStdoutTerminal <- asks getTerminal
     (isStdinTerminal,isStderrTerminal) <-
       liftIO ((,) <$> hIsTerminalDevice stdin
                   <*> hIsTerminalDevice stderr)
     pwd <- getWorkingDir
     when (isRemoteDocker &&
           maybe False (isInfixOf "boot2docker") dockerCertPath)
          ($logWarn "Warning: Using boot2docker is NOT supported, and not likely to perform well.")
     let image = dockerImage docker
     maybeImageInfo <- inspect envOverride image
     imageInfo <- case maybeImageInfo of
       Just ii -> return ii
       Nothing
         | dockerAutoPull docker ->
             do pullImage envOverride docker image
                mii2 <- inspect envOverride image
                case mii2 of
                  Just ii2 -> return ii2
                  Nothing -> throwM (InspectFailedException image)
         | otherwise -> throwM (NotPulledException image)
     (cmnd,args,envVars,extraMount) <- getCmdArgs envOverride imageInfo
     let imageEnvVars = map (break (== '=')) (icEnv (iiConfig imageInfo))
         sandboxID = fromMaybe "default" (lookupImageEnv sandboxIDEnvVar imageEnvVars)
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
     liftIO
       (do updateDockerImageLastUsed config
                                     (iiId imageInfo)
                                     (toFilePath projectRoot)

           mapM_ createTree
                 (concat [[sandboxHomeDir, sandboxSandboxDir, stackRoot] ++
                          sandboxSubdirs]))
     containerID <- (trim . decodeUtf8) <$> readDockerProcess
       envOverride
       (concat
         [["create"
          ,"--net=host"
          ,"-e",inContainerEnvVar ++ "=1"
          ,"-e",stackRootEnvVar ++ "=" ++ toFilePathNoTrailingSep stackRoot
          ,"-e","WORK_WD=" ++ toFilePathNoTrailingSep pwd
          ,"-e","WORK_HOME=" ++ toFilePathNoTrailingSep sandboxRepoDir
          ,"-e","WORK_ROOT=" ++ toFilePathNoTrailingSep projectRoot
          ,"-v",toFilePathNoTrailingSep stackRoot ++ ":" ++ toFilePathNoTrailingSep stackRoot
          ,"-v",toFilePathNoTrailingSep projectRoot ++ ":" ++ toFilePathNoTrailingSep projectRoot
          ,"-v",toFilePathNoTrailingSep sandboxSandboxDir ++ ":" ++ toFilePathNoTrailingSep sandboxDir
          ,"-v",toFilePathNoTrailingSep sandboxHomeDir ++ ":" ++ toFilePathNoTrailingSep sandboxRepoDir
          ,"-v",toFilePathNoTrailingSep stackRoot ++ ":" ++
                toFilePathNoTrailingSep (sandboxRepoDir </> $(mkRelDir ("." ++ stackProgName ++ "/")))]
         ,userEnvVars
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
     oldHandlers <- forM (concat [[(sigINT,sigTERM) | not keepStdinOpen]
                                 ,[(sigTERM,sigTERM)]]) $ \(sigIn,sigOut) -> do
       let sigHandler = runInBase (readProcessNull Nothing envOverride "docker"
                                     ["kill","--signal=" ++ show sigOut,containerID])
       oldHandler <- liftIO $ installHandler sigIn (Catch sigHandler) Nothing
       return (sigIn, oldHandler)
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
     envOverride <- getEnvOverride (configPlatform config)
     checkDockerVersion envOverride
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
        sortCreated l =
          reverse (sortBy (\(_,_,a) (_,_,b) -> compare a b)
                          (catMaybes (map (\(h,r) -> fmap (\ii -> (h,r,iiCreated ii))
                                                          (Map.lookup h inspectMap))
                                          l)))
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
     checkDockerVersion envOverride
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
    => EnvOverride -> m ()
checkDockerVersion envOverride =
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
             | otherwise ->
               return ()
           _ -> throwM InvalidVersionOutputException
       _ -> throwM InvalidVersionOutputException
  where minimumDockerVersion = $(mkVersion "1.3.0")
        prohibitedDockerVersions = [$(mkVersion "1.2.0")]

-- | Remove the project's Docker sandbox.
reset :: (MonadIO m) => Maybe (Path Abs Dir) -> Bool -> m ()
reset maybeProjectRoot keepHome =
  liftIO (removeDirectoryContents
            (projectDockerSandboxDir projectRoot)
            [homeDirName | keepHome]
            [])
  where projectRoot = fromMaybeProjectRoot maybeProjectRoot

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
readDockerProcess envOverride args =
  readProcessStdout Nothing envOverride "docker" args

-- | Subdirectories of the home directory to sandbox between GHC/Stackage versions.
sandboxedHomeSubdirectories :: [Path Rel Dir]
sandboxedHomeSubdirectories =
  [$(mkRelDir ".ghc/")
  ,$(mkRelDir ".cabal/")
  ,$(mkRelDir ".ghcjs/")]

-- | Name of home directory within docker sandbox.
homeDirName :: Path Rel Dir
homeDirName = $(mkRelDir "_home/")

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
inContainerEnvVar = concat [map toUpper stackProgName,"_IN_CONTAINER"]

-- | Command-line argument for "docker"
dockerCmdName :: String
dockerCmdName = "docker"

-- | Command-line argument for @docker pull@.
dockerPullCmdName :: String
dockerPullCmdName = "pull"

-- | Command-line argument for @docker cleanup@.
dockerCleanupCmdName :: String
dockerCleanupCmdName = "cleanup"

-- | Command-line option for @--internal-re-exec@.
reExecArgName :: String
reExecArgName = "internal-re-exec-version"

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
  ,iiVirtualSize :: Maybe Integer }
  deriving (Show)

-- | Parse @docker inspect@ output.
instance FromJSON Inspect where
  parseJSON v =
    do o <- parseJSON v
       (Inspect <$> o .: T.pack "Config"
                <*> o .: T.pack "Created"
                <*> o .: T.pack "Id"
                <*> o .:? T.pack "VirtualSize")

-- | Parsed @Config@ section of @docker inspect@ output.
data ImageConfig = ImageConfig
  {icEnv :: [String]}
  deriving (Show)

-- | Parse @Config@ section of @docker inspect@ output.
instance FromJSON ImageConfig where
  parseJSON v =
    do o <- parseJSON v
       ImageConfig <$> o .:? T.pack "Env" .!= []

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
    concat ["Docker must be enabled in your configuration file to use this command."]
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
           ,"' is required (you have '"
           ,versionString haveVersion
           ,"')."]
  show (DockerVersionProhibitedException prohibitedVersions haveVersion) =
    concat ["These Docker versions are prohibited (you have '"
           ,versionString haveVersion
           ,"'): "
           ,intercalate ", " (map versionString prohibitedVersions)
           ,"."]
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

-- | Platform that Docker containers run
dockerContainerPlatform :: Platform
dockerContainerPlatform = Platform X86_64 Linux

type M env m = (MonadIO m,MonadReader env m,MonadLogger m,MonadBaseControl IO m,MonadCatch m
               ,HasConfig env,HasTerminal env,HasReExec env,HasHttpManager env,MonadMask m)
