{-# LANGUAGE CPP, NamedFieldPuns, RankNTypes, RecordWildCards, TemplateHaskell, TupleSections #-}

--EKB FIXME: some way to sync `stack` between host and container
--EKB FIXME: make this work from Windows
--EKB FIXME: get this all using proper logging infrastructure
--EKB FIXME: throw exceptions instead of using `error`
--EKB FIXME: include build plan file in Docker image so that it does not need to be downloaded on 1st use

-- | Run commands in Docker containers
module Stack.Docker
  --EKB FIXME: trim the exports, remove unused functions, clarify remaining names.
  (checkHostStackageDockerVersion
  ,checkVersions
  ,cleanup
  ,CleanupOpts(..)
  ,CleanupAction(..)
  ,dockerCmdName
  ,dockerOptsParser
  ,dockerOptsFromMonoid
  ,dockerPullCmdName
  ,execProcessAndExit
  ,getInContainer
  ,preventInContainer
  ,pull
  ,rerunCmdWithOptionalContainer
  ,rerunCmdWithRequiredContainer
  ,rerunWithOptionalContainer
  ,reset
  ,runContainerAndExit
  ,runInContainerAndExit
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Writer (execWriter,runWriter,tell)
import           Data.Aeson (FromJSON(..),(.:),(.:?),(.!=),eitherDecode)
import           Data.ByteString.Builder (stringUtf8,charUtf8,toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isSpace,toUpper,isAscii)
import           Data.List (dropWhileEnd,find,intersperse,isPrefixOf,isInfixOf,foldl',sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time (UTCTime,LocalTime(..),diffDays,utcToLocalTime,getZonedTime,ZonedTime(..))
import           Development.Shake hiding (doesDirectoryExist)
import           Options.Applicative.Builder.Extra (maybeBoolFlags)
import           Options.Applicative (Parser,str,option,help,auto,metavar,long,value)
import           Path
import           Path.IO (getWorkingDir,listDirectory)
import           Paths_stack (version)
import           Stack.Constants (projectDockerSandboxDir,stackDotYaml,stackRootEnvVar)
import           Stack.Types
import           Stack.Docker.GlobalDB
import           System.Directory (createDirectoryIfMissing,removeDirectoryRecursive,removeFile)
import           System.Directory (doesDirectoryExist)
import           System.Environment (lookupEnv,unsetEnv,getProgName,getArgs)
import           System.Exit (ExitCode(ExitSuccess,ExitFailure),exitWith)
import           System.FilePath (takeBaseName)
import           System.IO (hPutStrLn,stderr,stdin,stdout,hIsTerminalDevice)
import           System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as Proc
import           System.Process.PagerEditor (editByteString)
import           Text.Printf (printf)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals (installHandler,sigTERM,Handler(Catch))
#endif

-- | If Docker is enabled, re-runs the currently running OS command in a Docker container.
-- Otherwise, runs the inner action.
rerunWithOptionalContainer :: Config -> Maybe (Path Abs Dir) -> IO () -> IO ()
rerunWithOptionalContainer config mprojectRoot inner =
  rerunCmdWithOptionalContainer config
                                mprojectRoot
                                ((,) <$> (takeBaseName <$> getProgName) <*> getArgs)
                                inner

-- | If Docker is enabled, re-runs the OS command returned by the second argument in a
-- Docker container.  Otherwise, runs the inner action.
rerunCmdWithOptionalContainer :: Config -> Maybe (Path Abs Dir) -> IO (FilePath, [String]) -> IO () -> IO ()
rerunCmdWithOptionalContainer config mprojectRoot getCmdArgs inner =
  do inContainer <- getInContainer
     if inContainer || not (dockerEnable (configDocker config))
        then inner
        else do (cmd_,args) <- getCmdArgs
                runContainerAndExit config mprojectRoot cmd_ args [] (return ())

-- | If Docker is enabled, re-runs the OS command returned by the second argument in a
-- Docker container.  Otherwise, runs the inner action.
rerunCmdWithRequiredContainer :: Config -> Maybe (Path Abs Dir) -> IO (FilePath, [String]) -> IO ()
rerunCmdWithRequiredContainer config mprojectRoot getCmdArgs =
  do when (not (dockerEnable (configDocker config)))
          (error (concat ["Docker must be enabled in your "
                         ,toFilePath stackDotYaml
                         ," to use this command."]))
     (cmd_,args) <- getCmdArgs
     runContainerAndExit config mprojectRoot cmd_ args [] (return ())

-- | Error if running in a container.
preventInContainer :: IO () -> IO ()
preventInContainer inner =
  do inContainer <- getInContainer
     if inContainer
        then error (concat ["This command must be run on host OS (not in a Docker container)."])
        else inner

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: IO Bool
getInContainer =
  do maybeEnvVar <- lookupEnv sandboxIDEnvVar
     case maybeEnvVar of
       Nothing -> return False
       Just _ -> return True

-- | Run a command in a new Docker container, then exit the process.
runContainerAndExit :: Config
                    -> Maybe (Path Abs Dir)
                    -> FilePath
                    -> [String]
                    -> [(String,String)]
                    -> IO ()
                    -> IO ()
runContainerAndExit config mprojectRoot cmnd args envVars successPostAction =
  runAction (runContainerAndExitAction config mprojectRoot cmnd args envVars successPostAction)

-- | Shake action to run a command in a new Docker container.
runContainerAndExitAction :: Config
                          -> Maybe (Path Abs Dir)
                          -> FilePath
                          -> [String]
                          -> [(String,String)]
                          -> IO ()
                          -> Action ()
runContainerAndExitAction config
                          mprojectRoot
                          cmnd
                          args
                          envVars
                          successPostAction =
  do checkDockerVersion
     (Stdout uidOut) <- cmd "id -u"
     (Stdout gidOut) <- cmd "id -g"
     (dockerHost,dockerCertPath,dockerTlsVerify,isStdinTerminal,isStdoutTerminal,isStderrTerminal
       ,pwd) <-
       liftIO ((,,,,,,) <$>
               lookupEnv "DOCKER_HOST" <*>
               lookupEnv "DOCKER_CERT_PATH" <*>
               lookupEnv "DOCKER_TLS_VERIFY" <*>
               hIsTerminalDevice stdin <*>
               hIsTerminalDevice stdout <*>
               hIsTerminalDevice stderr <*>
               getWorkingDir)
     when (maybe False (isPrefixOf "tcp://") dockerHost &&
           maybe False (isInfixOf "boot2docker") dockerCertPath)
          (liftIO (hPutStrLn stderr
             ("WARNING: using boot2docker is NOT supported, and not likely to perform well.")))
     let image = dockerImage docker
     maybeImageInfo <- inspect image
     imageInfo <- case maybeImageInfo of
       Just ii -> return ii
       Nothing
         | dockerAutoPull docker ->
             do pullImage docker image
                mii2 <- inspect image
                case mii2 of
                  Just ii2 -> return ii2
                  Nothing -> error ("'docker inspect' failed for image after pull: " ++ image)
         | otherwise ->
             do progName <- liftIO getProgName
                error ("The Docker image referenced by " ++ toFilePath stackDotYaml ++
                       " has not\nbeen downloaded:\n    " ++ image ++ "\n\n" ++
                       --EKB FIXME probably doesn't make sense to use progName here, since `stack docker pull` is still the right command even if something else is using the lib (also check other uses of progName)
                       "Run '" ++ unwords [takeBaseName progName, dockerCmdName, dockerPullCmdName] ++
                       "' to download it, then try again.")
     let (uid,gid) = (dropWhileEnd isSpace uidOut, dropWhileEnd isSpace gidOut)
         imageEnvVars = map (break (== '=')) (icEnv (iiConfig imageInfo))
         (sandboxID,oldImage) =
           case lookupImageEnv sandboxIDEnvVar imageEnvVars of
             Just x -> (x,False)
             Nothing ->
               --TODO: remove this and oldImage after lts-1.x images no longer in use
               let sandboxName = maybe "default" id (lookupImageEnv "SANDBOX_NAME" imageEnvVars)
                   maybeImageCabalRemoteRepoName = lookupImageEnv "CABAL_REMOTE_REPO_NAME" imageEnvVars
                   maybeImageStackageSlug = lookupImageEnv "STACKAGE_SLUG" imageEnvVars
                   maybeImageStackageDate = lookupImageEnv "STACKAGE_DATE" imageEnvVars
               in (case (maybeImageStackageSlug,maybeImageStackageDate) of
                     (Just stackageSlug,_) -> sandboxName ++ "_" ++ stackageSlug
                     (_,Just stackageDate) -> sandboxName ++ "_" ++ stackageDate
                     _ -> sandboxName ++ maybe "" ("_" ++) maybeImageCabalRemoteRepoName
                  ,True)
     sandboxIDDir <- liftIO (parseRelDir (sandboxID ++ "/"))
     let stackRoot = configStackRoot config
         sandboxDir = projectDockerSandboxDir projectRoot
         sandboxSandboxDir = sandboxDir </> $(mkRelDir ".sandbox/") </> sandboxIDDir
         sandboxHomeDir = sandboxDir </> homeDirName
         sandboxRepoDir = sandboxDir </> sandboxIDDir
         sandboxSubdirs = map (\d -> sandboxRepoDir </> d)
                              sandboxedHomeSubdirectories
         isTerm = isStdinTerminal && isStdoutTerminal && isStderrTerminal
         execDockerProcess =
           do mapM_ (createDirectoryIfMissing True)
                    (concat [[toFilePath sandboxHomeDir
                             ,toFilePath sandboxSandboxDir] ++
                             map toFilePath sandboxSubdirs])
              execProcessAndExit "docker"
                (concat
                  [["run"
                   ,"--net=host"
                   ,"-e",stackRootEnvVar ++ "=" ++ toFilePath stackRoot
                   ,"-e","WORK_UID=" ++ uid
                   ,"-e","WORK_GID=" ++ gid
                   ,"-e","WORK_WD=" ++ toFilePath pwd
                   ,"-e","WORK_HOME=" ++ toFilePath sandboxRepoDir
                   ,"-e","WORK_ROOT=" ++ toFilePath projectRoot
                   ,"-e",hostVersionEnvVar ++ "=" ++ versionString stackVersion
                   ,"-e",requireVersionEnvVar ++ "=" ++ versionString requireContainerVersion
                   ,"-v",toFilePath stackRoot ++ ":" ++ toFilePath stackRoot
                   ,"-v",toFilePath projectRoot ++ ":" ++ toFilePath projectRoot
                   ,"-v",toFilePath sandboxSandboxDir ++ ":" ++ toFilePath sandboxDir
                   ,"-v",toFilePath sandboxHomeDir ++ ":" ++ toFilePath sandboxRepoDir]
                  ,if oldImage
                     then ["-e",sandboxIDEnvVar ++ "=" ++ sandboxID
                          ,"--entrypoint=/root/entrypoint.sh"]
                     else []
                  ,case (dockerPassHost docker,dockerHost) of
                     (True,Just x@('u':'n':'i':'x':':':'/':'/':s)) -> ["-e","DOCKER_HOST=" ++ x
                                                                      ,"-v",s ++ ":" ++ s]
                     (True,Just x) -> ["-e","DOCKER_HOST=" ++ x]
                     (True,Nothing) -> ["-v","/var/run/docker.sock:/var/run/docker.sock"]
                     (False,_) -> []
                  ,case (dockerPassHost docker,dockerCertPath) of
                     (True,Just x) -> ["-e","DOCKER_CERT_PATH=" ++ x
                                      ,"-v",x ++ ":" ++ x]
                     _ -> []
                  ,case (dockerPassHost docker,dockerTlsVerify) of
                     (True,Just x )-> ["-e","DOCKER_TLS_VERIFY=" ++ x]
                     _ -> []
                  ,concatMap sandboxSubdirArg sandboxSubdirs
                  ,concatMap mountArg (dockerMount docker)
                  ,case dockerContainerName docker of
                     Just name -> ["--name=" ++ name]
                     Nothing -> []
                  ,if dockerDetach docker
                      then ["-d"]
                      else concat [["--rm" | not (dockerPersist docker)]
                                  ,["-t" | isTerm]
                                  ,["-i" | isTerm]]
                  ,dockerRunArgs docker
                  ,[image]
                  ,map (\(k,v) -> k ++ "=" ++ v) envVars
                  ,[cmnd]
                  ,args])
                successPostAction
     liftIO (do updateDockerImageLastUsed config
                                          (iiId imageInfo)
                                          (toFilePath projectRoot)
                execDockerProcess)

  where
    lookupImageEnv :: String -> [(String,String)] -> Maybe String
    lookupImageEnv name vars =
      case lookup name vars of
        Just ('=':val) -> Just val
        _ -> Nothing

    mountArg :: Mount -> [String]
    mountArg (Mount host container) = ["-v",host ++ ":" ++ container]

    sandboxSubdirArg :: Path Abs Dir -> [String]
    sandboxSubdirArg subdir = ["-v",toFilePath subdir++ ":" ++ toFilePath subdir]

    projectRoot :: Path Abs Dir
    projectRoot = fromMaybeProjectRoot mprojectRoot

    docker :: DockerOpts
    docker = configDocker config

-- | Clean-up old docker images and containers.
cleanup :: Config -> CleanupOpts -> IO ()
cleanup config opts = runAction (cleanupAction config opts)

-- | Cleanup action
cleanupAction :: Config -> CleanupOpts -> Action ()
cleanupAction config opts =
  do checkDockerVersion
     progName <- liftIO (takeBaseName <$> getProgName)
     (Stdout imagesOut) <- cmd "docker images --no-trunc -f dangling=false"
     (Stdout danglingImagesOut) <- cmd "docker images --no-trunc -f dangling=true"
     (Stdout runningContainersOut) <- cmd "docker ps -a --no-trunc -f status=running"
     (Stdout restartingContainersOut) <- cmd "docker ps -a --no-trunc -f status=restarting"
     (Stdout exitedContainersOut) <- cmd "docker ps -a --no-trunc -f status=exited"
     (Stdout pausedContainersOut) <- cmd "docker ps -a --no-trunc -f status=paused"
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
     plan <- liftIO
       (do imagesLastUsed <- getDockerImagesLastUsed config
           curTime <- getZonedTime
           let planWriter = buildPlan progName
                                      curTime
                                      imagesLastUsed
                                      imageRepos
                                      danglingImageHashes
                                      stoppedContainers
                                      runningContainers
                                      inspectMap
               plan = toLazyByteString (execWriter planWriter)
           case dcAction opts of
                                    --EKB FIXME: use constants to construct filename
             CleanupInteractive -> editByteString "stack-docker-cleanup-plan" plan
             CleanupImmediate -> return plan
             CleanupDryRun -> do LBS.hPut stdout plan
                                 return LBS.empty)
     withVerbosity
       Loud
       (mapM_ performPlanLine (reverse (filter filterPlanLine (lines (LBS.unpack plan)))))
     (Stdout allImageHashesOut) <- cmd "docker images -aq --no-trunc"
     liftIO (pruneDockerImagesLastUsed config (lines allImageHashesOut))
  where
    filterPlanLine line =
      case line of
        c:_ | isSpace c -> False
        _ -> True
    performPlanLine line =
      do (Exit _) <- case filter (not . null) (words (takeWhile (/= '#') line)) of
           [] -> return (Exit ExitSuccess)
           (c:_):t:v:_ | toUpper c == 'R' && t == imageStr -> cmd "docker rmi" [v]
                       | toUpper c == 'R' && t == containerStr -> cmd "docker rm -f " [v]
           _ -> error ("Invalid line in cleanup commands: '" ++ line ++ "'")
         return ()
    parseImagesOut = Map.fromListWith (++) . map parseImageRepo . drop 1 . lines
      where parseImageRepo :: String -> (String, [String])
            parseImageRepo line =
              case words line of
                repo:tag:hash:_
                  | repo == "<none>" -> (hash,[])
                  | tag == "<none>" -> (hash,[repo])
                  | otherwise -> (hash,[repo ++ ":" ++ tag])
                _ -> error ("Invalid 'docker images' output line: " ++ line)
    parseContainersOut = map parseContainer . drop 1 . lines
      where parseContainer line =
              case words line of
                hash:image:rest -> (hash,(image,last rest))
                _ -> error ("Invalid 'docker ps' output line: " ++ line)
    buildPlan progName
              curTime
              imagesLastUsed
              imageRepos
              danglingImageHashes
              stoppedContainers
              runningContainers
              inspectMap =
      do case dcAction opts of
           CleanupInteractive ->
             do buildStrLn
                  (unlines
                     ["# STACK DOCKER CLEANUP PLAN"
                     ,"#"
                     ,"# When you leave the editor, the lines in this plan will be processed."
                     ,"#"
                     ,"# Lines that begin with 'R' denote an image or container that will be."
                     ,"# removed.  You may change the first character to/from 'R' to remove/keep"
                     ,"# and image or container that would otherwise be kept/removed."
                     ,"#"
                     ,"# To cancel the cleanup, delete all lines in this file."
                     ,"#"
                     ,"# By default, the following images/containers will be removed:"
                     ,"#"])
                buildDefault dcRemoveKnownImagesLastUsedDaysAgo "Known images last used"
                buildDefault dcRemoveUnknownImagesCreatedDaysAgo "Unknown images created"
                buildDefault dcRemoveDanglingImagesCreatedDaysAgo "Dangling images created"
                buildDefault dcRemoveStoppedContainersCreatedDaysAgo "Stopped containers created"
                buildDefault dcRemoveRunningContainersCreatedDaysAgo "Running containers created"
                buildStrLn
                  (unlines
                     ["#"
                     ,"# The default plan can be adjusted using command-line arguments."
                      --EKB FIXME: `docker cleanup` should come from shared constants.
                     ,"# Run '" ++ takeBaseName progName ++ " docker cleanup --help' for details."
                     ,"#"])
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
             if or anyWrote
               then do buildSectionHead sectionHead
                       tell b
               else return ()
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
          do let display = (name ++ " (image: " ++ image ++ ")")
             buildTime containerStr removeAge created display
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
        buildTime t removeAge time display =
          case removeAge of
            Just d | daysAgo time >= d -> buildStrLn ("R " ++ t ++ " " ++ display)
            _ -> buildKeep t display
        buildKeep t d = buildStrLn ("  " ++ t ++ " " ++ d)
        buildKeepImage = buildKeep imageStr
        buildSectionHead s = buildStrLn ("\n#\n# " ++ s ++ "\n#\n")
        buildInfo = buildStrLn . ("        # " ++)
        buildStrLn l = do buildStr l
                          tell (charUtf8 '\n')
        buildStr = tell . stringUtf8

    imageStr = "image"
    containerStr = "container"

-- | Inspect multiple Docker images and/or containers.
inspects :: [String] -> Action (Map String Inspect)
inspects [] = return Map.empty
inspects images =
  do (Exit inspectExitCode, Stdout inspectOut) <- cmd "docker inspect" images
     case inspectExitCode of
       ExitSuccess ->
         -- filtering with 'isAscii' to workaround @docker inspect@ output containing invalid UTF-8
         case eitherDecode (LBS.pack (filter isAscii inspectOut)) of
           Left msg -> error ("Invalid 'docker inspect' output: " ++ msg ++ ".")
           Right results -> return (Map.fromList (map (\r -> (iiId r,r)) results))
       ExitFailure _ -> return Map.empty

-- | Inspect Docker image or container.
inspect :: String -> Action (Maybe Inspect)
inspect image =
  do results <- inspects [image]
     case Map.toList results of
       [] -> return Nothing
       [(_,i)] -> return (Just i)
       _ -> error ("Invalid 'docker inspect' output: expect a single result.")

-- | Pull latest version of configured Docker image from registry.
pull :: DockerOpts -> IO ()
pull docker =
  runAction (do checkDockerVersion
                pullImage docker (dockerImage docker))

-- | Pull Docker image from registry.
pullImage :: DockerOpts -> String -> Action ()
pullImage docker image =
  liftIO (do hPutStrLn stderr ("\nPulling from registry: " ++ image)
             when (dockerRegistryLogin docker)
                  (do hPutStrLn stderr "You may need to log in."
                      Proc.callProcess
                        "docker"
                        (concat
                           [["login"]
                           ,maybe [] (\u -> ["--username=" ++ u]) (dockerRegistryUsername docker)
                           ,maybe [] (\p -> ["--password=" ++ p]) (dockerRegistryPassword docker)
                           ,[takeWhile (/= '/') image]]))
             onException
               (Proc.callProcess "docker" ["pull", image])
               (error (concat ["Could not pull Docker image"
                              ,"\nIf the tag was not found, there may not be an image on the registry for your"
                              ,"\nresolver's LTS version in "
                              ,toFilePath stackDotYaml
                              ,"."])))

-- | Run a Shake action.
runAction :: Action () -> IO ()
runAction inner =
  --EKB FIXME construct "stack-docker" from constants
  withSystemTempDirectory
    "stack-docker."
    (\tmp -> do shake shakeOptions{shakeVerbosity = Quiet
                                  ,shakeFiles = tmp}
                      (action inner))

-- | Check docker version (throws exception if incorrect)
checkDockerVersion :: Action ()
checkDockerVersion =
  do (Stdout dockerVersionOut) <- cmd "docker --version"
       `actionOnException` putStrLn "\nCannot get Docker version.  IS DOCKER INSTALLED?\n"
     case words dockerVersionOut of
       (_:_:v:_) ->
         case parseVersionFromString (dropWhileEnd (== ',') v) of
           Just v'
             | v' < minimumDockerVersion ->
               error (concat ["Minimum docker version '"
                             ,versionString minimumDockerVersion
                             ,"' is required (you have '"
                             ,versionString v'
                             ,"')."])
             | v' `elem` prohibitedDockerVersions ->
               error (concat ["These Docker versions are prohibited (you have '"
                             ,versionString v'
                             ,"'): "
                             ,concat (intersperse ", " (map versionString prohibitedDockerVersions))
                             ,"."])
             | otherwise ->
               return ()
           _ -> error "Cannot get Docker version (invalid 'docker --version' output)."
       _ -> error "Cannot get Docker version (invalid 'docker --version' output)."
  where minimumDockerVersion = $(mkVersion "1.3.0")
        prohibitedDockerVersions = [$(mkVersion "1.2.0")]

-- | Run a command when we're already inside a Docker container.
runInContainerAndExit :: FilePath -> [String] -> IO ()
runInContainerAndExit cmnd args =
  do unsetEnv requireVersionEnvVar
     execProcessAndExit cmnd args (return ())

-- | Run a process, then exit with the same exit code.
execProcessAndExit :: FilePath -> [String] -> IO () -> IO ()
execProcessAndExit cmnd args successPostAction =
  do (_, _, _, h) <- Proc.createProcess (Proc.proc cmnd args){Proc.delegate_ctlc = True}
#ifndef mingw32_HOST_OS
     _ <- installHandler sigTERM (Catch (Proc.terminateProcess h)) Nothing
#endif
     exitCode <- Proc.waitForProcess h
     when (exitCode == ExitSuccess)
          successPostAction
     exitWith exitCode

-- | Remove the project's Docker sandbox.
reset :: Maybe (Path Abs Dir) -> Bool -> IO ()
reset maybeProjectRoot keepHome =
  removeDirectoryContents
    (projectDockerSandboxDir projectRoot)
    [homeDirName | keepHome]
    []
  where projectRoot = fromMaybeProjectRoot maybeProjectRoot

-- | Remove the contents of a directory, without removing the directory itself.
-- This is used instead of 'FS.removeTree' to clear bind-mounted directories, since
-- removing the root of the bind-mount won't work.
removeDirectoryContents :: Path Abs Dir -- ^ Directory to remove contents of
                        -> [Path Rel Dir] -- ^ Top-level directory names to exclude from removal
                        -> [Path Rel File] -- ^ Top-level file names to exclude from removal
                        -> IO ()
removeDirectoryContents path excludeDirs excludeFiles =
  do isRootDir <- doesDirectoryExist (toFilePath path)
     when isRootDir
          (do (lsd,lsf) <- listDirectory path
              forM_ lsd
                    (\d -> unless (dirname d `elem` excludeDirs)
                                  (removeDirectoryRecursive (toFilePath d)))
              forM_ lsf
                    (\f -> unless (filename f `elem` excludeFiles)
                                  (removeFile (toFilePath f))))

-- | Subdirectories of the home directory to sandbox between GHC/Stackage versions.
sandboxedHomeSubdirectories :: [Path Rel Dir]
sandboxedHomeSubdirectories =
  [$(mkRelDir ".ghc/")
  ,$(mkRelDir ".cabal/")
  ,$(mkRelDir ".ghcjs/")]

-- | Name of home directory within @.docker-sandbox@.
homeDirName :: Path Rel Dir
homeDirName = $(mkRelDir ".home/")

-- | Check host 'stack' version
checkHostStackageDockerVersion :: Version -> IO ()
checkHostStackageDockerVersion minVersion =
  do maybeHostVer <- lookupEnv hostVersionEnvVar
     progName <- takeBaseName <$> getProgName
     case parseVersionFromString =<< maybeHostVer of
       Just hostVer
         | hostVer < minVersion ->
             error ("Your host's version of '" ++ progName ++ "' is too old for this Docker image.\nVersion " ++
                    versionString minVersion ++
                    " is required; you have " ++
                    versionString hostVer ++
                    ".\n")
         | otherwise -> return ()
       Nothing ->
          do inContainer <- getInContainer
             if inContainer
                then error ("Your host's version of '" ++ progName ++ "' is too old.\nVersion " ++
                            versionString minVersion ++ " is required.")
                else return ()


-- | Check host and container 'stack' versions are compatible.
checkVersions :: IO ()
checkVersions =
  do inContainer <- getInContainer
     when inContainer
       (do checkHostStackageDockerVersion requireHostVersion
           maybeReqVer <- lookupEnv requireVersionEnvVar
           progName <- takeBaseName <$> getProgName
           case parseVersionFromString =<< maybeReqVer of
             Just reqVer
               | stackVersion < reqVer ->
                   error ("This Docker image's version of '" ++
                          progName ++
                          "' is too old.\nVersion " ++
                          versionString reqVer ++
                          " is required; you have " ++
                          versionString stackVersion ++
                          ".\nPlease update your '" ++
                          toFilePath stackDotYaml ++
                          "' to use a newer image.")
               | otherwise -> return ()
             _ -> return ())

-- | Options parser configuration for Docker.
dockerOptsParser :: Parser DockerOptsMonoid
dockerOptsParser =
    DockerOptsMonoid
    <$> maybeBoolFlags dockerCmdName
                       "using a Docker container"
    <*> ((Just . DockerMonoidRepo) <$> option str (long (dockerOptName dockerRepoArgName) <>
                                                   metavar "NAME" <>
                                                   help "Docker repository name") <|>
         (Just . DockerMonoidImage) <$> option str (long (dockerOptName dockerImageArgName) <>
                                                    metavar "IMAGE" <>
                                                    help "Exact Docker image ID (overrides docker-repo)") <|>
         pure Nothing)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> maybeBoolFlags (dockerOptName dockerAutoPullArgName)
                       "automatic pulling latest version of image"
    <*> maybeBoolFlags (dockerOptName dockerDetachArgName)
                        "running a detached Docker container"
    <*> maybeBoolFlags (dockerOptName dockerPersistArgName)
                       "not deleting container after it exits"
    <*> maybeStrOption (long (dockerOptName dockerContainerNameArgName) <>
                        metavar "NAME" <>
                        help "Docker container name")
    <*> wordsStrOption (long (dockerOptName dockerRunArgsArgName) <>
                        value [] <>
                        metavar "'ARG1 [ARG2 ...]'" <>
                        help ("Additional arguments to pass to 'docker run'"))
    <*> many (option auto (long (dockerOptName dockerMountArgName) <>
                           metavar "(PATH | HOST-PATH:CONTAINER-PATH)" <>
                           help ("Mount volumes from host in container " ++
                                 "(may specify mutliple times)")))
    <*> maybeBoolFlags (dockerOptName dockerPassHostArgName)
                       "passing Docker daemon connection information into container"
  where
    dockerOptName optName = dockerCmdName ++ "-" ++ T.unpack optName
    maybeStrOption = optional . option str
    wordsStrOption = option (fmap words str)

-- | Interprets DockerOptsMonoid options.
dockerOptsFromMonoid :: Maybe Project -> DockerOptsMonoid -> DockerOpts
dockerOptsFromMonoid mproject DockerOptsMonoid{..} = DockerOpts
  {dockerEnable = fromMaybe False dockerMonoidEnable
  ,dockerImage =
     let defaultTag =
           case mproject of
             Nothing -> ""
             Just proj ->
               case projectResolver proj of
                 ResolverSnapshot n@(LTS _ _) -> ":" ++  (T.unpack (renderSnapName n))
                 _ -> error (concat ["Resolver not supported for Docker images:\n    "
                                     ,show (projectResolver proj)
                                     ,"\nUse an LTS resolver, or set the '"
                                     ,T.unpack dockerImageArgName
                                     ,"' explicitly, in "
                                     ,toFilePath stackDotYaml
                                     ,"."])
     in case dockerMonoidRepoOrImage of
       Nothing -> "fpco/dev" ++ defaultTag
       Just (DockerMonoidImage image) -> image
       Just (DockerMonoidRepo repo) ->
         case find (`elem` ":@") repo of
           Just _ -> -- Repo already specified a tag or digest, so don't append default
                     repo
           Nothing -> repo ++ defaultTag
  ,dockerRegistryLogin = fromMaybe (isJust (emptyToNothing dockerMonoidRegistryUsername))
                                   dockerMonoidRegistryLogin
  ,dockerRegistryUsername = emptyToNothing dockerMonoidRegistryUsername
  ,dockerRegistryPassword = emptyToNothing dockerMonoidRegistryPassword
  ,dockerAutoPull = fromMaybe False dockerMonoidAutoPull
  ,dockerDetach = fromMaybe False dockerMonoidDetach
  ,dockerPersist = fromMaybe False dockerMonoidPersist
  ,dockerContainerName = emptyToNothing dockerMonoidContainerName
  ,dockerRunArgs = dockerMonoidRunArgs
  ,dockerMount = dockerMonoidMount
  ,dockerPassHost = fromMaybe False dockerMonoidPassHost
  }
  where emptyToNothing Nothing = Nothing
        emptyToNothing (Just s) | null s = Nothing
                                | otherwise = Just s

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot =
  fromMaybe (error "Cannot determine project root directory for Docker sandbox.")

-- | Environment variable to the host's stack version.
hostVersionEnvVar :: String
hostVersionEnvVar = "STACK_DOCKER_HOST_VERSION"

-- | Environment variable to pass required container stack version.
requireVersionEnvVar :: String
requireVersionEnvVar = "STACK_DOCKER_REQUIRE_VERSION"

-- | Environment variable that contains the sandbox ID.
sandboxIDEnvVar :: String
sandboxIDEnvVar = "DOCKER_SANDBOX_ID"

-- | Command-line argument for "docker"
dockerCmdName :: String
dockerCmdName = "docker"

-- | Command-line argument for @docker pull@.
dockerPullCmdName :: String
dockerPullCmdName = "pull"

-- | Version of 'stack' required to be installed in container.
requireContainerVersion :: Version
requireContainerVersion = $(mkVersion "0.0.0")

-- | Version of 'stack' required to be installed on the host.
requireHostVersion :: Version
requireHostVersion = $(mkVersion "0.0.0")

-- | Stack cabal package version
stackVersion :: Version
stackVersion = fromCabalVersion version

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
       (ImageConfig <$> o .:? T.pack "Env" .!= [])
