{-# LANGUAGE NamedFieldPuns, TupleSections, RankNTypes #-}

-- | Run commands in Docker containers
module Stack.Docker
  (resetOnHost
  ,resetInContainer
  ,getInContainer
  ,runContainerAndExit
  ,runInContainerAndExit
  ,warnIfNoContainer
  ,warnNoContainer
  ,checkVersions
  ,Cleanup(..)
  ,CleanupAction(..)
  ,cleanup
  ,pull
  ,execProcessAndExit
  ,checkHostStackageDockerVersion
  ,dockerPullCmdName)
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Writer (execWriter,runWriter,tell)
import           Data.Aeson (FromJSON(..),(.:),(.:?),(.!=),eitherDecode)
import           Data.ByteString.Builder (stringUtf8,charUtf8,toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isSpace,isPunctuation,toUpper,isAscii)
import           Data.List (dropWhileEnd,intersperse,isPrefixOf,isInfixOf,foldl',sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Data.Time (UTCTime,LocalTime(..),diffDays,utcToLocalTime,getZonedTime,ZonedTime(..))
import           Data.Version (Version(..),parseVersion,showVersion)
import           Development.Shake
import qualified Filesystem as FS
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FP
import qualified Path as FL
import           Path.Find (findFileUp)
import           Paths_stack (version)
import           Stack.Build (shakeFilesPath)
import           Stack.Constants (configFileName)
import           Stack.Config
import           Stack.Docker.GlobalDB (updateDockerImageLastUsed,getDockerImagesLastUsed,pruneDockerImagesLastUsed)
import           System.Directory
import           System.Environment (lookupEnv,unsetEnv)
import           System.Exit (ExitCode(ExitSuccess,ExitFailure),exitWith)
import           System.IO (hPutStrLn,stderr,stdin,stdout,hIsTerminalDevice)
import qualified System.Process as Proc
import           System.Process.PagerEditor (editByteString)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Printf (printf)

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: IO Bool
getInContainer =
  do maybeEnvVar <- lookupEnv sandboxIDEnvVar
     case maybeEnvVar of
       Nothing -> return False
       Just _ -> return True

-- | Run a command in a new Docker container, then exit the process.
runContainerAndExit :: Docker
                    -> FilePath
                    -> [String]
                    -> [(String,String)]
                    -> IO ()
                    -> IO ()
runContainerAndExit config cmnd args envVars successPostAction =
  runAction (Just config)
            (runContainerAndExitAction config cmnd args envVars successPostAction)

-- | Shake action to run a command in a new Docker container.
runContainerAndExitAction :: Docker
                          -> FilePath
                          -> [String]
                          -> [(String,String)]
                          -> IO ()
                          -> Action ()
runContainerAndExitAction config
                          cmnd
                          args
                          envVars
                          successPostAction =
  do checkDockerVersion
     (Stdout uidOut) <- cmd "id -u"
     (Stdout gidOut) <- cmd "id -g"
     (dockerHost,dockerCertPath,dockerTlsVerify,isStdinTerminal,isStdoutTerminal,isStderrTerminal
       ,pwdFP,maybeSandboxDir) <-
       liftIO ((,,,,,,,) <$>
               lookupEnv "DOCKER_HOST" <*>
               lookupEnv "DOCKER_CERT_PATH" <*>
               lookupEnv "DOCKER_TLS_VERIFY" <*>
               hIsTerminalDevice stdin <*>
               hIsTerminalDevice stdout <*>
               hIsTerminalDevice stderr <*>
               FS.getWorkingDirectory <*>
               getSandboxDir)
     when (maybe False (isPrefixOf "tcp://") dockerHost &&
           maybe False (isInfixOf "boot2docker") dockerCertPath)
          (liftIO (hPutStrLn stderr
             ("WARNING: using boot2docker is NOT supported, and not likely to perform well.")))
     sandboxDirFP <- case maybeSandboxDir of
       Just dir -> return dir
       Nothing -> do let sandboxDirFP = FP.decodeString (FL.toFilePath (dockerDir config))
                                        </> dockerSandboxName
                     liftIO (FS.createTree sandboxDirFP)
                     return sandboxDirFP
     let image = dockerImageName config
     maybeImageInfo <- inspect image
     imageInfo <- case maybeImageInfo of
       Just ii -> return ii
       Nothing
         | dockerAutoPull config ->
             do pullImage config image
                mii2 <- inspect image
                case mii2 of
                  Just ii2 -> return ii2
                  Nothing -> error ("`docker inspect' failed for image after pull: " ++ image)
         | otherwise ->
             error ("The Docker image referenced by " ++ FL.toFilePath configFileName ++
                    " has not\nbeen downloaded:\n\n" ++
                    "Run 'stackage-docker " ++ dockerPullCmdName ++
                    "' to download it, then try again.")
     let pwdS = FP.encodeString pwdFP
         sandboxDirS = FP.encodeString sandboxDirFP
         workRootDir = FP.encodeString (FP.directory (sandboxDirFP))
         (uid,gid) = (dropWhileEnd isSpace uidOut, dropWhileEnd isSpace gidOut)
         imageEnvVars = map (break (== '=')) (icEnv (iiConfig imageInfo))
         (sandboxID,oldImage) =
           case lookupImageEnv sandboxIDEnvVar imageEnvVars of
             Just x -> (x,False)
             Nothing ->
               --TODO: remove this and oldImage after lts-1.x images no longer in use
               let sandboxName = maybe (dockerRepo config) id (lookupImageEnv "SANDBOX_NAME" imageEnvVars)
                   maybeImageCabalRemoteRepoName = lookupImageEnv "CABAL_REMOTE_REPO_NAME" imageEnvVars
                   maybeImageStackageSlug = lookupImageEnv "STACKAGE_SLUG" imageEnvVars
                   maybeImageStackageDate = lookupImageEnv "STACKAGE_DATE" imageEnvVars
               in (case (maybeImageStackageSlug,maybeImageStackageDate) of
                     (Just stackageSlug,_) -> sandboxName ++ "_" ++ stackageSlug
                     (_,Just stackageDate) -> sandboxName ++ "_" ++ stackageDate
                     _ -> sandboxName ++ maybe "" ("_" ++) maybeImageCabalRemoteRepoName
                  ,True)
         sandboxIDFP = FP.decodeString sandboxID
         sandboxSandboxDirFP = sandboxDirFP </> FP.decodeString ".sandbox" </> sandboxIDFP
         sandboxSandboxDirS = FP.encodeString sandboxSandboxDirFP
         sandboxHomeDirFP = sandboxDirFP </> homeDirName
         sandboxHomeDirS = FP.encodeString sandboxHomeDirFP
         sandboxRepoDirFP = sandboxDirFP </> sandboxIDFP
         sandboxRepoDirS = FP.encodeString sandboxRepoDirFP
         sandboxSubdirsFP = map (\d -> sandboxRepoDirFP </> FP.decodeString d)
                                sandboxedHomeSubdirectories
         sandboxSubdirsS = map FP.encodeString sandboxSubdirsFP
         execDockerProcess =
           do mapM_ FS.createTree ([sandboxHomeDirFP
                                   ,sandboxSandboxDirFP] ++
                                   sandboxSubdirsFP)
              execProcessAndExit "docker"
                (concat
                  [["run"
                   ,"--net=host"
                   ,"-e","WORK_UID=" ++ uid
                   ,"-e","WORK_GID=" ++ gid
                   ,"-e","WORK_WD=" ++ pwdS
                   ,"-e","WORK_HOME=" ++ FP.encodeString sandboxRepoDirFP
                   ,"-e","WORK_ROOT=" ++ workRootDir
                   ,"-v",workRootDir ++ ":" ++ workRootDir
                   ,"-v",sandboxSandboxDirS ++ ":" ++ sandboxDirS
                   ,"-v",sandboxHomeDirS ++ ":" ++ sandboxRepoDirS]
                  ,if oldImage
                     then ["-e",sandboxIDEnvVar ++ "=" ++ sandboxID
                          ,"--entrypoint=/root/entrypoint.sh"]
                     else []
                  ,case (dockerPassHost config,dockerHost) of
                     (True,Just x@('u':'n':'i':'x':':':'/':'/':s)) -> ["-e","DOCKER_HOST=" ++ x
                                                                      ,"-v",s ++ ":" ++ s]
                     (True,Just x) -> ["-e","DOCKER_HOST=" ++ x]
                     (True,Nothing) -> ["-v","/var/run/docker.sock:/var/run/docker.sock"]
                     (False,_) -> []
                  ,case (dockerPassHost config,dockerCertPath) of
                     (True,Just x) -> ["-e","DOCKER_CERT_PATH=" ++ x
                                      ,"-v",x ++ ":" ++ x]
                     _ -> []
                  ,case (dockerPassHost config,dockerTlsVerify) of
                     (True,Just x )-> ["-e","DOCKER_TLS_VERIFY=" ++ x]
                     _ -> []
                  ,concatMap sandboxSubdirArg sandboxSubdirsS
                  ,concatMap mountArg (dockerMountDefault config)
                  ,concatMap mountArg (dockerMountExtra config)
                  ,case dockerContainerName config of
                     Just name -> ["--name=" ++ name]
                     Nothing -> []
                  ,if dockerDetach config
                      then ["-d"]
                      else concat [if dockerPersist config
                                      then []
                                      else ["--rm"]
                                  ,if isStdinTerminal && isStdoutTerminal && isStderrTerminal
                                      then ["-t", "-i"]
                                      else []]
                  ,dockerRunArgsDefault config
                  ,concat (dockerRunArgsExtra config)
                  ,[image
                   ,hostVersionEnvVar ++ "=" ++ showVersion version
                   ,requireVersionEnvVar ++ "=" ++ showVersion requireContainerVersion]
                  ,map (\(k,v) -> k ++ "=" ++ v) envVars
                  ,[cmnd]
                  ,args])
                successPostAction
     liftIO (do updateDockerImageLastUsed (iiId imageInfo)
                                          (FL.toFilePath (dockerDir config))
                execDockerProcess)

  where
    lookupImageEnv :: String -> [(String,String)] -> Maybe String
    lookupImageEnv name vars =
      case lookup name vars of
        Just ('=':val) -> Just val
        _ -> Nothing

    mountArg :: Mount -> [String]
    mountArg (Mount host container) = ["-v",host ++ ":" ++ container]

    sandboxSubdirArg :: String -> [String]
    sandboxSubdirArg subdir = ["-v",subdir++ ":" ++ subdir]

-- | Clean-up old docker images and containers.
cleanup :: Cleanup -> IO ()
cleanup opts = runAction Nothing (cleanupAction opts)

-- | Cleanup action
cleanupAction :: Cleanup -> Action ()
cleanupAction opts =
  do checkDockerVersion
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
       (do imagesLastUsed <- getDockerImagesLastUsed
           curTime <- getZonedTime
           let planWriter = buildPlan curTime
                                      imagesLastUsed
                                      imageRepos
                                      danglingImageHashes
                                      stoppedContainers
                                      runningContainers
                                      inspectMap
               plan = toLazyByteString (execWriter planWriter)
           case dcAction opts of
             CleanupInteractive -> editByteString "stackage-docker-docker-cleanup-plan" plan
             CleanupImmediate -> return plan
             CleanupDryRun -> do LBS.hPut stdout plan
                                 return LBS.empty)
     withVerbosity
       Loud
       (mapM_ performPlanLine (reverse (filter filterPlanLine (lines (LBS.unpack plan)))))
     (Stdout allImageHashesOut) <- cmd "docker images -aq --no-trunc"
     liftIO (pruneDockerImagesLastUsed (lines allImageHashesOut))
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
           _ -> error ("Invalid line in cleanup commands: `" ++ line ++ "'")
         return ()
    parseImagesOut = Map.fromListWith (++) . map parseImageRepo . drop 1 . lines
      where parseImageRepo :: String -> (String, [String])
            parseImageRepo line =
              case words line of
                repo:tag:hash:_
                  | repo == "<none>" -> (hash,[])
                  | tag == "<none>" -> (hash,[repo])
                  | otherwise -> (hash,[repo ++ ":" ++ tag])
                _ -> error ("Invalid `docker images' output line: " ++ line)
    parseContainersOut = map parseContainer . drop 1 . lines
      where parseContainer line =
              case words line of
                hash:image:rest -> (hash,(image,last rest))
                _ -> error ("Invalid `docker ps' output line: " ++ line)
    buildPlan curTime
              imagesLastUsed
              imageRepos
              danglingImageHashes
              stoppedContainers
              runningContainers
              inspectMap =
      do case dcAction opts of
           CleanupInteractive ->
             do buildStrLn "# STACKAGE-DOCKER DOCKER CLEANUP PLAN\n\
                           \#\n\
                           \# When you leave the editor, the lines in this plan will be processed.\n\
                           \#\n\
                           \# Lines that begin with `R' denote an image or container that will be.\n\
                           \# removed.  You may change the first character to/from `R' to remove/keep\n\
                           \# and image or container that would otherwise be kept/removed.\n\
                           \#\n\
                           \# To cancel the cleanup, delete all lines in this file.\n\
                           \#\n\
                           \# By default, the following images/containers will be removed:\n\
                           \#"
                buildDefault dcRemoveKnownImagesLastUsedDaysAgo "Known images last used"
                buildDefault dcRemoveUnknownImagesCreatedDaysAgo "Unknown images created"
                buildDefault dcRemoveDanglingImagesCreatedDaysAgo "Dangling images created"
                buildDefault dcRemoveStoppedContainersCreatedDaysAgo "Stopped containers created"
                buildDefault dcRemoveRunningContainersCreatedDaysAgo "Running containers created"
                buildStrLn "#\n\
                           \# The default plan can be adjusted using command-line arguments.\n\
                           \# Run `stackage-docker docker-cleanup --help' for details.\n\
                           \#"
           _ -> buildStrLn "# Lines that begin with `R' denote an image or container that will be.\n\
                           \# removed."
         buildSection "KNOWN IMAGES (pulled/used by stackage-docker)"
                      imagesLastUsed
                      buildKnownImage
         buildSection "UNKNOWN IMAGES (not managed by stackage-docker)"
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
           Left msg -> error ("Invalid `docker inspect' output: " ++ msg ++ ".")
           Right results -> return (Map.fromList (map (\r -> (iiId r,r)) results))
       ExitFailure _ -> return Map.empty

-- | Inspect Docker image or container.
inspect :: String -> Action (Maybe Inspect)
inspect image =
  do results <- inspects [image]
     case Map.toList results of
       [] -> return Nothing
       [(_,i)] -> return (Just i)
       _ -> error ("Invalid `docker inspect' output: expect a single result.")

-- | Pull Docker image from registry.
pullImage :: Docker -> String -> Action ()
pullImage config image =
  liftIO (do hPutStrLn stderr ("\nPulling from registry: " ++ image)
             when (dockerRegistryLogin config)
                  (do hPutStrLn stderr "You may need to log in."
                      Proc.callProcess
                        "docker"
                        (concat
                           [["login"]
                           ,maybe [] (\u -> ["--username=" ++ u]) (dockerRegistryUsername config)
                           ,maybe [] (\p -> ["--password=" ++ p]) (dockerRegistryPassword config)
                           ,[takeWhile (/= '/') image]]))
             Proc.callProcess "docker" ["pull", image])

-- | Pull latest version of configured Docker image from registry.
pull :: Docker -> IO ()
pull config =
  runAction
    (Just config)
    (do checkDockerVersion
        pullImage config (dockerImageName config))

-- | Run a Shake action.
runAction :: Maybe Docker -> Action () -> IO ()
runAction Nothing inner =
  shake shakeOptions{shakeVerbosity = Quiet}
        (action inner)
runAction (Just cfg) inner =
  shake shakeOptions{shakeVerbosity = Quiet
                    ,shakeFiles = FL.toFilePath (shakeFilesPath (dockerDir cfg))}
        (action inner)

-- | Check docker version (throws exception if incorrect)
checkDockerVersion :: Action ()
checkDockerVersion =
  do (Stdout dockerVersionOut) <- cmd "docker --version"
       `actionOnException` putStrLn "\nCannot get Docker version.  IS DOCKER INSTALLED?\n"
     case words dockerVersionOut of
       (_:_:v:_) ->
         case parseVersion' v of
           Just v'
             | v' < minimumDockerVersion ->
               error (concat ["Minimum docker version `"
                             ,showVersion minimumDockerVersion
                             ,"' is required (you have `"
                             ,showVersion v'
                             ,"')."])
             | v' `elem` prohibitedDockerVersions ->
               error (concat ["These Docker versions are prohibited (you have `"
                             ,showVersion v'
                             ,"'): "
                             ,concat (intersperse ", " (map showVersion prohibitedDockerVersions))
                             ,"."])
             | otherwise ->
               return ()
           _ -> error "Cannot get Docker version (invalid `docker --version' output)."
       _ -> error "Cannot get Docker version (invalid `docker --version' output)."
  where minimumDockerVersion = Version [1,3,0] []
        prohibitedDockerVersions = [Version [1,2,0] []]

-- | Run a command when we're already inside a Docker container.
runInContainerAndExit :: FilePath -> [String] -> IO ()
runInContainerAndExit cmnd args =
  do unsetEnv requireVersionEnvVar
     execProcessAndExit cmnd args (return ())

-- | Run a process, then exit with the same exit code.
execProcessAndExit :: FilePath -> [String] -> IO () -> IO ()
execProcessAndExit cmnd args successPostAction =
  do (_, _, _, h) <- Proc.createProcess (Proc.proc cmnd args){Proc.delegate_ctlc = True}
     exitCode <- Proc.waitForProcess h
     when (exitCode == ExitSuccess)
          successPostAction
     exitWith exitCode

-- | Perform the docker sandbox reset tasks that are performed on the host.
resetOnHost :: Bool -> IO ()
resetOnHost keepHome =
  do maybeSandboxDir <- getSandboxDir
     case maybeSandboxDir of
       Just sandboxDir -> removeDirectoryContents sandboxDir
                                                  [homeDirName | keepHome]
       Nothing -> return ()

-- | Perform the Docker sandbox reset tasks that are performed from within a container.
resetInContainer :: IO ()
resetInContainer =
  do inContainer <- getInContainer
     if inContainer
        then do home <- FS.getHomeDirectory
                forM_ sandboxedHomeSubdirectories
                      (removeSubdir home)
        else hPutStrLn stderr
                       ("WARNING: Not removing " ++ show sandboxedHomeSubdirectories ++
                        " from home directory since running with Docker disabled.")
  where
    removeSubdir home d =
      removeDirectoryContents (home </> FP.decodeString d)
                              (if d == ".cabal" then [FP.decodeString "config"
                                                     ,FP.decodeString "packages"]
                                                else [])

-- | Display a warning to the user if running without Docker enabled.
warnIfNoContainer :: String -> IO ()
warnIfNoContainer cmdName =
  do inContainer <- getInContainer
     unless inContainer (warnNoContainer cmdName)

-- | Display a warning to the user when running without Docker enabled or when already inside a container.
warnNoContainer :: String -> IO ()
warnNoContainer cmdName =
  do inContainer <- getInContainer
     if inContainer
        then hPutStrLn stderr
                       ("WARNING: Running `" ++ cmdName ++
                       "' when already in a Docker container.")
        else hPutStrLn stderr
                       ("WARNING: Running `" ++ cmdName ++
                       "' even though Docker is disabled.")

-- | Find location of the @.docker-sandbox@  directory.
getSandboxDir :: IO (Maybe FP.FilePath)
getSandboxDir =
  do pwd <- getCurrentDirectory >>= FL.parseAbsDir
     fmap (fmap (FP.decodeString . FL.toFilePath))
          (findFileUp
             pwd
             (\fp ->
                FP.filename (FP.decodeString (FL.toFilePath fp)) ==
                dockerSandboxName)
             Nothing)

-- | Subdirectories of the home directory to sandbox between GHC/Stackage versions.
sandboxedHomeSubdirectories :: [FilePath]
sandboxedHomeSubdirectories = [".ghc", ".cabal", ".ghcjs", ".stackage"]

-- | Name of @.docker-sandbox@ directory.
dockerSandboxName :: FP.FilePath
dockerSandboxName = FP.decodeString ".docker-sandbox"

-- | Name of home directory within @.docker-sandbox@.
homeDirName :: FP.FilePath
homeDirName = FP.decodeString ".home"

-- | Remove the contents of a directory, without removing the directory itself.
-- This is used instead of 'FS.removeTree' to clear bind-mounted directories, since
-- removing the root of the bind-mount won't work.
removeDirectoryContents :: FP.FilePath -- ^ Directory to remove contents of
                        -> [FP.FilePath] -- ^ Directory names to exclude from removal
                        -> IO ()
removeDirectoryContents path exclude =
  do isRootDir <- FS.isDirectory path
     when isRootDir
          (do ls <- FS.listDirectory path
              forM_ ls (\l -> unless (FP.filename l `elem` exclude)
                                     (do isDir <- FS.isDirectory l
                                         if isDir
                                           then FS.removeTree l
                                           else FS.removeFile l)))

-- | Check host `stackage-docker` version
checkHostStackageDockerVersion :: Version -> IO ()
checkHostStackageDockerVersion minVersion =
  do maybeHostVer <- lookupEnv hostVersionEnvVar
     case parseVersion' =<< maybeHostVer of
       Just hostVer
         | hostVer < minVersion ->
             error ("Your host's version of `stackage-docker' is too old.\nVersion " ++
                    showVersion minVersion ++
                    " is required; you have " ++
                    showVersion hostVer ++
                    ".\n")
         | otherwise -> return ()
       Nothing ->
          do inContainer <- getInContainer
             if inContainer
                then error ("Your host's version of `stackage-docker' is too old.\nVersion " ++
                            showVersion minVersion ++ " is required.")
                else return ()


-- | Check host and container `stackage-docker' versions are compatible.
checkVersions :: IO ()
checkVersions =
  do checkHostStackageDockerVersion requireHostVersion
     maybeReqVer <- lookupEnv requireVersionEnvVar
     case parseVersion' =<< maybeReqVer of
       Just reqVer
         | version < reqVer ->
             error ("Your Docker image's version of `stackage-docker' is too old.\nVersion " ++
                    showVersion reqVer ++
                    " is required; you have " ++
                    showVersion version ++
                    ".\nPlease update your `stackage-docker.conf' to use a newer image.")
         | otherwise -> return ()
       _ -> return ()

-- | Parse a version number.
parseVersion' :: String -> Maybe Version
parseVersion' v =
  case reverse (readP_to_S parseVersion (dropWhileEnd isPunctuation v)) of
    ((v',""):_) -> Just v'
    _ -> Nothing

-- | Construct full configured Docker image name/ID.
dockerImageName :: Docker -> String
dockerImageName config =
  case dockerImage config of
    Just i -> i
    Nothing -> concat [dockerRepoOwner config
                      ,if null (dockerRepoOwner config) then "" else "/"
                      ,dockerRepo config
                      ,dockerRepoSuffix config
                      ,maybe "" (const ":") (dockerImageTag config)
                      ,maybe "" id (dockerImageTag config)]

-- | Environment variable to the host's stackage-docker version.
hostVersionEnvVar :: String
hostVersionEnvVar = "STACKAGE_DOCKER_HOST_VERSION"

-- | Environment variable to pass required container stackage-docker version.
requireVersionEnvVar :: String
requireVersionEnvVar = "STACKAGE_DOCKER_REQUIRE_VERSION"

-- | Environment variable that contains the sandbox ID.
sandboxIDEnvVar :: String
sandboxIDEnvVar = "DOCKER_SANDBOX_ID"

-- | Command-line argument for @docker-pull@.
dockerPullCmdName :: String
dockerPullCmdName = "docker-pull"

-- | Version of `stackage-docker' required to be installed in container.
requireContainerVersion :: Version
requireContainerVersion = Version [0,0,4] []

-- | Version of `stackage-docker' required to be installed on the host.
requireHostVersion :: Version
requireHostVersion = Version [0,1,1] []

-- | Options for 'cleanup'.
data Cleanup = Cleanup
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
