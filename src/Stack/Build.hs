{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Build project(s).

module Stack.Build
  (build
  ,test
  ,haddock
  ,benchmark
  ,clean
  ,shakeFilesPath
  ,configDir
  ,checkGHCVersion
  ,getPackageInfos)
  where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$),($=))
import           Data.Conduit.Binary (sinkIOHandle,sourceHandle)
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.Function
import           Data.IORef
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Data.Set.Monad as S
import           Data.Streaming.Process hiding (env)
import qualified Data.Streaming.Process as Process
import qualified Data.Text as T
import           Development.Shake hiding (doesFileExist,doesDirectoryExist,getDirectoryContents)
import           Distribution.Package hiding (packageName,packageVersion,Package,PackageName,PackageIdentifier)
import           Network.HTTP.Download
import           Path as FL
import           Path.Find
import           Prelude hiding (FilePath,writeFile)
import           Stack.Build.Doc
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageIndex
import           Stack.Types
import           System.Directory hiding (findFiles)
import           System.Environment
import qualified System.FilePath as FilePath
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Posix.Files (createSymbolicLink,removeLink)
import           System.Process.Read (getExternalEnv, readProcessStdout)

--------------------------------------------------------------------------------
-- Top-level commands

-- | Build and test using Shake.
test :: (MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
     => TestConfig
     -> m ()
test conf =
  build (BuildOpts (tconfigTargets conf)
                   (tconfigVerbosity conf)
                   False
                   False
                   Nothing
                   DoTests
                   False
                   [])

-- | Build and haddock using Shake.
haddock :: (MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
        => HaddockConfig -> m ()
haddock conf =
  build (BuildOpts (hconfigTargets conf)
                   (hconfigVerbosity conf)
                   False
                   False
                   Nothing
                   DoHaddock
                   False
                   [])

-- | Build and benchmark using Shake.
benchmark :: (MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
          => BenchmarkConfig -> m ()
benchmark conf =
  build (BuildOpts (benchTargets conf)
                   (benchVerbosity conf)
                   False
                   False
                   Nothing
                   DoBenchmarks
                   False
                   [])

-- | Build using Shake.
build :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
      => BuildOpts -> m ()
build bopts =
  do env <- ask
     pinfos <- runResourceT (getPackageInfos (boptsFinalAction bopts))
     pkgIds <- getPackageIds
                [bcPackageDatabase $ getBuildConfig env]
                (map packageName (S.toList pinfos))
     pwd <- liftIO $ getCurrentDirectory >>= parseAbsDir
     docLoc <- liftIO $ getUserDocLoc
     installResource <- liftIO $ newResourceIO "cabal install" 1
     cfgVar <- liftIO $ newMVar ConfigLock
     plans <-
       forM (S.toList pinfos)
            (\pinfo ->
               do let wantedTarget =
                        wanted pwd pinfo
                  when (wantedTarget && boptsFinalAction bopts /= DoNothing)
                       (liftIO (deleteGenFile (packageDir pinfo)))
                  gconfig <- liftIO $
                    readGenConfigFile pkgIds
                                      (packageName pinfo)
                                      bopts
                                      (packageDir pinfo)
                                      wantedTarget
                                      pinfo
                                      cfgVar
                  return (makePlan pkgIds
                                   wantedTarget
                                   bopts
                                   (getBuildConfig env)
                                   gconfig
                                   pinfos
                                   pinfo
                                   installResource
                                   docLoc
                                   cfgVar))
     if boptsDryrun bopts
        then liftIO $ dryRunPrint pinfos
        else do
            let config = getConfig env
            liftIO $ withArgs []
                      (shakeArgs shakeOptions {shakeVerbosity = boptsVerbosity bopts
                                              ,shakeFiles =
                                                 FL.toFilePath (shakeFilesPath (configDir config))
                                              ,shakeThreads = defaultShakeThreads}
                                 (do sequence_ plans
                                     when (boptsFinalAction bopts ==
                                           DoHaddock)
                                          (buildDocIndex (wanted pwd)
                                                         docLoc
                                                         pinfos)))
  where wanted pwd pinfo =
          case boptsTargets bopts of
            [] ->
              FL.isParentOf pwd
                            (packageDir pinfo) ||
              packageDir pinfo == pwd
            packages ->
              elem (packageName pinfo)
                   (mapMaybe (parsePackageNameFromString . T.unpack) packages)

-- | Dry run output.
dryRunPrint :: Set Package -> IO ()
dryRunPrint pinfos =
  do putStrLn "The following packages will be built and installed:"
     forM_ (S.toList pinfos) (\pinfo -> putStrLn (packageNameString (packageName pinfo)))

-- | Reset the build (remove Shake database and .gen files).
clean :: forall m env.
         (MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
      => m ()
clean =
  do env <- ask
     pinfos <- runResourceT (getPackageInfos DoNothing)
     let config = getConfig env
     forM_ (S.toList pinfos)
           (\pinfo ->
              do deleteGenFile (packageDir pinfo)
                 let distDir =
                       FL.toFilePath (distDirFromDir (packageDir pinfo))
                 liftIO $ do
                     exists <- doesDirectoryExist distDir
                     when exists (removeDirectoryRecursive distDir))
     let listDir = FL.parent (shakeFilesPath (configDir config))
     ls <- liftIO $
       fmap (map (FL.toFilePath listDir ++))
            (getDirectoryContents (FL.toFilePath listDir))
     mapM_ (rmShakeMetadata (configDir config)) ls
  where rmShakeMetadata cfg p = liftIO $
          when (isPrefixOf
                  (FilePath.takeFileName
                     (FL.toFilePath (shakeFilesPath cfg) ++
                      "."))
                  (FilePath.takeFileName p))
               (do isDir <- doesDirectoryExist p
                   if isDir
                      then removeDirectoryRecursive p
                      else removeFile p)

--------------------------------------------------------------------------------
-- Shake plan

-- | Make a Shake plan for a package.
makePlan :: Map PackageName GhcPkgId
         -> Bool
         -> BuildOpts
         -> BuildConfig
         -> GenConfig
         -> Set Package
         -> Package
         -> Resource
         -> Path Abs Dir
         -> MVar ConfigLock
         -> Rules ()
makePlan pkgIds wanted bopts bconfig gconfig pinfos pinfo installResource docLoc cfgVar =
  do when wanted (want [target])
     target %>
       \_ ->
         do needDependencies pkgIds bopts pinfos pinfo cfgVar
            needTools pinfo
            needSourceFiles
            removeAfterwards <- liftIO (ensureSetupHs dir)
            actionFinally
              (buildPackage
                 bopts
                 bconfig
                 pinfos
                 pinfo
                 gconfig
                 (if wanted
                     then boptsFinalAction bopts
                     else DoNothing)
                 installResource
                 docLoc)
              removeAfterwards
            writeFinalFiles gconfig bconfig dir (packageName pinfo)
  where needSourceFiles =
          need (map FL.toFilePath (S.toList (packageFiles pinfo)))
        dir = packageDir pinfo
        target =
          FL.toFilePath (builtFileFromDir dir)

-- | Check that the build tools are available in PATH.
needTools :: Package -> Action ()
needTools pinfo =
  liftIO (mapM_ (\dep@(Dependency n _) ->
                   do m <- findExecutable (packageNameString (fromCabalPackageName n))
                      case m of
                        Nothing ->
                          liftIO (throwIO (MissingTool dep))
                        _ -> return ())
                (packageTools pinfo))

-- | Specify that the given package needs the following other
-- packages.
needDependencies :: Map PackageName GhcPkgId
                 -> BuildOpts
                 -> Set Package
                 -> Package
                 -> MVar ConfigLock
                 -> Action ()
needDependencies pkgIds bopts pinfos pinfo cfgVar =
  do deps <- mapM (\pinfo' ->
                     let dir' = packageDir pinfo'
                         genFile = builtFileFromDir dir'
                     in do void (liftIO (readGenConfigFile pkgIds
                                                           (packageName pinfo')
                                                           bopts
                                                           dir'
                                                           False
                                                           pinfo'
                                                           cfgVar))
                           return (FL.toFilePath genFile))
                  (mapMaybe (\name ->
                               find ((== name) . packageName)
                                    (S.toList pinfos))
                            (M.keys (packageDeps pinfo)))
     need deps

--------------------------------------------------------------------------------
-- Build actions

-- | Write the final generated files after a build successfully
-- completes.
writeFinalFiles :: (MonadIO m)
                => GenConfig -> BuildConfig -> Path Abs Dir -> PackageName -> m ()
writeFinalFiles gconfig bconfig dir name = liftIO $
         (do mpkigid <- flip runReaderT bconfig
                      $ findPackageId
                            [bcPackageDatabase bconfig]
                            name
             case mpkigid of
               Nothing -> throwIO (Couldn'tFindPkgId name)
               Just pkgid ->
                 do writeGenConfigFile
                      dir
                      gconfig {gconfigForceRecomp = False
                              ,gconfigPkgId = pkgid}
                    -- After a build has completed successfully for a given
                    -- configuration, no recompilation forcing is required.
                    updateGenFile dir)

-- | Build the given package with the given configuration.
buildPackage :: BuildOpts
             -> BuildConfig
             -> Set Package
             -> Package
             -> GenConfig
             -> FinalAction
             -> Resource
             -> Path Abs Dir
             -> Action ()
buildPackage bopts bconfig pinfos pinfo gconfig setupAction installResource docLoc =
  do liftIO (void (try (removeFile (FL.toFilePath (buildLogPath pinfo))) :: IO (Either IOException ())))
     runhaskell
       pinfo
       (getExternalEnv bconfig)
       (concat [["configure","--user"]
               ,["--package-db=clear","--package-db=global"]
               ,["--package-db=" ++ toFilePath (bcPackageDatabase bconfig)] -- FIXME do the more complicated double-package-database thing once we have two-phase builds
               ,["--enable-library-profiling" | gconfigLibProfiling gconfig]
               ,["--enable-executable-profiling" | gconfigExeProfiling gconfig]
               ,["--enable-tests" | setupAction == DoTests]
               ,["--enable-benchmarks" | setupAction == DoBenchmarks]
               ,map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (M.toList (packageFlags pinfo))])
     runhaskell
       pinfo
       (getExternalEnv bconfig)
       (concat [["build"]
               ,["--ghc-options=-O2" | gconfigOptimize gconfig]
               ,["--ghc-options=-fforce-recomp" | gconfigForceRecomp gconfig]
               ,concat [["--ghc-options",T.unpack opt] | opt <- boptsGhcOptions bopts]])
     case setupAction of
       DoTests ->
         runhaskell pinfo
                    (getExternalEnv bconfig)
                    ["test"]
       DoHaddock ->
           do liftIO (removeDocLinks docLoc pinfo)
              ifcOpts <- liftIO (haddockInterfaceOpts docLoc pinfo pinfos)
              runhaskell pinfo
                         (getExternalEnv bconfig)
                         ["haddock"
                         ,"--html"
                         ,"--hoogle"
                         ,"--hyperlink-source"
                         ,"--html-location=../$pkg-$version/"
                         ,"--haddock-options=" ++ intercalate " " ifcOpts]
              haddockLocs <-
                liftIO (findFiles (packageDocDir pinfo)
                                  (\loc -> FilePath.takeExtensions (toFilePath loc) == "." ++ haddockExtension)
                                  (not . isHiddenDir))
              forM_ haddockLocs $ \haddockLoc ->
                do let hoogleTxtPath = FilePath.replaceExtension (toFilePath haddockLoc) "txt"
                       hoogleDbPath = FilePath.replaceExtension hoogleTxtPath hoogleDbExtension
                   hoogleExists <- liftIO (doesFileExist hoogleTxtPath)
                   when hoogleExists
                        (command [EchoStdout False]
                                 "hoogle"
                                 ["convert"
                                 ,"--haddock"
                                 ,hoogleTxtPath
                                 ,hoogleDbPath])
       DoBenchmarks ->
         runhaskell pinfo
                    (getExternalEnv bconfig)
                    ["bench"]
       _ -> return ()
     withResource installResource 1
                  (runhaskell pinfo
                              (getExternalEnv bconfig)
                              ["install","--user"])
     case setupAction of
       DoHaddock -> liftIO (createDocLinks docLoc pinfo)
       _ -> return ()

-- | Run the Haskell command for the given package.
runhaskell :: Package -> Maybe [(String, String)] -> [String] -> Action ()
runhaskell pinfo menv args =
  do liftIO (createDirectoryIfMissing True
                                      (FL.toFilePath (stackageBuildDir pinfo)))
     putQuiet display
     outRef <- liftIO (newIORef mempty)
     errRef <- liftIO (newIORef mempty)
     join (liftIO (catch (do withCheckedProcess
                               cp {cwd =
                                     Just (FL.toFilePath dir)
                                  ,Process.env = menv
                                  ,std_err = Inherit}
                               (\ClosedStream stdout' stderr' ->
                                  do logFrom stdout' outRef
                                     logFrom stderr' errRef)
                             return (return ()))
                         (\e@ProcessExitedUnsuccessfully{} ->
                            return (do putQuiet (display <> ": ERROR")
                                       errs <- liftIO (readIORef errRef)
                                       outs <- liftIO (readIORef outRef)
                                       unless (S8.null outs)
                                              (do putQuiet "Stdout was:"
                                                  putQuiet (S8.unpack outs))
                                       unless (S8.null errs)
                                              (do putQuiet "Stderr was:"
                                                  putQuiet (S8.unpack errs))
                                       liftIO (throwIO e)))))
  where logFrom h ref =
          void (try (runResourceT
                       (sourceHandle h $=
                        CL.mapM (\chunk ->
                                   do liftIO (modifyIORef' ref (<> chunk))
                                      return chunk) $$
                        sinkIOHandle (openFile (FL.toFilePath (buildLogPath pinfo)) AppendMode)))
                :: IO (Either IOException ()))
        display =
          packageNameString (packageName pinfo) <>
          ": " <>
          case args of
            (cname:_) -> cname
            _ -> mempty
        dir = packageDir pinfo
        cp =
          proc "runhaskell" ("Setup.hs" : args)

-- | Ensure Setup.hs exists in the given directory. Returns an action
-- to remove it later.
ensureSetupHs :: Path Abs Dir -> IO (IO ())
ensureSetupHs dir =
  do exists <- doesFileExist (FL.toFilePath fp)
     if exists
        then return (return ())
        else do writeFile (FL.toFilePath fp) "import Distribution.Simple\nmain = defaultMain"
                return (removeFile (FL.toFilePath fp))
  where fp =
          dir </>
          $(mkRelFile "Setup.hs")

-- | Build the haddock documentation index and contents.
buildDocIndex :: (Package -> Bool) -> Path Abs Dir -> Set Package -> Rules ()
buildDocIndex wanted docLoc pinfos =
  do runHaddock "--gen-contents" $(mkRelFile "index.html")
     runHaddock "--gen-index" $(mkRelFile "doc-index.html")
     combineHoogle
  where
    runHaddock genOpt destFilename =
      do let destPath = toFilePath (docLoc </> destFilename)
         want [destPath]
         destPath %> \_ ->
           do needDeps
              ifcOpts <- liftIO (fmap concat (mapM toInterfaceOpt (S.toList pinfos)))
              command [Cwd (FL.toFilePath docLoc)]
                      "haddock"
                      (genOpt:ifcOpts)
    toInterfaceOpt pinfo =
      do let pv = joinPkgVer (packageName pinfo,packageVersion pinfo)
             srcPath = (toFilePath docLoc) ++ "/" ++
                       pv ++ "/" ++
                       packageNameString (packageName pinfo) ++ "." ++
                       haddockExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then ["-i"
                         ,"../" ++
                          pv ++
                          "," ++
                          srcPath]
                     else [])
    combineHoogle =
      do let destHoogleDbLoc = hoogleDatabaseFile docLoc
             destPath = FL.toFilePath destHoogleDbLoc
         want [destPath]
         destPath %> \_ ->
           do needDeps
              srcHoogleDbs <- liftIO (fmap concat (mapM toSrcHoogleDb (S.toList pinfos)))
              command [EchoStdout False]
                      "hoogle"
                      ("combine" :
                       "-o" :
                       FL.toFilePath destHoogleDbLoc :
                       srcHoogleDbs)
    toSrcHoogleDb pinfo =
      do let srcPath = toFilePath docLoc ++ "/" ++
                       joinPkgVer (packageName pinfo,packageVersion pinfo) ++ "/" ++
                       packageNameString (packageName pinfo) ++ "." ++
                       hoogleDbExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then [srcPath]
                    else [])
    needDeps =
      need (concatMap (\pinfo -> if wanted pinfo
                                    then let dir = packageDir pinfo
                                         in [FL.toFilePath (builtFileFromDir dir)]
                                    else [])
                      (S.toList pinfos))

-- | Remove existing links docs for package from @~/.cabal/fpdoc@.
removeDocLinks :: Path Abs Dir -> Package -> IO ()
removeDocLinks docLoc pinfo =
  do createDirectoryIfMissing True
                              (FL.toFilePath docLoc)
     userDocLs <-
       fmap (map (FL.toFilePath docLoc ++))
            (getDirectoryContents (FL.toFilePath docLoc))
     forM_ userDocLs $
       \docPath ->
         do isDir <- doesDirectoryExist docPath
            when isDir
                 (case breakPkgVer (FilePath.takeFileName docPath) of
                    Just (p,_) ->
                      when (p == packageName pinfo)
                           (removeLink docPath)
                    Nothing -> return ())

-- | Add link for package to @~/.cabal/fpdoc@.
createDocLinks :: Path Abs Dir -> Package -> IO ()
createDocLinks docLoc pinfo =
  do let pkgVer =
           joinPkgVer (packageName pinfo,(packageVersion pinfo))
     pkgVerLoc <- liftIO (parseRelDir pkgVer)
     let pkgDestDocLoc = docLoc </> pkgVerLoc
         pkgDestDocPath =
           FilePath.dropTrailingPathSeparator (FL.toFilePath pkgDestDocLoc)
     haddockLocs <-
       findFiles (docLoc </>
                  $(mkRelDir "../share/doc/"))
                 (\fileLoc ->
                    FilePath.takeExtensions (toFilePath fileLoc) ==
                    "." ++ haddockExtension &&
                    dirname (parent fileLoc) ==
                    $(mkRelDir "html/") &&
                    toFilePath (dirname (parent (parent fileLoc))) ==
                    (pkgVer ++ "/"))
                 (\dirLoc ->
                    not (isHiddenDir dirLoc) &&
                    dirname (parent (parent dirLoc)) /=
                    $(mkRelDir "html/"))
     case haddockLocs of
       [haddockLoc] ->
         case FL.stripDir (parent docLoc)
                          haddockLoc of
           Just relHaddockPath ->
             do let srcRelPathCollapsed =
                      FilePath.takeDirectory (FilePath.dropTrailingPathSeparator (FL.toFilePath relHaddockPath))
                    {-srcRelPath = "../" ++ srcRelPathCollapsed-}
                createSymbolicLink (FilePath.dropTrailingPathSeparator srcRelPathCollapsed)
                                   pkgDestDocPath
           Nothing -> return ()
       _ -> return ()

-- | Get @-i@ arguments for haddock for dependencies.
haddockInterfaceOpts :: Path Abs Dir -> Package -> Set Package -> IO [String]
haddockInterfaceOpts userDocLoc pinfo pinfos =
  do (mglobalDocLoc, _) <- getGlobalDocLocs
     globalPkgVers <-
       case mglobalDocLoc of
         Nothing -> return M.empty
         Just globalDocLoc -> getDocPackages globalDocLoc
     let toInterfaceOpt pn =
           case find (\dpi -> packageName dpi == pn) (S.toList pinfos) of
             Nothing ->
               return (case (M.lookup pn globalPkgVers,mglobalDocLoc) of
                         (Just (v:_),Just globalDocLoc) ->
                           ["-i"
                           ,"../" ++ joinPkgVer (pn,v) ++
                            "," ++
                            toFilePath globalDocLoc ++ "/" ++
                            joinPkgVer (pn,v) ++ "/" ++
                            packageNameString pn ++ "." ++
                            haddockExtension]
                         _ -> [])
             Just dpi ->
               do let destPath = (FL.toFilePath userDocLoc ++ "/" ++
                                 joinPkgVer (pn,packageVersion dpi) ++ "/" ++
                                 packageNameString pn ++ "." ++
                                 haddockExtension)
                  exists <- doesFileExist destPath
                  return (if exists
                             then ["-i"
                                  ,"../" ++
                                   joinPkgVer (pn,packageVersion dpi) ++
                                   "," ++
                                   destPath]
                             else [])
     --TODO: use not only direct dependencies, but dependencies of dependencies etc.
     --(e.g. redis-fp doesn't include @text@ in its dependencies which means the 'Text'
     --datatype isn't linked in its haddocks)
     fmap concat (mapM toInterfaceOpt (S.toList (packageAllDeps pinfo)))

--------------------------------------------------------------------------------
-- Generated files

-- | Should the generated config be considered invalid?
genFileInvalidated :: Map PackageName GhcPkgId
                   -> BuildOpts
                   -> GenConfig
                   -> PackageName
                   -> Package
                   -> Bool
genFileInvalidated pkgIds bopts gconfig pname pinfo =
  or [installedPkgIdChanged
     ,optimizationsChanged
     ,profilingChanged
     ,ghcOptsChanged
     ,flagsChanged]
  where installedPkgIdChanged =
          Just (gconfigPkgId gconfig) /=
          M.lookup pname pkgIds
        ghcOptsChanged = boptsGhcOptions bopts /= gconfigGhcOptions gconfig
        profilingChanged =
          (boptsLibProfile bopts &&
           not (gconfigLibProfiling gconfig)) ||
          (boptsExeProfile bopts &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          case boptsEnableOptimizations bopts of
            Just optimize
              | optimize /= gconfigOptimize gconfig && optimize -> True
            _ -> False
        flagsChanged = packageFlags pinfo /= gconfigFlags gconfig

-- | Should the generated config be updated?
genFileChanged :: Map PackageName GhcPkgId
               -> BuildOpts
               -> GenConfig
               -> PackageName
               -> Package
               -> Bool
genFileChanged pkgIds bopts gconfig pname pinfo =
  or [installedPkgIdChanged
     ,optimizationsChanged
     ,profilingChanged
     ,ghcOptsChanged
     ,flagsChanged]
  where installedPkgIdChanged =
          Just (gconfigPkgId gconfig) /=
          M.lookup pname pkgIds
        ghcOptsChanged = boptsGhcOptions bopts /= gconfigGhcOptions gconfig
        profilingChanged =
          (boptsLibProfile bopts &&
           not (gconfigLibProfiling gconfig)) ||
          (boptsExeProfile bopts &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          maybe False (/= gconfigOptimize gconfig) (boptsEnableOptimizations bopts)
        flagsChanged =
          packageFlags pinfo /=
          gconfigFlags gconfig

-- | Write out the gen file for the build dir.
updateGenFile :: MonadIO m => Path Abs Dir -> m ()
updateGenFile dir = liftIO $
  L.writeFile (FL.toFilePath (builtFileFromDir dir))
              ""

-- | Delete the gen file, which will cause a rebuild.
deleteGenFile :: MonadIO m => Path Abs Dir -> m ()
deleteGenFile dir = liftIO $
  catch (removeFile (FL.toFilePath (builtFileFromDir dir)))
        (\(_ :: IOException) -> return ())

-- | Save generated configuration.
writeGenConfigFile :: MonadIO m => Path Abs Dir -> GenConfig -> m ()
writeGenConfigFile dir gconfig = liftIO $
  do createDirectoryIfMissing True (FL.toFilePath (FL.parent (builtConfigFileFromDir dir)))
     L.writeFile (FL.toFilePath (builtConfigFileFromDir dir))
                 (encode gconfig)

-- | Read the generated config file, or return a default based on the
-- build configuration.
readGenConfigFile :: Map PackageName GhcPkgId
                  -> PackageName
                  -> BuildOpts
                  -> Path Abs Dir
                  -> Bool
                  -> Package
                  -> MVar ConfigLock
                  -> IO GenConfig
readGenConfigFile pkgIds name bopts dir wanted pinfo cfgVar = withMVar cfgVar (const go)
  where go =
          do bytes <-
               catch (fmap Just
                           (S.readFile (FL.toFilePath fp)))
                     (\(_ :: IOException) ->
                        return Nothing)
             case bytes >>= decode . L.fromStrict of
               Just gconfig ->
                 if genFileChanged pkgIds bopts gconfig name pinfo
                    then
                         -- If the build config has changed such that the gen
                         -- config needs to be regenerated...
                         do let invalidated =
                                  genFileInvalidated pkgIds bopts gconfig name pinfo
                            when (invalidated || wanted)
                                 (deleteGenFile dir)
                            let gconfig' =
                                  (newConfig gconfig bopts pinfo) {gconfigForceRecomp = invalidated}
                            -- When a file has been invalidated it means the
                            -- configuration has changed such that things need
                            -- to be recompiled, hence the above setting of force
                            -- recomp.
                            writeGenConfigFile dir gconfig'
                            return gconfig'
                    else return gconfig -- No change, the generated config is consistent with the build config.
               Nothing ->
                 do maybe (return ())
                          (const (putStrLn ("Warning: Couldn't parse config file for " ++
                                            packageNameString name ++
                                            ", migrating to latest configuration format. This will force a rebuild.")))
                          bytes
                    deleteGenFile dir
                    let gconfig' =
                          newConfig defaultGenConfig bopts pinfo
                    writeGenConfigFile dir gconfig'
                    return gconfig' -- Probably doesn't exist or is out of date (not parseable.)
        fp = builtConfigFileFromDir dir

-- | Update a gen configuration using the build configuration.
newConfig :: GenConfig -- ^ Build configuration.
          -> BuildOpts -- ^ A base gen configuration.
          -> Package
          -> GenConfig
newConfig gconfig bopts pinfo =
  defaultGenConfig
      {gconfigOptimize =
         maybe (gconfigOptimize gconfig)
               id
               (boptsEnableOptimizations bopts)
      ,gconfigLibProfiling = boptsLibProfile bopts ||
                             gconfigLibProfiling gconfig
      ,gconfigExeProfiling = boptsExeProfile bopts ||
                             gconfigExeProfiling gconfig
      ,gconfigGhcOptions = boptsGhcOptions bopts
      ,gconfigFlags = packageFlags pinfo
      ,gconfigPkgId = gconfigPkgId gconfig}

--------------------------------------------------------------------------------
-- Package info/dependencies/etc

-- | Get packages' information.
--
-- Needs to be refactored, see: https://github.com/fpco/stack/issues/40
getPackageInfos :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadThrow m
                   ,MonadMask m
                   ,HasHttpManager env,HasBuildConfig env,MonadReader env m)
                => FinalAction
                -> m (Set Package)
getPackageInfos finalAction =
    do cfg <- asks getConfig
       go True cfg
  where go retry cfg =
          do unless retry ($logInfo "Getting pack information, dependencies, etc. ...")
             bconfig <- asks getBuildConfig
             globalPackages <- getAllPackages [bcPackageDatabase bconfig]

             paths <- unpackPackageIdentsForBuild (configPackagesIdent cfg)

             (infos,errs) <-
               runWriterT
                 (buildDependencies finalAction
                                    (configGlobalFlags cfg)
                                    globalPackages
                                    bconfig
                                    (configPackagesPath cfg <> paths)
                                    (configPackageFlags cfg))
             case errs of
               [] -> return infos
               _
                 | configInstallDeps cfg
                 , retry ->
                   installDeps infos globalPackages cfg errs
                 | otherwise ->
                   liftIO (throwIO (DependencyIssues (nubBy (on (==) show) errs)))
        installDeps infos globalPackages cfg errs =
          do requireIndex
             results <- forM names checkPackageInIndex
             case lefts results of
               [] ->
                 do let okPkgVers = M.fromList (rights results)
                    bc <- asks getBuildConfig
                    !mapping <- case bcResolver bc of
                        ResolverSnapshot snapName -> do
                            bp <- loadBuildPlan snapName
                            $logInfo "Resolving build plan ..."
                            !mapping <-
                              resolveBuildPlan bp (M.keysSet okPkgVers)
                            $logDebug "Resolved. Checking ..."
                            return mapping
                    !validated <-
                      liftM catMaybes (mapM (validateSuggestion globalPackages okPkgVers)
                        (M.toList mapping))
                    $logDebug "Done checking."
                    case validated of
                      [] -> return infos
                      toFetch -> do
                        $logInfo "Fetching and unpacking third party packages ..."
                        let toUnpack = map (\(name, (ver, _flags)) ->
                                               fromTuple (name,ver))
                                                toFetch
                        withTempUnpacked toUnpack $ \newPkgDirs -> do
                           -- Here is where we inject third-party
                           -- dependencies and their flags and re-run
                           -- this function.
                           --
                           -- FIXME: New approach: two stage builds described
                           -- in:
                           --
                           -- https://github.com/fpco/stack/issues/42
                           $logDebug "Re-running build plan resolver ..."
                           go
                             False
                             cfg {configPackagesPath = configPackagesPath cfg <>
                                                   S.fromList newPkgDirs
                                 ,configPackageFlags =
                                    configPackageFlags cfg <>
                                    mconcat (map (\(name, (_version, flags)) ->
                                                    M.singleton name flags)
                                                 validated)}
               erroneous ->
                 liftIO (throwIO (DependencyIssues
                                    (map snd (mapMaybe (flip lookup names) erroneous))))
          where names = getMissingDeps errs

-- | Extract the missing dependencies and their version ranges, but
-- keeping hold of the original exception for later use.
getMissingDeps :: [StackBuildException] -> [(PackageName,(VersionRange,StackBuildException))]
getMissingDeps =
  mapMaybe (\x ->
              case x of
                MissingDep _ name range ->
                  Just (name,(range,x))
                _ -> Nothing)

-- | Validate that
--
-- * within the global package database,
-- * and with the locally (.cabal) specified dependencies with their
--   version ranges,
-- * the given package-version pair is within range of the locally
--   specified dependencies,
-- * or, if it's not in there, then if it's already in the installed
--   package database, check that the versions match up,
-- * if it's not in the installed package database, at this point
--   consider it good for installation.
--
validateSuggestion :: MonadIO m
                   => Map PackageName Version
                   -> Map PackageName VersionRange
                   -> (PackageName, (Version, Map FlagName Bool))
                   -> m (Maybe (PackageName, (Version, Map FlagName Bool)))
validateSuggestion globalPackages okPkgVers =
  (\suggestion@(name, (suggestedVer, _)) ->
     case M.lookup name okPkgVers of
       Just range
         | not (withinRange suggestedVer range) ->
           liftIO (throwIO (StackageDepVerMismatch name suggestedVer range))
       _ ->
         case M.lookup name globalPackages of
           Just installedVer
             | installedVer == suggestedVer ->
               return Nothing
             | otherwise ->
               liftIO (throwIO (StackageVersionMismatch name suggestedVer installedVer))
           Nothing ->
             return (Just suggestion))

-- | Check that a package is actually available in the general (Hackage) package
-- index.
checkPackageInIndex :: (MonadIO m, MonadLogger m,MonadThrow m
                       ,MonadReader env m,HasConfig env)
                    => (PackageName,(VersionRange,t))
                    -> m (Either PackageName (PackageName,VersionRange))
checkPackageInIndex (name,(range,_)) =
  do vers <- getPkgVersions name
     case () of
       ()
         | Set.null vers -> return (Right (name,range))
         | any (flip withinRange range)
               (S.toList vers) ->
           return (Right (name,range))
         | otherwise -> return (Left name)

-- | Build a map of package names to dependencies.
buildDependencies :: MonadIO m
                  => FinalAction
                  -> Map FlagName Bool
                  -> Map PackageName Version
                  -> BuildConfig
                  -> Set (Path Abs Dir)
                  -> Map PackageName (Map FlagName Bool)
                  -> WriterT [StackBuildException] m (Set Package)
buildDependencies finalAction flags globals bconfig packages pflags =
  do pkgs <-
       liftIO (S.mapM (getPackageInfo finalAction flags pflags bconfig) packages)
     S.mapM (sievePackages
               globals
               (M.fromList
                  (map (packageName &&& packageVersion)
                       (S.toList pkgs))))
            pkgs

-- | Remove third-party package dependencies, e.g. mtl, bytestring,
-- etc.
sievePackages :: MonadIO m
              => Map PackageName Version
              -> Map PackageName Version
              -> Package
              -> WriterT [StackBuildException] m Package
sievePackages globalNameVersions localNameVersions p =
  do deps <-
       filterM (\(name,range) ->
                  case M.lookup name localNameVersions of
                    Just ver | withinRange ver range -> return True
                             | otherwise -> do tell [MissingDep p name range]
                                               return False
                    Nothing -> case M.lookup name globalNameVersions of
                                 Just ver
                                   | withinRange ver range ->
                                     return False
                                   | otherwise ->
                                     do tell [MissingDep p name range]
                                        return False
                                 Nothing ->
                                   do tell [MissingDep p name range]
                                      return False)
               (M.toList (packageDeps p))
     return (p {packageDeps = M.fromList deps})

-- | Get the package name and dependencies from the given package
-- directory.
getPackageInfo :: FinalAction
               -> Map FlagName Bool
               -> Map PackageName (Map FlagName Bool)
               -> BuildConfig
               -> Path Abs Dir
               -> IO Package
getPackageInfo finalAction flags pflags bconfig pkgDir =
  do cabal <- getCabalFileName pkgDir
     pname <- parsePackageNameFromFilePath cabal
     let ghcVersion = bcGhcVersion bconfig
     info <-
       runStdoutLoggingT
                          (readPackage (cfg pname ghcVersion)
                                       cabal)
     existing <-
       fmap S.fromList
            (filterM (doesFileExist . FL.toFilePath)
                     (S.toList (packageFiles info)))
     return info {packageFiles = existing}
  where cfg pname ghcVersion =
          PackageConfig {packageConfigEnableTests =
                           case finalAction of
                             DoTests -> True
                             _ -> False
                        ,packageConfigEnableBenchmarks =
                           case finalAction of
                             DoBenchmarks -> True
                             _ -> False
                        ,packageConfigFlags =
                           composeFlags pname pflags flags
                        ,packageConfigGhcVersion = ghcVersion}

-- | Compose the package flags with the global flags in a left-biased
-- form, i.e., package-specific flags will be preferred over global
-- flags.
composeFlags :: PackageName
             -> Map PackageName (Map FlagName Bool)
             -> Map FlagName Bool
             -> Map FlagName Bool
composeFlags pname pflags gflags = collapse pflags <> gflags
  where collapse :: Map PackageName (Map FlagName Bool) -> Map FlagName Bool
        collapse = fromMaybe mempty . M.lookup pname

--------------------------------------------------------------------------------
-- Package fetching

-- | Fetch and unpack the package.
withTempUnpacked :: (MonadIO m,MonadThrow m,MonadLogger m,MonadMask m,MonadReader env m,HasHttpManager env,HasConfig env)
                 => [PackageIdentifier]
                 -> ([Path Abs Dir] -> m a)
                 -> m a
withTempUnpacked pkgs inner = withSystemTempDirectory "stack-unpack" $ \tmp -> do
    dest <- parseAbsDir tmp
    dirs <- fetchPackages $ map (, Just dest) pkgs
    inner dirs

--------------------------------------------------------------------------------
-- Paths

-- | Path to .shake files.
shakeFilesPath :: Path Abs Dir -> Path Abs File
shakeFilesPath dir =
  dir </>
  $(mkRelFile ".shake")

-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Path b Dir -> Bool
isHiddenDir = isPrefixOf "." . toFilePath . dirname

--------------------------------------------------------------------------------
-- | Check that the GHC on the PATH matches the expected GHC
checkGHCVersion :: (MonadIO m, MonadThrow m, MonadReader env m, HasBuildConfig env)
                => m ()
checkGHCVersion = do
    bs <- readProcessStdout "ghc" ["--numeric-version"]
    actualVersion <- parseVersion $ S8.takeWhile isValidChar bs
    bconfig <- asks getBuildConfig
    when (getMajorVersion actualVersion /= getMajorVersion (bcGhcVersion bconfig))
        $ throwM $ GHCVersionMismatch actualVersion (bcGhcVersion bconfig)
  where
    isValidChar '.' = True
    isValidChar c = '0' <= c && c <= '9'
