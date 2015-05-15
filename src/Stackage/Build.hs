{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Build project(s).

module Stackage.Build
  (build
  ,test
  ,haddock
  ,benchmark
  ,clean
  ,shakeFilesPath
  ,configDir
  ,getPackageInfos)
  where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Logger (runNoLoggingT,MonadLogger)
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString as S (readFile)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$),($=))
import           Data.Conduit.Binary (sinkIOHandle,sourceHandle)
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Either
import           Data.Function
import           Data.IORef
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set.Monad as S
import           Data.Streaming.Process
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.Shake hiding (doesFileExist,doesDirectoryExist,getDirectoryContents)
import           Distribution.Package hiding (packageName,packageVersion,Package,PackageName)
import           Distribution.Version
import           Path as FL
import           Path.Find
import           Prelude hiding (FilePath,writeFile)
import           Stackage.Build.Config
import           Stackage.Build.Defaults
import           Stackage.Build.Doc
import           Stackage.Build.Types
import           Stackage.Fetch
import           Stackage.GhcPkg
import           Stackage.GhcPkgId
import           Stackage.Package
import           Stackage.PackageIndex
import           Stackage.PackageName
import           Stackage.PackageVersion
import           Stackage.Resolve
import           System.Directory hiding (findFiles)
import           System.Environment
import qualified System.FilePath as FilePath
import           System.IO (openFile,IOMode(..),writeFile)
import           System.Posix.Files (createSymbolicLink,removeLink)

--------------------------------------------------------------------------------
-- Top-level commands

-- | Build and test using Shake.
test :: TestConfig -> IO ()
test conf =
  build (BuildConfig (tconfigTargets conf)
                     (tconfigVerbosity conf)
                     False
                     False
                     Nothing
                     DoTests
                     False
                     []
                     (tconfigInDocker conf))

-- | Build and haddock using Shake.
haddock :: HaddockConfig -> IO ()
haddock conf =
  build (BuildConfig (hconfigTargets conf)
                     (hconfigVerbosity conf)
                     False
                     False
                     Nothing
                     DoHaddock
                     False
                     []
                     (hconfigInDocker conf))

-- | Build and benchmark using Shake.
benchmark :: BenchmarkConfig -> IO ()
benchmark conf =
  build (BuildConfig (benchTargets conf)
                     (benchVerbosity conf)
                     False
                     False
                     Nothing
                     DoBenchmarks
                     False
                     []
                     (benchInDocker conf))

-- | Build using Shake.
build :: BuildConfig -> IO ()
build bconfig =
  do cfg <- readConfig
     pinfos <-
       runNoLoggingT (runResourceT (getPackageInfos (bconfigFinalAction bconfig)
                                    (Just bconfig)
                                    cfg))
     pkgIds <-
       getPackageIds (map packageName (S.toList pinfos))
     pwd <- getCurrentDirectory >>= parseAbsDir
     docLoc <- getUserDocLoc
     installResource <-
       newResourceIO "cabal install" 1
     plans <-
       forM (S.toList pinfos)
            (\pinfo ->
               do let wantedTarget =
                        wanted pwd pinfo
                  when (wantedTarget && bconfigFinalAction bconfig /= DoNothing)
                       (liftIO (deleteGenFile (packageDir pinfo)))
                  gconfig <-
                    readGenConfigFile pkgIds
                                      (packageName pinfo)
                                      bconfig
                                      (packageDir pinfo)
                                      wantedTarget
                                      pinfo
                  return (makePlan pkgIds
                                   wantedTarget
                                   bconfig
                                   gconfig
                                   pinfos
                                   pinfo
                                   installResource
                                   docLoc))
     if bconfigDryrun bconfig
        then dryRunPrint pinfos
        else withArgs []
                      (shakeArgs shakeOptions {shakeVerbosity = bconfigVerbosity bconfig
                                              ,shakeFiles =
                                                 FL.toFilePath (shakeFilesPath cfg)
                                              ,shakeThreads = defaultShakeThreads}
                                 (do sequence_ plans
                                     when (bconfigFinalAction bconfig ==
                                           DoHaddock)
                                          (buildDocIndex (wanted pwd)
                                                         docLoc
                                                         pinfos)))
  where wanted pwd pinfo =
          case bconfigTargets bconfig of
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
clean :: IO ()
clean =
  do cfg <- readConfig
     pinfos <-
       runNoLoggingT (runResourceT (getPackageInfos DoNothing Nothing cfg))
     forM_ (S.toList pinfos)
           (\pinfo ->
              do deleteGenFile (packageDir pinfo)
                 let distDir =
                       FL.toFilePath (distDirFromDir (packageDir pinfo))
                 exists <- doesDirectoryExist distDir
                 when exists (removeDirectoryRecursive distDir))
     let listDir = FL.parent (shakeFilesPath cfg)
     ls <-
       fmap (map (FL.toFilePath listDir ++))
            (getDirectoryContents (FL.toFilePath listDir))
     mapM_ (rmShakeMetadata cfg) ls
  where rmShakeMetadata cfg p =
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
         -> BuildConfig
         -> GenConfig
         -> Set Package
         -> Package
         -> Resource
         -> Path Abs Dir
         -> Rules ()
makePlan pkgIds wanted bconfig gconfig pinfos pinfo installResource docLoc =
  do when wanted (want [target])
     target %>
       \_ ->
         do needDependencies pkgIds bconfig pinfos pinfo
            needTools pinfo
            needSourceFiles
            removeAfterwards <- liftIO (ensureSetupHs dir)
            actionFinally
              (buildPackage
                 bconfig
                 pinfos
                 pinfo
                 gconfig
                 (if wanted
                     then bconfigFinalAction bconfig
                     else DoNothing)
                 installResource
                 docLoc)
              removeAfterwards
            writeFinalFiles gconfig dir (packageName pinfo)
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
                          liftIO (throwIO (FPMissingTool dep))
                        _ -> return ())
                (packageTools pinfo))

-- | Specify that the given package needs the following other
-- packages.
needDependencies :: Map PackageName GhcPkgId
                 -> BuildConfig
                 -> Set Package
                 -> Package
                 -> Action ()
needDependencies pkgIds bconfig pinfos pinfo =
  do deps <- mapM (\pinfo' ->
                     let dir' = packageDir pinfo'
                         genFile = builtFileFromDir dir'
                     in do void (liftIO (readGenConfigFile pkgIds
                                                           (packageName pinfo')
                                                           bconfig
                                                           dir'
                                                           False
                                                           pinfo'))
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
writeFinalFiles :: MonadIO m => GenConfig -> Path Abs Dir -> PackageName -> m ()
writeFinalFiles gconfig dir name =
  liftIO (do mpkigid <- findPackageId name
             case mpkigid of
               Nothing -> throwIO (FPCouldn'tFindPkgId name)
               Just pkgid ->
                 do writeGenConfigFile
                      dir
                      gconfig {gconfigForceRecomp = False
                              ,gconfigPkgId = pkgid}
                    -- After a build has completed successfully for a given
                    -- configuration, no recompilation forcing is required.
                    updateGenFile dir)

-- | Build the given package with the given configuration.
buildPackage :: BuildConfig
             -> Set Package
             -> Package
             -> GenConfig
             -> FinalAction
             -> Resource
             -> Path Abs Dir
             -> Action ()
buildPackage bconfig pinfos pinfo gconfig setupAction installResource docLoc =
  do liftIO (void (try (removeFile (FL.toFilePath (buildLogPath pinfo))) :: IO (Either IOException ())))
     runhaskell
       pinfo
       (concat [["configure","--user"]
               ,["--enable-library-profiling" | gconfigLibProfiling gconfig]
               ,["--enable-executable-profiling" | gconfigExeProfiling gconfig]
               ,["--enable-tests" | setupAction == DoTests]
               ,["--enable-benchmarks" | setupAction == DoBenchmarks]
               ,map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       T.unpack name)
                    (M.toList (packageFlags pinfo))])
     runhaskell
       pinfo
       (concat [["build"]
               ,["--ghc-options=-O2" | gconfigOptimize gconfig]
               ,["--ghc-options=-fforce-recomp" | gconfigForceRecomp gconfig]
               ,concat [["--ghc-options",T.unpack opt] | opt <- bconfigGhcOptions bconfig]])
     case setupAction of
       DoTests ->
         runhaskell pinfo
                    ["test"]
       DoHaddock ->
           do liftIO (removeDocLinks docLoc pinfo)
              ifcOpts <- liftIO (haddockInterfaceOpts docLoc pinfo pinfos)
              runhaskell pinfo
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
                    ["bench"]
       _ -> return ()
     withResource installResource 1
                  (runhaskell pinfo
                              ["install","--user"])
     case setupAction of
       DoHaddock -> liftIO (createDocLinks docLoc pinfo)
       _ -> return ()

-- | Run the Haskell command for the given package.
runhaskell :: Package -> [String] -> Action ()
runhaskell pinfo args =
  do liftIO (createDirectoryIfMissing True
                                      (FL.toFilePath (stackageBuildDir pinfo)))
     putQuiet display
     outRef <- liftIO (newIORef mempty)
     errRef <- liftIO (newIORef mempty)
     join (liftIO (catch (do withCheckedProcess
                               cp {cwd =
                                     Just (FL.toFilePath dir)
                                  ,std_err = Inherit}
                               (\ClosedStream stdout stderr ->
                                  do logFrom stdout outRef
                                     logFrom stderr errRef)
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
                   -> BuildConfig
                   -> GenConfig
                   -> PackageName
                   -> Package
                   -> Bool
genFileInvalidated pkgIds bconfig gconfig pname pinfo =
  or [installedPkgIdChanged
     ,optimizationsChanged
     ,profilingChanged
     ,ghcOptsChanged
     ,flagsChanged]
  where installedPkgIdChanged =
          Just (gconfigPkgId gconfig) /=
          M.lookup pname pkgIds
        ghcOptsChanged = bconfigGhcOptions bconfig /= gconfigGhcOptions gconfig
        profilingChanged =
          (bconfigLibProfile bconfig &&
           not (gconfigLibProfiling gconfig)) ||
          (bconfigExeProfile bconfig &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          case bconfigEnableOptimizations bconfig of
            Just optimize
              | optimize /= gconfigOptimize gconfig && optimize -> True
            _ -> False
        flagsChanged = packageFlags pinfo /= gconfigFlags gconfig

-- | Should the generated config be updated?
genFileChanged :: Map PackageName GhcPkgId
               -> BuildConfig
               -> GenConfig
               -> PackageName
               -> Package
               -> Bool
genFileChanged pkgIds bconfig gconfig pname pinfo =
  or [installedPkgIdChanged
     ,optimizationsChanged
     ,profilingChanged
     ,ghcOptsChanged
     ,flagsChanged]
  where installedPkgIdChanged =
          Just (gconfigPkgId gconfig) /=
          M.lookup pname pkgIds
        ghcOptsChanged = bconfigGhcOptions bconfig /= gconfigGhcOptions gconfig
        profilingChanged =
          (bconfigLibProfile bconfig &&
           not (gconfigLibProfiling gconfig)) ||
          (bconfigExeProfile bconfig &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          maybe False (/= gconfigOptimize gconfig) (bconfigEnableOptimizations bconfig)
        flagsChanged =
          packageFlags pinfo /=
          gconfigFlags gconfig

-- | Write out the gen file for the build dir.
updateGenFile :: Path Abs Dir -> IO ()
updateGenFile dir =
  L.writeFile (FL.toFilePath (builtFileFromDir dir))
              ""

-- | Delete the gen file, which will cause a rebuild.
deleteGenFile :: Path Abs Dir -> IO ()
deleteGenFile dir =
  catch (removeFile (FL.toFilePath (builtFileFromDir dir)))
        (\(_ :: IOException) -> return ())

-- | Save generated configuration.
writeGenConfigFile :: Path Abs Dir -> GenConfig -> IO ()
writeGenConfigFile dir gconfig =
  do createDirectoryIfMissing True (FL.toFilePath (FL.parent (builtConfigFileFromDir dir)))
     L.writeFile (FL.toFilePath (builtConfigFileFromDir dir))
                 (encode gconfig)

-- | Read the generated config file, or return a default based on the
-- build configuration.
readGenConfigFile :: Map PackageName GhcPkgId
                  -> PackageName
                  -> BuildConfig
                  -> Path Abs Dir
                  -> Bool
                  -> Package
                  -> IO GenConfig
readGenConfigFile pkgIds name bconfig dir wanted pinfo =
  do bytes <-
       catch (fmap Just (S.readFile (FL.toFilePath fp)))
             (\(_ :: IOException) ->
                do {-putStrLn ("(No config file for " ++ printPName name ++ ": One will be created.)")-}
                   return Nothing)
     case bytes >>= decode . L.fromStrict of
       Just gconfig ->
         if genFileChanged pkgIds bconfig gconfig name pinfo
            then
                 -- If the build config has changed such that the gen
                 -- config needs to be regenerated...
                 do let invalidated =
                          genFileInvalidated pkgIds bconfig gconfig name pinfo
                    when (invalidated || wanted)
                         (deleteGenFile dir)
                    let gconfig' =
                          (newConfig gconfig bconfig pinfo) {gconfigForceRecomp = invalidated}
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
            let gconfig' = newConfig def bconfig pinfo
            writeGenConfigFile dir gconfig'
            return gconfig' -- Probably doesn't exist or is out of date (not parseable.)
  where fp = builtConfigFileFromDir dir

-- | Update a gen configuration using the build configuration.
newConfig :: GenConfig -- ^ Build configuration.
          -> BuildConfig -- ^ A base gen configuration.
          -> Package
          -> GenConfig
newConfig gconfig bconfig pinfo =
  def {gconfigOptimize =
         maybe (gconfigOptimize gconfig)
               id
               (bconfigEnableOptimizations bconfig)
      ,gconfigLibProfiling = bconfigLibProfile bconfig ||
                             gconfigLibProfiling gconfig
      ,gconfigExeProfiling = bconfigExeProfile bconfig ||
                             gconfigExeProfiling gconfig
      ,gconfigGhcOptions = bconfigGhcOptions bconfig
      ,gconfigFlags = packageFlags pinfo
      ,gconfigPkgId = gconfigPkgId gconfig}

--------------------------------------------------------------------------------
-- Package info/dependencies/etc

-- | Get packages' information.
getPackageInfos :: (MonadBaseControl IO m,MonadIO m,MonadLogger m,MonadThrow m,MonadResource m,MonadMask m)
                => FinalAction -> Maybe BuildConfig -> Config -> m (Set Package)
getPackageInfos finalAction mbconfig = go True
  where go retry cfg =
          do globalPackages <- getAllPackages
             {-liftIO (putStrLn ("All global packages: " ++ show globalPackages))-}
             (infos,errs) <-
               runWriterT
                 (buildDependencies finalAction
                                    (configFlags cfg)
                                    globalPackages
                                    (configPackages cfg)
                                    (configPackageFlags cfg))
             {-liftIO (putStrLn ("Erroneous packages: " ++ show errs))-}
             case errs of
               [] -> return infos
               _
                 | Just bconfig <- mbconfig
                 , not (bconfigInDocker bconfig)
                 , retry ->
                   outsideOfDockerApproach infos globalPackages cfg errs
                 | otherwise ->
                   liftIO (throwIO (FPDependencyIssues (nubBy (on (==) show) errs)))
        outsideOfDockerApproach infos globalPackages cfg errs =
          do indexDir <- liftIO getIndexDir
             index <- loadPkgIndex indexDir
             results <- forM names (checkPackageInIndex index)
             case lefts results of
               [] ->
                 do let okPkgVers = M.fromList (rights results)
                    mapping <-
                      resolvePackageVersions (M.keys okPkgVers)
                    validated <-
                      liftM catMaybes (mapM (validateSuggestion globalPackages okPkgVers) mapping)
                    case validated of
                      [] -> do {-liftIO (putStrLn "No validated packages!")-}
                               return infos
                      toFetch ->
                        do {-liftIO (putStrLn ("Fetching: " ++ show toFetch))-}
                           newPackageDirs <-
                             forM toFetch
                                  (\(PackageSuggestion name ver _flags) ->
                                     runNoLoggingT (fetchPackage index name ver))
                           -- Here is where we inject third-party
                           -- dependencies and their flags and re-run
                           -- this function.
                           {-liftIO (putStrLn "Looping...")-}
                           go
                             False
                             cfg {configPackages = configPackages cfg <>
                                                   S.fromList newPackageDirs
                                 ,configPackageFlags =
                                    configPackageFlags cfg <>
                                    mconcat (map (\s ->
                                                    M.singleton (suggestionName s)
                                                                (suggestionFlags s))
                                                 validated)}
               erroneous ->
                 liftIO (throwIO (FPDependencyIssues
                                    (map snd (mapMaybe (flip lookup names) erroneous))))
          where names = getMissingDeps errs

-- | Extract the missing dependencies and their version ranges, but
-- keeping hold of the original exception for later use.
getMissingDeps :: [StackageBuildException] -> [(PackageName,(VersionRange,StackageBuildException))]
getMissingDeps =
  mapMaybe (\x ->
              case x of
                FPMissingDep _ name range ->
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
                   => Map PackageName PackageVersion
                   -> Map PackageName VersionRange
                   -> PackageSuggestion
                   -> m (Maybe PackageSuggestion)
validateSuggestion globalPackages okPkgVers =
  (\suggestion@(PackageSuggestion name suggestedVer _) ->
     case M.lookup name okPkgVers of
       Just range
         | not (withinRange (toCabalVersion suggestedVer) range) ->
           liftIO (throwIO (FPStackageDepVerMismatch name suggestedVer range))
       _ ->
         case M.lookup name globalPackages of
           Just installedVer
             | installedVer == suggestedVer ->
               return Nothing
             | otherwise ->
               liftIO (throwIO (FPStackagePackageVersionMismatch name suggestedVer installedVer))
           Nothing ->
             return (Just suggestion))

-- | Check that a package is actually available in the general (Hackage) package
-- index.
checkPackageInIndex :: (MonadIO m, MonadLogger m,MonadThrow m)
                    => PackageIndex
                    -> (PackageName,(VersionRange,t))
                    -> m (Either PackageName (PackageName,VersionRange))
checkPackageInIndex index (name,(range,_)) =
  do mversions <- getPkgVersions index name
     case mversions of
       Just vers
         | any (flip withinRange range . toCabalVersion)
               (S.toList vers) ->
           return (Right (name,range))
         | otherwise -> return (Left name)
       Nothing -> return (Right (name,range))

-- | Build a map of package names to dependencies.
buildDependencies :: MonadIO m
                  => FinalAction
                  -> Map Text Bool
                  -> Map PackageName PackageVersion
                  -> Set (Path Abs Dir)
                  -> Map PackageName (Map Text Bool)
                  -> WriterT [StackageBuildException] m (Set Package)
buildDependencies finalAction flags globals packages pflags =
  do pkgs <-
       liftIO (S.mapM (getPackageInfo finalAction flags pflags) packages)
     S.mapM (sievePackages
               globals
               (M.fromList
                  (map (packageName &&& packageVersion)
                       (S.toList pkgs))))
            pkgs

-- | Remove third-party package dependencies, e.g. mtl, bytestring,
-- etc.
sievePackages :: MonadIO m
              => Map PackageName PackageVersion
              -> Map PackageName PackageVersion
              -> Package
              -> WriterT [StackageBuildException] m Package
sievePackages globalNameVersions localNameVersions p =
  do deps <-
       filterM (\(name,range) ->
                  case M.lookup name localNameVersions of
                    Just ver | withinRange (toCabalVersion ver) range -> return True
                             | otherwise -> do tell [FPMissingDep p name range]
                                               return False
                    Nothing -> case M.lookup name globalNameVersions of
                                 Just ver
                                   | withinRange (toCabalVersion ver) range ->
                                     return False
                                   | otherwise ->
                                     do tell [FPMissingDep p name range]
                                        return False
                                 Nothing ->
                                   do tell [FPMissingDep p name range]
                                      return False)
               (M.toList (packageDeps p))
     return (p {packageDeps = M.fromList deps})

-- | Get the package name and dependencies from the given package
-- directory.
getPackageInfo :: FinalAction
               -> Map Text Bool
               -> Map PackageName (Map Text Bool)
               -> Path Abs Dir
               -> IO Package
getPackageInfo finalAction flags pflags pkgDir =
  do mcabal <-
       findFileUp
         pkgDir
         (flip hasExtension "cabal" .
          FL.toFilePath)
         (Just pkgDir)
     case mcabal of
       Nothing -> throwIO (FPNoCabalFile pkgDir)
       Just cabal ->
         do pname <- parsePackageNameFromFilePath cabal
            info <-
              runNoLoggingT
                (readPackage (cfg pname)
                             cabal)
            existing <-
              fmap S.fromList
                   (filterM (doesFileExist . FL.toFilePath)
                            (S.toList (packageFiles info)))
            return info {packageFiles = existing}
  where hasExtension fp x = FilePath.takeExtensions fp == "." ++ x
        cfg pname =
          PackageConfig {packageConfigEnableTests =
                           case finalAction of
                             DoTests -> True
                             _ -> False
                        ,packageConfigEnableBenchmarks =
                           case finalAction of
                             DoBenchmarks -> True
                             _ -> False
                        ,packageConfigFlags =
                           composeFlags pname pflags flags}

-- | Compose the package flags with the global flags in a left-biased
-- form, i.e., package-specific flags will be preferred over global
-- flags.
composeFlags :: PackageName
             -> Map PackageName (Map Text Bool)
             -> Map Text Bool
             -> Map Text Bool
composeFlags pname pflags gflags = collapse pflags <> gflags
  where collapse :: Map PackageName (Map Text Bool) -> Map Text Bool
        collapse = fromMaybe mempty . M.lookup pname

--------------------------------------------------------------------------------
-- Paths

-- | Path to .shake files.
shakeFilesPath :: Config -> Path Abs File
shakeFilesPath cfg =
  configDir cfg </>
  $(mkRelFile ".shake")

-- | Directory of configuration file, or throws 'FPNoConfigFile' if no configuration file.
configDir :: Config -> Path Abs Dir
configDir cfg =
  case configMaybeDir cfg of
    Just d -> d
    Nothing -> throw FPNoConfigFile

-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Path b Dir -> Bool
isHiddenDir = isPrefixOf "." . toFilePath . dirname
