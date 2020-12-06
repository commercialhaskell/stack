{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cache management during a build.
-- Handle all the pre-installed, precompiled & prebuilt stuff.
module Stack.Build.Precompiled
(
    copyPreCompiled
    , getPrecompiled
    , loadInstalledPkg
    , getExecutableBuildStatuses
) where

import           Stack.Prelude
import           Stack.GhcPkg
import           Stack.Types.Build
import           Stack.Types.Package
import           Stack.Types.Config
import           Stack.Types.Execute
import           Stack.Types.GhcPkgId
import           Stack.Build.Cache
import           Stack.PackageDump (conduitDumpPackage, ghcPkgDescribe)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Path.Extra (toFilePathNoTrailingSep)
import Path (
    filename, (</>), toFilePath, isProperPrefixOf, parseRelFile,
    parseRelDir
    )
import qualified Data.Conduit.List as CL
import RIO.Process (HasProcessContext, withModifyEnvVars, proc, readProcess_)
import Path.IO (ensureDir, copyFile, doesFileExist)
import System.PosixCompat.Files (createLink)
import Distribution.System (OS (..), Platform (..))
import Stack.Constants (relDirBuild)
import Stack.Constants.Config (distDirFromDir)

getPrecompiled :: HasEnvConfig env =>
    TaskType
    -> BaseConfigOpts
    -> ConfigCache
    -> RIO env (Maybe (PrecompiledCache Abs))
getPrecompiled taskTypeVal envConfigOpts cache =
    case taskTypeVal of
        TTRemotePackage Immutable _ loc -> do
            mpc <- readPrecompiledCache
                   loc
                   (configCacheOpts cache)
                   (configCacheHaddock cache)
                   (configCacheDeps cache)
            case mpc of
                Nothing -> return Nothing
                -- Only pay attention to precompiled caches that refer to packages within
                -- the snapshot.
                Just pc | maybe False
                                (bcoSnapInstallRoot envConfigOpts `isProperPrefixOf`)
                                (pcLibrary pc) ->
                    return Nothing
                -- If old precompiled cache files are left around but snapshots are deleted,
                -- it is possible for the precompiled file to refer to the very library
                -- we're building, and if flags are changed it may try to copy the library
                -- to itself. This check prevents that from happening.
                Just pc -> do
                    let allM _ [] = return True
                        allM f (x:xs) = do
                            b <- f x
                            if b then allM f xs else return False
                    b <- liftIO $ allM doesFileExist $ maybe id (:) (pcLibrary pc) $ pcExes pc
                    return $ if b then Just pc else Nothing
        _ -> return Nothing

copyPreCompiled :: HasEnvConfig env =>
    PackageName
    -> ExecuteEnv
    -> Task
    -> PrecompiledCache b0
    -> RIO env (Maybe Installed)
copyPreCompiled pname execEnv task (PrecompiledCache mlib sublibs exes) = do
    wc <- view $ actualCompilerVersionL.whichCompilerL
    announceTask execEnv task "using precompiled package"
    -- We need to copy .conf files for the main library and all sublibraries which exist in the cache,
    -- from their old snapshot to the new one. However, we must unregister any such library in the new
    -- snapshot, in case it was built with different flags.
    let
      subLibNames = map T.unpack . Set.toList $ case taskType task of
        TTLocalMutable lp -> packageInternalLibraries $ lpPackage lp
        TTRemotePackage _ p _ -> packageInternalLibraries p
      PackageIdentifier name version = taskProvides task
      mainLibName = packageNameString name
      mainLibVersion = versionString version
      ghcPkgName = mainLibName ++ "-" ++ mainLibVersion
      -- z-package-z-internal for internal lib internal of package package
      toCabalInternalLibName n = concat ["z-", mainLibName, "-z-", n, "-", mainLibVersion]
      allToUnregister = map (const ghcPkgName) (maybeToList mlib) ++ map toCabalInternalLibName subLibNames
      allToRegister = maybeToList mlib ++ sublibs
    unless (null allToRegister) $ do
        withMVar installLock $ \() -> do
            -- We want to ignore the global and user databases.
            -- Unfortunately, ghc-pkg doesn't take such arguments on the
            -- command line. Instead, we'll set GHC_PACKAGE_PATH. See:
            -- https://github.com/commercialhaskell/stack/issues/1146
            let modifyEnv = Map.insert
                  (ghcPkgPathEnvVar wc)
                  (T.pack $ toFilePathNoTrailingSep $ bcoSnapDB configOpts)
            withModifyEnvVars modifyEnv $ do
              GhcPkgExe ghcPkgExe <- getGhcPkgExe
              -- first unregister everything that needs to be unregistered
              forM_ allToUnregister $ \pName -> catchAny
                  (readProcessNull (toFilePath ghcPkgExe) [ "unregister", "--force", pName])
                  (const (return ()))
              -- now, register the cached conf files
              forM_ allToRegister $ \libpath ->
                proc (toFilePath ghcPkgExe) [ "register", "--force", toFilePath libpath] readProcess_
    liftIO $ forM_ exes $ \exe -> do
        ensureDir bindir
        let dst = bindir </> filename exe
        createLink (toFilePath exe) (toFilePath dst) `catchIO` \_ -> copyFile exe dst
    case (mlib, exes) of
        (Nothing, _:_) -> markExeInstalled (taskLocation task) taskProvidesVal
        _ -> return ()
    -- Find the package in the database
    let pkgDbs = [bcoSnapDB configOpts]
    case mlib of
        Nothing -> return $ Just $ Executable taskProvidesVal
        Just _ -> do
            mpkgid <- loadInstalledPkg pkgDbs snapshotDumpPkg pname
            return $ Just $
                case mpkgid of
                    Nothing -> assert False $ Executable taskProvidesVal
                    Just pkgid -> Library taskProvidesVal pkgid Nothing
  where
    installLock = eeInstallLock execEnv
    snapshotDumpPkg = eeSnapshotDumpPkgs execEnv
    configOpts = eeBaseConfigOpts execEnv
    taskProvidesVal = taskProvides task
    bindir = bcoSnapInstallRoot configOpts </> bindirSuffix

loadInstalledPkg :: (HasCompiler env,
    HasProcessContext env, HasLogFunc env) =>
    [Path Abs Dir]
    -> TVar (Map GhcPkgId DumpPackage)
    -> PackageName
    -> RIO env (Maybe GhcPkgId)
loadInstalledPkg pkgDbs tvar name = do
    pkgexe <- getGhcPkgExe
    dps <- ghcPkgDescribe pkgexe name pkgDbs $ conduitDumpPackage .| CL.consume
    case dps of
        [] -> return Nothing
        [dp] -> do
            liftIO $ atomically $ modifyTVar' tvar (Map.insert (dpGhcPkgId dp) dp)
            return $ Just (dpGhcPkgId dp)
        _ -> error $ "singleBuild: invariant violated: multiple results when describing installed package " ++ show (name, dps)

-- | Get the build status of all the package executables.
-- As opposed to libraries, we do not have a way to get installed packages through
-- ghc-pkg for executables, so we test whether their expected output file exists
-- e.g.
--
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha.exe
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha.jsexe/ (NOTE: a dir)
getExecutableBuildStatuses
    :: HasEnvConfig env
    => Package -> Path Abs Dir -> RIO env (Map Text ExecutableBuildStatus)
getExecutableBuildStatuses package pkgDir = do
    distDir <- distDirFromDir pkgDir
    platform <- view platformL
    fmap
        Map.fromList
        (mapM (checkExeStatus platform distDir) (Set.toList (packageExes package)))

-- | Check whether the given executable is defined in the given dist directory.
checkExeStatus
    :: HasLogFunc env
    => Platform
    -> Path b Dir
    -> Text
    -> RIO env (Text, ExecutableBuildStatus)
checkExeStatus platform distDir name = do
    exename <- parseRelDir (T.unpack name)
    exists <- checkPath (distDir </> relDirBuild </> exename)
    pure
        ( name
        , if exists
              then ExecutableBuilt
              else ExecutableNotBuilt)
  where
    checkPath base =
        case platform of
            Platform _ Windows -> do
                fileandext <- parseRelFile (file ++ ".exe")
                doesFileExist (base </> fileandext)
            _ -> do
                fileandext <- parseRelFile file
                doesFileExist (base </> fileandext)
      where
        file = T.unpack name
