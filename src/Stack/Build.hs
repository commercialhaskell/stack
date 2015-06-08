{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Build project(s).

module Stack.Build
  (build
  ,clean)
  where

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import qualified Data.ByteString.Char8 as S8
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.Package (Dependency (..))
import           Distribution.Version (intersectVersionRanges, anyVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import           Prelude hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Execute
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal

{- EKB TODO: doc generation for stack-doc-server
#ifndef mingw32_HOST_OS
import           System.Posix.Files (createSymbolicLink,removeLink)
#endif
--}

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)

data AddDepRes
    = ADRToInstall Task
    | ADRFound Version Installed
    deriving Show

type S = Map PackageName (Either ConstructPlanException AddDepRes)

adrVersion :: AddDepRes -> Version
adrVersion (ADRToInstall task) = packageIdentifierVersion $ taskProvides task
adrVersion (ADRFound v _) = v

constructPlan :: forall env m.
                 M env m
              => MiniBuildPlan
              -> BaseConfigOpts
              -> [LocalPackage]
              -> [PackageName] -- ^ additional packages that must be built
              -> Set GhcPkgId -- ^ locally registered
              -> (PackageName -> Version -> Map FlagName Bool -> m Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> m Plan
constructPlan mbp baseConfigOpts locals extraToBuild locallyRegistered loadPackage sourceMap installedMap = do
    m <- flip execStateT M.empty $ do
        let allTargets = Set.fromList
                       $ map (packageName . lpPackage) locals ++ extraToBuild
        mapM_ (addDep []) $ Set.toList allTargets
    let toEither (_, Left e)  = Left e
        toEither (k, Right v) = Right (k, v)
    case partitionEithers $ map toEither $ M.toList m of
        ([], adrs) -> do
            let toTask (_, ADRFound _ _) = Nothing
                toTask (name, ADRToInstall task) = Just (name, task)
                tasks = M.fromList $ mapMaybe toTask adrs
            return Plan
                { planTasks = tasks
                , planUnregisterLocal = mkUnregisterLocal tasks locallyRegistered
                }
        (errs, _) -> throwM $ ConstructPlanExceptions errs
  where
    addDep :: [PackageName] -- ^ call stack
           -> PackageName
           -> StateT S m (Either ConstructPlanException AddDepRes)
    addDep callStack name = do
        m <- get
        case M.lookup name m of
            Just res -> return res
            Nothing -> do
                res <- addDep' callStack name
                modify $ Map.insert name res
                return res

    addDep' callStack name | name `elem` callStack =
        return $ Left $ DependencyCycleDetected $ name : callStack
    addDep' callStack0 name = do
        case M.lookup name installedMap of
            Nothing ->
                case M.lookup name sourceMap of
                    Nothing -> return $ Left $ UnknownPackage name
                    Just (PSLocal lp) -> installLocalPackage callStack lp
                    Just (PSUpstream version loc flags) -> installUpstream callStack name version loc flags
            Just (version, Snap, installed) -> return $ Right $ ADRFound version installed
            Just (version, Local, installed) -> checkDirty callStack name version installed
      where
        callStack = name : callStack0

    installLocalPackage callStack lp = do
        eres <- checkPackage callStack (lpPackage lp)
        case eres of
            Left e -> return $ Left e
            Right (present, missing) -> return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier
                    (packageName $ lpPackage lp)
                    (packageVersion $ lpPackage lp)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            baseConfigOpts
                            allDeps
                            (lpWanted lp)
                            Local
                            (packageFlags $ lpPackage lp)
                , taskType = TTLocal lp AllSteps
                }

    installUpstream callStack name version loc flags = do
        package <- lift $ loadPackage name version flags
        eres <- checkPackage callStack package
        case eres of
            Left e -> return $ Left e
            Right (present, missing) -> return $ Right $ ADRToInstall Task
                { taskProvides = PackageIdentifier name version
                , taskType = TTUpstream package loc
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Set.union present missing'
                     in configureOpts
                            baseConfigOpts
                            allDeps
                            False
                            loc
                            flags
                }

    -- Check if a locally installed package is dirty and must be reinstalled
    checkDirty callStack name version installed =
        case M.lookup name sourceMap of
            Nothing -> return $ Right $ ADRFound version installed
            Just (PSLocal lp) -> assert (version == packageVersion (lpPackage lp)) $ do
                cpr <- checkPackage callStack $ lpPackage lp
                case cpr of
                    Left e -> return $ Left e
                    Right (present, missing) -> do
                        let configOpts = configureOpts baseConfigOpts present (lpWanted lp) Local (packageFlags $ lpPackage lp)
                        let mneededSteps
                                | not $ Set.null missing = Just AllSteps
                                | Just configOpts /= lpLastConfigOpts lp
                                    = Just AllSteps
                                | lpDirtyFiles lp = Just SkipConfig
                                | lpWanted lp = Just JustFinal -- FIXME this currently causes too much recompilation
                                | otherwise = Nothing
                        return $ Right $
                            case mneededSteps of
                                Nothing -> ADRFound version installed
                                Just neededSteps -> ADRToInstall Task
                                    { taskProvides = PackageIdentifier name version
                                    , taskType = TTLocal lp neededSteps
                                    , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                                        let allDeps = Set.union present missing'
                                         in configureOpts
                                                baseConfigOpts
                                                allDeps
                                                (lpWanted lp)
                                                Local
                                                (packageFlags $ lpPackage lp)
                                    }
            Just (PSUpstream version' loc flags) -> assert (version == version') $
                case loc of
                    Snap -> return $ Right $ ADRFound version installed
                    Local -> do
                        package <- lift $ loadPackage name version flags
                        eres <- checkPackage callStack package
                        let toInstall present missing = Right $ ADRToInstall Task
                                    { taskProvides = PackageIdentifier name version
                                    , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                                        let allDeps = Set.union present missing'
                                         in configureOpts
                                                baseConfigOpts
                                                allDeps
                                                False
                                                Local
                                                (packageFlags package)
                                    , taskType = TTUpstream package loc
                                    }
                        case eres of
                            Left e -> return $ Left e
                            Right (present, missing)
                                | Set.null missing ->
                                    case installed of
                                        Library gid -> do
                                            oldFlags <- tryGetFlagCache gid
                                            if oldFlags == Just flags
                                                then return $ Right $ ADRFound version installed
                                                else return $ toInstall present missing
                                        Executable -> return $ Right $ ADRFound version installed -- TODO track flags for executables too
                                | otherwise -> return $ toInstall present missing

    -- Check all of the dependencies for the given package
    checkPackage :: [PackageName] -- ^ call stack
                 -> Package
                 -> StateT S m (Either ConstructPlanException (Set GhcPkgId, Set PackageIdentifier))
    checkPackage callStack package = do
        eress <- forM (M.toList $ packageDepsWithTools package) $ \(name, range) -> do
            eres <- addDep callStack name
            case eres of
                Left _e -> return $ Left name -- FIXME do something better with this?
                Right adr
                    | adrVersion adr `withinRange` range -> return $ Right adr
                    | otherwise -> do
                        -- TODO change exception setup so we can give a meaningful error message about ranges here
                        return $ Left name
        case partitionEithers eress of
            ([], adrs) ->
                let loop present missing [] = (present, missing)
                    loop present missing (x:xs) =
                        case x of
                            ADRToInstall t -> loop present (Set.insert (taskProvides t) missing) xs
                            ADRFound _ Executable -> loop present missing xs
                            ADRFound _ (Library gid) -> loop (Set.insert gid present) missing xs
                 in return $ Right $ loop Set.empty Set.empty adrs
            (errs, _) -> return $ Left $ DependencyPlanFailures (packageName package) (Set.fromList errs)

    toolMap = getToolMap mbp
    toolToPackages (Dependency name _) =
        Map.fromList
      $ map (, anyVersion)
      $ maybe [] Set.toList
      $ Map.lookup (S8.pack . packageNameString . fromCabalPackageName $ name) toolMap
    packageDepsWithTools p = Map.unionsWith intersectVersionRanges
        $ packageDeps p
        : map toolToPackages (packageTools p)

mkUnregisterLocal :: Map PackageName Task -> Set GhcPkgId -> Set GhcPkgId
mkUnregisterLocal tasks locallyRegistered =
    Set.filter toUnregister locallyRegistered
  where
    toUnregister gid =
        case M.lookup name tasks of
            Nothing -> False
            Just task ->
                case taskType task of
                    TTLocal _ JustFinal -> False
                    _ -> True
      where
        ident = ghcPkgIdPackageIdentifier gid
        name = packageIdentifierName ident

mkBaseConfigOpts :: (MonadIO m, MonadReader env m, HasBuildConfig env, MonadThrow m)
                 => BuildOpts -> m BaseConfigOpts
mkBaseConfigOpts bopts = do
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    snapInstallRoot <- installationRootDeps
    localInstallRoot <- installationRootLocal
    return BaseConfigOpts
        { bcoSnapDB = snapDBPath
        , bcoLocalDB = localDBPath
        , bcoSnapInstallRoot = snapInstallRoot
        , bcoLocalInstallRoot = localInstallRoot
        , bcoLibProfiling = boptsLibProfile bopts
        , bcoExeProfiling = boptsExeProfile bopts
        , bcoFinalAction = boptsFinalAction bopts
        , bcoGhcOptions = boptsGhcOptions bopts
        }

withLoadPackage :: M env m
                => EnvOverride
                -> ((PackageName -> Version -> Map FlagName Bool -> m Package) -> m a)
                -> m a
withLoadPackage menv inner = do
    bconfig <- asks getBuildConfig
    withCabalLoader menv $ \cabalLoader ->
        inner $ \name version flags -> do
            bs <- cabalLoader $ PackageIdentifier name version -- TODO automatically update index the first time this fails
            readPackageBS (depPackageConfig bconfig flags) bs
  where
    -- | Package config to be used for dependencies
    depPackageConfig :: BuildConfig -> Map FlagName Bool -> PackageConfig
    depPackageConfig bconfig flags = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags = flags
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }

-- | Build using Shake.
build :: M env m => BuildOpts -> m ()
build bopts = do
    menv <- getMinimalEnvOverride
    cabalPkgVer <- getCabalPkgVer menv

    (mbp, locals, sourceMap) <- loadSourceMap bopts
    (installedMap, locallyRegistered) <- getInstalled menv profiling sourceMap

    baseConfigOpts <- mkBaseConfigOpts bopts
    let extraToBuild = either (const []) id $ boptsTargets bopts
    plan <- withLoadPackage menv $ \loadPackage ->
        constructPlan mbp baseConfigOpts locals extraToBuild locallyRegistered loadPackage sourceMap installedMap

    if boptsDryrun bopts
        then printPlan plan
        else executePlan menv bopts baseConfigOpts cabalPkgVer locals plan
  where
    profiling = boptsLibProfile bopts || boptsExeProfile bopts

-- | Reset the build (remove Shake database and .gen files).
clean :: (M env m) => m ()
clean = do
    bconfig <- asks getBuildConfig
    menv <- getMinimalEnvOverride
    cabalPkgVer <- getCabalPkgVer menv
    forM_
        (S.toList (bcPackages bconfig))
        (distDirFromDir cabalPkgVer >=> removeTreeIfExists)

{- EKB TODO: doc generation for stack-doc-server
            (boptsFinalAction bopts == DoHaddock)
            (buildDocIndex
                 (wanted pwd)
                 docLoc
                 packages
                 mgr
                 logLevel)
                                  -}

{- EKB TODO: doc generation for stack-doc-server
-- | Build the haddock documentation index and contents.
buildDocIndex :: (Package -> Wanted)
              -> Path Abs Dir
              -> Set Package
              -> Manager
              -> LogLevel
              -> Rules ()
buildDocIndex wanted docLoc packages mgr logLevel =
  do runHaddock "--gen-contents" $(mkRelFile "index.html")
     runHaddock "--gen-index" $(mkRelFile "doc-index.html")
     combineHoogle
  where
    runWithLogging = runStackLoggingT mgr logLevel
    runHaddock genOpt destFilename =
      do let destPath = toFilePath (docLoc </> destFilename)
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   ifcOpts <- liftIO (fmap concat (mapM toInterfaceOpt (S.toList packages)))
                   runIn docLoc
                         "haddock"
                         mempty
                         (genOpt:ifcOpts)
                         Nothing)
    toInterfaceOpt package =
      do let pv = joinPkgVer (packageName package,packageVersion package)
             srcPath = (toFilePath docLoc) ++ "/" ++
                       pv ++ "/" ++
                       packageNameString (packageName package) ++ "." ++
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
             destPath = toFilePath destHoogleDbLoc
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   srcHoogleDbs <- liftIO (fmap concat (mapM toSrcHoogleDb (S.toList packages)))
                   callProcess
                        "hoogle"
                        ("combine" :
                         "-o" :
                         toFilePath destHoogleDbLoc :
                         srcHoogleDbs))
    toSrcHoogleDb package =
      do let srcPath = toFilePath docLoc ++ "/" ++
                       joinPkgVer (packageName package,packageVersion package) ++ "/" ++
                       packageNameString (packageName package) ++ "." ++
                       hoogleDbExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then [srcPath]
                    else [])
    needDeps =
      need (concatMap (\package -> if wanted package == Wanted
                                    then let dir = packageDir package
                                         in [toFilePath (builtFileFromDir dir)]
                                    else [])
                      (S.toList packages))

#ifndef mingw32_HOST_OS
-- | Remove existing links docs for package from @~/.shake/doc@.
removeDocLinks :: Path Abs Dir -> Package -> IO ()
removeDocLinks docLoc package =
  do createDirectoryIfMissing True
                              (toFilePath docLoc)
     userDocLs <-
       fmap (map (toFilePath docLoc ++))
            (getDirectoryContents (toFilePath docLoc))
     forM_ userDocLs $
       \docPath ->
         do isDir <- doesDirectoryExist docPath
            when isDir
                 (case breakPkgVer (FilePath.takeFileName docPath) of
                    Just (p,_) ->
                      when (p == packageName package)
                           (removeLink docPath)
                    Nothing -> return ())

-- | Add link for package to @~/.shake/doc@.
createDocLinks :: Path Abs Dir -> Package -> IO ()
createDocLinks docLoc package =
  do let pkgVer =
           joinPkgVer (packageName package,(packageVersion package))
     pkgVerLoc <- liftIO (parseRelDir pkgVer)
     let pkgDestDocLoc = docLoc </> pkgVerLoc
         pkgDestDocPath =
           FilePath.dropTrailingPathSeparator (toFilePath pkgDestDocLoc)
         cabalDocLoc = parent docLoc </>
                       $(mkRelDir "share/doc/")
     haddockLocs <-
       do cabalDocExists <- doesDirectoryExist (toFilePath cabalDocLoc)
          if cabalDocExists
             then findFiles cabalDocLoc
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
             else return []
     case haddockLocs of
       [haddockLoc] ->
         case stripDir (parent docLoc)
                          haddockLoc of
           Just relHaddockPath ->
             do let srcRelPathCollapsed =
                      FilePath.takeDirectory (FilePath.dropTrailingPathSeparator (toFilePath relHaddockPath))
                    {-srcRelPath = "../" ++ srcRelPathCollapsed-}
                createSymbolicLink (FilePath.dropTrailingPathSeparator srcRelPathCollapsed)
                                   pkgDestDocPath
           Nothing -> return ()
       _ -> return ()
#endif /* not defined(mingw32_HOST_OS) */

-- | Get @-i@ arguments for haddock for dependencies.
haddockInterfaceOpts :: Path Abs Dir -> Package -> Set Package -> IO [String]
haddockInterfaceOpts userDocLoc package packages =
  do mglobalDocLoc <- getGlobalDocPath
     globalPkgVers <-
       case mglobalDocLoc of
         Nothing -> return M.empty
         Just globalDocLoc -> getDocPackages globalDocLoc
     let toInterfaceOpt pn =
           case find (\dpi -> packageName dpi == pn) (S.toList packages) of
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
               do let destPath = (toFilePath userDocLoc ++ "/" ++
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
     fmap concat (mapM toInterfaceOpt (S.toList (packageAllDeps package)))

--------------------------------------------------------------------------------
-- Paths

{- EKB TODO: doc generation for stack-doc-server
-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Path b Dir -> Bool
isHiddenDir = isPrefixOf "." . toFilePath . dirname
        -}
--}

configureOpts :: BaseConfigOpts
              -> Set GhcPkgId -- ^ dependencies
              -> Bool -- ^ wanted?
              -> Location
              -> Map FlagName Bool
              -> [Text]
configureOpts bco deps wanted loc flags = map T.pack $ concat
    [ ["--user", "--package-db=clear", "--package-db=global"]
    , map (("--package-db=" ++) . toFilePath) $ case loc of
        Snap -> [bcoSnapDB bco]
        Local -> [bcoSnapDB bco, bcoLocalDB bco]
    , depOptions
    , [ "--libdir=" ++ toFilePath (installRoot </> $(mkRelDir "lib"))
      , "--bindir=" ++ toFilePath (installRoot </> bindirSuffix)
      , "--datadir=" ++ toFilePath (installRoot </> $(mkRelDir "share"))
      , "--docdir=" ++ toFilePath (installRoot </> $(mkRelDir "doc"))
      ]
    , ["--enable-library-profiling" | bcoLibProfiling bco || bcoExeProfiling bco]
    , ["--enable-executable-profiling" | bcoLibProfiling bco]
    , ["--enable-tests" | wanted && bcoFinalAction bco == DoTests]
    , ["--enable-benchmarks" | wanted && bcoFinalAction bco == DoBenchmarks]
    , map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (Map.toList flags)
    -- FIXME Chris: where does this come from now? , ["--ghc-options=-O2" | gconfigOptimize gconfig]
    , if wanted
        then concatMap (\x -> ["--ghc-options", T.unpack x]) (bcoGhcOptions bco)
        else []
    ]
  where
    installRoot =
        case loc of
            Snap -> bcoSnapInstallRoot bco
            Local -> bcoLocalInstallRoot bco

    depOptions = map toDepOption $ Set.toList deps

    {- TODO does this work with some versions of Cabal?
    toDepOption gid = T.pack $ concat
        [ "--dependency="
        , packageNameString $ packageIdentifierName $ ghcPkgIdPackageIdentifier gid
        , "="
        , ghcPkgIdString gid
        ]
    -}
    toDepOption gid = concat
        [ "--constraint="
        , packageNameString name
        , "=="
        , versionString version
        ]
      where
        PackageIdentifier name version = ghcPkgIdPackageIdentifier gid
