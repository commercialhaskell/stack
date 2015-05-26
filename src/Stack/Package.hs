{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dealing with Cabal.

module Stack.Package
  (readPackage
  ,readPackageUnresolved
  ,resolvePackage
  ,getCabalFileName
  ,Package(..)
  ,PackageType(..)
  ,PackageConfig(..)
  ,buildLogPath
  ,configureLogPath
  ,packageDocDir
  ,stackageBuildDir
  ,PackageException (..))
  where

import           Control.Exception hiding (try)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger,logWarn)
import           Control.Monad.Reader
import qualified Data.ByteString as S
import           Data.Data
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Maybe.Extra
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Yaml (ParseException)
import           Distribution.Compiler
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.ModuleName as Cabal
import           Distribution.Package hiding (Package,PackageName,packageName,packageVersion)
import           Distribution.PackageDescription hiding (FlagName)
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Utils
import           Distribution.System
import           Path as FL
import           Path.Find
import           Path.IO
import           Prelude hiding (FilePath)
import           Stack.Constants
import           Stack.Types
import qualified System.FilePath as FilePath

-- | All exceptions thrown by the library.
data PackageException
  = PackageConfigError ParseException
  | PackageNoConfigFile
  | PackageNoCabalFile (Path Abs Dir)
  | PackageInvalidCabalFile (Path Abs File) PError
  | PackageDepCycle PackageName
  | PackageMissingDep Package PackageName VersionRange
  | PackageDependencyIssues [PackageException]
  | PackageMissingTool Dependency
  | PackageCouldn'tFindPkgId PackageName
  | PackageStackageVersionMismatch PackageName Version Version
  | PackageStackageDepVerMismatch PackageName Version VersionRange
  | PackageNoCabalFileFound (Path Abs Dir)
  | PackageMultipleCabalFilesFound (Path Abs Dir) [Path Abs File]
  deriving (Show,Typeable)
instance Exception PackageException

-- | Some package info.
data Package =
  Package {packageName :: !PackageName                    -- ^ Name of the package.
          ,packageVersion :: !Version                     -- ^ Version of the package
          ,packageDir :: !(Path Abs Dir)                  -- ^ Directory of the package.
          ,packageCabalFile :: !(Path Abs File)           -- ^ The .cabal file
          ,packageFiles :: !(Set (Path Abs File))         -- ^ Files that the package depends on.
          ,packageDeps :: !(Map PackageName VersionRange) -- ^ Packages that the package depends on.
          ,packageTools :: ![Dependency]                  -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageFlags :: !(Map FlagName Bool)           -- ^ Flags used on package.
          ,packageType :: !PackageType
          ,packageHasLibrary :: !Bool                     -- ^ does the package have a buildable library stanza?
          }
 deriving (Show,Typeable)

-- | Is this package a user target package, or a dependency?
data PackageType = PTUser | PTDep
 deriving (Show,Typeable,Eq)

-- | Package build configuration
data PackageConfig =
  PackageConfig {packageConfigEnableTests :: !Bool        -- ^ Are tests enabled?
                ,packageConfigEnableBenchmarks :: !Bool   -- ^ Are benchmarks enabled?
                ,packageConfigFlags :: !(Map FlagName Bool)   -- ^ Package config flags.
                ,packageConfigGhcVersion :: !Version      -- ^ GHC version
                }
 deriving (Show,Typeable)

-- | Compares the package name.
instance Ord Package where
  compare = on compare packageName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) packageName

-- | Read the raw, unresolved package information.
readPackageUnresolved :: (MonadLogger m, MonadIO m, MonadThrow m)
                      => Path Abs File
                      -> m GenericPackageDescription
readPackageUnresolved cabalfp = do
  do bs <- liftIO (S.readFile (FL.toFilePath cabalfp))
     let chars = T.unpack (decodeUtf8With lenientDecode bs)
     case parsePackageDescription chars of
       ParseFailed per ->
         throwM (PackageInvalidCabalFile cabalfp per)
       ParseOk _ gpkg -> return gpkg

-- | Reads and exposes the package information
readPackage :: (MonadLogger m, MonadIO m, MonadThrow m)
            => PackageConfig
            -> Path Abs File
            -> PackageType
            -> m Package
readPackage packageConfig cabalfp ptype =
  readPackageUnresolved cabalfp >>= resolvePackage packageConfig cabalfp ptype

-- | Resolve a parsed cabal file into a 'Package'.
resolvePackage :: (MonadLogger m, MonadIO m, MonadThrow m)
               => PackageConfig
               -> Path Abs File
               -> PackageType
               -> GenericPackageDescription
               -> m Package
resolvePackage packageConfig cabalfp ptype gpkg = do
     let pkgId =
           package (packageDescription gpkg)
         name = fromCabalPackageName (pkgName pkgId)
         pkgFlags =
           packageConfigFlags packageConfig
         pkg =
           resolvePackageDescription packageConfig gpkg
     case packageDependencies pkg of
       deps ->
           do let dir = FL.parent cabalfp
              pkgFiles <-
                runReaderT (packageDescFiles pkg) cabalfp
              let files = cabalfp : pkgFiles
                  deps' =
                    M.filterWithKey (const . (/= name))
                                    deps
              return (Package {packageName = name
                            ,packageVersion = fromCabalVersion (pkgVersion pkgId)
                            ,packageDeps = deps'
                            ,packageDir = dir
                            ,packageCabalFile = cabalfp
                            ,packageFiles = S.fromList files
                            ,packageTools = packageDescTools pkg
                            ,packageFlags = pkgFlags
                            ,packageAllDeps =
                               S.fromList (M.keys deps')
                            ,packageType = ptype
                            ,packageHasLibrary = maybe
                                False
                                (buildable . libBuildInfo)
                                (library pkg)
                            })

-- | Get all dependencies of the package (buildable targets only).
packageDependencies :: PackageDescription -> Map PackageName VersionRange
packageDependencies =
  M.fromList .
  concatMap (map (\dep -> ((depName dep),depRange dep)) .
             targetBuildDepends) .
  allBuildInfo'

-- | Get all dependencies of the package (buildable targets only).
packageDescTools :: PackageDescription -> [Dependency]
packageDescTools = concatMap buildTools . allBuildInfo'

-- | This is a copy-paste from Cabal's @allBuildInfo@ function, but with the
-- @buildable@ test removed. The reason is that (surprise) Cabal is broken,
-- see: https://github.com/haskell/cabal/issues/1725
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ bi | Just lib <- [library pkg_descr]
                              , let bi = libBuildInfo lib
                              , True || buildable bi ]
                      ++ [ bi | exe <- executables pkg_descr
                              , let bi = buildInfo exe
                              , True || buildable bi ]
                      ++ [ bi | tst <- testSuites pkg_descr
                              , let bi = testBuildInfo tst
                              , True || buildable bi
                              , testEnabled tst ]
                      ++ [ bi | tst <- benchmarks pkg_descr
                              , let bi = benchmarkBuildInfo tst
                              , True || buildable bi
                              , benchmarkEnabled tst ]

-- | Get all files referenced by the package.
packageDescFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m)
                 => PackageDescription -> m [Path Abs File]
packageDescFiles pkg =
  do libfiles <-
       liftM concat
             (mapM libraryFiles
                   (maybe [] return (library pkg)))
     exefiles <-
       liftM concat
             (mapM executableFiles
                   (executables pkg))
     dfiles <-
       resolveGlobFiles (map (dataDir pkg FilePath.</>) (dataFiles pkg))
     srcfiles <-
       resolveGlobFiles (extraSrcFiles pkg)
     -- extraTmpFiles purposely not included here, as those are files generated
     -- by the build script. Another possible implementation: include them, but
     -- don't error out if not present
     docfiles <-
       resolveGlobFiles (extraDocFiles pkg)
     return (concat [libfiles,exefiles,dfiles,srcfiles,docfiles])

-- | Resolve globbing of files (e.g. data files) to absolute paths.
resolveGlobFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m)
                 => [String] -> m [Path Abs File]
resolveGlobFiles = liftM (catMaybes . concat) . mapM resolve
  where resolve name =
          if any (== '*') name
             then explode name
             else liftM return (resolveFileOrWarn name)
        explode name = do
            dir <- asks parent
            names <- liftIO (matchDirFileGlob (FL.toFilePath dir) name)
            mapM resolveFileOrWarn names

-- | Get all files referenced by the executable.
executableFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m)
                => Executable -> m [Path Abs File]
executableFiles exe =
  do dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
     dir <- asks parent
     exposed <-
       resolveFiles
         (dirs ++ [dir])
         [Right (modulePath exe)]
         haskellFileExts
     bfiles <- buildFiles dir build
     return (concat [bfiles,exposed])
  where build = buildInfo exe

-- | Get all files referenced by the library.
libraryFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m)
             => Library -> m [Path Abs File]
libraryFiles lib =
  do dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
     dir <- asks parent
     exposed <- resolveFiles
                  (dirs ++ [dir])
                  (map Left (exposedModules lib))
                  haskellFileExts
     bfiles <- buildFiles dir build
     return (concat [bfiles,exposed])
  where build = libBuildInfo lib

-- | Get all files in a build.
buildFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m)
           => Path Abs Dir -> BuildInfo -> m [Path Abs File]
buildFiles dir build = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    other <- resolveFiles
                (dirs ++ [dir])
                (map Left (otherModules build))
                haskellFileExts
    cSources' <- mapMaybeM resolveFileOrWarn (cSources build)
    return (other ++ cSources')

-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackageDescription :: PackageConfig
                          -> GenericPackageDescription
                          -> PackageDescription
resolvePackageDescription packageConfig (GenericPackageDescription desc defaultFlags mlib exes tests benches) =
  desc {library =
          fmap (resolveConditions rc updateLibDeps) mlib
       ,executables =
          map (resolveConditions rc updateExeDeps .
               snd)
              exes
       ,testSuites =
          map (resolveConditions rc updateTestDeps .
               snd)
              tests
       ,benchmarks =
          map (resolveConditions rc updateBenchmarkDeps .
               snd)
              benches}
  where flags =
          M.union (packageConfigFlags packageConfig)
                  (flagMap defaultFlags)

        rc = mkResolveConditions
                (packageConfigGhcVersion packageConfig)
                flags

        updateLibDeps lib deps =
          lib {libBuildInfo =
                 ((libBuildInfo lib) {targetBuildDepends =
                                        deps})}
        updateExeDeps exe deps =
          exe {buildInfo =
                 (buildInfo exe) {targetBuildDepends = deps}}
        updateTestDeps test deps =
          test {testBuildInfo =
                  (testBuildInfo test) {targetBuildDepends = deps}
               ,testEnabled = packageConfigEnableTests packageConfig}
        updateBenchmarkDeps benchmark deps =
          benchmark {benchmarkBuildInfo =
                       (benchmarkBuildInfo benchmark) {targetBuildDepends = deps}
                    ,benchmarkEnabled = packageConfigEnableBenchmarks packageConfig}

-- | Make a map from a list of flag specifications.
--
-- What is @flagManual@ for?
flagMap :: [Flag] -> Map FlagName Bool
flagMap = M.fromList . map pair
  where pair :: Flag -> (FlagName, Bool)
        pair (MkFlag (fromCabalFlagName -> name) _desc def _manual) = (name,def)

data ResolveConditions = ResolveConditions
    { rcFlags :: Map FlagName Bool
    , rcGhcVersion :: Version
    , rcOS :: OS
    , rcArch :: Arch
    }

-- | Generic a @ResolveConditions@ using sensible defaults.
mkResolveConditions :: Version -- ^ GHC version
                    -> Map FlagName Bool -- ^ enabled flags
                    -> ResolveConditions
mkResolveConditions ghcVersion flags = ResolveConditions
    { rcFlags = flags
    , rcGhcVersion = ghcVersion
    , rcOS = buildOS
    , rcArch = buildArch
    }

-- | Resolve the condition tree for the library.
resolveConditions :: (Monoid target,Show target)
                  => ResolveConditions
                  -> (target -> cs -> target)
                  -> CondTree ConfVar cs target
                  -> target
resolveConditions rc addDeps (CondNode lib deps cs) = basic <> children
  where basic = addDeps lib deps
        children = mconcat (map apply cs)
          where apply (cond,node,mcs) =
                  if (condSatisfied cond)
                     then resolveConditions rc addDeps node
                     else maybe mempty (resolveConditions rc addDeps) mcs
                condSatisfied c =
                  case c of
                    Var v -> varSatisifed v
                    Lit b -> b
                    CNot c' ->
                      not (condSatisfied c')
                    COr cx cy ->
                      or [condSatisfied cx,condSatisfied cy]
                    CAnd cx cy ->
                      and [condSatisfied cx,condSatisfied cy]
                varSatisifed v =
                  case v of
                    OS os -> os == rcOS rc
                    Arch arch -> arch == rcArch rc
                    Flag flag ->
                        case M.lookup (fromCabalFlagName flag) (rcFlags rc) of
                            Just x -> x
                            Nothing ->
                                -- NOTE: This should never happen, as all flags
                                -- which are used must be declared. Defaulting
                                -- to False
                                False
                    Impl flavor range ->
                        flavor == GHC &&
                        withinRange (rcGhcVersion rc) range

-- | Get the name of a dependency.
depName :: Dependency -> PackageName
depName = \(Dependency n _) -> fromCabalPackageName n

-- | Get the version range of a dependency.
depRange :: Dependency -> VersionRange
depRange = \(Dependency _ r) -> r

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles :: MonadIO m
             => [Path Abs Dir] -- ^ Directories to look in.
             -> [Either ModuleName String] -- ^ Base names.
             -> [Text] -- ^ Extentions.
             -> m [Path Abs File]
resolveFiles dirs names exts =
  liftM catMaybes (forM names (liftIO . makeNameCandidates))
  where makeNameCandidates name =
          fmap (listToMaybe . rights . concat)
               (mapM (makeDirCandidates name) dirs)
        makeDirCandidates :: Either ModuleName String
                          -> Path Abs Dir
                          -> IO [Either ResolveException (Path Abs File)]
        makeDirCandidates name dir =
          mapM (\ext ->
                  try (case name of
                         Left mn ->
                           resolveFile dir
                                       (Cabal.toFilePath mn ++ "." ++ ext)
                         Right fp ->
                           resolveFile dir fp))
               (map T.unpack exts)

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
getCabalFileName
    :: (MonadThrow m, MonadIO m)
    => Path Abs Dir -- ^ package directory
    -> m (Path Abs File)
getCabalFileName pkgDir = do
    files <- liftIO $ findFiles
        pkgDir
        (flip hasExtension "cabal" . FL.toFilePath)
        (const False)
    case files of
        [] -> throwM $ PackageNoCabalFileFound pkgDir
        [x] -> return x
        _:_ -> throwM $ PackageMultipleCabalFilesFound pkgDir files
  where hasExtension fp x = FilePath.takeExtensions fp == "." ++ x

-- | Path for the project's build log.
buildLogPath :: Package -> Path Abs File
buildLogPath package' =
  stackageBuildDir package' </>
  $(mkRelFile "build-log")

-- | Path for the project's configure log.
configureLogPath :: Package -> Path Abs File
configureLogPath package' =
  stackageBuildDir package' </>
  $(mkRelFile "configure-log")

-- | Get the build directory.
stackageBuildDir :: Package -> Path Abs Dir
stackageBuildDir package' =
  distDirFromDir dir </>
  $(mkRelDir "stack-build")
  where dir = packageDir package'

-- | Package's documentation directory.
packageDocDir :: Package -> Path Abs Dir
packageDocDir package' =
  distDirFromDir (packageDir package') </>
  $(mkRelDir "doc/")

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn :: (MonadThrow m,MonadIO m,MonadLogger m,MonadReader (Path Abs File) m)
                  => FilePath.FilePath
                  -> m (Maybe (Path Abs File))
resolveFileOrWarn y =
  do cwd <- getWorkingDir
     file <- ask
     dir <- asks parent
     result <- resolveFileMaybe dir y
     case result of
       Nothing ->
         $logWarn ("Warning: File listed in " <>
                   T.pack (maybe (FL.toFilePath file) FL.toFilePath (stripDir cwd file)) <>
                   " file does not exist: " <>
                   T.pack y)
       _ -> return ()
     return result

-- | Resolve the directory, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveDirOrWarn :: (MonadThrow m,MonadIO m,MonadLogger m,MonadReader (Path Abs File) m)
                 => FilePath.FilePath
                 -> m (Maybe (Path Abs Dir))
resolveDirOrWarn y =
  do cwd <- getWorkingDir
     file <- ask
     dir <- asks parent
     result <- resolveDirMaybe dir y
     case result of
       Nothing ->
         $logWarn ("Warning: Directory listed in " <>
                   T.pack (maybe (FL.toFilePath file) FL.toFilePath (stripDir cwd file)) <>
                   " file does not exist: " <>
                   T.pack y)
       _ -> return ()
     return result
