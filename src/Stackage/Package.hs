{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dealing with Cabal.

module Stackage.Package
  (readPackage
  ,Package(..)
  ,PackageConfig(..))
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Loops
import           Data.Data
import           Data.Function
import           Data.List

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml (ParseException)
import           Distribution.Compiler
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.ModuleName as Cabal
import           Distribution.Package hiding (Package,PackageName,packageName,packageVersion)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Utils
import           Distribution.System
import           Distribution.Version
import           Path as FL
import           Prelude hiding (FilePath)
import           Stackage.Constants
import           Stackage.PackageName
import           Stackage.PackageVersion
import           System.Directory (doesFileExist)

-- | All exceptions thrown by the library.
data PackageException
  = PackageConfigError ParseException
  | PackageNoConfigFile
  | PackageNoCabalFile (Path Abs Dir)
  | PackageInvalidCabalFile (Path Abs File) PError
  | PackageNoDeps (Path Abs File)
  | PackageDepCycle PackageName
  | PackageMissingDep Package PackageName VersionRange
  | PackageDependencyIssues [PackageException]
  | PackageMissingTool Dependency
  | PackageCouldn'tFindPkgId PackageName
  | PackageStackagePackageVersionMismatch PackageName Version Version
  | PackageStackageDepVerMismatch PackageName Version VersionRange
  deriving (Show,Typeable)
instance Exception PackageException

-- | Some package info.
data Package =
  Package {packageName :: !PackageName                    -- ^ Name of the package.
          ,packageVersion :: !PackageVersion              -- ^ Version of the package
          ,packageDir :: !(Path Abs Dir)                  -- ^ Directory of the package.
          ,packageFiles :: !(Set (Path Abs File))         -- ^ Files that the package depends on.
          ,packageDeps :: !(Map PackageName VersionRange) -- ^ Packages that the package depends on.
          ,packageTools :: ![Dependency]                  -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageFlags :: !(Map Text Bool)               -- ^ Flags used on package.
          }
 deriving (Show,Typeable)

-- | Package build configuration
data PackageConfig =
  PackageConfig {packageConfigEnableTests :: !Bool        -- ^ Are tests enabled?
                ,packageConfigEnableBenchmarks :: !Bool   -- ^ Are benchmarks enabled?
                ,packageConfigFlags :: !(Map Text Bool)   -- ^ Package config flags.
                }
 deriving (Show,Typeable)

-- | Compares the package name.
instance Ord Package where
  compare = on compare packageName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) packageName

-- | Reads and exposes the package information
readPackage :: (MonadLogger m, MonadIO m, MonadThrow m)
            => PackageConfig
            -> Path Abs File
            -> m Package
readPackage packageConfig cabalfp =
  do chars <-
       liftIO (Prelude.readFile (FL.toFilePath cabalfp))
     case parsePackageDescription chars of
       ParseFailed per ->
         throwM (PackageInvalidCabalFile cabalfp per)
       ParseOk _ gpkg ->
         let pkgId =
               package (packageDescription gpkg)
             name = fromCabalPackageName (pkgName pkgId)
             pkgFlags =
               packageConfigFlags packageConfig
             pkg =
               resolvePackage packageConfig gpkg
         in case packageDependencies pkg of
              deps
                | M.null deps ->
                  throwM (PackageNoDeps cabalfp)
                | otherwise ->
                  do let dir = FL.parent cabalfp
                     pkgFiles <-
                       liftIO (packageDescFiles dir pkg)
                     let files = cabalfp : pkgFiles
                         deps' =
                           M.filterWithKey (const . (/= name))
                                           deps
                     return (Package {packageName = name
                                   ,packageVersion = fromCabalVersion (pkgVersion pkgId)
                                   ,packageDeps = deps'
                                   ,packageDir = dir
                                   ,packageFiles = S.fromList files
                                   ,packageTools = packageDescTools pkg
                                   ,packageFlags = pkgFlags
                                   ,packageAllDeps =
                                      S.fromList (M.keys deps')})

-- | Get all dependencies of the package (buildable targets only).
packageDependencies :: PackageDescription -> Map PackageName VersionRange
packageDependencies =
  M.fromList .
  concatMap (map (\dep -> ((depName dep),depRange dep)) .
             targetBuildDepends) .
  allBuildInfo

-- | Get all dependencies of the package (buildable targets only).
packageDescTools :: PackageDescription -> [Dependency]
packageDescTools = concatMap buildTools . allBuildInfo

-- | Get all files referenced by the package.
packageDescFiles :: Path Abs Dir -> PackageDescription -> IO [Path Abs File]
packageDescFiles dir pkg =
  do libfiles <- fmap concat
                      (mapM (libraryFiles dir)
                            (maybe [] return (library pkg)))
     exefiles <- fmap concat
                      (mapM (executableFiles dir)
                            (executables pkg))
     dfiles <- resolveGlobFiles dir
                                (dataFiles pkg)
     srcfiles <- resolveGlobFiles dir
                                  (extraSrcFiles pkg)
     tmpfiles <- resolveGlobFiles dir
                                  (extraTmpFiles pkg)
     docfiles <- resolveGlobFiles dir
                                  (extraDocFiles pkg)
     return (concat [libfiles,exefiles,dfiles,srcfiles,tmpfiles,docfiles])

-- | Resolve globbing of files (e.g. data files) to absolute paths.
resolveGlobFiles :: Path Abs Dir -> [String] -> IO [Path Abs File]
resolveGlobFiles dir = fmap concat . mapM resolve
  where resolve name =
          if any (== '*') name
             then explode name
             else return [(either (error . show)
                                  (dir </>)
                                  (FL.parseRelFile name))]
        explode name =
          fmap (map (either (error . show)
                            (dir </>) .
                     FL.parseRelFile))
               (matchDirFileGlob (FL.toFilePath dir)
                                 name)

-- | Get all files referenced by the executable.
executableFiles :: Path Abs Dir -> Executable -> IO [Path Abs File]
executableFiles dir exe =
  do exposed <-
       resolveFiles
         (map (either (error . show) (dir </>) .
               FL.parseRelDir)
              (hsSourceDirs build) ++
          [dir])
         [Right (modulePath exe)]
         haskellFileExts
     bfiles <- buildFiles dir build
     return (concat [bfiles,exposed])
  where build = buildInfo exe

-- | Get all files referenced by the library.
libraryFiles :: Path Abs Dir -> Library -> IO [Path Abs File]
libraryFiles dir lib =
  do exposed <- resolveFiles
                  (map (either (error . show) (dir </>) .
                        FL.parseRelDir)
                       (hsSourceDirs build) ++
                   [dir])
                  (map Left (exposedModules lib))
                  haskellFileExts
     bfiles <- buildFiles dir build
     return (concat [bfiles,exposed])
  where build = libBuildInfo lib

-- | Get all files in a build.
buildFiles :: Path Abs Dir -> BuildInfo -> IO [Path Abs File]
buildFiles dir build =
  do other <- resolveFiles
                (map (either (error . show) (dir </>) .
                      FL.parseRelDir)
                     (hsSourceDirs build) ++
                 [dir])
                (map Left (otherModules build))
                haskellFileExts
     return (concat [other
                    ,map (either (error . show) (dir </>) .
                          FL.parseRelFile)
                         (cSources build)])

-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackage :: PackageConfig
               -> GenericPackageDescription
               -> PackageDescription
resolvePackage packageConfig (GenericPackageDescription desc defaultFlags mlib exes tests benches) =
  desc {library =
          fmap (resolveConditions flags' updateLibDeps) mlib
       ,executables =
          map (resolveConditions flags' updateExeDeps .
               snd)
              exes
       ,testSuites =
          map (resolveConditions flags' updateTestDeps .
               snd)
              tests
       ,benchmarks =
          map (resolveConditions flags' updateBenchmarkDeps .
               snd)
              benches}
  where flags =
          M.union (packageConfigFlags packageConfig)
                  (flagMap defaultFlags)
        flags' =
          (map (FlagName . T.unpack)
               (map fst (filter snd (M.toList flags))))
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
flagMap :: [Flag] -> Map Text Bool
flagMap = M.fromList . map pair
  where pair :: Flag -> (Text, Bool)
        pair (MkFlag (unName -> name) _desc def _manual) = (name,def)
        unName (FlagName t) = T.pack t

-- | Resolve the condition tree for the library.
resolveConditions :: (Monoid target,Show target)
                  => [FlagName]
                  -> (target -> cs -> target)
                  -> CondTree ConfVar cs target
                  -> target
resolveConditions flags addDeps (CondNode lib deps cs) = basic <> children
  where basic = addDeps lib deps
        children = mconcat (map apply cs)
          where apply (cond,node,mcs) =
                  if (condSatisfied cond)
                     then resolveConditions flags addDeps node
                     else maybe mempty (resolveConditions flags addDeps) mcs
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
                    OS os -> os == buildOS
                    Arch arch -> arch == buildArch
                    Flag flag -> elem flag flags
                    Impl flavor range ->
                      case buildCompilerId of
                        CompilerId flavor' ver ->
                          flavor' == flavor &&
                          withinRange ver range

-- | Get the name of a dependency.
depName :: Dependency -> PackageName
depName = \(Dependency n _) -> fromCabalPackageName n

-- | Get the version range of a dependency.
depRange :: Dependency -> VersionRange
depRange = \(Dependency _ r) -> r

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles :: [Path Abs Dir] -- ^ Directories to look in.
             -> [Either ModuleName String] -- ^ Base names.
             -> [Text] -- ^ Extentions.
             -> IO [Path Abs File]
resolveFiles dirs names exts =
  fmap catMaybes (forM names makeNameCandidates)
  where makeNameCandidates name =
          firstM (doesFileExist . FL.toFilePath)
                 (concatMap (makeDirCandidates name) dirs)
        makeDirCandidates :: Either ModuleName String
                          -> Path Abs Dir
                          -> [Path Abs File]
        makeDirCandidates name dir =
          map (\ext ->
                 case name of
                   Left mn ->
                     (either (error . show)
                             (dir </>)
                             (FL.parseRelFile
                                (Cabal.toFilePath mn ++
                                 "." ++ ext)))
                   Right fp ->
                     either (error . show)
                            (dir </>)
                            (FL.parseRelFile fp))
              (map T.unpack exts)
