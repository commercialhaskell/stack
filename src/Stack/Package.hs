{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dealing with Cabal.

module Stack.Package
  (readPackage
  ,readPackageBS
  ,readPackageDir
  ,readPackageUnresolved
  ,readPackageUnresolvedBS
  ,resolvePackage
  ,getCabalFileName
  ,Package(..)
  ,GetPackageFiles(..)
  ,PackageConfig(..)
  ,buildLogPath
  ,PackageException (..)
  ,resolvePackageDescription
  ,packageToolDependencies
  ,packageDependencies
  ,packageIdentifier)
  where

import           Control.Exception hiding (try,catch)
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
import           Distribution.Compiler
import           Distribution.InstalledPackageInfo (PError)
import qualified Distribution.ModuleName as Cabal
import           Distribution.ModuleName (ModuleName)
import           Distribution.Package hiding (Package,PackageName,packageName,packageVersion,PackageIdentifier)
import           Distribution.PackageDescription hiding (FlagName)
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Utils
import           Distribution.System (OS, Arch, Platform (..))
import           Distribution.Version (intersectVersionRanges)
import           Path as FL
import           Path.Find
import           Path.IO
import           Prelude hiding (FilePath)
import           Stack.Constants
import           Stack.Types
import qualified Stack.Types.PackageIdentifier
import           System.Directory (getDirectoryContents)
import           System.FilePath (splitExtensions)
import qualified System.FilePath as FilePath
import           System.IO.Error

-- | All exceptions thrown by the library.
data PackageException
  = PackageInvalidCabalFile (Maybe (Path Abs File)) PError
  | PackageNoCabalFileFound (Path Abs Dir)
  | PackageMultipleCabalFilesFound (Path Abs Dir) [Path Abs File]
  | MismatchedCabalName (Path Abs File) PackageName
  deriving Typeable
instance Exception PackageException
instance Show PackageException where
    show (PackageInvalidCabalFile mfile err) =
        "Unable to parse cabal file" ++
        (case mfile of
            Nothing -> ""
            Just file -> ' ' : toFilePath file) ++
        ": " ++
        show err
    show (PackageNoCabalFileFound dir) =
        "No .cabal file found in directory " ++
        toFilePath dir
    show (PackageMultipleCabalFilesFound dir files) =
        "Multiple .cabal files found in directory " ++
        toFilePath dir ++
        ": " ++
        intercalate ", " (map (toFilePath . filename) files)
    show (MismatchedCabalName fp name) = concat
        [ "cabal file "
        , toFilePath fp
        , " has a mismatched package name: "
        , packageNameString name
        ]

-- | Some package info.
data Package =
  Package {packageName :: !PackageName                    -- ^ Name of the package.
          ,packageVersion :: !Version                     -- ^ Version of the package
          ,packageFiles :: !GetPackageFiles
          ,packageDeps :: !(Map PackageName VersionRange) -- ^ Packages that the package depends on.
          ,packageTools :: ![Dependency]                  -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageFlags :: !(Map FlagName Bool)           -- ^ Flags used on package.
          ,packageHasLibrary :: !Bool                     -- ^ does the package have a buildable library stanza?
          ,packageTests :: !(Set Text)                    -- ^ names of test suites
          ,packageExes :: !(Set Text)                     -- ^ names of executables
          }
 deriving (Show,Typeable)

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the .cabal file
newtype GetPackageFiles = GetPackageFiles
    { getPackageFiles :: forall m. (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m)
                      => Path Abs File
                      -> m (Set (Path Abs File))
    }
instance Show GetPackageFiles where
    show _ = "<GetPackageFiles>"

-- | Get the identifier of the package.
packageIdentifier :: Package -> Stack.Types.PackageIdentifier.PackageIdentifier
packageIdentifier pkg =
    Stack.Types.PackageIdentifier.PackageIdentifier
        (packageName pkg)
        (packageVersion pkg)

-- | Package build configuration
data PackageConfig =
  PackageConfig {packageConfigEnableTests :: !Bool        -- ^ Are tests enabled?
                ,packageConfigEnableBenchmarks :: !Bool   -- ^ Are benchmarks enabled?
                ,packageConfigFlags :: !(Map FlagName Bool)   -- ^ Package config flags.
                ,packageConfigGhcVersion :: !Version      -- ^ GHC version
                ,packageConfigPlatform :: !Platform       -- ^ host platform
                }
 deriving (Show,Typeable)

-- | Compares the package name.
instance Ord Package where
  compare = on compare packageName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) packageName

-- | Read the raw, unresolved package information.
readPackageUnresolved :: (MonadIO m, MonadThrow m)
                      => Path Abs File
                      -> m GenericPackageDescription
readPackageUnresolved cabalfp =
  liftIO (S.readFile (FL.toFilePath cabalfp))
  >>= readPackageUnresolvedBS (Just cabalfp)

-- | Read the raw, unresolved package information from a ByteString.
readPackageUnresolvedBS :: (MonadThrow m)
                        => Maybe (Path Abs File)
                        -> S.ByteString
                        -> m GenericPackageDescription
readPackageUnresolvedBS mcabalfp bs =
    case parsePackageDescription chars of
       ParseFailed per ->
         throwM (PackageInvalidCabalFile mcabalfp per)
       ParseOk _ gpkg -> return gpkg
  where
    chars = T.unpack (dropBOM (decodeUtf8With lenientDecode bs))

    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ T.stripPrefix "\xFEFF" t

-- | Reads and exposes the package information
readPackage :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
            => PackageConfig
            -> Path Abs File
            -> m Package
readPackage packageConfig cabalfp =
  resolvePackage packageConfig `liftM` readPackageUnresolved cabalfp

-- | Reads and exposes the package information, from a ByteString
readPackageBS :: (MonadThrow m)
              => PackageConfig
              -> S.ByteString
              -> m Package
readPackageBS packageConfig bs =
  resolvePackage packageConfig `liftM` readPackageUnresolvedBS Nothing bs

-- | Convenience wrapper around @readPackage@ that first finds the cabal file
-- in the given directory.
readPackageDir :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
               => PackageConfig
               -> Path Abs Dir
               -> m (Path Abs File, Package)
readPackageDir packageConfig dir = do
    cabalfp <- getCabalFileName dir
    pkg <- readPackage packageConfig cabalfp
    name <- parsePackageNameFromFilePath cabalfp
    when (packageName pkg /= name)
        $ throwM $ MismatchedCabalName cabalfp name
    return (cabalfp, pkg)

-- | Resolve a parsed cabal file into a 'Package'.
resolvePackage :: PackageConfig
               -> GenericPackageDescription
               -> Package
resolvePackage packageConfig gpkg = Package
    { packageName = name
    , packageVersion = fromCabalVersion (pkgVersion pkgId)
    , packageDeps = deps
    , packageFiles = GetPackageFiles $ \cabalfp -> do
        files <- runReaderT (packageDescFiles pkg) cabalfp
        return $ S.fromList $ cabalfp : files
    , packageTools = packageDescTools pkg
    , packageFlags = packageConfigFlags packageConfig
    , packageAllDeps = S.fromList (M.keys deps)
    , packageHasLibrary = maybe False (buildable . libBuildInfo) (library pkg)
    , packageTests = S.fromList $ map (T.pack . fst) $ condTestSuites gpkg -- FIXME need to test if it's buildable
    , packageExes = S.fromList $ map (T.pack . fst) $ condExecutables gpkg -- FIXME need to test if it's buildable
    }

  where
    pkgId = package (packageDescription gpkg)
    name = fromCabalPackageName (pkgName pkgId)
    pkg = resolvePackageDescription packageConfig gpkg
    deps = M.filterWithKey (const . (/= name)) (packageDependencies pkg)

-- | Get all dependencies of the package (buildable targets only).
packageDependencies :: PackageDescription -> Map PackageName VersionRange
packageDependencies =
  M.fromListWith intersectVersionRanges .
  concatMap (map (\dep -> ((depName dep),depRange dep)) .
             targetBuildDepends) .
  allBuildInfo'

-- | Get all build tool dependencies of the package (buildable targets only).
packageToolDependencies :: PackageDescription -> Map S.ByteString VersionRange
packageToolDependencies =
  M.fromList .
  concatMap (map (\dep -> ((packageNameByteString $ depName dep),depRange dep)) .
             buildTools) .
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
packageDescFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m,MonadCatch m)
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
resolveGlobFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File) m,MonadCatch m)
                 => [String] -> m [Path Abs File]
resolveGlobFiles =
    liftM (catMaybes . concat) .
    mapM resolve
  where
    resolve name =
        if any (== '*') name
            then explode name
            else liftM return (resolveFileOrWarn name)
    explode name = do
        dir <- asks parent
        names <-
            matchDirFileGlob'
                (FL.toFilePath dir)
                name
        mapM resolveFileOrWarn names
    matchDirFileGlob' dir glob =
        catch
            (liftIO (matchDirFileGlob_ dir glob))
            (\(e :: IOException) ->
                  if isUserError e
                      then do
                          $logWarn
                              ("Wildcard does not match any files: " <> T.pack glob <> "\n" <>
                               "in directory: " <> T.pack dir)
                          return []
                      else throwM e)

-- | This is a copy/paste of the Cabal library function, but with
--
-- @ext == ext'@
--
-- Changed to
--
-- @isSuffixOf ext ext'@
--
-- So that this will work:
--
-- @
-- λ> matchDirFileGlob_ "." "test/package-dump/*.txt"
-- ["test/package-dump/ghc-7.8.txt","test/package-dump/ghc-7.10.txt"]
-- @
--
matchDirFileGlob_ :: String -> String -> IO [String]
matchDirFileGlob_ dir filepath = case parseFileGlob filepath of
  Nothing -> die $ "invalid file glob '" ++ filepath
                ++ "'. Wildcards '*' are only allowed in place of the file"
                ++ " name, not in the directory name or file extension."
                ++ " If a wildcard is used it must be with an file extension."
  Just (NoGlob filepath') -> return [filepath']
  Just (FileGlob dir' ext) -> do
    files <- getDirectoryContents (dir FilePath.</> dir')
    case   [ dir' FilePath.</> file
           | file <- files
           , let (name, ext') = splitExtensions file
           , not (null name) && isSuffixOf ext ext' ] of
      []      -> die $ "filepath wildcard '" ++ filepath
                    ++ "' does not match any files."
      matches -> return matches

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
                (packageConfigPlatform packageConfig)
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
                    -> Platform -- ^ installation target platform
                    -> Map FlagName Bool -- ^ enabled flags
                    -> ResolveConditions
mkResolveConditions ghcVersion (Platform arch os) flags = ResolveConditions
    { rcFlags = flags
    , rcGhcVersion = ghcVersion
    , rcOS = os
    , rcArch = arch
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

-- | Path for the package's build log.
buildLogPath :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
             => Package -> m (Path Abs File)
buildLogPath package' = do
  env <- ask
  let stack = configProjectWorkDir env
  fp <- parseRelFile $ concat
    [ packageNameString $ packageName package'
    , "-"
    , versionString $ packageVersion package'
    , ".log"
    ]
  return $ stack </> $(mkRelDir "logs") </> fp

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
