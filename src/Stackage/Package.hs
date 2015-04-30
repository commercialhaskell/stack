{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dealing with Cabal.

module Stackage.Build.Cabal
       (getPackage, flagsForPackage, parsePackageName, parseVersion,
        getPkgIndex, resolvePackageVersions, fetchPackage, getPkgVersions,
        downloadPkgIndex, loadPkgIndex, PackageIndex, PackageSuggestion(..))
       where

import           Codec.Archive.Tar
import           Codec.Compression.GZip as GZip
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (logDebug,MonadLogger)
import           Control.Monad.Loops
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
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
import qualified Data.Text.Encoding as T
import qualified Data.Version as V ( parseVersion )
import           Data.Yaml (ParseException)
import           Distribution.Compiler
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.ModuleName as Cabal
import           Distribution.Package hiding (Package,PackageName)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Utils
import           Distribution.System
import           Distribution.Text (display)
import           Distribution.Version
import           Filesystem
import           Filesystem.Loc as FL
import qualified Filesystem.Path.CurrentOS as FP
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Prelude hiding (FilePath)
import           Stackage.Constants
import           Stackage.PackageName
import           System.Directory
import           System.IO
import           System.IO.Temp
import qualified Text.ParserCombinators.ReadP as ReadP ( readP_to_S )

-- | All exceptions thrown by the library.
data FPException
  = FPConfigError ParseException
  | FPNoConfigFile
  | FPNoCabalFile (Loc Absolute Dir)
  | FPInvalidCabalFile (Loc Absolute File) PError
  | FPNoDeps (Loc Absolute File)
  | FPDepCycle PackageName
  | FPMissingDep Package PackageName VersionRange
  | FPDependencyIssues [FPException]
  | FPMissingTool Dependency
  | FPCouldn'tFindPkgId PackageName
  | FPPackageDownloadError PackageName (Response L.ByteString)
  | FPResolvePackagesError [PackageName] (Response L.ByteString)
  | FPIndexDownloadError (Response L.ByteString)
  | FPStackagePackageVersionMismatch PackageName Version Version
  | FPStackageDepVerMismatch PackageName Version VersionRange
  deriving (Show,Typeable)
instance Exception FPException

-- | Run a Setup.hs action after building a package, before installing.
data FinalAction
  = DoTests
  | DoBenchmarks
  | DoHaddock
  | DoNothing
  deriving (Eq,Bounded,Enum,Show)

-- | A suggestion for package version/flags from stackage.org
data PackageSuggestion =
  PackageSuggestion {suggestionName :: !PackageName
                    ,suggestionVersion :: !Version
                    ,suggestionFlags :: !(Map Text Bool)}
  deriving (Show)

instance FromJSON PackageSuggestion where
  parseJSON j =
    do o <- parseJSON j
       name <- fmap unAeson (o .: "name")
       ver <- fmap unAeson (o .: "version")
       flags <- o .: "flags"
       return (PackageSuggestion name ver flags)

-- | Simple wrapper for orphan instances.
newtype Aeson a = Aeson { unAeson :: a}

instance FromJSON (Aeson Version) where
  parseJSON j =
    do s <- parseJSON j
       case parseVersion s of
         Nothing ->
           fail "Couldn't parse version."
         Just ver -> return (Aeson ver)

instance FromJSON (Aeson PackageName) where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageName (T.encodeUtf8 s) of
         Nothing ->
           fail "Couldn't parse version."
         Just n -> return (Aeson n)

-- | Wrapper to an existant package index.
newtype PackageIndex =
  PackageIndex (Loc Absolute Dir)

-- | Some package info.
data Package =
  Package {pinfoName :: !PackageName                      -- ^ Name of the package.
               ,pinfoVersion :: !Version                 -- ^ Version of the package
               ,pinfoDir :: !(Loc Absolute Dir)          -- ^ Directory of the package.
               ,pinfoFiles :: !(Set (Loc Absolute File)) -- ^ Files that the package depends on.
               ,pinfoDeps :: !(Map PackageName VersionRange)   -- ^ Packages that the package depends on.
               ,pinfoTools :: ![Dependency]              -- ^ A build tool name.
               ,pinfoAllDeps :: !(Set PackageName)             -- ^ Original dependencies (not sieved).
               ,pinfoFlags :: !(Map Text Bool)           -- ^ Flags used on package.
               }
  deriving (Show,Typeable,Data)

-- | Compares the package name.
instance Ord Package where
  compare = on compare pinfoName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) pinfoName

-- | Stackage build config.

-- | Get dependencies of a package.
getPackage :: FinalAction
         -> Map Text Bool
         -> Map PackageName (Map Text Bool)
         -> Loc Absolute File
         -> IO Package
getPackage finalAction flags packageFlags cabalfp =
  do chars <-
       Prelude.readFile (FL.encodeString cabalfp)
     case parsePackageDescription chars of
       ParseFailed per ->
         throwIO (FPInvalidCabalFile cabalfp per)
       ParseOk _ gpkg ->
         let pkgId =
               package (packageDescription gpkg)
             name = fromCabalPackageName (pkgName pkgId)
             pkgFlags =
               flagsForPackage gpkg flags packageFlags name
             pkg =
               resolvePackage finalAction pkgFlags gpkg
         in case packageDependencies pkg of
              deps
                | M.null deps ->
                  throwIO (FPNoDeps cabalfp)
                | otherwise ->
                  do let dir = FL.parent cabalfp
                     pkgFiles <-
                       packageFiles dir pkg
                     let files = cabalfp : pkgFiles
                         deps' =
                           M.filterWithKey (const . (/= name))
                                           deps
                     return (Package {pinfoName = name
                                   ,pinfoVersion = pkgVersion pkgId
                                   ,pinfoDeps = deps'
                                   ,pinfoDir = dir
                                   ,pinfoFiles = S.fromList files
                                   ,pinfoTools = packageTools pkg
                                   ,pinfoFlags = pkgFlags
                                   ,pinfoAllDeps =
                                      S.fromList (M.keys deps')})

-- | Combine global and package-specific flags together, with
-- preference for the package-specific flags overriding global ones.
flagsForPackage :: GenericPackageDescription
                -> Map Text Bool
                -> Map PackageName (Map Text Bool)
                -> PackageName
                -> Map Text Bool
flagsForPackage gpkg globalFlags packageFlags pname =
  M.union (fromMaybe mempty (M.lookup pname packageFlags))
          (M.union globalFlags defaultFlags)
  where defaultFlags =
          M.fromList
            (map (unFlagName . flagName &&& flagDefault)
                 (genPackageFlags gpkg))
          where unFlagName (FlagName n) = T.pack n

-- | Get all dependencies of the package (buildable targets only).
packageDependencies :: PackageDescription -> Map PackageName VersionRange
packageDependencies =
  M.fromList .
  concatMap (map (\dep -> ( (depName dep),depRange dep)) .
             targetBuildDepends) .
  allBuildInfo

-- | Get all dependencies of the package (buildable targets only).
packageTools :: PackageDescription -> [Dependency]
packageTools = concatMap buildTools . allBuildInfo

-- | Get all files referenced by the package.
packageFiles :: Loc Absolute Dir -> PackageDescription -> IO [Loc Absolute File]
packageFiles dir pkg =
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
resolveGlobFiles :: Loc Absolute Dir -> [String] -> IO [Loc Absolute File]
resolveGlobFiles dir = fmap concat . mapM resolve
  where resolve name =
          if any (== '*') name
             then explode name
             else return [(either (error . show)
                                  (appendLoc dir)
                                  (FL.parseRelativeFileLoc (FP.decodeString name)))]
        explode name =
          fmap (map (either (error . show) (appendLoc dir) .
                     FL.parseRelativeFileLoc . FP.decodeString))
               (matchDirFileGlob (FL.encodeString dir)
                                 name)

-- | Get all files referenced by the executable.
executableFiles :: Loc Absolute Dir -> Executable -> IO [Loc Absolute File]
executableFiles dir exe =
  do exposed <- resolveFiles
                  (map (either (error . show)
                               (appendLoc dir) .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (hsSourceDirs build) ++
                   [dir])
                  [Right (modulePath exe)]
                  haskellFileExts
     bfiles <- buildFiles dir build
     return (concat [bfiles,exposed])
  where build = buildInfo exe

-- | Get all files referenced by the library.
libraryFiles :: Loc Absolute Dir -> Library -> IO [Loc Absolute File]
libraryFiles dir lib =
  do exposed <- resolveFiles
                  (map (either (error . show) (appendLoc dir) .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (hsSourceDirs build) ++
                   [dir])
                  (map Left (exposedModules lib))
                  haskellFileExts
     bfiles <- buildFiles dir build
     return (concat [bfiles,exposed])
  where build = libBuildInfo lib

-- | Get all files in a build.
buildFiles :: Loc Absolute Dir -> BuildInfo -> IO [Loc Absolute File]
buildFiles dir build =
  do other <- resolveFiles
                (map (either (error . show) (appendLoc dir) .
                      FL.parseRelativeDirLoc . FP.decodeString)
                     (hsSourceDirs build) ++
                 [dir])
                (map Left (otherModules build))
                haskellFileExts
     return (concat [other
                    ,map (either (error . show) (appendLoc dir) .
                          FL.parseRelativeFileLoc . FP.decodeString)
                         (cSources build)])

-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackage :: FinalAction
               -> Map Text Bool
               -> GenericPackageDescription
               -> PackageDescription
resolvePackage finalAction passedFlags (GenericPackageDescription desc defaultFlags mlib exes tests benches) =
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
          M.union passedFlags
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
               ,testEnabled =
                  case finalAction of
                    DoTests -> True
                    _ -> False}
        updateBenchmarkDeps benchmark deps =
          benchmark {benchmarkBuildInfo =
                       (benchmarkBuildInfo benchmark) {targetBuildDepends = deps}
                    ,benchmarkEnabled =
                       case finalAction of
                         DoBenchmarks -> True
                         _ -> False}

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
resolveFiles :: [Loc Absolute Dir] -- ^ Directories to look in.
             -> [Either ModuleName String] -- ^ Base names.
             -> [Text] -- ^ Extentions.
             -> IO [Loc Absolute File]
resolveFiles dirs names exts =
  fmap catMaybes (forM names makeNameCandidates)
  where makeNameCandidates name =
          firstM (isFile . FL.toFilePath)
                 (concatMap (makeDirCandidates name) dirs)
        makeDirCandidates :: Either ModuleName String
                          -> Loc Absolute Dir
                          -> [Loc Absolute File]
        makeDirCandidates name dir =
          map (\ext ->
                 case name of
                   Left mn ->
                     (either (error . show)
                             (appendLoc dir)
                             (FL.parseRelativeFileLoc
                                (FP.addExtension (FP.decodeString (Cabal.toFilePath mn))
                                                 ext)))
                   Right fp ->
                     either (error . show)
                            (appendLoc dir)
                            (FL.parseRelativeFileLoc (FP.decodeString fp)))
              exts

-- | Parse a package version.
parseVersion :: String -> Maybe Version
parseVersion s =
  case reverse (ReadP.readP_to_S V.parseVersion s) of
    ((ver,""):_) -> Just ver
    _ -> Nothing

-- | Try to get the package index.
getPkgIndex :: MonadIO m => Loc Absolute Dir -> m (Maybe PackageIndex)
getPkgIndex dir =
  do exists <-
       liftIO (doesDirectoryExist (FL.encodeString dir))
     return (if exists
                then Just (PackageIndex dir)
                else Nothing)

-- | Load the package index, if it does not exist, download it.
loadPkgIndex :: (MonadMask m,MonadLogger m,MonadThrow m,MonadIO m)
             => Loc Absolute Dir -> m PackageIndex
loadPkgIndex dir =
  do mindex <- liftIO (getPkgIndex dir)
     case mindex of
       Just index -> return index
       Nothing ->
         do liftIO (putStrLn "No package index. Downloading latest ...")
            index <- downloadPkgIndex dir "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
            liftIO (putStrLn "Downloaded and unpacked package index.")
            return index

-- | Get the package index.
-- TODO: Catch http exceptions.
-- Example usage:
-- getPkgIndex $(mkAbsoluteDir "/home/chris/.stackage/pkg-index") "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
downloadPkgIndex :: (MonadMask m,MonadLogger m,MonadThrow m,MonadIO m)
                 => Loc Absolute Dir -> String -> m PackageIndex
downloadPkgIndex dir url =
  do req <- parseUrl url
     $logDebug "Downloading package index ..."
     resp <-
       liftIO (withManager defaultManagerSettings
                           (httpLbs req))
     case responseStatus resp of
       Status 200 _ ->
         withSystemTempFile
           "pkg-index"
           (\fp h ->
              do $logDebug "Decompressing ..."
                 liftIO (L.hPutStr h (GZip.decompress (responseBody resp)))
                 liftIO (hClose h)
                 $logDebug "Extracting ..."
                 liftIO (createDirectoryIfMissing True (FL.encodeString dir))
                 liftIO (extract (FL.encodeString dir) fp)
                 return (PackageIndex dir))
       _ ->
         liftIO (throwIO (FPIndexDownloadError resp))

-- | Get versions available for the given package in the index.
getPkgVersions :: MonadIO m => PackageIndex -> PackageName -> m (Maybe (Set Version))
getPkgVersions (PackageIndex dir) name =
  liftIO (do exists <-
               doesDirectoryExist (FL.encodeString pkgDir)
             if exists
                then do contents <-
                          fmap (mapMaybe parseVersion)
                               (getDirectoryContents (FL.encodeString pkgDir))
                        return (Just (S.fromList contents))
                else return Nothing)
  where pkgDir =
          appendLoc dir
                    (fromMaybe (error "Unable to produce valid directory name for package.")
                               (parseRelativeDirLoc (FP.decodeString ((packageNameString name)))))

-- | Resolve package versions.
-- TODO: Catch http exceptions.
-- TODO: Handle non-existent package case.
-- Example usage:
-- runNoLoggingT (resolvePackageVersions [PackageName "warp",PackageName "snap"])
resolvePackageVersions :: (MonadThrow m,MonadIO m) => [PackageName] -> m [PackageSuggestion]
resolvePackageVersions (null -> True) = return []
resolvePackageVersions names =
  do liftIO (putStrLn "Resolving package versions against Stackage ...")
     req <- parseUrl url
     resp <-
       liftIO (withManager defaultManagerSettings
                           (httpLbs req))
     case responseStatus resp of
       Status 200 _ ->
         case decode (responseBody resp) of
           Nothing ->
             liftIO (throwIO (FPResolvePackagesError names resp))
           Just vers -> return vers
       _ ->
         liftIO (throwIO (FPResolvePackagesError names resp))
  where url = "http://www.stackage.org/lts/build-plan?" ++
              packages ++ "&_accept=application/json"
        packages =
          intercalate
            "&"
            (map (\x ->
                    "package=" ++
                    (packageNameString x))
                 names)


-- | Fetch the package index.
-- Example usage: runStdoutLoggingT (fetchPackage $(mkAbsoluteDir "/home/chris/.stackage/pkg-index") (fromJust (parsePackageName "lens")) (fromJust (parseVersion "4.6.0.1")))
fetchPackage :: (MonadMask m,MonadLogger m,MonadThrow m,MonadIO m)
             => PackageIndex -> PackageName -> Version -> m (Loc Absolute Dir)
fetchPackage (PackageIndex dir) name ver =
  do unpacked <-
       packageUnpacked (PackageIndex dir)
                       name
                       ver
     if unpacked
        then return pkgVerContentsDir
        else do req <- parseUrl url
                liftIO (putStrLn ((packageNameString name) ++
                                  ": downloading " ++ display ver))
                resp <-
                  liftIO (withManager defaultManagerSettings
                                      (httpLbs req))
                case responseStatus resp of
                  Status 200 _ ->
                    withSystemTempFile
                      "pkg-index"
                      (\fp h ->
                         do indexCabalFile <-
                              liftIO (S.readFile oldCabalFilePath)
                            $logDebug (T.pack ("Decompressing " ++
                                               (packageNameString name)))
                            liftIO (L.hPutStr h (GZip.decompress (responseBody resp)))
                            liftIO (hClose h)
                            $logDebug (T.pack ("Extracting to " ++
                                               FL.encodeString pkgVerDir))
                            liftIO (extract (FL.encodeString pkgVerDir) fp)
                            $logDebug (T.pack ("Updating cabal file " ++
                                               newCabalFilePath))
                            liftIO (S.writeFile newCabalFilePath indexCabalFile)
                            return pkgVerContentsDir)
                  _ ->
                    liftIO (throwIO (FPPackageDownloadError name resp))
  where newCabalFilePath =
          FL.encodeString
            (appendLoc pkgVerDir
                       (fromMaybe (error "Unable to make valid .cabal file name.")
                                  (parseRelativeFileLoc
                                     (FP.decodeString
                                        (nameVer ++
                                         "/" ++
                                         (packageNameString name) ++
                                         ".cabal")))))
        oldCabalFilePath =
          FL.encodeString
            (appendLoc pkgVerDir
                       (fromMaybe (error "Unable to make valid .cabal file name.")
                                  (parseRelativeFileLoc
                                     (FP.decodeString
                                        ((packageNameString name) ++
                                         ".cabal")))))
        url =
          concat ["http://hackage.haskell.org/package/"
                 ,nameVer
                 ,"/"
                 ,nameVer
                 ,".tar.gz"] -- TODO: customize this.
        nameVer =
          (packageNameString name) ++
          "-" ++ display ver
        pkgVerContentsDir :: Loc Absolute Dir
        pkgVerContentsDir =
          mkPkgVerContentsDir dir name ver
        pkgVerDir :: Loc Absolute Dir
        pkgVerDir = mkPkgVerDir dir name ver

-- | Has the package been unpacked already?
packageUnpacked :: (MonadIO m)
                => PackageIndex -> PackageName -> Version -> m Bool
packageUnpacked (PackageIndex dir) name ver =
  liftIO (doesDirectoryExist (FL.encodeString (mkPkgVerContentsDir dir name ver)))

-- | Make the directory for the package version (with a single .cabal file in it).
mkPkgVerDir :: Loc Absolute Dir -> PackageName -> Version -> Loc Absolute Dir
mkPkgVerDir dir name ver =
  appendLoc dir
            (fromMaybe (error "Unable to make valid path name for package-version.")
                       (parseRelativeDirLoc
                          (FP.decodeString
                             ((packageNameString name) ++
                              "/" ++ display ver))))

-- | Make the directory for the package contents (with the .cabal and sources, etc).
mkPkgVerContentsDir :: Loc Absolute Dir -> PackageName -> Version -> Loc Absolute Dir
mkPkgVerContentsDir dir name ver =
  appendLoc (mkPkgVerDir dir name ver)
            (fromMaybe (error "Unable to make valid path name for package-version.")
                       (parseRelativeDirLoc
                          (FP.decodeString
                             ((packageNameString name) ++
                              "-" ++ display ver))))
