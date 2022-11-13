{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | A module which exports all package-level file-gathering logic.
module Stack.PackageFile
  ( getPackageFile
  , packageDescModulesAndFiles
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.CabalSpecVersion ( CabalSpecVersion )
import           Distribution.ModuleName ( ModuleName )
import           Distribution.PackageDescription
                   ( BuildType (..), PackageDescription, benchmarkName
                   , benchmarks, buildType, dataDir, dataFiles, exeName
                   , executables, extraSrcFiles, libName, library
                   , libraryNameString, specVersion, subLibraries, testName
                   , testSuites )
import           Distribution.Simple.Glob ( matchDirFileGlob )
import qualified Distribution.Types.UnqualComponentName as Cabal
import           Path ( parent, (</>) )
import           Path.Extra ( forgivingResolveFile, rejectMissingFile )
import           Path.IO ( doesFileExist )
import           Stack.ComponentFile
                   ( benchmarkFiles, executableFiles, libraryFiles
                   , resolveOrWarn, testFiles
                   )
import           Stack.Constants
                   ( relFileHpackPackageConfig, relFileSetupHs, relFileSetupLhs
                   )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude
import           Stack.Types.BuildConfig ( HasBuildConfig (..) )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.EnvConfig ( HasEnvConfig )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.PackageFile
                   ( DotCabalPath (..), GetPackageFileContext (..)
                   , PackageWarning (..)
                   )
import qualified System.FilePath as FilePath
import           System.IO.Error ( isUserError )
import Stack.Constants.Config ( distDirFromDir )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.BuildConfig
                   ( HasBuildConfig(buildConfigL) )
import           Stack.Constants
                   ( relFileSetupHs, relFileSetupLhs, relFileHpackPackageConfig )
import           Path.IO (doesFileExist)

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn :: FilePath.FilePath
                  -> RIO GetPackageFileContext (Maybe (Path Abs File))
resolveFileOrWarn = resolveOrWarn "File" f
 where
  f p x = forgivingResolveFile p x >>= rejectMissingFile

-- | Get all files referenced by the package.
packageDescModulesAndFiles ::
     PackageDescription
  -> RIO
       GetPackageFileContext
       ( Map NamedComponent (Map ModuleName (Path Abs File))
       , Map NamedComponent [DotCabalPath]
       , Set (Path Abs File)
       , [PackageWarning]
       )
packageDescModulesAndFiles pkg = do
  (libraryMods, libDotCabalFiles, libWarnings) <-
    maybe
      (pure (M.empty, M.empty, []))
      (asModuleAndFileMap libComponent libraryFiles)
      (library pkg)
  (subLibrariesMods, subLibDotCabalFiles, subLibWarnings) <-
    fmap
      foldTuples
      ( mapM
          (asModuleAndFileMap subLibComponent libraryFiles)
          (subLibraries pkg)
      )
  (executableMods, exeDotCabalFiles, exeWarnings) <-
    fmap
      foldTuples
      ( mapM
          (asModuleAndFileMap exeComponent executableFiles)
          (executables pkg)
      )
  (testMods, testDotCabalFiles, testWarnings) <-
    fmap
      foldTuples
      (mapM (asModuleAndFileMap testComponent testFiles) (testSuites pkg))
  (benchModules, benchDotCabalPaths, benchWarnings) <-
    fmap
      foldTuples
      ( mapM
          (asModuleAndFileMap benchComponent benchmarkFiles)
          (benchmarks pkg)
      )
  dfiles <- resolveGlobFiles
              (specVersion pkg)
              ( extraSrcFiles pkg
                ++ map (dataDir pkg FilePath.</>) (dataFiles pkg)
              )
  let modules = libraryMods <> subLibrariesMods <> executableMods <> testMods <>
                  benchModules
      files = libDotCabalFiles <> subLibDotCabalFiles <> exeDotCabalFiles <>
                testDotCabalFiles <> benchDotCabalPaths
      warnings = libWarnings <> subLibWarnings <> exeWarnings <> testWarnings <>
                   benchWarnings
  pure (modules, files, dfiles, warnings)
 where
  libComponent = const CLib
  subLibComponent =
    CSubLib . T.pack . maybe
      "" Cabal.unUnqualComponentName . libraryNameString . libName
  exeComponent = CExe . T.pack . Cabal.unUnqualComponentName . exeName
  testComponent = CTest . T.pack . Cabal.unUnqualComponentName . testName
  benchComponent = CBench . T.pack . Cabal.unUnqualComponentName . benchmarkName
  asModuleAndFileMap label f lib = do
    (a, b, c) <- f (label lib) lib
    pure (M.singleton (label lib) a, M.singleton (label lib) b, c)
  foldTuples = foldl' (<>) (M.empty, M.empty, [])


-- | Resolve globbing of files (e.g. data files) to absolute paths.
resolveGlobFiles ::
     CabalSpecVersion -- ^ Cabal file version
  -> [String]
  -> RIO GetPackageFileContext (Set (Path Abs File))
resolveGlobFiles cabalFileVersion =
  fmap (S.fromList . catMaybes . concat) .
  mapM resolve
 where
  resolve name =
    if '*' `elem` name
      then explode name
      else fmap pure (resolveFileOrWarn name)
  explode name = do
    dir <- asks (parent . ctxFile)
    names <- matchDirFileGlob' (toFilePath dir) name
    mapM resolveFileOrWarn names
  matchDirFileGlob' dir glob =
    catch
      (liftIO (matchDirFileGlob minBound cabalFileVersion dir glob))
      ( \(e :: IOException) ->
        if isUserError e
          then do
            prettyWarnL
              [ flow "Wildcard does not match any files:"
              , style File $ fromString glob
              , line <> flow "in directory:"
              , style Dir $ fromString dir
              ]
            pure []
          else throwIO e
      )

-- | Gets all of the modules, files, build files, and data files that constitute
-- the package. This is primarily used for dirtiness checking during build, as
-- well as use by "stack ghci"
getPackageFile ::
     ( HasEnvConfig s, MonadReader s m, MonadThrow m, MonadUnliftIO m )
  => PackageDescription
  -> Path Abs File
  -> m ( Map NamedComponent (Map ModuleName (Path Abs File))
       , Map NamedComponent [DotCabalPath]
       , Set (Path Abs File)
       , [PackageWarning]
       )
getPackageFile pkg cabalfp =
  debugBracket ("getPackageFiles" <+> pretty cabalfp) $ do
    let pkgDir = parent cabalfp
    distDir <- distDirFromDir pkgDir
    bc <- view buildConfigL
    cabalVer <- view cabalVersionL
    (componentModules, componentFiles, dataFiles', warnings) <-
      runRIO
        (GetPackageFileContext cabalfp distDir bc cabalVer)
        (packageDescModulesAndFiles pkg)
    setupFiles <-
      if buildType pkg == Custom
      then do
        let setupHsPath = pkgDir </> relFileSetupHs
            setupLhsPath = pkgDir </> relFileSetupLhs
        setupHsExists <- doesFileExist setupHsPath
        if setupHsExists
          then pure (S.singleton setupHsPath)
          else do
            setupLhsExists <- doesFileExist setupLhsPath
            if setupLhsExists
              then pure (S.singleton setupLhsPath)
              else pure S.empty
      else pure S.empty
    buildFiles <- fmap (S.insert cabalfp . S.union setupFiles) $ do
      let hpackPath = pkgDir </> relFileHpackPackageConfig
      hpackExists <- doesFileExist hpackPath
      pure $ if hpackExists then S.singleton hpackPath else S.empty
    pure
      ( componentModules
      , componentFiles
      , buildFiles <> dataFiles'
      , warnings
      )
