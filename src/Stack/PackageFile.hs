{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | A module which exports all package-level file-gathering logic.
module Stack.PackageFile
  ( getPackageFile
  , stackPackageFileFromCabal
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Distribution.CabalSpecVersion ( CabalSpecVersion )
import qualified Distribution.PackageDescription as Cabal
import           Distribution.Simple.Glob ( matchDirFileGlob )
import           Distribution.Utils.Path ( makeSymbolicPath, getSymbolicPath )
import           Path ( parent, (</>) )
import           Path.Extra ( forgivingResolveFile, rejectMissingFile )
import           Path.IO ( doesFileExist )
import           Stack.ComponentFile
                   ( ComponentFile (..), resolveOrWarn, stackBenchmarkFiles
                   , stackExecutableFiles, stackLibraryFiles
                   , stackTestSuiteFiles
                   )
import           Stack.Constants
                   ( relFileHpackPackageConfig, relFileSetupHs, relFileSetupLhs
                   )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude
import           Stack.Types.BuildConfig ( HasBuildConfig (..) )
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.EnvConfig ( HasEnvConfig (..) )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Package ( Package(..) )
import           Stack.Types.PackageFile
                   ( GetPackageFileContext (..), PackageComponentFile (..)
                   , StackPackageFile (..)
                   )
import qualified System.FilePath as FilePath
import           System.IO.Error ( isUserError )

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn ::
     FilePath.FilePath
  -> RIO GetPackageFileContext (Maybe (Path Abs File))
resolveFileOrWarn = resolveOrWarn "File" f
 where
  f p x = forgivingResolveFile p x >>= rejectMissingFile

-- | Get all files referenced by the package.
packageDescModulesAndFiles ::
     Package
  -> RIO
       GetPackageFileContext
       PackageComponentFile
packageDescModulesAndFiles pkg = do
  packageExtraFile <- resolveGlobFilesFromStackPackageFile
              pkg.cabalSpec pkg.file
  let initialValue = mempty{packageExtraFile=packageExtraFile}
  let accumulator f comp st = (insertComponentFile <$> st) <*> f comp
  let gatherCompFileCollection createCompFileFn getCompFn res =
        foldr' (accumulator createCompFileFn) res (getCompFn pkg)
  gatherCompFileCollection stackLibraryFiles (.library)
    . gatherCompFileCollection stackLibraryFiles (.subLibraries)
    . gatherCompFileCollection stackExecutableFiles (.executables)
    . gatherCompFileCollection stackTestSuiteFiles (.testSuites)
    . gatherCompFileCollection stackBenchmarkFiles (.benchmarks)
    $ pure initialValue

resolveGlobFilesFromStackPackageFile ::
     CabalSpecVersion
  -> StackPackageFile
  -> RIO GetPackageFileContext (Set (Path Abs File))
resolveGlobFilesFromStackPackageFile
    csvV
    (StackPackageFile extraSrcFilesV dataDirV dataFilesV)
  = resolveGlobFiles
      csvV
      (extraSrcFilesV ++ map (dataDirV FilePath.</>) dataFilesV)

-- | Resolve globbing of files (e.g. data files) to absolute paths.
resolveGlobFiles ::
     CabalSpecVersion -- ^ Cabal file version
  -> [String]
  -> RIO GetPackageFileContext (Set (Path Abs File))
resolveGlobFiles cabalFileVersion =
  fmap (S.fromList . concatMap catMaybes) . mapM resolve
 where
  resolve :: FilePath -> RIO GetPackageFileContext [Maybe (Path Abs File)]
  resolve name =
    if '*' `elem` name
      then explode name
      else fmap pure (resolveFileOrWarn name)

  explode :: FilePath -> RIO GetPackageFileContext [Maybe (Path Abs File)]
  explode name = do
    dir <- asks (parent . (.file))
    names <- matchDirFileGlob' (toFilePath dir) name
    mapM resolveFileOrWarn names

  matchDirFileGlob' ::
       FilePath
    -> FilePath
    -> RIO GetPackageFileContext [FilePath]
  matchDirFileGlob' dir glob = map getSymbolicPath <$> do
    catch
      (liftIO $ matchDirFileGlob minBound cabalFileVersion (Just $ makeSymbolicPath dir) (makeSymbolicPath glob))
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
  => Package
  -> Path Abs File
  -> m PackageComponentFile
getPackageFile pkg cabalFP =
  debugBracket ("getPackageFiles" <+> pretty cabalFP) $ do
    let pkgDir = parent cabalFP
    distDir <- distDirFromDir pkgDir
    bc <- view buildConfigL
    cabalVer <- view cabalVersionL
    packageComponentFile <-
      runRIO
        (GetPackageFileContext cabalFP distDir bc cabalVer)
        (packageDescModulesAndFiles pkg)
    setupFiles <-
      if pkg.buildType == Cabal.Custom
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
    moreBuildFiles <- fmap (S.insert cabalFP . S.union setupFiles) $ do
      let hpackPath = pkgDir </> relFileHpackPackageConfig
      hpackExists <- doesFileExist hpackPath
      pure $ if hpackExists then S.singleton hpackPath else S.empty
    pure packageComponentFile
      { packageExtraFile =
          moreBuildFiles <> packageComponentFile.packageExtraFile
      }

stackPackageFileFromCabal :: Cabal.PackageDescription -> StackPackageFile
stackPackageFileFromCabal cabalPkg =
  StackPackageFile
    (map getSymbolicPath $ Cabal.extraSrcFiles cabalPkg)
    (getSymbolicPath $ Cabal.dataDir cabalPkg)
    (map getSymbolicPath $ Cabal.dataFiles cabalPkg)

insertComponentFile ::
     PackageComponentFile
  -> (NamedComponent, ComponentFile)
  -> PackageComponentFile
insertComponentFile packageCompFile (name, compFile) =
  PackageComponentFile nCompFile nDotCollec packageExtraFile nWarnings
 where
  (ComponentFile moduleFileMap dotCabalFileList warningsCollec) = compFile
  (PackageComponentFile modules files packageExtraFile warnings) =
    packageCompFile
  nCompFile = M.insert name moduleFileMap modules
  nDotCollec = M.insert name dotCabalFileList files
  nWarnings = warningsCollec ++ warnings
