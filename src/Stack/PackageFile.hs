{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | A module which exports all package-level file-gathering logic.
module Stack.PackageFile
  ( getPackageFile
  , stackPackageFileFromCabal
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Distribution.CabalSpecVersion ( CabalSpecVersion )
import           Distribution.PackageDescription
                  ( PackageDescription(dataFiles, extraSrcFiles, dataDir),
                  BuildType(Custom) )
import           Distribution.Simple.Glob ( matchDirFileGlob )
import           Path ( parent, (</>) )
import           Path.Extra ( forgivingResolveFile, rejectMissingFile )
import           Path.IO ( doesFileExist )
import           Stack.ComponentFile
                   ( resolveOrWarn, ComponentFile (ComponentFile)
                   , stackLibraryFiles, stackExecutableFiles, stackBenchmarkFiles
                   )
import           Stack.Constants
                   ( relFileHpackPackageConfig, relFileSetupHs, relFileSetupLhs
                   )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude
import           Stack.Types.CompilerPaths ( cabalVersionL )
import           Stack.Types.EnvConfig
                   ( HasEnvConfig (..) )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.PackageFile
                   ( GetPackageFileContext (..)
                   , StackPackageFile (StackPackageFile), PackageComponentFile (PackageComponentFile, packageExtraFile)
                   )
import qualified System.FilePath as FilePath
import           System.IO.Error ( isUserError )
import           Stack.Types.BuildConfig
                   ( HasBuildConfig(buildConfigL) )
import           Stack.Types.Package (Package(..))
import           Data.Foldable (Foldable(..))

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn :: FilePath.FilePath
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
              (packageCabalSpec pkg) (packageFile pkg)
  let initialValue = mempty{packageExtraFile=packageExtraFile}
  let accumulator f comp st = (insertComponentFile <$> st) <*> f comp
  let gatherCompFileCollection createCompFileFn getCompFn res = foldr' (accumulator createCompFileFn) res (getCompFn pkg)
  gatherCompFileCollection stackLibraryFiles packageLibrary
          . gatherCompFileCollection stackLibraryFiles packageSubLibraries
          . gatherCompFileCollection stackExecutableFiles packageExecutables
          . gatherCompFileCollection stackBenchmarkFiles packageBenchmarkSuites $ pure initialValue


resolveGlobFilesFromStackPackageFile :: CabalSpecVersion -> StackPackageFile -> RIO GetPackageFileContext (Set (Path Abs File))
resolveGlobFilesFromStackPackageFile csvV (StackPackageFile extraSrcFilesV dataDirV dataFilesV) =
  resolveGlobFiles
                csvV
                ( extraSrcFilesV
                  ++ map (dataDirV FilePath.</>) dataFilesV
                )

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
  => Package
  -> Path Abs File
  -> m PackageComponentFile
getPackageFile pkg cabalfp =
  debugBracket ("getPackageFiles" <+> pretty cabalfp) $ do
    let pkgDir = parent cabalfp
    distDir <- distDirFromDir pkgDir
    bc <- view buildConfigL
    cabalVer <- view cabalVersionL
    packageComponentFile <-
      runRIO
        (GetPackageFileContext cabalfp distDir bc cabalVer)
        (packageDescModulesAndFiles pkg)
    setupFiles <-
      if packageBuildType pkg == Custom
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
    moreBuildFiles <- fmap (S.insert cabalfp . S.union setupFiles) $ do
      let hpackPath = pkgDir </> relFileHpackPackageConfig
      hpackExists <- doesFileExist hpackPath
      pure $ if hpackExists then S.singleton hpackPath else S.empty
    pure packageComponentFile{packageExtraFile = moreBuildFiles <> packageExtraFile packageComponentFile}

stackPackageFileFromCabal :: PackageDescription -> StackPackageFile
stackPackageFileFromCabal cabalPkg =
  StackPackageFile (extraSrcFiles cabalPkg) (dataDir cabalPkg) (dataFiles cabalPkg)

insertComponentFile :: PackageComponentFile -> (NamedComponent, ComponentFile) -> PackageComponentFile
insertComponentFile packageCompFile (name, compFile) =
  PackageComponentFile nCompFile nDotCollec packageExtraFile nWarnings
  where
    (ComponentFile moduleFileMap dotCabalFileList warningsCollec) = compFile
    (PackageComponentFile modules files packageExtraFile warnings) = packageCompFile
    nCompFile = M.insert name moduleFileMap modules
    nDotCollec = M.insert name dotCabalFileList files
    nWarnings = warningsCollec ++ warnings
