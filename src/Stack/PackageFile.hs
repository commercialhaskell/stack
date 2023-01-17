{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module which exports all package-level file-gathering logic.
module Stack.PackageFile
  ( packageDescModulesAndFiles
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.CabalSpecVersion ( CabalSpecVersion )
import           Distribution.ModuleName ( ModuleName )
import           Distribution.PackageDescription hiding ( FlagName )
import           Distribution.Simple.Glob ( matchDirFileGlob )
import qualified Distribution.Types.UnqualComponentName as Cabal
import           Path ( parent )
import           Path.Extra ( forgivingResolveFile, rejectMissingFile )
import           Stack.ComponentFile
                   ( benchmarkFiles, executableFiles, libraryFiles
                   , resolveOrWarn, testFiles
                   )
import           Stack.Prelude
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.PackageFile
                   ( DotCabalPath (..), GetPackageFileContext (..)
                   , PackageWarning (..)
                   )
import qualified System.FilePath as FilePath
import           System.IO.Error ( isUserError )

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn :: FilePath.FilePath
                  -> RIO GetPackageFileContext (Maybe (Path Abs File))
resolveFileOrWarn = resolveOrWarn "File" f
 where
  f p x = liftIO (forgivingResolveFile p x) >>= rejectMissingFile

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
          (asModuleAndFileMap internalLibComponent libraryFiles)
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
  internalLibComponent =
    CInternalLib . T.pack . maybe
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
