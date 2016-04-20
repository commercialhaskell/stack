{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

-- | Dealing with Cabal.

module Stack.Package
  (readPackage
  ,readPackageBS
  ,readPackageDescriptionDir
  ,readPackageUnresolved
  ,readPackageUnresolvedBS
  ,resolvePackage
  ,findOrGenerateCabalFile
  ,hpack
  ,Package(..)
  ,GetPackageFiles(..)
  ,GetPackageOpts(..)
  ,PackageConfig(..)
  ,buildLogPath
  ,PackageException (..)
  ,resolvePackageDescription
  ,packageToolDependencies
  ,packageDependencies
  ,autogenDir
  ,checkCabalFileName
  ,printCabalFileWarning
  ,cabalFilePackageId)
  where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative (Applicative, (<$>), (<*>))
#endif

import           Control.Arrow ((&&&))
import           Control.Exception hiding (try,catch)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger,logWarn)
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Function
import           Data.List
import           Data.List.Extra (nubOrd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Maybe.Extra
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Version (showVersion)
import           Distribution.Compiler
import           Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as D
import           Distribution.Package hiding (Package,PackageName,packageName,packageVersion,PackageIdentifier)
import qualified Distribution.PackageDescription as D
import           Distribution.PackageDescription hiding (FlagName)
import           Distribution.PackageDescription.Parse
import qualified Distribution.PackageDescription.Parse as D
import           Distribution.ParseUtils
import           Distribution.Simple.Utils
import           Distribution.System (OS (..), Arch, Platform (..))
import           Distribution.Text (display, simpleParse)
import qualified Distribution.Verbosity as D
import qualified Hpack
import qualified Hpack.Config as Hpack
import           Path as FL
import           Path.Extra
import           Path.Find
import           Path.IO hiding (findFiles)
import           Prelude
import           Safe (headDef, tailSafe)
import           Stack.Build.Installed
import           Stack.Constants
import           Stack.Types
import qualified System.Directory as D
import           System.FilePath (splitExtensions, replaceExtension)
import qualified System.FilePath as FilePath
import           System.IO.Error

-- | Read the raw, unresolved package information.
readPackageUnresolved :: (MonadIO m, MonadThrow m)
                      => Path Abs File
                      -> m ([PWarning],GenericPackageDescription)
readPackageUnresolved cabalfp =
  liftIO (BS.readFile (FL.toFilePath cabalfp))
  >>= readPackageUnresolvedBS (Just cabalfp)

-- | Read the raw, unresolved package information from a ByteString.
readPackageUnresolvedBS :: (MonadThrow m)
                        => Maybe (Path Abs File)
                        -> BS.ByteString
                        -> m ([PWarning],GenericPackageDescription)
readPackageUnresolvedBS mcabalfp bs =
    case parsePackageDescription chars of
       ParseFailed per ->
         throwM (PackageInvalidCabalFile mcabalfp per)
       ParseOk warnings gpkg -> return (warnings,gpkg)
  where
    chars = T.unpack (dropBOM (decodeUtf8With lenientDecode bs))

    -- https://github.com/haskell/hackage-server/issues/351
    dropBOM t = fromMaybe t $ T.stripPrefix "\xFEFF" t

-- | Reads and exposes the package information
readPackage :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
            => PackageConfig
            -> Path Abs File
            -> m ([PWarning],Package)
readPackage packageConfig cabalfp =
  do (warnings,gpkg) <- readPackageUnresolved cabalfp
     return (warnings,resolvePackage packageConfig gpkg)

-- | Reads and exposes the package information, from a ByteString
readPackageBS :: (MonadThrow m)
              => PackageConfig
              -> BS.ByteString
              -> m ([PWarning],Package)
readPackageBS packageConfig bs =
  do (warnings,gpkg) <- readPackageUnresolvedBS Nothing bs
     return (warnings,resolvePackage packageConfig gpkg)

-- | Get 'GenericPackageDescription' and 'PackageDescription' reading info
-- from given directory.
readPackageDescriptionDir :: (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m)
  => PackageConfig
  -> Path Abs Dir
  -> m (GenericPackageDescription, PackageDescription)
readPackageDescriptionDir config pkgDir = do
    cabalfp <- findOrGenerateCabalFile pkgDir
    gdesc   <- liftM snd (readPackageUnresolved cabalfp)
    return (gdesc, resolvePackageDescription config gdesc)

-- | Print cabal file warnings.
printCabalFileWarning
    :: (MonadLogger m)
    => Path Abs File -> PWarning -> m ()
printCabalFileWarning cabalfp =
    \case
        (PWarning x) ->
            $logWarn
                ("Cabal file warning in " <> T.pack (toFilePath cabalfp) <>
                 ": " <>
                 T.pack x)
        (UTFWarning line msg) ->
            $logWarn
                ("Cabal file warning in " <> T.pack (toFilePath cabalfp) <> ":" <>
                 T.pack (show line) <>
                 ": " <>
                 T.pack msg)

-- | Check if the given name in the @Package@ matches the name of the .cabal file
checkCabalFileName :: MonadThrow m => PackageName -> Path Abs File -> m ()
checkCabalFileName name cabalfp = do
    -- Previously, we just use parsePackageNameFromFilePath. However, that can
    -- lead to confusing error messages. See:
    -- https://github.com/commercialhaskell/stack/issues/895
    let expected = packageNameString name ++ ".cabal"
    when (expected /= toFilePath (filename cabalfp))
        $ throwM $ MismatchedCabalName cabalfp name

-- | Resolve a parsed cabal file into a 'Package'.
resolvePackage :: PackageConfig
               -> GenericPackageDescription
               -> Package
resolvePackage packageConfig gpkg =
    Package
    { packageName = name
    , packageVersion = fromCabalVersion (pkgVersion pkgId)
    , packageDeps = deps
    , packageFiles = pkgFiles
    , packageTools = packageDescTools pkg
    , packageFlags = packageConfigFlags packageConfig
    , packageDefaultFlags = M.fromList
      [(fromCabalFlagName (flagName flag), flagDefault flag) | flag <- genPackageFlags gpkg]
    , packageAllDeps = S.fromList (M.keys deps)
    , packageHasLibrary = maybe False (buildable . libBuildInfo) (library pkg)
    , packageTests = M.fromList
      [(T.pack (testName t), testInterface t) | t <- testSuites pkg
                                              , buildable (testBuildInfo t)]
    , packageBenchmarks = S.fromList
      [T.pack (benchmarkName b) | b <- benchmarks pkg
                                , buildable (benchmarkBuildInfo b)]
    , packageExes = S.fromList
      [T.pack (exeName b) | b <- executables pkg
                          , buildable (buildInfo b)]
    , packageOpts = GetPackageOpts $
      \sourceMap installedMap omitPkgs addPkgs cabalfp ->
           do (componentsModules,componentFiles,_,_) <- getPackageFiles pkgFiles cabalfp
              componentsOpts <-
                  generatePkgDescOpts sourceMap installedMap omitPkgs addPkgs cabalfp pkg componentFiles
              return (componentsModules,componentFiles,componentsOpts)
    , packageHasExposedModules = maybe
          False
          (not . null . exposedModules)
          (library pkg)
    , packageSimpleType = buildType (packageDescription gpkg) == Just Simple
    }
  where
    pkgFiles = GetPackageFiles $
        \cabalfp ->
             do let pkgDir = parent cabalfp
                distDir <- distDirFromDir pkgDir
                (componentModules,componentFiles,dataFiles',warnings) <-
                    runReaderT
                        (packageDescModulesAndFiles pkg)
                        (cabalfp, buildDir distDir)
                setupFiles <-
                    if buildType pkg `elem` [Nothing, Just Custom]
                    then do
                        let setupHsPath = pkgDir </> $(mkRelFile "Setup.hs")
                            setupLhsPath = pkgDir </> $(mkRelFile "Setup.lhs")
                        setupHsExists <- doesFileExist setupHsPath
                        if setupHsExists then return (S.singleton setupHsPath) else do
                            setupLhsExists <- doesFileExist setupLhsPath
                            if setupLhsExists then return (S.singleton setupLhsPath) else return S.empty
                    else return S.empty
                buildFiles <- liftM (S.insert cabalfp . S.union setupFiles) $ do
                    let hpackPath = pkgDir </> $(mkRelFile Hpack.packageConfig)
                    hpackExists <- doesFileExist hpackPath
                    return $ if hpackExists then S.singleton hpackPath else S.empty
                return (componentModules, componentFiles, buildFiles <> dataFiles', warnings)
    pkgId = package (packageDescription gpkg)
    name = fromCabalPackageName (pkgName pkgId)
    pkg = resolvePackageDescription packageConfig gpkg
    deps = M.filterWithKey (const . (/= name)) (packageDependencies pkg)

-- | Generate GHC options for the package's components, and a list of
-- options which apply generally to the package, not one specific
-- component.
generatePkgDescOpts
    :: (HasEnvConfig env, HasPlatform env, MonadThrow m, MonadReader env m, MonadIO m)
    => SourceMap
    -> InstalledMap
    -> [PackageName] -- ^ Packages to omit from the "-package" / "-package-id" flags
    -> [PackageName] -- ^ Packages to add to the "-package" flags
    -> Path Abs File
    -> PackageDescription
    -> Map NamedComponent (Set DotCabalPath)
    -> m (Map NamedComponent BuildInfoOpts)
generatePkgDescOpts sourceMap installedMap omitPkgs addPkgs cabalfp pkg componentPaths = do
    distDir <- distDirFromDir cabalDir
    let cabalMacros = autogenDir distDir </> $(mkRelFile "cabal_macros.h")
    exists <- doesFileExist cabalMacros
    let mcabalMacros =
            if exists
                then Just cabalMacros
                else Nothing
    let generate namedComponent binfo =
            ( namedComponent
            , generateBuildInfoOpts
                  sourceMap
                  installedMap
                  mcabalMacros
                  cabalDir
                  distDir
                  omitPkgs
                  addPkgs
                  binfo
                  (fromMaybe mempty (M.lookup namedComponent componentPaths))
                  namedComponent)
    return
        ( M.fromList
              (concat
                   [ maybe
                         []
                         (return . generate CLib . libBuildInfo)
                         (library pkg)
                   , fmap
                         (\exe ->
                               generate
                                    (CExe (T.pack (exeName exe)))
                                    (buildInfo exe))
                         (executables pkg)
                   , fmap
                         (\bench ->
                               generate
                                    (CBench (T.pack (benchmarkName bench)))
                                    (benchmarkBuildInfo bench))
                         (benchmarks pkg)
                   , fmap
                         (\test ->
                               generate
                                    (CTest (T.pack (testName test)))
                                    (testBuildInfo test))
                         (testSuites pkg)]))
  where
    cabalDir = parent cabalfp

-- | Generate GHC options for the target.
generateBuildInfoOpts
    :: SourceMap
    -> InstalledMap
    -> Maybe (Path Abs File)
    -> Path Abs Dir
    -> Path Abs Dir
    -> [PackageName]
    -> [PackageName]
    -> BuildInfo
    -> Set DotCabalPath
    -> NamedComponent
    -> BuildInfoOpts
generateBuildInfoOpts sourceMap installedMap mcabalMacros cabalDir distDir omitPkgs addPkgs b dotCabalPaths componentName =
    BuildInfoOpts
        { bioOpts = ghcOpts b ++ cppOptions b
        -- NOTE for future changes: Due to this use of nubOrd (and other uses
        -- downstream), these generated options must not rely on multiple
        -- argument sequences.  For example, ["--main-is", "Foo.hs", "--main-
        -- is", "Bar.hs"] would potentially break due to the duplicate
        -- "--main-is" being removed.
        --
        -- See https://github.com/commercialhaskell/stack/issues/1255
        , bioOneWordOpts = nubOrd $ concat
            [extOpts b, srcOpts, includeOpts, extra b, extraDirs, fworks b, cObjectFiles]
        , bioPackageFlags = deps
        , bioCabalMacros = mcabalMacros
        }
  where
    cObjectFiles =
        mapMaybe (fmap toFilePath .
                  makeObjectFilePathFromC cabalDir componentName distDir)
                 cfiles
    cfiles = mapMaybe dotCabalCFilePath (S.toList dotCabalPaths)
    deps =
        concat
            [ case M.lookup name installedMap of
                Just (_, Stack.Types.Library _ident ipid) -> ["-package-id=" <> ghcPkgIdString ipid]
                _ -> ["-package=" <> packageNameString name <>
                 maybe "" -- This empty case applies to e.g. base.
                     ((("-" <>) . versionString) . sourceVersion)
                     (M.lookup name sourceMap)]
            | name <- pkgs]
    pkgs =
        addPkgs ++
        [ name
        | Dependency cname _ <- targetBuildDepends b
        , let name = fromCabalPackageName cname
        , name `notElem` omitPkgs]
    -- Generates: -package=base -package=base16-bytestring-0.1.1.6 ...
    sourceVersion (PSUpstream ver _ _) = ver
    sourceVersion (PSLocal localPkg) = packageVersion (lpPackage localPkg)
    ghcOpts = concatMap snd . filter (isGhc . fst) . options
      where
        isGhc GHC = True
        isGhc _ = False
    extOpts = map (("-X" ++) . display) . usedExtensions
    srcOpts =
        map
            (("-i" <>) . toFilePathNoTrailingSep)
            ([cabalDir | null (hsSourceDirs b)] <>
             mapMaybe toIncludeDir (hsSourceDirs b) <>
             [autogenDir distDir,buildDir distDir] <>
             [makeGenDir (buildDir distDir)
             | Just makeGenDir <- [fileGenDirFromComponentName componentName]]) ++
        ["-stubdir=" ++ toFilePathNoTrailingSep (buildDir distDir)]
    toIncludeDir "." = Just cabalDir
    toIncludeDir x = fmap (cabalDir </>) (parseRelDir x)
    includeOpts =
        [ "-I" <> toFilePathNoTrailingSep absDir
        | dir <- includeDirs b
        , absDir <- case (parseAbsDir dir, parseRelDir dir) of
          (Just ab, _       ) -> [ab]
          (_      , Just rel) -> [cabalDir </> rel]
          (Nothing, Nothing ) -> []
        ]
    extra
        = map ("-l" <>)
        . extraLibs
    extraDirs =
        [ "-L" <> toFilePathNoTrailingSep absDir
        | dir <- extraLibDirs b
        , absDir <- case (parseAbsDir dir, parseRelDir dir) of
          (Just ab, _       ) -> [ab]
          (_      , Just rel) -> [cabalDir </> rel]
          (Nothing, Nothing ) -> []
        ]
    fworks = map (\fwk -> "-framework=" <> fwk) . frameworks

-- | Make the .o path from the .c file path for a component. Example:
--
-- @
-- executable FOO
--   c-sources:        cbits/text_search.c
-- @
--
-- Produces
--
-- <dist-dir>/build/FOO/FOO-tmp/cbits/text_search.o
--
-- Example:
--
-- λ> makeObjectFilePathFromC
--     $(mkAbsDir "/Users/chris/Repos/hoogle")
--     CLib
--     $(mkAbsDir "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist")
--     $(mkAbsFile "/Users/chris/Repos/hoogle/cbits/text_search.c")
-- Just "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist/build/cbits/text_search.o"
-- λ> makeObjectFilePathFromC
--     $(mkAbsDir "/Users/chris/Repos/hoogle")
--     (CExe "hoogle")
--     $(mkAbsDir "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist")
--     $(mkAbsFile "/Users/chris/Repos/hoogle/cbits/text_search.c")
-- Just "/Users/chris/Repos/hoogle/.stack-work/Cabal-x.x.x/dist/build/hoogle/hoogle-tmp/cbits/text_search.o"
-- λ>
makeObjectFilePathFromC
    :: MonadThrow m
    => Path Abs Dir          -- ^ The cabal directory.
    -> NamedComponent        -- ^ The name of the component.
    -> Path Abs Dir          -- ^ Dist directory.
    -> Path Abs File         -- ^ The path to the .c file.
    -> m (Path Abs File) -- ^ The path to the .o file for the component.
makeObjectFilePathFromC cabalDir namedComponent distDir cFilePath = do
    relCFilePath <- stripDir cabalDir cFilePath
    relOFilePath <-
        parseRelFile (replaceExtension (toFilePath relCFilePath) "o")
    addComponentPrefix <- fileGenDirFromComponentName namedComponent
    return (addComponentPrefix (buildDir distDir) </> relOFilePath)

-- | The directory where generated files are put like .o or .hs (from .x files).
fileGenDirFromComponentName
    :: MonadThrow m
    => NamedComponent -> m (Path b Dir -> Path b Dir)
fileGenDirFromComponentName namedComponent =
    case namedComponent of
        CLib -> return id
        CExe name -> makeTmp name
        CTest name -> makeTmp name
        CBench name -> makeTmp name
  where makeTmp name = do
            prefix <- parseRelDir (T.unpack name <> "/" <> T.unpack name <> "-tmp")
            return (</> prefix)

-- | Make the autogen dir.
autogenDir :: Path Abs Dir -> Path Abs Dir
autogenDir distDir = buildDir distDir </> $(mkRelDir "autogen")

-- | Make the build dir.
buildDir :: Path Abs Dir -> Path Abs Dir
buildDir distDir = distDir </> $(mkRelDir "build")

-- | Make the component-specific subdirectory of the build directory.
getBuildComponentDir :: Maybe String -> Maybe (Path Rel Dir)
getBuildComponentDir Nothing = Nothing
getBuildComponentDir (Just name) = parseRelDir (name FilePath.</> (name ++ "-tmp"))

-- | Get all dependencies of the package (buildable targets only).
packageDependencies :: PackageDescription -> Map PackageName VersionRange
packageDependencies =
  M.fromListWith intersectVersionRanges .
  concatMap (fmap (depName &&& depRange) .
             targetBuildDepends) .
  allBuildInfo'

-- | Get all build tool dependencies of the package (buildable targets only).
packageToolDependencies :: PackageDescription -> Map Text VersionRange
packageToolDependencies =
  M.fromList .
  concatMap (fmap (packageNameText . depName &&& depRange) .
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
packageDescModulesAndFiles
    :: (MonadLogger m, MonadIO m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m, MonadCatch m)
    => PackageDescription
    -> m (Map NamedComponent (Set ModuleName), Map NamedComponent (Set DotCabalPath), Set (Path Abs File), [PackageWarning])
packageDescModulesAndFiles pkg = do
    (libraryMods,libDotCabalFiles,libWarnings) <-
        maybe
            (return (M.empty, M.empty, []))
            (asModuleAndFileMap libComponent libraryFiles)
            (library pkg)
    (executableMods,exeDotCabalFiles,exeWarnings) <-
        liftM
            foldTuples
            (mapM
                 (asModuleAndFileMap exeComponent executableFiles)
                 (executables pkg))
    (testMods,testDotCabalFiles,testWarnings) <-
        liftM
            foldTuples
            (mapM (asModuleAndFileMap testComponent testFiles) (testSuites pkg))
    (benchModules,benchDotCabalPaths,benchWarnings) <-
        liftM
            foldTuples
            (mapM
                 (asModuleAndFileMap benchComponent benchmarkFiles)
                 (benchmarks pkg))
    (dfiles) <- resolveGlobFiles (map (dataDir pkg FilePath.</>) (dataFiles pkg))
    let modules = libraryMods <> executableMods <> testMods <> benchModules
        files =
            libDotCabalFiles <> exeDotCabalFiles <> testDotCabalFiles <>
            benchDotCabalPaths
        warnings = libWarnings <> exeWarnings <> testWarnings <> benchWarnings
    return (modules, files, dfiles, warnings)
  where
    libComponent = const CLib
    exeComponent = CExe . T.pack . exeName
    testComponent = CTest . T.pack . testName
    benchComponent = CBench . T.pack . benchmarkName
    asModuleAndFileMap label f lib = do
        (a,b,c) <- f lib
        return (M.singleton (label lib) a, M.singleton (label lib) b, c)
    foldTuples = foldl' (<>) (M.empty, M.empty, [])

-- | Resolve globbing of files (e.g. data files) to absolute paths.
resolveGlobFiles :: (MonadLogger m,MonadIO m,MonadThrow m,MonadReader (Path Abs File, Path Abs Dir) m,MonadCatch m)
                 => [String] -> m (Set (Path Abs File))
resolveGlobFiles =
    liftM (S.fromList . catMaybes . concat) .
    mapM resolve
  where
    resolve name =
        if '*' `elem` name
            then explode name
            else liftM return (resolveFileOrWarn name)
    explode name = do
        dir <- asks (parent . fst)
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
    files <- D.getDirectoryContents (dir FilePath.</> dir')
    case   [ dir' FilePath.</> file
           | file <- files
           , let (name, ext') = splitExtensions file
           , not (null name) && isSuffixOf ext ext' ] of
      []      -> die $ "filepath wildcard '" ++ filepath
                    ++ "' does not match any files."
      matches -> return matches

-- | Get all files referenced by the benchmark.
benchmarkFiles
    :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader (Path Abs File, Path Abs Dir) m)
    => Benchmark -> m (Set ModuleName, Set DotCabalPath, [PackageWarning])
benchmarkFiles bench = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (modules,files,warnings) <-
        resolveFilesAndDeps
            (Just $ benchmarkName bench)
            (dirs ++ [dir])
            (bnames <> exposed)
            haskellModuleExts
    cfiles <- buildOtherSources build
    return (modules, files <> cfiles, warnings)
  where
    exposed =
        case benchmarkInterface bench of
            BenchmarkExeV10 _ fp -> [DotCabalMain fp]
            BenchmarkUnsupported _ -> []
    bnames = map DotCabalModule (otherModules build)
    build = benchmarkBuildInfo bench

-- | Get all files referenced by the test.
testFiles
    :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader (Path Abs File, Path Abs Dir) m)
    => TestSuite
    -> m (Set ModuleName, Set DotCabalPath, [PackageWarning])
testFiles test = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (modules,files,warnings) <-
        resolveFilesAndDeps
            (Just $ testName test)
            (dirs ++ [dir])
            (bnames <> exposed)
            haskellModuleExts
    cfiles <- buildOtherSources build
    return (modules, files <> cfiles, warnings)
  where
    exposed =
        case testInterface test of
            TestSuiteExeV10 _ fp -> [DotCabalMain fp]
            TestSuiteLibV09 _ mn -> [DotCabalModule mn]
            TestSuiteUnsupported _ -> []
    bnames = map DotCabalModule (otherModules build)
    build = testBuildInfo test

-- | Get all files referenced by the executable.
executableFiles
    :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader (Path Abs File, Path Abs Dir) m)
    => Executable
    -> m (Set ModuleName, Set DotCabalPath, [PackageWarning])
executableFiles exe = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (modules,files,warnings) <-
        resolveFilesAndDeps
            (Just $ exeName exe)
            (dirs ++ [dir])
            (map DotCabalModule (otherModules build) ++
             [DotCabalMain (modulePath exe)])
            haskellModuleExts
    cfiles <- buildOtherSources build
    return (modules, files <> cfiles, warnings)
  where
    build = buildInfo exe

-- | Get all files referenced by the library.
libraryFiles
    :: (MonadLogger m, MonadIO m, MonadCatch m, MonadReader (Path Abs File, Path Abs Dir) m)
    => Library -> m (Set ModuleName, Set DotCabalPath, [PackageWarning])
libraryFiles lib = do
    dirs <- mapMaybeM resolveDirOrWarn (hsSourceDirs build)
    dir <- asks (parent . fst)
    (modules,files,warnings) <-
        resolveFilesAndDeps
            Nothing
            (dirs ++ [dir])
            (names <> exposed)
            haskellModuleExts
    cfiles <- buildOtherSources build
    return (modules, files <> cfiles, warnings)
  where
    names = bnames ++ exposed
    exposed = map DotCabalModule (exposedModules lib)
    bnames = map DotCabalModule (otherModules build)
    build = libBuildInfo lib

-- | Get all C sources and extra source files in a build.
buildOtherSources :: (MonadLogger m,MonadIO m,MonadCatch m,MonadReader (Path Abs File, Path Abs Dir) m)
           => BuildInfo -> m (Set DotCabalPath)
buildOtherSources build =
    do csources <- liftM
                       (S.map DotCabalCFilePath . S.fromList)
                       (mapMaybeM resolveFileOrWarn (cSources build))
       jsources <- liftM
                       (S.map DotCabalFilePath . S.fromList)
                       (mapMaybeM resolveFileOrWarn (targetJsSources build))
       return (csources <> jsources)

-- | Get the target's JS sources.
targetJsSources :: BuildInfo -> [FilePath]
#if MIN_VERSION_Cabal(1, 22, 0)
targetJsSources = jsSources
#else
targetJsSources = const []
#endif

-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackageDescription :: PackageConfig
                          -> GenericPackageDescription
                          -> PackageDescription
resolvePackageDescription packageConfig (GenericPackageDescription desc defaultFlags mlib exes tests benches) =
  desc {library =
          fmap (resolveConditions rc updateLibDeps) mlib
       ,executables =
          map (\(n, v) -> (resolveConditions rc updateExeDeps v){exeName=n})
              exes
       ,testSuites =
          map (\(n,v) -> (resolveConditions rc updateTestDeps v){testName=n})
              tests
       ,benchmarks =
          map (\(n,v) -> (resolveConditions rc updateBenchmarkDeps v){benchmarkName=n})
              benches}
  where flags =
          M.union (packageConfigFlags packageConfig)
                  (flagMap defaultFlags)

        rc = mkResolveConditions
                (packageConfigCompilerVersion packageConfig)
                (packageConfigPlatform packageConfig)
                flags

        updateLibDeps lib deps =
          lib {libBuildInfo =
                 (libBuildInfo lib) {targetBuildDepends = deps}}
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
    , rcCompilerVersion :: CompilerVersion
    , rcOS :: OS
    , rcArch :: Arch
    }

-- | Generic a @ResolveConditions@ using sensible defaults.
mkResolveConditions :: CompilerVersion -- ^ Compiler version
                    -> Platform -- ^ installation target platform
                    -> Map FlagName Bool -- ^ enabled flags
                    -> ResolveConditions
mkResolveConditions compilerVersion (Platform arch os) flags = ResolveConditions
    { rcFlags = flags
    , rcCompilerVersion = compilerVersion
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
                  if condSatisfied cond
                     then resolveConditions rc addDeps node
                     else maybe mempty (resolveConditions rc addDeps) mcs
                condSatisfied c =
                  case c of
                    Var v -> varSatisifed v
                    Lit b -> b
                    CNot c' ->
                      not (condSatisfied c')
                    COr cx cy ->
                      condSatisfied cx || condSatisfied cy
                    CAnd cx cy ->
                      condSatisfied cx && condSatisfied cy
                varSatisifed v =
                  case v of
                    OS os -> os == rcOS rc
                    Arch arch -> arch == rcArch rc
                    Flag flag ->
                      fromMaybe False $ M.lookup (fromCabalFlagName flag) (rcFlags rc)
                      -- NOTE:  ^^^^^ This should never happen, as all flags
                      -- which are used must be declared. Defaulting to
                      -- False.
                    Impl flavor range ->
                      case (flavor, rcCompilerVersion rc) of
                        (GHC, GhcVersion vghc) -> vghc `withinRange` range
                        (GHC, GhcjsVersion _ vghc) -> vghc `withinRange` range
#if MIN_VERSION_Cabal(1, 22, 0)
                        (GHCJS, GhcjsVersion vghcjs _) ->
#else
                        (OtherCompiler "ghcjs", GhcjsVersion vghcjs _) ->
#endif
                          vghcjs `withinRange` range
                        _ -> False

-- | Get the name of a dependency.
depName :: Dependency -> PackageName
depName (Dependency n _) = fromCabalPackageName n

-- | Get the version range of a dependency.
depRange :: Dependency -> VersionRange
depRange (Dependency _ r) = r

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions, plus find any of their module and TemplateHaskell
-- dependencies.
resolveFilesAndDeps
    :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
    => Maybe String         -- ^ Package component name
    -> [Path Abs Dir]       -- ^ Directories to look in.
    -> [DotCabalDescriptor] -- ^ Base names.
    -> [Text]               -- ^ Extensions.
    -> m (Set ModuleName,Set DotCabalPath,[PackageWarning])
resolveFilesAndDeps component dirs names0 exts = do
    (dotCabalPaths, foundModules, missingModules) <- loop names0 S.empty
    warnings <- liftM2 (++) (warnUnlisted foundModules) (warnMissing missingModules)
    return (foundModules, dotCabalPaths, warnings)
  where
    loop [] _ = return (S.empty, S.empty, [])
    loop names doneModules0 = do
        resolved <- resolveFiles dirs names exts
        let foundFiles = mapMaybe snd resolved
            (foundModules', missingModules') = partition (isJust . snd) resolved
            foundModules = mapMaybe (dotCabalModule . fst) foundModules'
            missingModules = mapMaybe (dotCabalModule . fst) missingModules'
        pairs <- mapM (getDependencies component) foundFiles
        let doneModules =
                S.union
                    doneModules0
                    (S.fromList (mapMaybe dotCabalModule names))
            moduleDeps = S.unions (map fst pairs)
            thDepFiles = concatMap snd pairs
            modulesRemaining = S.difference moduleDeps doneModules
        -- Ignore missing modules discovered as dependencies - they may
        -- have been deleted.
        (resolvedFiles, resolvedModules, _) <-
            loop (map DotCabalModule (S.toList modulesRemaining)) doneModules
        return
            ( S.union
                  (S.fromList
                       (foundFiles <> map DotCabalFilePath thDepFiles))
                  resolvedFiles
            , S.union
                  (S.fromList foundModules)
                  resolvedModules
            , missingModules)
    warnUnlisted foundModules = do
        let unlistedModules =
                foundModules `S.difference`
                S.fromList (mapMaybe dotCabalModule names0)
        cabalfp <- asks fst
        return $
            if S.null unlistedModules
                then []
                else [ UnlistedModulesWarning
                           cabalfp
                           component
                           (S.toList unlistedModules)]
    warnMissing _missingModules = do
        return []
        {- FIXME: the issue with this is it's noisy for modules like Paths_*
        cabalfp <- asks fst
        return $
            if null missingModules
               then []
               else [ MissingModulesWarning
                           cabalfp
                           component
                           missingModules]
        -}


-- | Get the dependencies of a Haskell module file.
getDependencies
    :: (MonadReader (Path Abs File, Path Abs Dir) m, MonadIO m)
    => Maybe String -> DotCabalPath -> m (Set ModuleName, [Path Abs File])
getDependencies component dotCabalPath =
    case dotCabalPath of
        DotCabalModulePath resolvedFile -> readResolvedHi resolvedFile
        DotCabalMainPath resolvedFile -> readResolvedHi resolvedFile
        DotCabalFilePath{} -> return (S.empty, [])
        DotCabalCFilePath{} -> return (S.empty, [])
  where
    readResolvedHi resolvedFile = do
        dumpHIDir <- getDumpHIDir
        dir <- asks (parent . fst)
        case stripDir dir resolvedFile of
            Nothing -> return (S.empty, [])
            Just fileRel -> do
                let dumpHIPath =
                        FilePath.replaceExtension
                            (toFilePath (dumpHIDir </> fileRel))
                            ".dump-hi"
                dumpHIExists <- liftIO $ D.doesFileExist dumpHIPath
                if dumpHIExists
                    then parseDumpHI dumpHIPath
                    else return (S.empty, [])
    getDumpHIDir = do
        bld <- asks snd
        return $ maybe bld (bld </>) (getBuildComponentDir component)

-- | Parse a .dump-hi file into a set of modules and files.
parseDumpHI
    :: (MonadReader (Path Abs File, void) m, MonadIO m)
    => FilePath -> m (Set ModuleName, [Path Abs File])
parseDumpHI dumpHIPath = do
    dir <- asks (parent . fst)
    dumpHI <- liftIO $ fmap C8.lines (C8.readFile dumpHIPath)
    let startModuleDeps =
            dropWhile (not . ("module dependencies:" `C8.isPrefixOf`)) dumpHI
        moduleDeps =
            S.fromList $
            mapMaybe (simpleParse . T.unpack . decodeUtf8) $
            C8.words $
            C8.concat $
            C8.dropWhile (/= ' ') (headDef "" startModuleDeps) :
            takeWhile (" " `C8.isPrefixOf`) (tailSafe startModuleDeps)
        thDeps =
            -- The dependent file path is surrounded by quotes but is not escaped.
            -- It can be an absolute or relative path.
            mapMaybe
                (parseAbsOrRelFile dir <=<
                 (fmap T.unpack .
                  (T.stripSuffix "\"" <=< T.stripPrefix "\"") .
                  T.dropWhileEnd (== '\r') . decodeUtf8 . C8.dropWhile (/= '"'))) $
            filter ("addDependentFile \"" `C8.isPrefixOf`) dumpHI
    return (moduleDeps, thDeps)
  where
    parseAbsOrRelFile dir fp =
        case parseRelFile fp of
            Just rel -> Just (dir </> rel)
            Nothing -> parseAbsFile fp

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles
    :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
    => [Path Abs Dir] -- ^ Directories to look in.
    -> [DotCabalDescriptor] -- ^ Base names.
    -> [Text] -- ^ Extensions.
    -> m [(DotCabalDescriptor, Maybe DotCabalPath)]
resolveFiles dirs names exts =
    forM names (\name -> liftM (name, ) (findCandidate dirs exts name))

-- | Find a candidate for the given module-or-filename from the list
-- of directories and given extensions.
findCandidate
    :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
    => [Path Abs Dir]
    -> [Text]
    -> DotCabalDescriptor
    -> m (Maybe DotCabalPath)
findCandidate dirs exts name = do
    pkg <- asks fst >>= parsePackageNameFromFilePath
    candidates <- liftIO makeNameCandidates
    case candidates of
        [candidate] -> return (Just (cons candidate))
        [] -> do
            case name of
                DotCabalModule mn
                  | display mn /= paths_pkg pkg -> logPossibilities dirs mn
                _ -> return ()
            return Nothing
        (candidate:rest) -> do
            warnMultiple name candidate rest
            return (Just (cons candidate))
  where
    cons =
        case name of
            DotCabalModule{} -> DotCabalModulePath
            DotCabalMain{} -> DotCabalMainPath
            DotCabalFile{} -> DotCabalFilePath
            DotCabalCFile{} -> DotCabalCFilePath
    paths_pkg pkg = "Paths_" ++ packageNameString pkg
    makeNameCandidates =
        liftM (nubOrd . concat) (mapM makeDirCandidates dirs)
    makeDirCandidates :: Path Abs Dir
                      -> IO [Path Abs File]
    makeDirCandidates dir =
        case name of
            DotCabalMain fp -> resolveCandidate dir fp
            DotCabalFile fp -> resolveCandidate dir fp
            DotCabalCFile fp -> resolveCandidate dir fp
            DotCabalModule mn ->
                liftM concat
                $ mapM
                  ((\ ext ->
                     resolveCandidate dir (Cabal.toFilePath mn ++ "." ++ ext))
                   . T.unpack)
                   exts
    resolveCandidate
        :: (MonadIO m, MonadThrow m)
        => Path Abs Dir -> FilePath.FilePath -> m [Path Abs File]
    resolveCandidate x y = do
        -- The standard canonicalizePath does not work for this case
        p <- parseCollapsedAbsFile (toFilePath x FilePath.</> y)
        exists <- doesFileExist p
        return $ if exists then [p] else []

-- | Warn the user that multiple candidates are available for an
-- entry, but that we picked one anyway and continued.
warnMultiple
    :: MonadLogger m
    => DotCabalDescriptor -> Path b t -> [Path b t] -> m ()
warnMultiple name candidate rest =
    $logWarn
        ("There were multiple candidates for the Cabal entry \"" <>
         showName name <>
         "\" (" <>
         T.intercalate "," (map (T.pack . toFilePath) rest) <>
         "), picking " <>
         T.pack (toFilePath candidate))
  where showName (DotCabalModule name') = T.pack (display name')
        showName (DotCabalMain fp) = T.pack fp
        showName (DotCabalFile fp) = T.pack fp
        showName (DotCabalCFile fp) = T.pack fp

-- | Log that we couldn't find a candidate, but there are
-- possibilities for custom preprocessor extensions.
--
-- For example: .erb for a Ruby file might exist in one of the
-- directories.
logPossibilities
    :: (MonadIO m, MonadThrow m, MonadLogger m)
    => [Path Abs Dir] -> ModuleName -> m ()
logPossibilities dirs mn = do
    possibilities <- liftM concat (makePossibilities mn)
    case possibilities of
        [] -> return ()
        _ ->
            $logWarn
                ("Unable to find a known candidate for the Cabal entry \"" <>
                 T.pack (display mn) <>
                 "\", but did find: " <>
                 T.intercalate ", " (map (T.pack . toFilePath) possibilities) <>
                 ". If you are using a custom preprocessor for this module " <>
                 "with its own file extension, consider adding the file(s) " <>
                 "to your .cabal under extra-source-files.")
  where
    makePossibilities name =
        mapM
            (\dir ->
                  do (_,files) <- listDir dir
                     return
                         (map
                              filename
                              (filter
                                   (isPrefixOf (display name) .
                                    toFilePath . filename)
                                   files)))
            dirs

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
--
-- If the directory contains a file named package.yaml, hpack is used to
-- generate a .cabal file from it.
findOrGenerateCabalFile
    :: forall m. (MonadThrow m, MonadIO m)
    => Path Abs Dir -- ^ package directory
    -> m (Path Abs File)
findOrGenerateCabalFile pkgDir = do
    mPackageYaml <- Path.IO.findFile [pkgDir] $(mkRelFile "package.yaml")
    case mPackageYaml of
        Nothing -> findCabalFile
        Just packageYaml -> do
            eCabalFile <- findCabalFile'
            case eCabalFile of
                -- Check that cabal file is fresh enough
                Right cabalFile -> do
                    packageYamlModified <- getModificationTime packageYaml
                    cabalFileModified <- getModificationTime cabalFile
                    when (cabalFileModified < packageYamlModified) $ liftIO $ hpack pkgDir
                    -- Important to research again
                    findCabalFile

                -- If we cannot find cabal file, but have package.yaml
                -- run hpack and try to find cabal file again
                Left (PackageNoCabalFileFound _) -> do
                    liftIO $ hpack pkgDir
                    findCabalFile

                -- Rethrow exception
                Left e -> throwM e
  where
    findCabalFile :: m (Path Abs File)
    findCabalFile = findCabalFile' >>= either throwM return

    findCabalFile' :: m (Either PackageException (Path Abs File))
    findCabalFile' = do
        files <- liftIO $ findFiles
            pkgDir
            (flip hasExtension "cabal" . FL.toFilePath)
            (const False)
        return $ case files of
            [] -> Left $ PackageNoCabalFileFound pkgDir
            [x] -> Right x
            -- If there are multiple files, ignore files that start with
            -- ".". On unixlike environments these are hidden, and this
            -- character is not valid in package names. The main goal is
            -- to ignore emacs lock files - see
            -- https://github.com/commercialhaskell/stack/issues/1897.
            (filter (not . ("." `isPrefixOf`) . toFilePath . filename) -> [x]) -> Right x
            _:_ -> Left $ PackageMultipleCabalFilesFound pkgDir files
      where hasExtension fp x = FilePath.takeExtension fp == "." ++ x

-- | Generate .cabal file from package.yaml, if necessary.
hpack :: Path Abs Dir -> IO ()
hpack pkgDir = do
    exists <- doesFileExist (pkgDir </> $(mkRelFile Hpack.packageConfig))
    when exists $ do
        Hpack.hpack (toFilePath pkgDir) True

-- | Path for the package's build log.
buildLogPath :: (MonadReader env m, HasBuildConfig env, MonadThrow m)
             => Package -> Maybe String -> m (Path Abs File)
buildLogPath package' msuffix = do
  env <- ask
  let stack = getProjectWorkDir env
  fp <- parseRelFile $ concat $
    packageIdentifierString (packageIdentifier package') :
    maybe id (\suffix -> ("-" :) . (suffix :)) msuffix [".log"]
  return $ stack </> $(mkRelDir "logs") </> fp

-- Internal helper to define resolveFileOrWarn and resolveDirOrWarn
resolveOrWarn :: (MonadLogger m, MonadIO m, MonadThrow m, MonadReader (Path Abs File, Path Abs Dir) m)
              => Text
              -> (Path Abs Dir -> String -> m (Maybe a))
              -> FilePath.FilePath
              -> m (Maybe a)
resolveOrWarn subject resolver path =
  do cwd <- getCurrentDir
     file <- asks fst
     dir <- asks (parent . fst)
     result <- resolver dir path
     when (isNothing result) $
       $logWarn ("Warning: " <> subject <> " listed in " <>
         T.pack (maybe (FL.toFilePath file) FL.toFilePath (stripDir cwd file)) <>
         " file does not exist: " <>
         T.pack path)
     return result

-- | Resolve the file, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveFileOrWarn :: (MonadCatch m,MonadIO m,MonadLogger m,MonadReader (Path Abs File, Path Abs Dir) m)
                  => FilePath.FilePath
                  -> m (Maybe (Path Abs File))
resolveFileOrWarn = resolveOrWarn "File" f
  where f p x = forgivingAbsence (resolveFile p x) >>= rejectMissingFile

-- | Resolve the directory, if it can't be resolved, warn for the user
-- (purely to be helpful).
resolveDirOrWarn :: (MonadCatch m,MonadIO m,MonadLogger m,MonadReader (Path Abs File, Path Abs Dir) m)
                 => FilePath.FilePath
                 -> m (Maybe (Path Abs Dir))
resolveDirOrWarn = resolveOrWarn "Directory" f
  where f p x = forgivingAbsence (resolveDir p x) >>= rejectMissingDir

-- | Extract the @PackageIdentifier@ given an exploded haskell package
-- path.
cabalFilePackageId
    :: (Applicative m, MonadIO m, MonadThrow m)
    => Path Abs File -> m PackageIdentifier
cabalFilePackageId fp = do
    pkgDescr <- liftIO (D.readPackageDescription D.silent $ toFilePath fp)
    (toStackPI . D.package . D.packageDescription) pkgDescr
  where
    toStackPI (D.PackageIdentifier (D.PackageName name) ver) =
        PackageIdentifier <$> parsePackageNameFromString name <*>
        parseVersionFromString (showVersion ver)
