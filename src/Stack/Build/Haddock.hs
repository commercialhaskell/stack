{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Build.Haddock
Description : Generate Haddock documentation.
License     : BSD-3-Clause

Generate Haddock documentation.
-}

module Stack.Build.Haddock
  ( generateDepsHaddockIndex
  , generateLocalHaddockIndex
  , generateSnapHaddockIndex
  , openHaddocksInBrowser
  , shouldHaddockDeps
  , shouldHaddockPackage
  , generateLocalHaddockForHackageArchives
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Text ( display )
import           Path
                   ( (</>), addExtension, dirname, fileExtension, filename
                   , fromAbsDir, fromAbsFile, fromRelDir, parent, parseRelDir
                   , parseRelFile
                   )
import           Path.Extra
                   ( parseCollapsedAbsFile, toFilePathNoTrailingSep
                   , tryGetModificationTime
                   )
import           Path.IO
                   ( copyDirRecur, copyDirRecur', doesDirExist, doesFileExist
                   , ensureDir, ignoringAbsence, listDir, removeDirRecur
                   )
import qualified RIO.ByteString.Lazy as BL
import           RIO.List ( intercalate, intersperse )
import           RIO.Process ( HasProcessContext, withWorkingDir )
import           Stack.Constants
                   ( docDirSuffix, htmlDirSuffix, relDirAll, relFileIndexHtml )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.Build.Exception ( BuildException (..) )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import           Stack.Types.BuildOpts ( BuildOpts (..), HaddockOpts (..) )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI (..), BuildSubset (BSOnlyDependencies, BSOnlySnapshot) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig ( EnvConfig (..), HasEnvConfig (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.InterfaceOpt ( InterfaceOpt (..) )
import           Stack.Types.Package
                   ( InstallLocation (..), LocalPackage (..), Package (..) )
import qualified System.FilePath as FP
import           Web.Browser ( openBrowser )
import RIO.FilePath (dropTrailingPathSeparator)

openHaddocksInBrowser ::
     HasTerm env
  => BaseConfigOpts
  -> Map PackageName (PackageIdentifier, InstallLocation)
  -- ^ Available packages and their locations for the current project
  -> Set PackageName
  -- ^ Build targets as determined by 'Stack.Build.Source.loadSourceMap'
  -> RIO env ()
openHaddocksInBrowser bco pkgLocations buildTargets = do
  let cliTargets = bco.buildOptsCLI.targetsCLI
      getDocIndex = do
        let localDocs = haddockIndexFile (localDepsDocDir bco)
        localExists <- doesFileExist localDocs
        if localExists
          then pure localDocs
          else do
            let snapDocs = haddockIndexFile (snapDocDir bco)
            snapExists <- doesFileExist snapDocs
            if snapExists
              then pure snapDocs
              else throwIO HaddockIndexNotFound
  docFile <-
    case (cliTargets, map (`Map.lookup` pkgLocations) (Set.toList buildTargets)) of
      ([_], [Just (pkgId, iloc)]) -> do
        pkgRelDir <- (parseRelDir . packageIdentifierString) pkgId
        let docLocation =
              case iloc of
                Snap -> snapDocDir bco
                Local -> localDocDir bco
        let docFile = haddockIndexFile (docLocation </> pkgRelDir)
        exists <- doesFileExist docFile
        if exists
            then pure docFile
            else do
              prettyWarnL
                [ flow "Expected to find documentation at"
                , pretty docFile <> ","
                , flow "but that file is missing. Opening doc index instead."
                ]
              getDocIndex
      _ -> getDocIndex
  prettyInfo $ "Opening" <+> pretty docFile <+> "in the browser."
  void $ liftIO $ openBrowser (toFilePath docFile)

-- | Determine whether we should haddock for a package.
shouldHaddockPackage ::
     BuildOpts
  -> Set PackageName
     -- ^ Packages that we want to generate haddocks for in any case (whether or
     -- not we are going to generate haddocks for dependencies)
  -> PackageName
  -> Bool
shouldHaddockPackage bopts wanted name =
  if Set.member name wanted
    then bopts.buildHaddocks
    else shouldHaddockDeps bopts

-- | Determine whether to build haddocks for dependencies.
shouldHaddockDeps :: BuildOpts -> Bool
shouldHaddockDeps bopts = fromMaybe bopts.buildHaddocks bopts.haddockDeps

-- | Generate Haddock index and contents for project packages.
generateLocalHaddockIndex ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => BaseConfigOpts
  -> Map GhcPkgId DumpPackage  -- ^ Local package dump
  -> [LocalPackage]
  -> RIO env ()
generateLocalHaddockIndex bco localDumpPkgs locals = do
  let dumpPackages =
        mapMaybe
          ( \LocalPackage {package = Package {name, version}} ->
              F.find
                ( \dp -> dp.packageIdent ==
                         PackageIdentifier name version
                )
                localDumpPkgs
          )
          locals
  generateHaddockIndex
    "project packages"
    bco
    dumpPackages
    "."
    (localDocDir bco)

-- | Generate Haddock index and contents for project packages and their
-- dependencies.
generateDepsHaddockIndex ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => BaseConfigOpts
  -> Map GhcPkgId DumpPackage  -- ^ Global dump information
  -> Map GhcPkgId DumpPackage  -- ^ Snapshot dump information
  -> Map GhcPkgId DumpPackage  -- ^ Local dump information
  -> [LocalPackage]
  -> RIO env ()
generateDepsHaddockIndex bco globalDumpPkgs snapshotDumpPkgs localDumpPkgs locals = do
  let deps = ( mapMaybe
                 (`lookupDumpPackage` allDumpPkgs)
                 . nubOrd
                 . findTransitiveDepends
                 . mapMaybe getGhcPkgId
             ) locals
      depDocDir = localDepsDocDir bco
  generateHaddockIndex
    "project packages and dependencies"
    bco
    deps
    ".."
    depDocDir
 where
  getGhcPkgId :: LocalPackage -> Maybe GhcPkgId
  getGhcPkgId LocalPackage {package = Package {name, version}} =
    let pkgId = PackageIdentifier name version
        mdpPkg = F.find (\dp -> dp.packageIdent == pkgId) localDumpPkgs
    in  fmap (.ghcPkgId) mdpPkg
  findTransitiveDepends :: [GhcPkgId] -> [GhcPkgId]
  findTransitiveDepends = (`go` HS.empty) . HS.fromList
   where
    go todo checked =
      case HS.toList todo of
        [] -> HS.toList checked
        (ghcPkgId:_) ->
          let deps = case lookupDumpPackage ghcPkgId allDumpPkgs of
                       Nothing -> HS.empty
                       Just pkgDP -> HS.fromList pkgDP.depends
              deps' = deps `HS.difference` checked
              todo' = HS.delete ghcPkgId (deps' `HS.union` todo)
              checked' = HS.insert ghcPkgId checked
          in  go todo' checked'
  allDumpPkgs = [localDumpPkgs, snapshotDumpPkgs, globalDumpPkgs]

-- | Generate Haddock index and contents for all snapshot packages.
generateSnapHaddockIndex ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => BaseConfigOpts
  -> Map GhcPkgId DumpPackage  -- ^ Global package dump
  -> Map GhcPkgId DumpPackage  -- ^ Snapshot package dump
  -> RIO env ()
generateSnapHaddockIndex bco globalDumpPkgs snapshotDumpPkgs =
  generateHaddockIndex
    "snapshot packages"
    bco
    (Map.elems snapshotDumpPkgs ++ Map.elems globalDumpPkgs)
    "."
    (snapDocDir bco)

-- | Generate Haddock index and contents for specified packages.
generateHaddockIndex ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => Text
  -> BaseConfigOpts
  -> [DumpPackage]
  -> FilePath
  -> Path Abs Dir
  -> RIO env ()
generateHaddockIndex descr bco dumpPackages docRelFP destDir = do
  ensureDir destDir
  interfaceOpts <-
    (liftIO . fmap nubOrd . mapMaybeM toInterfaceOpt) dumpPackages
  unless (null interfaceOpts) $ do
    let destIndexFile = haddockIndexFile destDir
        prettyDescr = style Current (fromString $ T.unpack descr)
    needUpdate <- liftIO (tryGetModificationTime destIndexFile) <&> \case
      Left _ -> True
      Right indexModTime ->
        or [ mt > indexModTime
           | mt <- map (.srcInterfaceFileModTime) interfaceOpts
           ]
    if needUpdate
      then do
        prettyInfo $
             fillSep
               [ flow "Updating Haddock index for"
               , prettyDescr
               , "in:"
               ]
          <> line
          <> pretty destIndexFile
        liftIO (mapM_ copyPkgDocs interfaceOpts)
        haddockExeName <- view $ compilerPathsL . to (toFilePath . (.haddock))
        withWorkingDir (toFilePath destDir) $ readProcessNull
          haddockExeName
          ( map
              (("--optghc=-package-db=" ++ ) . toFilePathNoTrailingSep)
                 [bco.snapDB, bco.localDB]
              ++ bco.buildOpts.haddockOpts.additionalArgs
              ++ ["--gen-contents", "--gen-index"]
              ++ [x | xs <- map (.readInterfaceArgs) interfaceOpts, x <- xs]
          )
      else
        prettyInfo $
             fillSep
               [ flow "Haddock index for"
               , prettyDescr
               , flow "already up to date at:"
               ]
          <> line
          <> pretty destIndexFile
 where
  toInterfaceOpt ::
       DumpPackage
    -> IO (Maybe InterfaceOpt)
  toInterfaceOpt dp =
    case dp.haddockInterfaces of
      [] -> pure Nothing
      srcInterfaceFP:_ -> do
        srcInterfaceFile <- parseCollapsedAbsFile srcInterfaceFP
        let (PackageIdentifier name _) = dp.packageIdent
            srcInterfaceDir = parent srcInterfaceFile
        compInterfaceDirsAndFiles <- do
          -- It is possible that the *.haddock file specified by the
          -- haddock-interfaces key for an installed package may not exist. For
          -- example, with GHC 9.6.6 on Windows, there is no
          --
          -- ${pkgroot}/../doc/html/libraries/rts-1.0.2\rts.haddock
          (srcInterfaceSubDirs, _) <- doesDirExist srcInterfaceDir >>= \case
            True -> listDir srcInterfaceDir
            False -> pure ([], [])
          -- This assumes that Cabal (the library) `haddock --executables` for
          -- component my-component of package my-package puts one *.haddock
          -- file and associated files in directory:
          --
          -- my-package/my-component
          --
          -- Not all directories in directory my-package relate to components.
          -- For example, my-package/src relates to the files for the
          -- colourised code of the main library of package my-package.
          let isCompInterfaceDir dir = do
                (_, files) <- listDir dir
                pure $ (dir, ) <$> F.find isInterface files
               where
                isInterface file = fileExtension file == Just ".haddock"
          mapMaybeM isCompInterfaceDir srcInterfaceSubDirs
        -- Lift a copy of the component's Haddock directory up to the same level
        -- as the main library's Haddock directory. For compontent my-component
        -- of package my-package we name the directory my-package_my-component.
        let liftcompInterfaceDir dir file = do
              let parentDir = parent dir
                  parentName = dirname parentDir
                  compName = dirname dir
              uniqueName <- do
                let parentName' =
                      dropTrailingPathSeparator $ toFilePath parentName
                    compName' =
                      dropTrailingPathSeparator $ toFilePath compName
                parseRelDir $ parentName' <> "_" <> compName'
              let destCompDir = parent parentDir </> uniqueName
                  destCompFile = destCompDir </> filename file
              ignoringAbsence (removeDirRecur destCompDir)
              ensureDir destCompDir
              onException
                (copyDirRecur dir destCompDir)
                (ignoringAbsence (removeDirRecur destCompDir))
              pure (destCompFile, uniqueName)
            destInterfaceRelFP =
              docRelFP FP.</>
              packageIdentifierString dp.packageIdent FP.</>
              (packageNameString name FP.<.> "haddock")
            docPathRelFP =
              fmap ((docRelFP FP.</>) . FP.takeFileName) dp.haddockHtml
            mkInterface :: Maybe FilePath -> FilePath -> String
            mkInterface mDocPath file =
              intercalate "," $ mcons mDocPath [file]
            compInterface :: (Path Abs Dir, Path Abs File) -> IO String
            compInterface (dir, file) = do
              (file', uniqueName) <- liftcompInterfaceDir dir file
              let compDir = dropTrailingPathSeparator $ toFilePath uniqueName
                  docDir = docRelFP FP.</> compDir
              pure $ mkInterface (Just docDir) (toFilePath file')
            interfaces = mkInterface docPathRelFP srcInterfaceFP
        compInterfaces <- forM compInterfaceDirsAndFiles compInterface
        let readInterfaceArgs =
              "-i" : intersperse "-i" (interfaces : compInterfaces)
        destInterfaceFile <-
          parseCollapsedAbsFile (toFilePath destDir FP.</> destInterfaceRelFP)
        tryGetModificationTime srcInterfaceFile <&> \case
          Left _ -> Nothing
          Right srcInterfaceFileModTime ->
            Just InterfaceOpt
              { readInterfaceArgs
              , srcInterfaceFileModTime
              , srcInterfaceFile
              , destInterfaceFile
              }
  copyPkgDocs :: InterfaceOpt -> IO ()
  copyPkgDocs opts =
  -- Copy dependencies' haddocks to documentation directory.  This way,
  -- relative @../$pkg-$ver@ links work and it's easy to upload docs to a web
  -- server or otherwise view them in a non-local-filesystem context. We copy
  -- instead of symlink for two reasons: (1) symlinks aren't reliably supported
  -- on Windows, and (2) the filesystem containing dependencies' docs may not be
  -- available where viewing the docs (e.g. if building in a Docker container).
    tryGetModificationTime opts.destInterfaceFile >>= \case
      Left _ -> doCopy
      Right destInterfaceModTime
        | destInterfaceModTime < opts.srcInterfaceFileModTime -> doCopy
        | otherwise -> pure ()
   where
    doCopy = do
      ignoringAbsence (removeDirRecur destHtmlAbsDir)
      ensureDir destHtmlAbsDir
      onException
        (copyDirRecur' (parent opts.srcInterfaceFile) destHtmlAbsDir)
        (ignoringAbsence (removeDirRecur destHtmlAbsDir))
    destHtmlAbsDir = parent opts.destInterfaceFile

-- | Find first DumpPackage matching the GhcPkgId
lookupDumpPackage ::
     GhcPkgId
  -> [Map GhcPkgId DumpPackage]
  -> Maybe DumpPackage
lookupDumpPackage ghcPkgId dumpPkgs =
  listToMaybe $ mapMaybe (Map.lookup ghcPkgId) dumpPkgs

-- | Path of haddock index file.
haddockIndexFile :: Path Abs Dir -> Path Abs File
haddockIndexFile destDir = destDir </> relFileIndexHtml

-- | Path of project packages documentation directory.
localDocDir :: BaseConfigOpts -> Path Abs Dir
localDocDir bco = bco.localInstallRoot </> docDirSuffix

-- | Path of documentation directory for the dependencies of project packages
localDepsDocDir :: BaseConfigOpts -> Path Abs Dir
localDepsDocDir bco = localDocDir bco </> relDirAll

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bco.snapInstallRoot </> docDirSuffix

generateLocalHaddockForHackageArchives ::
     (HasEnvConfig env, HasTerm env)
  => [LocalPackage]
  -> RIO env ()
generateLocalHaddockForHackageArchives lps = do
  buildSubset <- view $ envConfigL . to (.buildOptsCLI.buildSubset)
  let localsExcluded =
        buildSubset == BSOnlyDependencies || buildSubset == BSOnlySnapshot
  unless localsExcluded $
    forM_ lps $ \lp ->
      let pkg = lp.package
          pkgId = PackageIdentifier pkg.name pkg.version
          pkgDir = parent lp.cabalFP
      in  when lp.wanted $
            generateLocalHaddockForHackageArchive pkgDir pkgId

-- | Generate an archive file containing local Haddock documentation for
-- Hackage, in a form accepted by Hackage.
generateLocalHaddockForHackageArchive ::
     (HasEnvConfig env, HasTerm env)
  => Path Abs Dir
     -- ^ The package directory.
  -> PackageIdentifier
     -- ^ The package name and version.
  -> RIO env ()
generateLocalHaddockForHackageArchive pkgDir pkgId = do
  distDir <- distDirFromDir pkgDir
  let pkgIdName = display pkgId
      name = pkgIdName <> "-docs"
      (nameRelDir, tarGzFileName) = fromMaybe
        (error "impossible")
        ( do relDir <- parseRelDir name
             nameRelFile <- parseRelFile name
             tarGz <- addExtension ".gz" =<< addExtension ".tar" nameRelFile
             pure (relDir, tarGz)
        )
      tarGzFile = distDir </> tarGzFileName
      docDir = distDir </> docDirSuffix </> htmlDirSuffix
  tarGzFileCreated <- createTarGzFile tarGzFile docDir nameRelDir
  if tarGzFileCreated
    then
      prettyInfo $
           fillSep
             [ flow "Archive of Haddock documentation for Hackage for"
             , style Current (fromString pkgIdName)
             , flow "created at:"
             ]
        <> line
        <> pretty tarGzFile
    else
      prettyWarnL
        [ flow "No Haddock documentation for Hackage available for"
        , style Error (fromString pkgIdName) <> "."
        ]

createTarGzFile ::
     Path Abs File
     -- ^ Full path to archive file
  -> Path Abs Dir
     -- ^ Base directory
  -> Path Rel Dir
     -- ^ Directory to archive, relative to base directory
  -> RIO env Bool
createTarGzFile tar base dir = do
  dirExists <- doesDirExist $ base </> dir
  if dirExists
    then do
      entries <- liftIO $ Tar.pack base' [dir']
      if null entries
        then pure False
        else do
          ensureDir $ parent tar
          BL.writeFile tar' $ GZip.compress $ Tar.write entries
          pure True
    else pure False
 where
  base' = fromAbsDir base
  dir' = fromRelDir dir
  tar' = fromAbsFile tar
