{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Generate haddocks
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
import           Data.Time ( UTCTime )
import           Distribution.Text ( display )
import           Path
                   ( (</>), addExtension, fromAbsDir, fromAbsFile, fromRelDir
                   , parent, parseRelDir, parseRelFile
                   )
import           Path.Extra
                   ( parseCollapsedAbsFile, toFilePathNoTrailingSep
                   , tryGetModificationTime
                   )
import           Path.IO
                   ( copyDirRecur', doesFileExist, ensureDir, ignoringAbsence
                   , removeDirRecur
                   )
import qualified RIO.ByteString.Lazy as BL
import           RIO.List ( intercalate )
import           RIO.Process ( HasProcessContext, withWorkingDir )
import           Stack.Constants
                   ( docDirSuffix, htmlDirSuffix, relDirAll, relFileIndexHtml )
import           Stack.Constants.Config ( distDirFromDir )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.Types.Build.Exception ( BuildException (..) )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import           Stack.Types.BuildOpts
                   ( BuildOpts (..), BuildOptsCLI (..), HaddockOpts (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig ( HasEnvConfig (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Package
                   ( InstallLocation (..), LocalPackage (..), Package (..) )
import qualified System.FilePath as FP
import           Web.Browser ( openBrowser )

openHaddocksInBrowser ::
     HasTerm env
  => BaseConfigOpts
  -> Map PackageName (PackageIdentifier, InstallLocation)
  -- ^ Available packages and their locations for the current project
  -> Set PackageName
  -- ^ Build targets as determined by 'Stack.Build.Source.loadSourceMap'
  -> RIO env ()
openHaddocksInBrowser bco pkgLocations buildTargets = do
  let cliTargets = bco.bcoBuildOptsCLI.boptsCLITargets
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
  _ <- liftIO $ openBrowser (toFilePath docFile)
  pure ()

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
    then bopts.boptsHaddock
    else shouldHaddockDeps bopts

-- | Determine whether to build haddocks for dependencies.
shouldHaddockDeps :: BuildOpts -> Bool
shouldHaddockDeps bopts = fromMaybe bopts.boptsHaddock bopts.boptsHaddockDeps

-- | Generate Haddock index and contents for local packages.
generateLocalHaddockIndex ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => BaseConfigOpts
  -> Map GhcPkgId DumpPackage  -- ^ Local package dump
  -> [LocalPackage]
  -> RIO env ()
generateLocalHaddockIndex bco localDumpPkgs locals = do
  let dumpPackages =
        mapMaybe
          ( \LocalPackage{lpPackage = Package{packageName, packageVersion}} ->
              F.find
                ( \dp -> dp.packageIdent ==
                         PackageIdentifier packageName packageVersion
                )
                localDumpPkgs
          )
          locals
  generateHaddockIndex
    "local packages"
    bco
    dumpPackages
    "."
    (localDocDir bco)

-- | Generate Haddock index and contents for local packages and their
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
    "local packages and dependencies"
    bco
    deps
    ".."
    depDocDir
 where
  getGhcPkgId :: LocalPackage -> Maybe GhcPkgId
  getGhcPkgId LocalPackage{lpPackage = Package{packageName, packageVersion}} =
    let pkgId = PackageIdentifier packageName packageVersion
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
    eindexModTime <- liftIO (tryGetModificationTime destIndexFile)
    let needUpdate =
          case eindexModTime of
            Left _ -> True
            Right indexModTime ->
              or [mt > indexModTime | (_, mt, _, _) <- interfaceOpts]
        prettyDescr = style Current (fromString $ T.unpack descr)
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
        haddockExeName <- view $ compilerPathsL . to (toFilePath . (.cpHaddock))
        withWorkingDir (toFilePath destDir) $ readProcessNull
          haddockExeName
          ( map
              (("--optghc=-package-db=" ++ ) . toFilePathNoTrailingSep)
                 [bco.bcoSnapDB, bco.bcoLocalDB]
              ++ bco.bcoBuildOpts.boptsHaddockOpts.hoAdditionalArgs
              ++ ["--gen-contents", "--gen-index"]
              ++ [x | (xs, _, _, _) <- interfaceOpts, x <- xs]
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
    -> IO (Maybe ([String], UTCTime, Path Abs File, Path Abs File))
  toInterfaceOpt DumpPackage {haddockInterfaces, packageIdent, haddockHtml} =
    case haddockInterfaces of
      [] -> pure Nothing
      srcInterfaceFP:_ -> do
        srcInterfaceAbsFile <- parseCollapsedAbsFile srcInterfaceFP
        let (PackageIdentifier name _) = packageIdent
            destInterfaceRelFP =
              docRelFP FP.</>
              packageIdentifierString packageIdent FP.</>
              (packageNameString name FP.<.> "haddock")
            docPathRelFP =
              fmap ((docRelFP FP.</>) . FP.takeFileName) haddockHtml
            interfaces = intercalate "," $ mcons docPathRelFP [srcInterfaceFP]

        destInterfaceAbsFile <-
          parseCollapsedAbsFile (toFilePath destDir FP.</> destInterfaceRelFP)
        esrcInterfaceModTime <- tryGetModificationTime srcInterfaceAbsFile
        pure $
          case esrcInterfaceModTime of
            Left _ -> Nothing
            Right srcInterfaceModTime ->
              Just
                ( [ "-i", interfaces ]
                , srcInterfaceModTime
                , srcInterfaceAbsFile
                , destInterfaceAbsFile
                )
  copyPkgDocs :: (a, UTCTime, Path Abs File, Path Abs File) -> IO ()
  copyPkgDocs (_, srcInterfaceModTime, srcInterfaceAbsFile, destInterfaceAbsFile) = do
  -- Copy dependencies' haddocks to documentation directory.  This way,
  -- relative @../$pkg-$ver@ links work and it's easy to upload docs to a web
  -- server or otherwise view them in a non-local-filesystem context. We copy
  -- instead of symlink for two reasons: (1) symlinks aren't reliably supported
  -- on Windows, and (2) the filesystem containing dependencies' docs may not be
  -- available where viewing the docs (e.g. if building in a Docker container).
    edestInterfaceModTime <- tryGetModificationTime destInterfaceAbsFile
    case edestInterfaceModTime of
      Left _ -> doCopy
      Right destInterfaceModTime
        | destInterfaceModTime < srcInterfaceModTime -> doCopy
        | otherwise -> pure ()
   where
    doCopy = do
      ignoringAbsence (removeDirRecur destHtmlAbsDir)
      ensureDir destHtmlAbsDir
      onException
        (copyDirRecur' (parent srcInterfaceAbsFile) destHtmlAbsDir)
        (ignoringAbsence (removeDirRecur destHtmlAbsDir))
    destHtmlAbsDir = parent destInterfaceAbsFile

-- | Find first DumpPackage matching the GhcPkgId
lookupDumpPackage :: GhcPkgId
                  -> [Map GhcPkgId DumpPackage]
                  -> Maybe DumpPackage
lookupDumpPackage ghcPkgId dumpPkgs =
  listToMaybe $ mapMaybe (Map.lookup ghcPkgId) dumpPkgs

-- | Path of haddock index file.
haddockIndexFile :: Path Abs Dir -> Path Abs File
haddockIndexFile destDir = destDir </> relFileIndexHtml

-- | Path of local packages documentation directory.
localDocDir :: BaseConfigOpts -> Path Abs Dir
localDocDir bco = bco.bcoLocalInstallRoot </> docDirSuffix

-- | Path of documentation directory for the dependencies of local packages
localDepsDocDir :: BaseConfigOpts -> Path Abs Dir
localDepsDocDir bco = localDocDir bco </> relDirAll

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bco.bcoSnapInstallRoot </> docDirSuffix

generateLocalHaddockForHackageArchives ::
     (HasEnvConfig env, HasTerm env)
  => [LocalPackage]
  -> RIO env ()
generateLocalHaddockForHackageArchives =
  mapM_
    ( \lp ->
        let pkg = lp.lpPackage
            pkgId = PackageIdentifier pkg.packageName pkg.packageVersion
            pkgDir = parent lp.lpCabalFile
        in generateLocalHaddockForHackageArchive pkgDir pkgId
    )

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
  createTarGzFile tarGzFile docDir nameRelDir
  prettyInfo $
       fillSep
         [ flow "Archive of Haddock documentation for Hackage for"
         , style Current (fromString pkgIdName)
         , flow "created at:"
         ]
    <> line
    <> pretty tarGzFile

createTarGzFile ::
     Path Abs File
     -- ^ Full path to archive file
  -> Path Abs Dir
     -- ^ Base directory
  -> Path Rel Dir
     -- ^ Directory to archive, relative to base directory
  -> RIO env ()
createTarGzFile tar base dir = do
   entries <- liftIO $ Tar.pack base' [dir']
   BL.writeFile tar' $ GZip.compress $ Tar.write entries
 where
  base' = fromAbsDir base
  dir' = fromRelDir dir
  tar' = fromAbsFile tar
