{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Generate haddocks
module Stack.Build.Haddock
  ( generateDepsHaddockIndex
  , generateLocalHaddockIndex
  , generateSnapHaddockIndex
  , openHaddocksInBrowser
  , shouldHaddockDeps
  , shouldHaddockPackage
  ) where

import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time ( UTCTime )
import           Path ( (</>), parent, parseRelDir )
import           Path.Extra
                   ( parseCollapsedAbsFile, toFilePathNoTrailingSep
                   , tryGetModificationTime
                   )
import           Path.IO
                   ( copyDirRecur', doesFileExist, ensureDir, ignoringAbsence
                   , removeDirRecur
                   )
import           RIO.List ( intercalate )
import           RIO.Process ( HasProcessContext, withWorkingDir )
import           Stack.Constants ( docDirSuffix, relDirAll, relFileIndexHtml )
import           Stack.Prelude
import           Stack.Types.Build ( BaseConfigOpts (..), BuildException (..) )
import           Stack.Types.Config
                   ( BuildOpts (..), BuildOptsCLI (..), CompilerPaths (..)
                   , DumpPackage (..), HaddockOpts (..), HasCompiler
                   , compilerPathsL
                   )
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
  let cliTargets = (boptsCLITargets . bcoBuildOptsCLI) bco
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
    then boptsHaddock bopts
    else shouldHaddockDeps bopts

-- | Determine whether to build haddocks for dependencies.
shouldHaddockDeps :: BuildOpts -> Bool
shouldHaddockDeps bopts =
  fromMaybe (boptsHaddock bopts) (boptsHaddockDeps bopts)

-- | Generate Haddock index and contents for local packages.
generateLocalHaddockIndex ::
     (HasProcessContext env, HasLogFunc env, HasCompiler env)
  => BaseConfigOpts
  -> Map GhcPkgId DumpPackage  -- ^ Local package dump
  -> [LocalPackage]
  -> RIO env ()
generateLocalHaddockIndex bco localDumpPkgs locals = do
  let dumpPackages =
        mapMaybe
          ( \LocalPackage{lpPackage = Package{packageName, packageVersion}} ->
              F.find
                ( \dp -> dpPackageIdent dp ==
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
     (HasProcessContext env, HasLogFunc env, HasCompiler env)
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
        mdpPkg = F.find (\dp -> dpPackageIdent dp == pkgId) localDumpPkgs
    in  fmap dpGhcPkgId mdpPkg
  findTransitiveDepends :: [GhcPkgId] -> [GhcPkgId]
  findTransitiveDepends = (`go` HS.empty) . HS.fromList
   where
    go todo checked =
      case HS.toList todo of
        [] -> HS.toList checked
        (ghcPkgId:_) ->
          let deps = case lookupDumpPackage ghcPkgId allDumpPkgs of
                       Nothing -> HS.empty
                       Just pkgDP -> HS.fromList (dpDepends pkgDP)
              deps' = deps `HS.difference` checked
              todo' = HS.delete ghcPkgId (deps' `HS.union` todo)
              checked' = HS.insert ghcPkgId checked
          in  go todo' checked'
  allDumpPkgs = [localDumpPkgs, snapshotDumpPkgs, globalDumpPkgs]

-- | Generate Haddock index and contents for all snapshot packages.
generateSnapHaddockIndex ::
     (HasProcessContext env, HasLogFunc env, HasCompiler env)
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
     (HasProcessContext env, HasLogFunc env, HasCompiler env)
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
    if needUpdate
      then do
        logInfo $
             "Updating Haddock index for "
          <> Stack.Prelude.display descr
          <> " in\n"
          <> fromString (toFilePath destIndexFile)
        liftIO (mapM_ copyPkgDocs interfaceOpts)
        haddockExeName <- view $ compilerPathsL.to (toFilePath . cpHaddock)
        withWorkingDir (toFilePath destDir) $ readProcessNull
          haddockExeName
          ( map
              (("--optghc=-package-db=" ++ ) . toFilePathNoTrailingSep)
                 [bcoSnapDB bco, bcoLocalDB bco]
              ++ hoAdditionalArgs (boptsHaddockOpts (bcoBuildOpts bco))
              ++ ["--gen-contents", "--gen-index"]
              ++ [x | (xs, _, _, _) <- interfaceOpts, x <- xs]
          )
      else
        logInfo $
             "Haddock index for "
          <> Stack.Prelude.display descr
          <> " already up to date at:\n"
          <> fromString (toFilePath destIndexFile)
 where
  toInterfaceOpt ::
       DumpPackage
    -> IO (Maybe ([String], UTCTime, Path Abs File, Path Abs File))
  toInterfaceOpt DumpPackage {dpHaddockInterfaces, dpPackageIdent, dpHaddockHtml} =
    case dpHaddockInterfaces of
      [] -> pure Nothing
      srcInterfaceFP:_ -> do
        srcInterfaceAbsFile <- parseCollapsedAbsFile srcInterfaceFP
        let (PackageIdentifier name _) = dpPackageIdent
            destInterfaceRelFP =
              docRelFP FP.</>
              packageIdentifierString dpPackageIdent FP.</>
              (packageNameString name FP.<.> "haddock")
            docPathRelFP =
              fmap ((docRelFP FP.</>) . FP.takeFileName) dpHaddockHtml
            interfaces = intercalate "," $
              maybeToList docPathRelFP ++ [srcInterfaceFP]

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
localDocDir bco = bcoLocalInstallRoot bco </> docDirSuffix

-- | Path of documentation directory for the dependencies of local packages
localDepsDocDir :: BaseConfigOpts -> Path Abs Dir
localDepsDocDir bco = localDocDir bco </> relDirAll

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bcoSnapInstallRoot bco </> docDirSuffix
