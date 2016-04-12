{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Generate haddocks
module Stack.Build.Haddock
    ( generateLocalHaddockIndex
    , generateDepsHaddockIndex
    , generateSnapHaddockIndex
    , openHaddocksInBrowser
    , shouldHaddockPackage
    , shouldHaddockDeps
    ) where

import           Control.Exception (tryJust, onException)
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.Foldable as F
import           Data.Function
import qualified Data.HashSet as HS
import           Data.List
import           Data.List.Extra (nubOrd)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Extra (mapMaybeM)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Path
import           Path.Extra
import           Path.IO
import           Prelude
import           Stack.PackageDump
import           Stack.Types
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)
import           System.Process.Read
import           Web.Browser (openBrowser)

openHaddocksInBrowser
    :: (MonadIO m, MonadReader env m, HasBuildConfig env, MonadThrow m, MonadLogger m)
    => BaseConfigOpts
    -> Map PackageName (PackageIdentifier, InstallLocation)
    -- ^ Available packages and their locations for the current project
    -> Set PackageName
    -- ^ Build targets as determined by 'Stack.Build.Source.loadSourceMap'
    -> m ()
openHaddocksInBrowser bco pkgLocations buildTargets = do
    let cliTargets = (boptsCLITargets . bcoBuildOptsCLI) bco
        getDocIndex = do
            let localDocs = haddockIndexFile (localDepsDocDir bco)
            localExists <- doesFileExist localDocs
            if localExists
                then return localDocs
                else do
                    let snapDocs = haddockIndexFile (snapDocDir bco)
                    snapExists <- doesFileExist snapDocs
                    if snapExists
                        then return snapDocs
                        else fail "No local or snapshot doc index found to open."
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
                    then return docFile
                    else do
                        $logWarn $
                            "Expected to find documentation at " <>
                            T.pack (toFilePath docFile) <>
                            ", but that file is missing.  Opening doc index instead."
                        getDocIndex
            _ -> getDocIndex
    $logInfo ("Opening " <> T.pack (toFilePath docFile) <> " in the browser.")
    _ <- liftIO $ openBrowser (toFilePath docFile)
    return ()

-- | Determine whether we should haddock for a package.
shouldHaddockPackage :: BuildOpts
                     -> Set PackageName  -- ^ Packages that we want to generate haddocks for
                                         -- in any case (whether or not we are going to generate
                                         -- haddocks for dependencies)
                     -> PackageName
                     -> Bool
shouldHaddockPackage bopts wanted name =
    if Set.member name wanted
        then boptsHaddock bopts
        else shouldHaddockDeps bopts

-- | Determine whether to build haddocks for dependencies.
shouldHaddockDeps :: BuildOpts -> Bool
shouldHaddockDeps bopts = fromMaybe (boptsHaddock bopts) (boptsHaddockDeps bopts)

-- | Generate Haddock index and contents for local packages.
generateLocalHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride
    -> WhichCompiler
    -> BaseConfigOpts
    -> Map GhcPkgId (DumpPackage () ())  -- ^ Local package dump
    -> [LocalPackage]
    -> m ()
generateLocalHaddockIndex envOverride wc bco localDumpPkgs locals = do
    let dumpPackages =
            mapMaybe
                (\LocalPackage{lpPackage = Package{..}} ->
                    F.find
                        (\dp -> dpPackageIdent dp == PackageIdentifier packageName packageVersion)
                        localDumpPkgs)
                locals
    generateHaddockIndex
        "local packages"
        envOverride
        wc
        dumpPackages
        "."
        (localDocDir bco)

-- | Generate Haddock index and contents for local packages and their dependencies.
generateDepsHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride
    -> WhichCompiler
    -> BaseConfigOpts
    -> Map GhcPkgId (DumpPackage () ())  -- ^ Global dump information
    -> Map GhcPkgId (DumpPackage () ())  -- ^ Snapshot dump information
    -> Map GhcPkgId (DumpPackage () ())  -- ^ Local dump information
    -> [LocalPackage]
    -> m ()
generateDepsHaddockIndex envOverride wc bco globalDumpPkgs snapshotDumpPkgs localDumpPkgs locals = do
    let deps = (mapMaybe (`lookupDumpPackage` allDumpPkgs) . nubOrd . findTransitiveDepends . mapMaybe getGhcPkgId) locals
        depDocDir = localDepsDocDir bco
    generateHaddockIndex
        "local packages and dependencies"
        envOverride
        wc
        deps
        ".."
        depDocDir
  where
    getGhcPkgId :: LocalPackage -> Maybe GhcPkgId
    getGhcPkgId LocalPackage{lpPackage = Package{..}} =
        let pkgId = PackageIdentifier packageName packageVersion
            mdpPkg = F.find (\dp -> dpPackageIdent dp == pkgId) localDumpPkgs
        in fmap dpGhcPkgId mdpPkg
    findTransitiveDepends :: [GhcPkgId] -> [GhcPkgId]
    findTransitiveDepends = (`go` HS.empty) . HS.fromList
      where
        go todo checked =
            case HS.toList todo of
                [] -> HS.toList checked
                (ghcPkgId:_) ->
                    let deps =
                            case lookupDumpPackage ghcPkgId allDumpPkgs of
                                Nothing -> HS.empty
                                Just pkgDP -> HS.fromList (dpDepends pkgDP)
                        deps' = deps `HS.difference` checked
                        todo' = HS.delete ghcPkgId (deps' `HS.union` todo)
                        checked' = HS.insert ghcPkgId checked
                    in go todo' checked'
    allDumpPkgs = [localDumpPkgs, snapshotDumpPkgs, globalDumpPkgs]

-- | Generate Haddock index and contents for all snapshot packages.
generateSnapHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride
    -> WhichCompiler
    -> BaseConfigOpts
    -> Map GhcPkgId (DumpPackage () ())  -- ^ Global package dump
    -> Map GhcPkgId (DumpPackage () ())  -- ^ Snapshot package dump
    -> m ()
generateSnapHaddockIndex envOverride wc bco globalDumpPkgs snapshotDumpPkgs =
    generateHaddockIndex
        "snapshot packages"
        envOverride
        wc
        (Map.elems snapshotDumpPkgs ++ Map.elems globalDumpPkgs)
        "."
        (snapDocDir bco)

-- | Generate Haddock index and contents for specified packages.
generateHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadLogger m, MonadBaseControl IO m)
    => Text
    -> EnvOverride
    -> WhichCompiler
    -> [DumpPackage () ()]
    -> FilePath
    -> Path Abs Dir
    -> m ()
generateHaddockIndex descr envOverride wc dumpPackages docRelFP destDir = do
    ensureDir destDir
    interfaceOpts <- (liftIO . fmap nubOrd . mapMaybeM toInterfaceOpt) dumpPackages
    unless (null interfaceOpts) $ do
        let destIndexFile = haddockIndexFile destDir
        eindexModTime <- liftIO (tryGetModificationTime destIndexFile)
        let needUpdate =
                case eindexModTime of
                    Left _ -> True
                    Right indexModTime ->
                        or [mt > indexModTime | (_,mt,_,_) <- interfaceOpts]
        when needUpdate $ do
            $logInfo
                (T.concat ["Updating Haddock index for ", descr, " in\n",
                           T.pack (toFilePath destIndexFile)])
            liftIO (mapM_ copyPkgDocs interfaceOpts)
            readProcessNull
                (Just destDir)
                envOverride
                (haddockExeName wc)
                (["--gen-contents", "--gen-index"] ++ [x | (xs,_,_,_) <- interfaceOpts, x <- xs])
  where
    toInterfaceOpt :: DumpPackage a b -> IO (Maybe ([String], UTCTime, Path Abs File, Path Abs File))
    toInterfaceOpt DumpPackage {..} = do
        case dpHaddockInterfaces of
            [] -> return Nothing
            srcInterfaceFP:_ -> do
                srcInterfaceAbsFile <- parseCollapsedAbsFile srcInterfaceFP
                let (PackageIdentifier name _) = dpPackageIdent
                    destInterfaceRelFP =
                        docRelFP FP.</>
                        packageIdentifierString dpPackageIdent FP.</>
                        (packageNameString name FP.<.> "haddock")
                destInterfaceAbsFile <- parseCollapsedAbsFile (toFilePath destDir FP.</> destInterfaceRelFP)
                esrcInterfaceModTime <- tryGetModificationTime srcInterfaceAbsFile
                return $
                    case esrcInterfaceModTime of
                        Left _ -> Nothing
                        Right srcInterfaceModTime ->
                            Just
                                ( [ "-i"
                                  , concat
                                        [ docRelFP FP.</> packageIdentifierString dpPackageIdent
                                        , ","
                                        , destInterfaceRelFP ]]
                                , srcInterfaceModTime
                                , srcInterfaceAbsFile
                                , destInterfaceAbsFile )
    tryGetModificationTime :: Path Abs File -> IO (Either () UTCTime)
    tryGetModificationTime = tryJust (guard . isDoesNotExistError) . getModificationTime
    copyPkgDocs :: (a, UTCTime, Path Abs File, Path Abs File) -> IO ()
    copyPkgDocs (_,srcInterfaceModTime,srcInterfaceAbsFile,destInterfaceAbsFile) = do
        -- Copy dependencies' haddocks to documentation directory.  This way, relative @../$pkg-$ver@
        -- links work and it's easy to upload docs to a web server or otherwise view them in a
        -- non-local-filesystem context. We copy instead of symlink for two reasons: (1) symlinks
        -- aren't reliably supported on Windows, and (2) the filesystem containing dependencies'
        -- docs may not be available where viewing the docs (e.g. if building in a Docker
        -- container).
        edestInterfaceModTime <- tryGetModificationTime destInterfaceAbsFile
        case edestInterfaceModTime of
            Left _ -> doCopy
            Right destInterfaceModTime
                | destInterfaceModTime < srcInterfaceModTime -> doCopy
                | otherwise -> return ()
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
                  -> [Map GhcPkgId (DumpPackage () ())]
                  -> Maybe (DumpPackage () ())
lookupDumpPackage ghcPkgId dumpPkgs =
    listToMaybe $ mapMaybe (Map.lookup ghcPkgId) dumpPkgs

-- | Path of haddock index file.
haddockIndexFile :: Path Abs Dir -> Path Abs File
haddockIndexFile destDir = destDir </> $(mkRelFile "index.html")

-- | Path of local packages documentation directory.
localDocDir :: BaseConfigOpts -> Path Abs Dir
localDocDir bco = bcoLocalInstallRoot bco </> docDirSuffix

-- | Path of documentation directory for the dependencies of local packages
localDepsDocDir :: BaseConfigOpts -> Path Abs Dir
localDepsDocDir bco = localDocDir bco </> $(mkRelDir "all")

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bcoSnapInstallRoot bco </> docDirSuffix
