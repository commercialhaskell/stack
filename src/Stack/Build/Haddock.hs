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
    , shouldHaddockPackage
    , shouldHaddockDeps
    ) where

import           Control.Exception              (tryJust, onException)
import           Control.Monad
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.Foldable                  as F
import           Data.Function
import qualified Data.HashSet                   as HS
import           Data.List
import           Data.List.Extra                (nubOrd)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Maybe.Extra               (mapMaybeM)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Path
import           Path.Extra
import           Path.IO
import           Prelude
import           Safe                           (maximumMay)
import           Stack.Types.Build
import           Stack.PackageDump
import           Stack.Types
import           System.Directory               (getModificationTime)
import qualified System.FilePath                as FP
import           System.IO.Error                (isDoesNotExistError)
import           System.Process.Read

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
        depDocDir = localDocDir bco </> $(mkRelDir "all")
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
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => Text
    -> EnvOverride
    -> WhichCompiler
    -> [DumpPackage () ()]
    -> FilePath
    -> Path Abs Dir
    -> m ()
generateHaddockIndex descr envOverride wc dumpPackages docRelFP destDir = do
    createTree destDir
    interfaceOpts <- (liftIO . fmap nubOrd . mapMaybeM toInterfaceOpt) dumpPackages
    case maximumMay (map (\(_,x,_,_) -> x) interfaceOpts) of
        Nothing -> return ()
        Just maxInterfaceModTime -> do
            eindexModTime <-
                liftIO $
                tryJust (guard . isDoesNotExistError) $
                getModificationTime (toFilePath (haddockIndexFile destDir))
            let needUpdate =
                    case eindexModTime of
                        Left _ -> True
                        Right indexModTime ->
                            indexModTime < maxInterfaceModTime
            when
                needUpdate $
                do $logInfo
                       (T.concat ["Updating Haddock index for ", descr, " in\n",
                                  T.pack (toFilePath (haddockIndexFile destDir))])
                   liftIO (mapM_ copyPkgDocs interfaceOpts)
                   readProcessNull
                       (Just destDir)
                       envOverride
                       (haddockExeName wc)
                       (["--gen-contents", "--gen-index"] ++ concatMap (\(x,_,_,_) -> x) interfaceOpts)
  where
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
                esrcInterfaceModTime <-
                    tryJust (guard . isDoesNotExistError) $
                    getModificationTime (toFilePath srcInterfaceAbsFile)
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
    copyPkgDocs (_,srcInterfaceModTime,srcInterfaceAbsFile,destInterfaceAbsFile) = do
        -- Copy dependencies' haddocks to documentation directory.  This way, relative @../$pkg-$ver@
        -- links work and it's easy to upload docs to a web server or otherwise view them in a
        -- non-local-filesystem context. We copy instead of symlink for two reasons: (1) symlinks
        -- aren't reliably supported on Windows, and (2) the filesystem containing dependencies'
        -- docs may not be available where viewing the docs (e.g. if building in a Docker
        -- container).
        edestInterfaceModTime <-
            tryJust (guard . isDoesNotExistError) $
            getModificationTime (toFilePath destInterfaceAbsFile)
        case edestInterfaceModTime of
            Left _ -> doCopy
            Right destInterfaceModTime
                | destInterfaceModTime < srcInterfaceModTime -> doCopy
                | otherwise -> return ()
      where
        doCopy = do
            removeTreeIfExists destHtmlAbsDir
            createTree destHtmlAbsDir
            onException
                (copyDirectoryRecursive (parent srcInterfaceAbsFile) destHtmlAbsDir)
                (removeTreeIfExists destHtmlAbsDir)
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

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bcoSnapInstallRoot bco </> docDirSuffix
