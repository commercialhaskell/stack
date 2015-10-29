{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Generate haddocks
module Stack.Build.Haddock
    ( copyDepHaddocks
    , generateLocalHaddockIndex
    , generateDepsHaddockIndex
    , generateSnapHaddockIndex
    , shouldHaddockPackage
    , shouldHaddockDeps
    ) where

import           Control.Exception              (tryJust)
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
import           Data.Maybe
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Path
import           Path.IO
import           Prelude
import           Safe                           (maximumMay)
import           Stack.Types.Build
import           Stack.PackageDump
import           Stack.Types
import           System.Directory               (getModificationTime, canonicalizePath,
                                                 doesDirectoryExist)
import qualified System.FilePath                as FP
import           System.IO.Error                (isDoesNotExistError)
import           System.Process.Read

-- | Determine whether we should haddock for a package.
shouldHaddockPackage :: BuildOpts -> Set PackageName -> PackageName -> Bool
shouldHaddockPackage bopts wanted name =
    if Set.member name wanted
        then boptsHaddock bopts
        else shouldHaddockDeps bopts

-- | Determine whether to build haddocks for dependencies.
shouldHaddockDeps :: BuildOpts -> Bool
shouldHaddockDeps bopts = fromMaybe (boptsHaddock bopts) (boptsHaddockDeps bopts)

-- | Copy dependencies' haddocks to documentation directory.  This way, relative @../$pkg-$ver@
-- links work and it's easy to upload docs to a web server or otherwise view them in a
-- non-local-filesystem context. We copy instead of symlink for two reasons: (1) symlinks aren't
-- reliably supported on Windows, and (2) the filesystem containing dependencies' docs may not be
-- available where viewing the docs (e.g. if building in a Docker container).
copyDepHaddocks :: (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m)
                => BaseConfigOpts
                -> [Map GhcPkgId (DumpPackage () ())]
                -> GhcPkgId
                -> Set (Path Abs Dir)
                -> m ()
copyDepHaddocks bco dumpPkgs ghcPkgId extraDestDirs = do
    let mdp = lookupDumpPackage ghcPkgId dumpPkgs
    case mdp of
        Nothing -> return ()
        Just dp ->
            forM_ (dpDepends dp) $ \depDP ->
                case dpHaddockHtml dp of
                    Nothing -> return ()
                    Just pkgHtmlFP -> do
                        pkgHtmlDir <- parseAbsDir pkgHtmlFP
                        copyDepWhenNeeded pkgHtmlDir depDP
  where
    copyDepWhenNeeded pkgHtmlDir depGhcPkgId = do
        let mDepDP = lookupDumpPackage depGhcPkgId dumpPkgs
        case mDepDP of
            Nothing -> return ()
            Just depDP ->
                case dpHaddockHtml depDP of
                    Nothing -> return ()
                    Just depOrigFP0 -> do
                        let extraDestDirs' =
                                -- Parent test ensures we don't try to copy docs to global locations
                                if bcoSnapInstallRoot bco `isParentOf` pkgHtmlDir ||
                                   bcoLocalInstallRoot bco `isParentOf` pkgHtmlDir
                                    then Set.insert (parent pkgHtmlDir) extraDestDirs
                                    else extraDestDirs
                        depOrigFP <- liftIO $ do
                            exists <- doesDirectoryExist depOrigFP0
                            if exists
                                then canonicalizePath depOrigFP0
                                else return depOrigFP0
                        depOrigDir <- parseAbsDir depOrigFP
                        copyWhenNeeded extraDestDirs' (dpPackageIdent depDP) (dpGhcPkgId depDP) depOrigDir
    copyWhenNeeded destDirs depId depGhcPkgId depOrigDir = do
        depRelDir <- parseRelDir (packageIdentifierString depId)
        copied <- forM (Set.toList destDirs) $ \destDir -> do
            let depCopyDir = destDir </> depRelDir
            if depCopyDir == depOrigDir
                then return False
                else do
                    needCopy <- getNeedCopy depOrigDir depCopyDir
                    when needCopy $ doCopy depOrigDir depCopyDir
                    return needCopy
        when (or copied) $
            copyDepHaddocks bco dumpPkgs depGhcPkgId destDirs
    getNeedCopy depOrigDir depCopyDir = do
        let depOrigIndex = haddockIndexFile depOrigDir
            depCopyIndex = haddockIndexFile depCopyDir
        depOrigExists <- fileExists depOrigIndex
        depCopyExists <- fileExists depCopyIndex
        case (depOrigExists, depCopyExists) of
            (False, _) -> return False
            (True, False) -> return True
            (True, True) -> do
                copyMod <- liftIO $ getModificationTime (toFilePath depCopyIndex)
                origMod <- liftIO $ getModificationTime (toFilePath depOrigIndex)
                return (copyMod <= origMod)
    doCopy depOrigDir depCopyDir = do
        removeTreeIfExists depCopyDir
        createTree depCopyDir
        copyDirectoryRecursive depOrigDir depCopyDir

-- | Generate Haddock index and contents for local packages.
generateLocalHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride -> WhichCompiler -> BaseConfigOpts -> [LocalPackage] -> m ()
generateLocalHaddockIndex envOverride wc bco locals = do
    let packageIDs =
            map
                (\LocalPackage{lpPackage = Package{..}} ->
                      PackageIdentifier packageName packageVersion)
                locals
    generateHaddockIndex
        "local packages"
        envOverride
        wc
        packageIDs
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
    let deps = (nubOrd . mapMaybe getPkgId .  findTransitiveDepends . mapMaybe getGhcPkgId) locals
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
    getPkgId :: GhcPkgId -> Maybe PackageIdentifier
    getPkgId = fmap dpPackageIdent . (`lookupDumpPackage` allDumpPkgs)
    allDumpPkgs = [localDumpPkgs, snapshotDumpPkgs, globalDumpPkgs]

-- | Generate Haddock index and contents for all snapshot packages.
generateSnapHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride
    -> WhichCompiler
    -> BaseConfigOpts
    -> Map GhcPkgId (DumpPackage () ())
    -> Map GhcPkgId (DumpPackage () ())
    -> m ()
generateSnapHaddockIndex envOverride wc bco globalDumpPkgs snapshotDumpPkgs =
    generateHaddockIndex
        "snapshot packages"
        envOverride
        wc
        (nubOrd $
         map
             dpPackageIdent
             (Map.elems globalDumpPkgs ++ Map.elems snapshotDumpPkgs))
        "."
        (snapDocDir bco)

-- | Generate Haddock index and contents for specified packages.
generateHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => Text
    -> EnvOverride
    -> WhichCompiler
    -> [PackageIdentifier]
    -> FilePath
    -> Path Abs Dir
    -> m ()
generateHaddockIndex descr envOverride wc packageIDs docRelDir destDir = do
    createTree destDir
    interfaceOpts <- liftIO $ fmap catMaybes (mapM toInterfaceOpt packageIDs)
    case maximumMay (map snd interfaceOpts) of
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
                   readProcessNull
                       (Just destDir)
                       envOverride
                       (haddockExeName wc)
                       (["--gen-contents", "--gen-index"] ++ concatMap fst interfaceOpts)
  where
    toInterfaceOpt pid@(PackageIdentifier name _) = do
        let interfaceRelFile =
                docRelDir FP.</> packageIdentifierString pid FP.</>
                packageNameString name FP.<.>
                "haddock"
            interfaceAbsFile = toFilePath destDir FP.</> interfaceRelFile
        einterfaceModTime <-
            tryJust (guard . isDoesNotExistError) $
            getModificationTime interfaceAbsFile
        return $
            case einterfaceModTime of
                Left _ -> Nothing
                Right interfaceModTime ->
                    Just
                        ( [ "-i"
                          , concat
                                [ docRelDir FP.</> packageIdentifierString pid
                                , ","
                                , interfaceRelFile]]
                        , interfaceModTime)

-- | Find first DumpPackage matching the GhcPkgId
lookupDumpPackage :: GhcPkgId
                  -> [Map GhcPkgId (DumpPackage () ())]
                  -> Maybe (DumpPackage () ())
lookupDumpPackage ghcPkgId dumpPkgs =
    listToMaybe $ catMaybes $ map (Map.lookup ghcPkgId) dumpPkgs

-- | Path of haddock index file.
haddockIndexFile :: Path Abs Dir -> Path Abs File
haddockIndexFile destDir = destDir </> $(mkRelFile "index.html")

-- | Path of local packages documentation directory.
localDocDir :: BaseConfigOpts -> Path Abs Dir
localDocDir bco = bcoLocalInstallRoot bco </> docDirSuffix

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bcoSnapInstallRoot bco </> docDirSuffix
