{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
import           Control.Monad.Writer
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Path
import           Path.IO
import           Prelude
import           Safe                           (maximumMay)
import           Stack.Types.Build
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           System.Directory               (getModificationTime)
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
                => EnvOverride
                -> WhichCompiler
                -> BaseConfigOpts
                -> [Path Abs Dir]
                -> PackageIdentifier
                -> Set (Path Abs Dir)
                -> m ()
copyDepHaddocks envOverride wc bco pkgDbs pkgId extraDestDirs = do
    mpkgHtmlDir <- findGhcPkgHaddockHtml envOverride wc pkgDbs pkgId
    case mpkgHtmlDir of
        Nothing -> return ()
        Just pkgHtmlDir -> do
            depGhcIds <- findGhcPkgDepends envOverride wc pkgDbs pkgId
            forM_ (map ghcPkgIdPackageIdentifier depGhcIds) $
                copyDepWhenNeeded pkgHtmlDir
  where
    copyDepWhenNeeded pkgHtmlDir depId = do
        mDepOrigDir <- findGhcPkgHaddockHtml envOverride wc pkgDbs depId
        case mDepOrigDir of
            Nothing -> return ()
            Just depOrigDir -> do
                let extraDestDirs' =
                        -- Parent test ensures we don't try to copy docs to global locations
                        if bcoSnapInstallRoot bco `isParentOf` pkgHtmlDir ||
                           bcoLocalInstallRoot bco `isParentOf` pkgHtmlDir
                            then Set.insert (parent pkgHtmlDir) extraDestDirs
                            else extraDestDirs
                copyWhenNeeded extraDestDirs' depId depOrigDir
    copyWhenNeeded destDirs depId depOrigDir = do
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
            copyDepHaddocks envOverride wc bco pkgDbs depId destDirs
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
    => EnvOverride -> WhichCompiler -> BaseConfigOpts -> [LocalPackage] -> m ()
generateDepsHaddockIndex envOverride wc bco locals = do
    depSets <-
        mapM
            (\LocalPackage{lpPackage = Package{..}} ->
                  findTransitiveGhcPkgDepends
                      envOverride
                      wc
                      [bcoSnapDB bco, bcoLocalDB bco]
                      (PackageIdentifier packageName packageVersion))
            locals
    generateHaddockIndex
        "local packages and dependencies"
        envOverride
        wc
        (Set.toList (Set.unions depSets))
        ".."
        (localDocDir bco </> $(mkRelDir "all"))

-- | Generate Haddock index and contents for all snapshot packages.
generateSnapHaddockIndex
    :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride -> WhichCompiler -> BaseConfigOpts -> Path Abs Dir -> m ()
generateSnapHaddockIndex envOverride wc bco globalDB = do
    pkgIds <- listGhcPkgDbs envOverride wc [globalDB, bcoSnapDB bco]
    generateHaddockIndex
        "snapshot packages"
        envOverride
        wc
        pkgIds
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
                       ("Updating Haddock index for " <> descr <> " in\n" <>
                        T.pack (toFilePath (haddockIndexFile destDir)))
                   readProcessNull
                       (Just destDir)
                       envOverride
                       (compilerExeName wc)
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

-- | Path of haddock index file.
haddockIndexFile :: Path Abs Dir -> Path Abs File
haddockIndexFile destDir = destDir </> $(mkRelFile "index.html")

-- | Path of local packages documentation directory.
localDocDir :: BaseConfigOpts -> Path Abs Dir
localDocDir bco = bcoLocalInstallRoot bco </> docDirSuffix

-- | Path of snapshot packages documentation directory.
snapDocDir :: BaseConfigOpts -> Path Abs Dir
snapDocDir bco = bcoSnapInstallRoot bco </> docDirSuffix
