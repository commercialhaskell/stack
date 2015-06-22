{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Generate haddocks
module Stack.Build.Haddock
    ( copyDepHaddocks
    , generateHaddockIndex
    , shouldHaddockPackage
    , shouldHaddockDeps
    ) where

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
import qualified Data.Text                      as T
import           Path
import           Path.IO
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Build.Types
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           System.Directory               hiding (findExecutable,
                                                 findFiles)
import qualified System.FilePath                as FP
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
                -> [Path Abs Dir]
                -> PackageIdentifier
                -> Set (Path Abs Dir)
                -> m ()
copyDepHaddocks envOverride pkgDbs pkgId extraDestDirs = do
    mpkgHtmlDir <- findGhcPkgHaddockHtml envOverride pkgDbs pkgId
    case mpkgHtmlDir of
        Nothing -> return ()
        Just pkgHtmlDir -> do
            depGhcIds <- findGhcPkgDepends envOverride pkgDbs pkgId
            forM_ (map ghcPkgIdPackageIdentifier depGhcIds) $
                copyDepWhenNeeded pkgHtmlDir
  where
    copyDepWhenNeeded pkgHtmlDir depId = do
        mDepOrigDir <- findGhcPkgHaddockHtml envOverride pkgDbs depId
        case mDepOrigDir of
            Nothing -> return ()
            Just depOrigDir ->
                copyWhenNeeded (Set.insert (parent pkgHtmlDir) extraDestDirs)
                               depId depOrigDir
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
            copyDepHaddocks envOverride pkgDbs depId destDirs
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
        depCopyDirExists <- dirExists depCopyDir
        liftIO $ do
            when depCopyDirExists $
                removeDirectoryRecursive (toFilePath depCopyDir)
            createDirectoryIfMissing True (toFilePath depCopyDir)
        copyDirectoryRecursive depOrigDir depCopyDir

-- | Generate Haddock index and contents for local packages.
generateHaddockIndex :: (MonadIO m, MonadCatch m, MonadThrow m, MonadLogger m, MonadBaseControl IO m)
                     => EnvOverride
                     -> BaseConfigOpts
                     -> [LocalPackage]
                     -> m ()
generateHaddockIndex envOverride bco locals = do
    $logInfo ("Generating Haddock index in\n" <>
              T.pack (toFilePath (haddockIndexFile docDir)))
    interfaceArgs <- mapM (\LocalPackage {lpPackage = Package {..}} ->
                              toInterfaceOpt (PackageIdentifier packageName packageVersion))
                          locals
    readProcessNull
        (Just docDir)
        envOverride
        "haddock"
        (["--gen-contents", "--gen-index"] ++ concat interfaceArgs)
  where
    docDir = bcoLocalInstallRoot bco </> docdirSuffix
    toInterfaceOpt pid@(PackageIdentifier name _) = do
        interfaceRelFile <- parseRelFile (packageIdentifierString pid FP.</>
                                          packageNameString name FP.<.>
                                          "haddock")
        interfaceExists <- fileExists (docDir </> interfaceRelFile)
        return $ if interfaceExists
            then [ "-i"
                 , concat
                     [ packageIdentifierString pid
                     , ","
                     , toFilePath interfaceRelFile ] ]
            else []

haddockIndexFile :: Path Abs Dir -> Path Abs File
haddockIndexFile docDir = docDir </> $(mkRelFile "index.html")
