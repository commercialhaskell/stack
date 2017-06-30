{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Deal with downloading, cloning, or whatever else is necessary for
-- getting a 'PackageLocation' into something Stack can work with.
module Stack.PackageLocation
  ( resolvePackageLocation
  ) where

import Path
import Stack.Types.BuildPlan
import Stack.Types.Config
import System.Process.Read (EnvOverride)

-- | Resolve a 'PackageLocation' into a list of paths, downloading and cloning as
-- necessary.
resolvePackageLocation
    :: forall env m.
       (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation
    -> m [Path Abs Dir]
resolvePackageLocation = error "resolvePackageLocation"
    {- FIXME
resolvePackageEntry menv projRoot pe = do
    entryRoot <- resolvePackageLocation menv projRoot (peLocation pe)
    paths <-
        case peSubdirs pe of
            [] -> return [entryRoot]
            subs -> mapM (resolveDir entryRoot) subs
    extraDep <-
        case peExtraDepMaybe pe of
            Just e -> return e
            Nothing ->
                case peLocation pe of
                    PLFilePath _ ->
                        -- we don't give a warning on missing explicit
                        -- value here, user intent is almost always
                        -- the default for a local directory
                        return False
                    PLRemote url _ -> do
                        $logWarn $ mconcat
                            [ "No extra-dep setting found for package at URL:\n\n"
                            , url
                            , "\n\n"
                            , "This is usually a mistake, external packages "
                            , "should typically\nbe treated as extra-deps to avoid "
                            , "spurious test case failures."
                            ]
                        return False
                    PLIndex ident -> do
                        $logWarn $ mconcat
                            [ "No extra-dep setting found for package :\n\n"
                            , T.pack $ packageIdentifierRevisionString ident
                            , "\n\n"
                            , "This is usually a mistake, external packages "
                            , "should typically\nbe treated as extra-deps to avoid "
                            , "spurious test case failures."
                            ]
                        return False
    return $ map (, extraDep) paths

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
resolvePackageLocation
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation
    -> m (Path Abs Dir)
resolvePackageLocation _ projRoot (PLFilePath fp) = resolveDir projRoot fp
resolvePackageLocation menv projRoot (PLRemote url remotePackageType) = do
    workDir <- view workDirL
    let nameBeforeHashing = case remotePackageType of
            RPTHttp{} -> url
            RPTGit commit -> T.unwords [url, commit]
            RPTHg commit -> T.unwords [url, commit, "hg"]
        -- TODO: dedupe with code for snapshot hash?
        name = T.unpack $ decodeUtf8 $ S.take 12 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $ encodeUtf8 nameBeforeHashing
        root = projRoot </> workDir </> $(mkRelDir "downloaded")
        fileExtension' = case remotePackageType of
            RPTHttp -> ".http-archive"
            _       -> ".unused"

    fileRel <- parseRelFile $ name ++ fileExtension'
    dirRel <- parseRelDir name
    dirRelTmp <- parseRelDir $ name ++ ".tmp"
    let file = root </> fileRel
        dir = root </> dirRel

    exists <- doesDirExist dir
    unless exists $ do
        ignoringAbsence (removeDirRecur dir)

        let cloneAndExtract commandName cloneArgs resetCommand commit = do
                ensureDir root
                callProcessInheritStderrStdout Cmd
                    { cmdDirectoryToRunIn = Just root
                    , cmdCommandToRun = commandName
                    , cmdEnvOverride = menv
                    , cmdCommandLineArguments =
                        "clone" :
                        cloneArgs ++
                        [ T.unpack url
                        , toFilePathNoTrailingSep dir
                        ]
                    }
                created <- doesDirExist dir
                unless created $ throwM $ FailedToCloneRepo commandName
                readProcessNull (Just dir) menv commandName
                    (resetCommand ++ [T.unpack commit, "--"])
                    `catch` \case
                        ex@ProcessFailed{} -> do
                            $logInfo $ "Please ensure that commit " <> commit <> " exists within " <> url
                            throwM ex
                        ex -> throwM ex

        case remotePackageType of
            RPTHttp -> do
                let dirTmp = root </> dirRelTmp
                ignoringAbsence (removeDirRecur dirTmp)

                let fp = toFilePath file
                req <- parseUrlThrow $ T.unpack url
                _ <- download req file

                let tryTar = do
                        $logDebug $ "Trying to untar " <> T.pack fp
                        liftIO $ withBinaryFile fp ReadMode $ \h -> do
                            lbs <- L.hGetContents h
                            let entries = Tar.read $ GZip.decompress lbs
                            Tar.unpack (toFilePath dirTmp) entries
                    tryZip = do
                        $logDebug $ "Trying to unzip " <> T.pack fp
                        archive <- fmap Zip.toArchive $ liftIO $ L.readFile fp
                        liftIO $  Zip.extractFilesFromArchive [Zip.OptDestination
                                                               (toFilePath dirTmp)] archive
                    err = throwM $ UnableToExtractArchive url file

                    catchAllLog goodpath handler =
                        catchAll goodpath $ \e -> do
                            $logDebug $ "Got exception: " <> T.pack (show e)
                            handler

                tryTar `catchAllLog` tryZip `catchAllLog` err
                renameDir dirTmp dir

            -- Passes in --git-dir to git and --repository to hg, in order
            -- to avoid the update commands being applied to the user's
            -- repo.  See https://github.com/commercialhaskell/stack/issues/2748
            RPTGit commit -> cloneAndExtract "git" ["--recursive"] ["--git-dir=.git", "reset", "--hard"] commit
            RPTHg  commit -> cloneAndExtract "hg"  []              ["--repository", ".", "update", "-C"] commit

    case remotePackageType of
        RPTHttp -> do
            x <- listDir dir
            case x of
                ([dir'], []) -> return dir'
                (dirs, files) -> do
                    ignoringAbsence (removeFile file)
                    ignoringAbsence (removeDirRecur dir)
                    throwM $ UnexpectedArchiveContents dirs files
        _ -> return dir
    -}
