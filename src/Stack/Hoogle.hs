{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A wrapper around hoogle.
module Stack.Hoogle
    ( hoogleCmd
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char (isSpace)
import qualified Data.Text as T
import           Distribution.Types.PackageName (mkPackageName)
import           Distribution.Version (mkVersion)
import           Lens.Micro ((?~))
import           Path (parseAbsFile)
import           Path.IO hiding (findExecutable)
import qualified Stack.Build
import           Stack.Build.Target (NeedTargets(NeedTargets))
import           Stack.Runners
import           Stack.Types.Config
import           System.Exit
import           RIO.Process

-- | Hoogle command.
hoogleCmd :: ([String],Bool,Bool,Bool) -> RIO Runner ()
hoogleCmd (args,setup,rebuild,startServer) =
  local (over globalOptsL modifyGO) $
  withConfig YesReexec $
  withDefaultEnvConfig $ do
    hooglePath <- ensureHoogleInPath
    generateDbIfNeeded hooglePath
    runHoogle hooglePath args'
  where
    modifyGO :: GlobalOpts -> GlobalOpts
    modifyGO = globalOptsBuildOptsMonoidL . buildOptsMonoidHaddockL ?~ True

    args' :: [String]
    args' = if startServer
                 then ["server", "--local", "--port", "8080"]
                 else []
            ++ args
    generateDbIfNeeded :: Path Abs File -> RIO EnvConfig ()
    generateDbIfNeeded hooglePath = do
        databaseExists <- checkDatabaseExists
        if databaseExists && not rebuild
            then return ()
            else if setup || rebuild
                     then do
                         logWarn
                             (if rebuild
                                  then "Rebuilding database ..."
                                  else "No Hoogle database yet. Automatically building haddocks and hoogle database (use --no-setup to disable) ...")
                         buildHaddocks
                         logInfo "Built docs."
                         generateDb hooglePath
                         logInfo "Generated DB."
                     else do
                         logError
                             "No Hoogle database. Not building one due to --no-setup"
                         bail
    generateDb :: Path Abs File -> RIO EnvConfig ()
    generateDb hooglePath = do
        do dir <- hoogleRoot
           createDirIfMissing True dir
           runHoogle hooglePath ["generate", "--local"]
    buildHaddocks :: RIO EnvConfig ()
    buildHaddocks = do
      config <- view configL
      runRIO config $ -- a bit weird that we have to drop down like this
        catch (withDefaultEnvConfig $ Stack.Build.build Nothing)
              (\(_ :: ExitCode) -> return ())
    hooglePackageName = mkPackageName "hoogle"
    hoogleMinVersion = mkVersion [5, 0]
    hoogleMinIdent =
        PackageIdentifier hooglePackageName hoogleMinVersion
    installHoogle :: RIO EnvConfig ()
    installHoogle = do
        hooglePackageIdentifier <- do
          mversion <- getLatestHackageVersion YesRequireHackageIndex hooglePackageName UsePreferredVersions

          -- FIXME For a while, we've been following the logic of
          -- taking the latest Hoogle version available. However, we
          -- may want to instead grab the version of Hoogle present in
          -- the snapshot current being used instead.
          pure $ fromMaybe (Left hoogleMinIdent) $ do
            pir@(PackageIdentifierRevision _ ver _) <- mversion
            guard $ ver >= hoogleMinVersion
            Just $ Right pir

        case hooglePackageIdentifier of
            Left{} -> logInfo $
              "Minimum " <>
              fromString (packageIdentifierString hoogleMinIdent) <>
              " is not in your index. Installing the minimum version."
            Right ident -> logInfo $
              "Minimum version is " <>
              fromString (packageIdentifierString hoogleMinIdent) <>
              ". Found acceptable " <>
              display ident <>
              " in your index, installing it."
        config <- view configL
        menv <- liftIO $ configProcessContextSettings config envSettings
        let boptsCLI = defaultBuildOptsCLI
                { boptsCLITargets =
                    pure $
                    T.pack . packageIdentifierString $
                    either
                    id
                    (\(PackageIdentifierRevision n v _) -> PackageIdentifier n v)
                    hooglePackageIdentifier
                }
        runRIO config $ catch -- Also a bit weird
                 (withEnvConfig
                      NeedTargets
                      boptsCLI $
                      Stack.Build.build Nothing
                 )
                 (\(e :: ExitCode) ->
                       case e of
                           ExitSuccess -> runRIO menv resetExeCache
                           _ -> throwIO e)
    runHoogle :: Path Abs File -> [String] -> RIO EnvConfig ()
    runHoogle hooglePath hoogleArgs = do
        config <- view configL
        menv <- liftIO $ configProcessContextSettings config envSettings
        dbpath <- hoogleDatabasePath
        let databaseArg = ["--database=" ++ toFilePath dbpath]
        withProcessContext menv $ proc
          (toFilePath hooglePath)
          (hoogleArgs ++ databaseArg)
          runProcess_
    bail :: RIO EnvConfig a
    bail = liftIO (exitWith (ExitFailure (-1)))
    checkDatabaseExists = do
        path <- hoogleDatabasePath
        liftIO (doesFileExist path)
    ensureHoogleInPath :: RIO EnvConfig (Path Abs File)
    ensureHoogleInPath = do
        config <- view configL
        menv <- liftIO $ configProcessContextSettings config envSettings
        mhooglePath <- runRIO menv $ findExecutable "hoogle"
        eres <- case mhooglePath of
            Left _ -> return $ Left "Hoogle isn't installed."
            Right hooglePath -> do
                result <- withProcessContext menv
                        $ proc hooglePath ["--numeric-version"]
                        $ tryAny . fmap fst . readProcess_
                let unexpectedResult got = Left $ T.concat
                        [ "'"
                        , T.pack hooglePath
                        , " --numeric-version' did not respond with expected value. Got: "
                        , got
                        ]
                return $ case result of
                    Left err -> unexpectedResult $ T.pack (show err)
                    Right bs -> case parseVersion (takeWhile (not . isSpace) (BL8.unpack bs)) of
                        Nothing -> unexpectedResult $ T.pack (BL8.unpack bs)
                        Just ver
                            | ver >= hoogleMinVersion -> Right hooglePath
                            | otherwise -> Left $ T.concat
                                [ "Installed Hoogle is too old, "
                                , T.pack hooglePath
                                , " is version "
                                , T.pack $ versionString ver
                                , " but >= 5.0 is required."
                                ]
        case eres of
            Right hooglePath -> parseAbsFile hooglePath
            Left err
                | setup -> do
                    logWarn $ display err <> " Automatically installing (use --no-setup to disable) ..."
                    installHoogle
                    mhooglePath' <- runRIO menv $ findExecutable "hoogle"
                    case mhooglePath' of
                        Right hooglePath -> parseAbsFile hooglePath
                        Left _ -> do
                            logWarn "Couldn't find hoogle in path after installing.  This shouldn't happen, may be a bug."
                            bail
                | otherwise -> do
                    logWarn $ display err <> " Not installing it due to --no-setup."
                    bail
    envSettings =
        EnvSettings
        { esIncludeLocals = True
        , esIncludeGhcPackagePath = True
        , esStackExe = True
        , esLocaleUtf8 = False
        , esKeepGhcRts = False
        }
