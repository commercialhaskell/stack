{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A wrapper around hoogle.
module Stack.Hoogle
    ( hoogleCmd
    ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as S8
import           Data.List (find)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Data.Set as Set
import           Lens.Micro
import           Path
import           Path.IO
import qualified Stack.Build
import           Stack.Fetch
import           Stack.Runners
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.StackT
import           Stack.Types.Version
import           System.Exit
import           System.Process.Read (resetExeCache, tryProcessStdout)
import           System.Process.Run

-- | Hoogle command.
hoogleCmd :: ([String],Bool,Bool) -> GlobalOpts -> IO ()
hoogleCmd (args,setup,rebuild) go = withBuildConfig go pathToHaddocks
  where
    pathToHaddocks :: StackT EnvConfig IO ()
    pathToHaddocks = do
        hoogleIsInPath <- checkHoogleInPath
        if hoogleIsInPath
            then haddocksToDb
            else do
                if setup
                    then do
                        $logWarn
                            "Hoogle isn't installed or is too old. Automatically installing (use --no-setup to disable) ..."
                        installHoogle
                        haddocksToDb
                    else do
                        $logError
                            "Hoogle isn't installed or is too old. Not installing it due to --no-setup."
                        bail
    haddocksToDb :: StackT EnvConfig IO ()
    haddocksToDb = do
        databaseExists <- checkDatabaseExists
        if databaseExists && not rebuild
            then runHoogle args
            else if setup || rebuild
                     then do
                         $logWarn
                             (if rebuild
                                  then "Rebuilding database ..."
                                  else "No Hoogle database yet. Automatically building haddocks and hoogle database (use --no-setup to disable) ...")
                         buildHaddocks
                         $logInfo "Built docs."
                         generateDb
                         $logInfo "Generated DB."
                         runHoogle args
                     else do
                         $logError
                             "No Hoogle database. Not building one due to --no-setup"
                         bail
    generateDb :: StackT EnvConfig IO ()
    generateDb = do
        do dir <- hoogleRoot
           createDirIfMissing True dir
           runHoogle ["generate", "--local"]
    buildHaddocks :: StackT EnvConfig IO ()
    buildHaddocks =
        liftIO
            (catch
                 (withBuildConfigAndLock
                      (set
                           (globalOptsBuildOptsMonoidL . buildOptsMonoidHaddockL)
                           (Just True)
                           go)
                      (\lk ->
                            Stack.Build.build
                                (const (return ()))
                                lk
                                defaultBuildOptsCLI))
                 (\(_ :: ExitCode) ->
                       return ()))
    installHoogle :: StackT EnvConfig IO ()
    installHoogle = do
        let hooglePackageName = $(mkPackageName "hoogle")
            hoogleMinVersion = $(mkVersion "5.0")
            hoogleMinIdent =
                PackageIdentifier hooglePackageName hoogleMinVersion
        hooglePackageIdentifier <-
            do (_,_,resolved) <-
                   resolvePackagesAllowMissing
                       mempty
                       (Set.fromList [hooglePackageName])
               return
                   (case find
                             ((== hooglePackageName) . packageIdentifierName)
                             (Map.keys resolved) of
                        Just ident@(PackageIdentifier _ ver)
                          | ver >= hoogleMinVersion -> Right ident
                        _ -> Left hoogleMinIdent)
        case hooglePackageIdentifier of
            Left{} ->
                $logInfo
                    ("Minimum " <> packageIdentifierText hoogleMinIdent <>
                     " is not in your index. Installing the minimum version.")
            Right ident ->
                $logInfo
                    ("Minimum version is " <> packageIdentifierText hoogleMinIdent <>
                     ". Found acceptable " <>
                     packageIdentifierText ident <>
                     " in your index, installing it.")
        config <- view configL
        menv <- liftIO $ configEnvOverride config envSettings
        liftIO
            (catch
                 (withBuildConfigAndLock
                      go
                      (\lk ->
                            Stack.Build.build
                                (const (return ()))
                                lk
                                defaultBuildOptsCLI
                                { boptsCLITargets = [ packageIdentifierText
                                                          (either
                                                               id
                                                               id
                                                               hooglePackageIdentifier)]
                                }))
                 (\(e :: ExitCode) ->
                       case e of
                           ExitSuccess -> resetExeCache menv
                           _ -> throwIO e))
    runHoogle :: [String] -> StackT EnvConfig IO ()
    runHoogle hoogleArgs = do
        config <- view configL
        menv <- liftIO $ configEnvOverride config envSettings
        dbpath <- hoogleDatabasePath
        let databaseArg = ["--database=" ++ toFilePath dbpath]
        runCmd
            Cmd
             { cmdDirectoryToRunIn = Nothing
             , cmdCommandToRun = "hoogle"
             , cmdEnvOverride = menv
             , cmdCommandLineArguments = hoogleArgs ++ databaseArg
             }
            Nothing
    bail :: StackT EnvConfig IO ()
    bail = liftIO (exitWith (ExitFailure (-1)))
    checkDatabaseExists = do
        path <- hoogleDatabasePath
        liftIO (doesFileExist path)
    checkHoogleInPath = do
        config <- view configL
        menv <- liftIO $ configEnvOverride config envSettings
        result <- tryProcessStdout Nothing menv "hoogle" ["--numeric-version"]
        case fmap (reads . S8.unpack) result of
            Right [(ver :: Double,_)] -> return (ver >= 5.0)
            _ -> return False
    envSettings =
        EnvSettings
        { esIncludeLocals = True
        , esIncludeGhcPackagePath = True
        , esStackExe = True
        , esLocaleUtf8 = False
        }
