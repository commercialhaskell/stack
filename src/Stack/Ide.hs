{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Run a IDE configured with the user's project(s).

module Stack.Ide (ide) where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import           Path
import           Path.IO
import           Stack.Build.Source
import           Stack.Constants
import           Stack.Exec (defaultEnvSettings)
import           Stack.Package
import           Stack.Types
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.Exit
import           System.IO
import qualified System.Process as P
import           System.Process.Read
import qualified Data.Text as T

-- | Launch a GHCi IDE for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ide
    :: (HasConfig r, HasBuildConfig r, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m)
    => [Text] -- ^ Targets.
    -> [String] -- ^ GHC options.
    -> m ()
ide targets useropts = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    pwd <- getWorkingDir
    locals <-
        liftM catMaybes $
        forM (M.toList (bcPackages bconfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                if validWanted && wanted pwd cabalfp name
                    then return (Just (name, cabalfp))
                    else return Nothing
    pkgs <-
        liftM catMaybes $
        forM (M.toList (bcPackages bconfig)) $
        \(dir,validWanted) ->
             do cabalfp <- getCabalFileName dir
                name <- parsePackageNameFromFilePath cabalfp
                let config =
                        PackageConfig
                        { packageConfigEnableTests = True
                        , packageConfigEnableBenchmarks = True
                        , packageConfigFlags = localFlags mempty bconfig name
                        , packageConfigGhcVersion = envConfigGhcVersion econfig
                        , packageConfigPlatform = configPlatform
                              (getConfig bconfig)
                        }
                pkg <- readPackage config cabalfp
                if validWanted && wanted pwd cabalfp name
                    then do
                        pkgOpts <-
                            getPackageOpts
                                (packageOpts pkg)
                                (map fst locals)
                                cabalfp
                        srcfiles <-
                            getPackageFiles (packageFiles pkg) Modules cabalfp
                        dist <- distDirFromDir dir
                        autogen <- return (autogenDir dist)
                        paths_foo <-
                            liftM
                                (autogen </>)
                                (parseRelFile
                                     ("Paths_" ++
                                      packageNameString name ++ ".hs"))
                        paths_foo_exists <- fileExists paths_foo
                        return
                            (Just
                                 ( packageName pkg
                                 , ["--dist-dir=" <> toFilePath dist] ++
                                   map
                                       ("--ghc-option=" ++)
                                       (filter (not . badForGhci) pkgOpts)
                                 , mapMaybe
                                       (stripDir pwd)
                                       (S.toList srcfiles <>
                                        if paths_foo_exists
                                            then [paths_foo]
                                            else [])))
                    else return Nothing
    localdb <- packageDatabaseLocal
    depsdb <- packageDatabaseDeps
    mpath <- liftIO $ lookupEnv "PATH"
    bindirs <- extraBinDirs `ap` return True {- include local bin -}
    let pkgopts = concat (map _2 pkgs)
        srcfiles = concatMap (map toFilePath . _3) pkgs
        pkgdbs =
            ["--package-db=" <> toFilePath depsdb <> ":" <> toFilePath localdb]
        paths =
            ["--ide-backend-tools-path=" <> intercalate ":" (map toFilePath bindirs) <> (maybe "" (':':) mpath)
            ]
        args =
            ["--verbose"] <>
            ["--local-work-dir=" ++ toFilePath pwd] <>
            map ("--ghc-option=" ++) (filter (not . badForGhci) useropts) <>
            paths <>
            pkgopts <>
            pkgdbs
    $logDebug $ "Running stack-ide " <> T.pack (unwords args)
    exec "stack-ide" args (encode (initialRequest srcfiles))
  where
    wanted pwd cabalfp name = isInWantedList || targetsEmptyAndInDir
      where
        isInWantedList = elem (packageNameText name) targets
        targetsEmptyAndInDir = null targets || isParentOf (parent cabalfp) pwd
    badForGhci x =
        isPrefixOf "-O" x || elem x (words "-debug -threaded -ticky")
    _1 (x,_,_) = x
    _2 (_,x,_) = x
    _3 (_,_,x) = x

-- | Make the initial request.
initialRequest :: [FilePath] -> Value
initialRequest srcfiles =
    object
        [ "tag" .= "RequestUpdateSession"
        , "contents" .=
            [ object
                [ "tag" .= "RequestUpdateTargets"
                , "contents" .= object
                    [ "tag" .= "TargetsInclude"
                    , "contents" .= srcfiles ]
                ]
            ]
        ]

-- | Execute a process within the Stack configured environment.
exec :: (HasConfig r, MonadReader r m, MonadIO m, MonadLogger m, MonadThrow m)
        => String -> [String] -> ByteString -> m b
exec cmd args input = do
    config <- asks getConfig
    menv <-
        liftIO
            (configEnvOverride
                 config
                 defaultEnvSettings
                 { esIncludeGhcPackagePath = False
                 })
    exists <- liftIO $ doesFileExist cmd
    cmd' <-
        if exists
            then return cmd
            else liftM toFilePath $
                 join $ System.Process.Read.findExecutable menv cmd
    let cp =
            (P.proc cmd' args)
            { P.env = envHelper menv
            , P.delegate_ctlc = True
            , P.std_in = P.CreatePipe
            }
    $logProcessRun cmd' args
    (Just procin,Nothing,Nothing,ph) <- liftIO (P.createProcess cp)
    liftIO
        (do hSetBuffering stdin LineBuffering
            hSetBuffering procin LineBuffering)
    liftIO (do {-S8.hPutStrLn stdout (L.toStrict input)-}
               S8.hPutStrLn procin (L.toStrict input))
    _tid <-
        liftIO
            (forkIO
                 (forever
                      (do bytes <- S.getLine
                          S.hPutStr procin bytes)))
    ec <- liftIO (P.waitForProcess ph)
    liftIO (exitWith ec)
