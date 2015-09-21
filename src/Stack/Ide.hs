{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client.Conduit
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Exec (defaultEnvSettings)
import           Stack.Ghci (GhciPkgInfo(..), ghciSetup)
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.Exit
import           System.IO
import qualified System.Process as P
import           System.Process.Read

-- | Launch a GHCi IDE for the given local project targets with the
-- given options and configure it with the load paths and extensions
-- of those targets.
ide
    :: (HasConfig r, HasBuildConfig r, HasTerminal r, HasLogLevel r, MonadMask m, HasEnvConfig r, MonadReader r m, MonadIO m, MonadThrow m, MonadLogger m, MonadCatch m, MonadBaseControl IO m, HasHttpManager r)
    => [Text] -- ^ Targets.
    -> [String] -- ^ GHC options.
    -> m ()
ide targets useropts = do
    (_realTargets,_,pkgs) <- ghciSetup Nothing targets
    pwd <- getWorkingDir
    (pkgopts,srcfiles) <-
        liftM mconcat $
        forM pkgs $
        \pkg ->
             do dist <- distDirFromDir (ghciPkgDir pkg)
                autogen <- return (autogenDir dist)
                paths_foo <-
                    liftM
                        (autogen </>)
                        (parseRelFile
                             ("Paths_" ++
                              packageNameString (ghciPkgName pkg) ++ ".hs"))
                paths_foo_exists <- fileExists paths_foo
                return
                    ( ["--dist-dir=" <> toFilePath dist] ++
                      map ("--ghc-option=" ++) (ghciPkgOpts pkg)
                    , mapMaybe
                          (fmap toFilePath . stripDir pwd)
                          (S.toList (ghciPkgModFiles pkg) <>
                           if paths_foo_exists
                               then [paths_foo]
                               else []))
    localdb <- packageDatabaseLocal
    depsdb <- packageDatabaseDeps
    mpath <- liftIO $ lookupEnv "PATH"
    bindirs <- extraBinDirs `ap` return True {- include local bin -}
    let pkgdbs =
            ["--package-db=" <> toFilePath depsdb <> ":" <> toFilePath localdb]
        paths =
            [ "--ide-backend-tools-path=" <>
              intercalate ":" (map toFilePath bindirs) <>
              (maybe "" (':' :) mpath)]
        args =
            ["--verbose"] <> ["--local-work-dir=" ++ toFilePath pwd] <>
            map ("--ghc-option=" ++) useropts <>
            paths <>
            pkgopts <>
            pkgdbs
    let initialStdin = encode (initialRequest srcfiles)
    $logDebug $ "Initial stack-ide request: " <> T.pack (show initialStdin)
    exec "stack-ide" args initialStdin

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
