{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Stack.Setup.Installed
    ( getCompilerVersion
    , markInstalled
    , unmarkInstalled
    , listInstalled
    , Tool (..)
    , toolString
    , toolNameString
    , parseToolText
    , extraDirs
    , installDir
    , tempInstallDir
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import           Data.List hiding (concat, elem, maximumBy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Distribution.System (Platform (..))
import qualified Distribution.System as Cabal
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Types.Compiler
import           Stack.Types.Config
import           RIO.Process

data Tool
    = Tool PackageIdentifier -- ^ e.g. ghc-7.8.4, msys2-20150512
    | ToolGhcGit !Text !Text   -- ^ e.g. ghc-git-COMMIT_ID-FLAVOUR
    deriving (Eq)

toolString :: Tool -> String
toolString (Tool ident) = packageIdentifierString ident
toolString (ToolGhcGit commit flavour) = "ghc-git-" ++ T.unpack commit ++ "-" ++ T.unpack flavour

toolNameString :: Tool -> String
toolNameString (Tool ident) = packageNameString $ pkgName ident
toolNameString ToolGhcGit{} = "ghc-git"

parseToolText :: Text -> Maybe Tool
parseToolText (parseWantedCompiler -> Right WCGhcjs{}) = Nothing
parseToolText (parseWantedCompiler -> Right (WCGhcGit c f)) = Just (ToolGhcGit c f)
parseToolText (parsePackageIdentifier . T.unpack -> Just pkgId) = Just (Tool pkgId)
parseToolText _ = Nothing

markInstalled :: (MonadIO m, MonadThrow m)
              => Path Abs Dir
              -> Tool
              -> m ()
markInstalled programsPath tool = do
    fpRel <- parseRelFile $ toolString tool ++ ".installed"
    writeBinaryFileAtomic (programsPath </> fpRel) "installed"

unmarkInstalled :: MonadIO m
                => Path Abs Dir
                -> Tool
                -> m ()
unmarkInstalled programsPath tool = liftIO $ do
    fpRel <- parseRelFile $ toolString tool ++ ".installed"
    ignoringAbsence (removeFile $ programsPath </> fpRel)

listInstalled :: (MonadIO m, MonadThrow m)
              => Path Abs Dir
              -> m [Tool]
listInstalled programsPath = do
    doesDirExist programsPath >>= \case
        False -> return []
        True -> do (_, files) <- listDir programsPath
                   return $ mapMaybe toTool files
  where
    toTool fp = do
        x <- T.stripSuffix ".installed" $ T.pack $ toFilePath $ filename fp
        parseToolText x

getCompilerVersion
  :: (HasProcessContext env, HasLogFunc env)
  => WhichCompiler
  -> Path Abs File -- ^ executable
  -> RIO env ActualCompiler
getCompilerVersion wc exe = do
    case wc of
        Ghc -> do
            logDebug "Asking GHC for its version"
            bs <- fst <$> proc (toFilePath exe) ["--numeric-version"] readProcess_
            let (_, ghcVersion) = versionFromEnd $ BL.toStrict bs
            x <- ACGhc <$> parseVersionThrowing (T.unpack $ T.decodeUtf8 ghcVersion)
            logDebug $ "GHC version is: " <> display x
            return x
  where
    versionFromEnd = S8.spanEnd isValid . fst . S8.breakEnd isValid
    isValid c = c == '.' || ('0' <= c && c <= '9')

-- | Binary directories for the given installed package
extraDirs :: HasConfig env => Tool -> RIO env ExtraDirs
extraDirs tool = do
    config <- view configL
    dir <- installDir (configLocalPrograms config) tool
    case (configPlatform config, toolNameString tool) of
        (Platform _ Cabal.Windows, isGHC -> True) -> return mempty
            { edBins =
                [ dir </> relDirBin
                , dir </> relDirMingw </> relDirBin
                ]
            }
        (Platform Cabal.I386 Cabal.Windows, "msys2") -> return mempty
            { edBins =
                [ dir </> relDirMingw32 </> relDirBin
                , dir </> relDirUsr </> relDirBin
                , dir </> relDirUsr </> relDirLocal </> relDirBin
                ]
            , edInclude =
                [ dir </> relDirMingw32 </> relDirInclude
                ]
            , edLib =
                [ dir </> relDirMingw32 </> relDirLib
                , dir </> relDirMingw32 </> relDirBin
                ]
            }
        (Platform Cabal.X86_64 Cabal.Windows, "msys2") -> return mempty
            { edBins =
                [ dir </> relDirMingw64 </> relDirBin
                , dir </> relDirUsr </> relDirBin
                , dir </> relDirUsr </> relDirLocal </> relDirBin
                ]
            , edInclude =
                [ dir </> relDirMingw64 </> relDirInclude
                ]
            , edLib =
                [ dir </> relDirMingw64 </> relDirLib
                , dir </> relDirMingw64 </> relDirBin
                ]
            }
        (_, isGHC -> True) -> return mempty
            { edBins =
                [ dir </> relDirBin
                ]
            }
        (Platform _ x, toolName) -> do
            logWarn $ "binDirs: unexpected OS/tool combo: " <> displayShow (x, toolName)
            return mempty
  where
    isGHC n = "ghc" == n || "ghc-" `isPrefixOf` n

installDir :: (MonadReader env m, MonadThrow m)
           => Path Abs Dir
           -> Tool
           -> m (Path Abs Dir)
installDir programsDir tool = do
    relativeDir <- parseRelDir $ toolString tool
    return $ programsDir </> relativeDir

tempInstallDir :: (MonadReader env m, MonadThrow m)
           => Path Abs Dir
           -> Tool
           -> m (Path Abs Dir)
tempInstallDir programsDir tool = do
    relativeDir <- parseRelDir $ toolString tool ++ ".temp"
    return $ programsDir </> relativeDir
