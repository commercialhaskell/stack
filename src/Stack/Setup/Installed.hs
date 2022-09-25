{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Stack.Setup.Installed
    ( getCompilerVersion
    , markInstalled
    , unmarkInstalled
    , listInstalled
    , Tool (..)
    , toolString
    , toolNameString
    , parseToolText
    , filterTools
    , extraDirs
    , installDir
    , tempInstallDir
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
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

-- | 'Tool' values are ordered by name (being @ghc-git@, for @ToolGhcGit _ _@)
-- alphabetically and then by version (later versions are ordered before
-- earlier versions, where applicable).
instance Ord Tool where
  compare (Tool pkgId1) (Tool pkgId2) = if pkgName1 == pkgName2
      then compare pkgVersion2 pkgVersion1 -- Later versions ordered first
      else compare pkgName1 pkgName2
    where
      PackageIdentifier pkgName1 pkgVersion1 = pkgId1
      PackageIdentifier pkgName2 pkgVersion2 = pkgId2
  compare (Tool pkgId) (ToolGhcGit _ _) = compare (pkgName pkgId) "ghc-git"
  compare (ToolGhcGit _ _) (Tool pkgId) = compare "ghc-git" (pkgName pkgId)
  compare (ToolGhcGit c1 f1) (ToolGhcGit c2 f2) = if f1 == f2
      then compare c1 c2
      else compare f1 f2

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
        False -> pure []
        True -> do (_, files) <- listDir programsPath
                   pure $ mapMaybe toTool files
  where
    toTool fp = do
        x <- T.stripSuffix ".installed" $ T.pack $ toFilePath $ filename fp
        parseToolText x

filterTools :: PackageName       -- ^ package to find
            -> (Version -> Bool) -- ^ which versions are acceptable
            -> [Tool]            -- ^ tools to filter
            -> [PackageIdentifier]
filterTools name goodVersion installed =
    [ pkgId | Tool pkgId <- installed
            , pkgName pkgId == name
            , goodVersion (pkgVersion pkgId) ]

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
            pure x
  where
    versionFromEnd = S8.spanEnd isValid . fst . S8.breakEnd isValid
    isValid c = c == '.' || ('0' <= c && c <= '9')

-- | Binary directories for the given installed package
extraDirs :: HasConfig env => Tool -> RIO env ExtraDirs
extraDirs tool = do
    config <- view configL
    dir <- installDir (configLocalPrograms config) tool
    case (configPlatform config, toolNameString tool) of
        (Platform _ Cabal.Windows, isGHC -> True) -> pure mempty
            { edBins =
                [ dir </> relDirBin
                , dir </> relDirMingw </> relDirBin
                ]
            }
        (Platform Cabal.I386 Cabal.Windows, "msys2") -> pure mempty
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
        (Platform Cabal.X86_64 Cabal.Windows, "msys2") -> pure mempty
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
        (_, isGHC -> True) -> pure mempty
            { edBins =
                [ dir </> relDirBin
                ]
            }
        (Platform _ x, toolName) -> do
            logWarn $ "binDirs: unexpected OS/tool combo: " <> displayShow (x, toolName)
            pure mempty
  where
    isGHC n = "ghc" == n || "ghc-" `L.isPrefixOf` n

installDir :: (MonadReader env m, MonadThrow m)
           => Path Abs Dir
           -> Tool
           -> m (Path Abs Dir)
installDir programsDir tool = do
    relativeDir <- parseRelDir $ toolString tool
    pure $ programsDir </> relativeDir

tempInstallDir :: (MonadReader env m, MonadThrow m)
           => Path Abs Dir
           -> Tool
           -> m (Path Abs Dir)
tempInstallDir programsDir tool = do
    relativeDir <- parseRelDir $ toolString tool ++ ".temp"
    pure $ programsDir </> relativeDir
