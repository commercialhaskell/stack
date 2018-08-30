{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , ExtraDirs (..)
    , extraDirs
    , installDir
    , tempInstallDir
    ) where

import           Stack.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import           Data.List hiding (concat, elem, maximumBy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Distribution.System (Platform (..))
import qualified Distribution.System as Cabal
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Types.Compiler
import           Stack.Types.Config
import           RIO.Process

data Tool
    = Tool PackageIdentifier -- ^ e.g. ghc-7.8.4, msys2-20150512
    | ToolGhcjs ActualCompiler -- ^ e.g. ghcjs-0.1.0_ghc-7.10.2

toolString :: Tool -> String
toolString (Tool ident) = packageIdentifierString ident
toolString (ToolGhcjs cv) = compilerVersionString cv

toolNameString :: Tool -> String
toolNameString (Tool ident) = packageNameString $ pkgName ident
toolNameString ToolGhcjs{} = "ghcjs"

parseToolText :: Text -> Maybe Tool
parseToolText (parseWantedCompiler -> Right (WCGhcjs x y)) = Just (ToolGhcjs (ACGhcjs x y))
parseToolText (parsePackageIdentifier . T.unpack -> Just pkgId) = Just (Tool pkgId)
parseToolText _ = Nothing

markInstalled :: (MonadIO m, MonadThrow m)
              => Path Abs Dir
              -> Tool
              -> m ()
markInstalled programsPath tool = do
    fpRel <- parseRelFile $ toolString tool ++ ".installed"
    liftIO $ B.writeFile (toFilePath $ programsPath </> fpRel) "installed"

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

-- | See https://github.com/commercialhaskell/stack/issues/4086.
warnAboutGHCJS :: HasLogFunc env => RIO env ()
warnAboutGHCJS =
    logWarn $ "Building a GHCJS project. " <> fromString ghcjsWarning

ghcjsWarning :: String
ghcjsWarning = unwords
     [ "Note that GHCJS support in Stack is EXPERIMENTAL"
     ]

getCompilerVersion
  :: (HasProcessContext env, HasLogFunc env)
  => WhichCompiler
  -> RIO env ActualCompiler
getCompilerVersion wc =
    case wc of
        Ghc -> do
            logDebug "Asking GHC for its version"
            bs <- fst <$> proc "ghc" ["--numeric-version"] readProcess_
            let (_, ghcVersion) = versionFromEnd $ BL.toStrict bs
            x <- ACGhc <$> parseVersionThrowing (T.unpack $ T.decodeUtf8 ghcVersion)
            logDebug $ "GHC version is: " <> display x
            return x
        Ghcjs -> do
            warnAboutGHCJS
            logDebug "Asking GHCJS for its version"
            -- Output looks like
            --
            -- The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.1.0 (GHC 7.10.2)
            bs <- fst <$> proc "ghcjs" ["--version"] readProcess_
            let (rest, ghcVersion) = T.unpack . T.decodeUtf8 <$> versionFromEnd (BL.toStrict bs)
                (_, ghcjsVersion) = T.unpack . T.decodeUtf8 <$> versionFromEnd rest
            ACGhcjs <$> parseVersionThrowing ghcjsVersion <*> parseVersionThrowing ghcVersion
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
        (_, isGHCJS -> True) -> return mempty
            { edBins =
                [ dir </> relDirBin
                ]
            }
        (Platform _ x, toolName) -> do
            logWarn $ "binDirs: unexpected OS/tool combo: " <> displayShow (x, toolName)
            return mempty
  where
    isGHC n = "ghc" == n || "ghc-" `isPrefixOf` n
    isGHCJS n = "ghcjs" == n

data ExtraDirs = ExtraDirs
    { edBins :: ![Path Abs Dir]
    , edInclude :: ![Path Abs Dir]
    , edLib :: ![Path Abs Dir]
    } deriving (Show, Generic)
instance Semigroup ExtraDirs where
    (<>) = mappenddefault
instance Monoid ExtraDirs where
    mempty = memptydefault
    mappend = (<>)

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
