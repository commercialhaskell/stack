{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           RIO.Process

data Tool
    = Tool PackageIdentifier -- ^ e.g. ghc-7.8.4, msys2-20150512
    | ToolGhcjs (CompilerVersion 'CVActual) -- ^ e.g. ghcjs-0.1.0_ghc-7.10.2

toolString :: Tool -> String
toolString (Tool ident) = packageIdentifierString ident
toolString (ToolGhcjs cv) = compilerVersionString cv

toolNameString :: Tool -> String
toolNameString (Tool ident) = packageNameString $ packageIdentifierName ident
toolNameString ToolGhcjs{} = "ghcjs"

parseToolText :: Text -> Maybe Tool
parseToolText (parseCompilerVersion -> Just (cv@GhcjsVersion{})) = Just (ToolGhcjs cv)
parseToolText (parsePackageIdentifierFromString . T.unpack -> Just pkgId) = Just (Tool pkgId)
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

getCompilerVersion
  :: (HasProcessContext env, HasLogFunc env)
  => WhichCompiler
  -> RIO env (CompilerVersion 'CVActual)
getCompilerVersion wc =
    case wc of
        Ghc -> do
            logDebug "Asking GHC for its version"
            bs <- proc "ghc" ["--numeric-version"] readProcessStdout_
            let (_, ghcVersion) = versionFromEnd $ BL.toStrict bs
            x <- GhcVersion <$> parseVersion (T.decodeUtf8 ghcVersion)
            logDebug $ "GHC version is: " <> display x
            return x
        Ghcjs -> do
            logDebug "Asking GHCJS for its version"
            -- Output looks like
            --
            -- The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.1.0 (GHC 7.10.2)
            bs <- proc "ghcjs" ["--version"] readProcessStdout_
            let (rest, ghcVersion) = T.decodeUtf8 <$> versionFromEnd (BL.toStrict bs)
                (_, ghcjsVersion) = T.decodeUtf8 <$> versionFromEnd rest
            GhcjsVersion <$> parseVersion ghcjsVersion <*> parseVersion ghcVersion
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
                [ dir </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "mingw") </> $(mkRelDir "bin")
                ]
            }
        (Platform Cabal.I386 Cabal.Windows, "msys2") -> return mempty
            { edBins =
                [ dir </> $(mkRelDir "mingw32") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "local") </> $(mkRelDir "bin")
                ]
            , edInclude =
                [ dir </> $(mkRelDir "mingw32") </> $(mkRelDir "include")
                ]
            , edLib =
                [ dir </> $(mkRelDir "mingw32") </> $(mkRelDir "lib")
                , dir </> $(mkRelDir "mingw32") </> $(mkRelDir "bin")
                ]
            }
        (Platform Cabal.X86_64 Cabal.Windows, "msys2") -> return mempty
            { edBins =
                [ dir </> $(mkRelDir "mingw64") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "local") </> $(mkRelDir "bin")
                ]
            , edInclude =
                [ dir </> $(mkRelDir "mingw64") </> $(mkRelDir "include")
                ]
            , edLib =
                [ dir </> $(mkRelDir "mingw64") </> $(mkRelDir "lib")
                , dir </> $(mkRelDir "mingw64") </> $(mkRelDir "bin")
                ]
            }
        (_, isGHC -> True) -> return mempty
            { edBins =
                [ dir </> $(mkRelDir "bin")
                ]
            }
        (_, isGHCJS -> True) -> return mempty
            { edBins =
                [ dir </> $(mkRelDir "bin")
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
instance Monoid ExtraDirs where
    mempty = memptydefault
    mappend = mappenddefault

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
