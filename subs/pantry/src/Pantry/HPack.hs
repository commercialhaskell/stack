{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pantry.HPack (findOrGenerateCabalFile, hpack, hpackVersion, hpackToCabal) where

import RIO
import RIO.Process
import qualified RIO.ByteString as B
import Pantry.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import qualified Hpack
import qualified Hpack.Config as Hpack
import Data.Char (isSpace, isDigit)
import Path (Path, Abs, File, toFilePath, Dir, (</>), filename, parseAbsDir, parseRelFile, fromAbsFile)
import Path.IO (doesFileExist, listDir)

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
--
-- If the directory contains a file named package.yaml, hpack is used to
-- generate a .cabal file from it.
findOrGenerateCabalFile
    :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => Path Abs Dir -- ^ package directory
    -> RIO env (PackageName, Path Abs File)
findOrGenerateCabalFile pkgDir = do
    hpack pkgDir
    files <- filter (flip hasExtension "cabal" . toFilePath) . snd
         <$> listDir pkgDir
    -- If there are multiple files, ignore files that start with
    -- ".". On unixlike environments these are hidden, and this
    -- character is not valid in package names. The main goal is
    -- to ignore emacs lock files - see
    -- https://github.com/commercialhaskell/stack/issues/1897.
    let isHidden ('.':_) = True
        isHidden _ = False
    case filter (not . isHidden . toFilePath . filename) files of
        [] -> throwIO $ NoCabalFileFound pkgDir
        [x] -> maybe
          (throwIO $ InvalidCabalFilePath x)
          (\pn -> pure $ (pn, x)) $
            List.stripSuffix ".cabal" (toFilePath (filename x)) >>=
            parsePackageName
        _:_ -> throwIO $ MultipleCabalFilesFound pkgDir files
      where hasExtension fp x = FilePath.takeExtension fp == "." ++ x


hpackVersion
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RIO env Version
hpackVersion = do
  he <- view $ pantryConfigL.to pcHpackExecutable
  case he of
    HpackBundled -> do
                 version <- BL.unpack <$> (proc "stack" ["--hpack-numeric-version"] readProcessStdout_)
                 parseVersionThrowing $ filter (not . isSpace) version
    HpackCommand command -> do
                 version <- BL.unpack <$> proc command ["--version"] readProcessStdout_
                 let version' = dropWhile (not . isDigit) version
                     version'' = filter (not . isSpace) version'
                 parseVersionThrowing version''

-- | Generate .cabal file from package.yaml, if necessary.
hpack
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir
  -> RIO env ()
hpack pkgDir = do
    packageConfigRelFile <- parseRelFile Hpack.packageConfig
    let hpackFile = pkgDir Path.</> packageConfigRelFile
    whenM (doesFileExist hpackFile) $ do
        logDebug $ "Running hpack on " <> fromString (toFilePath hpackFile)

        he <- view $ pantryConfigL.to pcHpackExecutable
        case he of
            HpackBundled -> do
                r <- liftIO $ Hpack.hpackResult $ Hpack.setProgramName "stack" $ Hpack.setTarget (toFilePath hpackFile) Hpack.defaultOptions
                forM_ (Hpack.resultWarnings r) (logWarn . fromString)
                let cabalFile = fromString . Hpack.resultCabalFile $ r
                case Hpack.resultStatus r of
                    Hpack.Generated -> logDebug $ "hpack generated a modified version of " <> cabalFile
                    Hpack.OutputUnchanged -> logDebug $ "hpack output unchanged in " <> cabalFile
                    Hpack.AlreadyGeneratedByNewerHpack -> logWarn $
                        cabalFile <>
                        " was generated with a newer version of hpack,\n" <>
                        "please upgrade and try again."
                    Hpack.ExistingCabalFileWasModifiedManually -> logWarn $
                        cabalFile <>
                        " was modified manually. Ignoring " <>
                        fromString (toFilePath hpackFile) <>
                        " in favor of the cabal file.\nIf you want to use the " <>
                        fromString (toFilePath (filename hpackFile)) <>
                        " file instead of the cabal file,\n" <>
                        "then please delete the cabal file."
            HpackCommand command ->
                withWorkingDir (toFilePath pkgDir) $
                proc command [] runProcess_


hpackToCabal :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
           => ByteString -- Hpack's content
           -> RIO env (PackageName, ByteString)
hpackToCabal hpackBs = withSystemTempDirectory "hpack-pkg-dir" $ \tmpdir -> withWorkingDir tmpdir $ do
               B.writeFile (tmpdir FilePath.</> Hpack.packageConfig) hpackBs
               tdir <- parseAbsDir tmpdir
               (packageName, cfile) <- findOrGenerateCabalFile tdir
               bs <- B.readFile (fromAbsFile cfile)
               return (packageName, bs)
