{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pantry.HPack
    (
     hpack
    , hpackVersion
    ) where

import RIO
import RIO.Process
import Pantry.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Hpack
import qualified Hpack.Config as Hpack
import Data.Char (isSpace, isDigit)
import Path (Path, Abs, toFilePath, Dir, (</>), filename, parseRelFile)
import Path.IO (doesFileExist)


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
