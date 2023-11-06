{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @upload@ command.
module Stack.Options.UploadParser
  ( uploadOptsParser
  ) where

import qualified Data.Text as T
import           Options.Applicative
                   ( Parser, completeWith, completer, flag, help, idm, long
                   , metavar, option, readerError, short, strArgument, strOption
                   , switch
                   )
import           Options.Applicative.Builder.Extra ( boolFlags, dirCompleter )
import           Options.Applicative.Types ( readerAsk )
import           Stack.Prelude
import           Stack.Upload ( UploadOpts (..), UploadVariant (..) )
import           Stack.Types.PvpBounds ( PvpBounds (..), parsePvpBounds )

-- | Parse command line arguments for Stack's @upload@ command.
uploadOptsParser :: Parser UploadOpts
uploadOptsParser = UploadOpts
  <$> itemsToWorkWithParser
  <*> documentationParser
  <*> optional pvpBoundsOption
  <*> ignoreCheckSwitch
  <*> buildPackageOption
  <*> tarDirParser
  <*> uploadVariantParser
 where
  itemsToWorkWithParser = many (strArgument
    (  metavar "ITEM"
    <> completer dirCompleter
    <> help "A relative path to a package directory or, for package upload \
            \only, an sdist tarball."
    ))
  documentationParser = flag False True
    (  long "documentation"
    <> short 'd'
    <> help "Upload documentation for packages (not packages)."
    )
  pvpBoundsOption :: Parser PvpBounds
  pvpBoundsOption = option readPvpBounds
    (  long "pvp-bounds"
    <> metavar "PVP-BOUNDS"
    <> completeWith ["none", "lower", "upper", "both"]
    <> help "For package upload, how PVP version bounds should be added to \
            \Cabal file: none, lower, upper, both."
    )
   where
    readPvpBounds = do
      s <- readerAsk
      case parsePvpBounds $ T.pack s of
        Left e -> readerError e
        Right v -> pure v
  ignoreCheckSwitch = switch
      (  long "ignore-check"
      <> help "For package upload, do not check packages for common mistakes."
      )
  buildPackageOption = boolFlags False
      "test-tarball"
      "building of the resulting sdist tarball(s), for package upload."
      idm
  tarDirParser = optional (strOption
    (  long "tar-dir"
    <> help "For package upload, if specified, copy all the tar to this \
            \directory."
    ))
  uploadVariantParser = flag Publishing Candidate
    (  long "candidate"
    <> help "Upload as, or for, a package candidate."
    )
