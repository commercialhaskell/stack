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
import           Options.Applicative.Builder.Extra
                   ( boolFlags, dirCompleter, firstBoolFlagsTrue )
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
  <*> saveHackageCredsOption
 where
  itemsToWorkWithParser = many (strArgument
    (  metavar "ITEM"
    <> completer dirCompleter
    <> help "A relative path to a package directory or, for package upload \
            \only, an sdist tarball. Can be specified multiple times."
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
      <> help "Do not check packages, for upload, for common mistakes."
      )
  buildPackageOption = boolFlags False
      "test-tarball"
      "building of the resulting generated files, for package upload."
      idm
  tarDirParser = optional (strOption
    (  long "tar-dir"
    <> help "If specified, copy all the generated files, for package upload, \
            \to this directory."
    ))
  uploadVariantParser = flag Publishing Candidate
    (  long "candidate"
    <> help "Upload as, or for, a package candidate."
    )
  saveHackageCredsOption = firstBoolFlagsTrue
      "save-hackage-creds"
      "saving user's Hackage username and password in a local file."
      idm
