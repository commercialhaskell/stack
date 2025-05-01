{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.UploadParser
Description : Parse arguments for Stack's @upload@ command.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @upload@ command.
-}

module Stack.Options.UploadParser
  ( uploadOptsParser
  ) where

import           Options.Applicative
                   ( Parser, completer, flag, help, idm, long, metavar, short
                   , strArgument, strOption, switch
                   )
import           Options.Applicative.Builder.Extra
                   ( boolFlags, dirCompleter, firstBoolFlagsTrue )
import           Stack.Options.PvpBoundsParser ( pvpBoundsParser )
import           Stack.Prelude
import           Stack.Upload ( UploadOpts (..), UploadVariant (..) )

-- | Parse command line arguments for Stack's @upload@ command.
uploadOptsParser :: Parser UploadOpts
uploadOptsParser = UploadOpts
  <$> itemsToWorkWithParser
  <*> documentationParser
  <*> optional (pvpBoundsParser (Just "For package upload"))
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
