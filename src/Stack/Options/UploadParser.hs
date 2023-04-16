{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions to parse command line arguments for Stack's @upload@ command.
module Stack.Options.UploadParser
  ( uploadOptsParser
  ) where

import           Options.Applicative
import           Stack.Options.SDistParser ( sdistOptsParser )
import           Stack.Prelude
import           Stack.Upload ( UploadOpts (..), UploadVariant (..) )

-- | Parse command line arguments for Stack's @upload@ command.
uploadOptsParser :: Parser UploadOpts
uploadOptsParser =
  UploadOpts
    <$> sdistOptsParser
    <*> uploadVariant
  where
    uploadVariant = flag Publishing Candidate
      (  long "candidate"
      <> help "Upload as a package candidate"
      )
