{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Options.UploadParser
  ( UploadOpts (..)
  , UploadVariant (..)
  , uploadOptsParser
  ) where

import           Options.Applicative
import           Stack.Options.SDistParser ( sdistOptsParser )
import           Stack.Prelude
import           Stack.SDist ( SDistOpts (..) )

data UploadOpts = UploadOpts
  { uoptsSDistOpts :: SDistOpts
  , uoptsUploadVariant :: UploadVariant
  -- ^ Says whether to publish the package or upload as a release candidate
  }

data UploadVariant
  = Publishing
  -- ^ Publish the package
  | Candidate
  -- ^ Create a package candidate

-- | Parser for arguments to `stack upload`
uploadOptsParser :: Parser UploadOpts
uploadOptsParser =
  UploadOpts
    <$> sdistOptsParser
    <*> uploadVariant
  where
    uploadVariant =
      flag Publishing Candidate
        (long "candidate" <>
         help "Upload as a package candidate")
