{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.UploadOpts
Description : Types for command line options for Stack's @upload@ command.
License     : BSD-3-Clause

Types for command line options for Stack's @upload@ command.
-}

module Stack.Types.UploadOpts
  ( UploadOpts (..)
  , UploadVariant (..)
  ) where

import           Stack.Prelude
import           Stack.Types.PvpBounds (PvpBounds)

-- | Type representing command line options for the @stack upload@ command.
data UploadOpts = UploadOpts
  { itemsToWorkWith :: ![String]
    -- ^ The items to work with.
  , documentation :: !Bool
    -- ^ Uploading documentation for packages?
  , pvpBounds :: !(Maybe PvpBounds)
  , check :: !Bool
  , buildPackage :: !Bool
  , tarPath :: !(Maybe FilePath)
  , uploadVariant :: !UploadVariant
  , saveHackageCreds :: !FirstTrue
    -- ^ Save user's Hackage username and password in a local file?
  }

-- | Type representing variants for uploading to Hackage.
data UploadVariant
  = Publishing
    -- ^ Publish the package/a published package.
  | Candidate
    -- ^ Create a package candidate/a package candidate.
