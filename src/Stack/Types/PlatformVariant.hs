{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.PlatformVariant
  ( PlatformVariant (..)
  , platformVariantSuffix
  ) where

import           Stack.Prelude

-- | A variant of the platform, used to differentiate Docker builds from host
data PlatformVariant
  = PlatformVariantNone
  | PlatformVariant String

-- | Render a platform variant to a String suffix.
platformVariantSuffix :: PlatformVariant -> String
platformVariantSuffix PlatformVariantNone = ""
platformVariantSuffix (PlatformVariant v) = "-" ++ v
