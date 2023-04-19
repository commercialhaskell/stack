{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.Platform
  ( PlatformVariant (..)
  , HasPlatform (..)
  , platformVariantSuffix
  , platformOnlyRelDir
  ) where

import           Distribution.System ( Platform )
import           Distribution.Text ( display )
import           Lens.Micro ( _1, _2 )
import           Path ( parseRelDir )
import           Stack.Prelude

-- | A variant of the platform, used to differentiate Docker builds from host
data PlatformVariant
  = PlatformVariantNone
  | PlatformVariant String

-- | Class for environment values which have a Platform
class HasPlatform env where
  platformL :: Lens' env Platform
  platformVariantL :: Lens' env PlatformVariant

instance HasPlatform (Platform, PlatformVariant) where
  platformL = _1
  platformVariantL = _2

-- | Render a platform variant to a String suffix.
platformVariantSuffix :: PlatformVariant -> String
platformVariantSuffix PlatformVariantNone = ""
platformVariantSuffix (PlatformVariant v) = "-" ++ v

-- | Relative directory for the platform identifier
platformOnlyRelDir ::
     (MonadReader env m, HasPlatform env, MonadThrow m)
  => m (Path Rel Dir)
platformOnlyRelDir = do
  platform <- view platformL
  platformVariant <- view platformVariantL
  parseRelDir
    (  Distribution.Text.display platform
    ++ platformVariantSuffix platformVariant
    )
