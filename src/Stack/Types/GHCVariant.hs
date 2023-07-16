{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.GHCVariant
  ( GHCVariant (..)
  , HasGHCVariant (..)
  , ghcVariantName
  , ghcVariantSuffix
  , parseGHCVariant
  ) where

import           Data.Aeson.Types ( FromJSON, parseJSON, withText )
import           Data.List ( stripPrefix )
import qualified Data.Text as T
import           Stack.Prelude

-- | Specialized variant of GHC (e.g. libgmp4 or integer-simple)
data GHCVariant
  = GHCStandard
  -- ^ Standard bindist
  | GHCIntegerSimple
  -- ^ Bindist that uses integer-simple
  | GHCNativeBignum
  -- ^ Bindist that uses the Haskell-native big-integer backend
  | GHCCustom String
  -- ^ Other bindists
  deriving Show

instance FromJSON GHCVariant where
  -- Strange structuring is to give consistent error messages
  parseJSON =
    withText
      "GHCVariant"
      (either (fail . show) pure . parseGHCVariant . T.unpack)

-- | Class for environment values which have a GHCVariant
class HasGHCVariant env where
  ghcVariantL :: SimpleGetter env GHCVariant

instance HasGHCVariant GHCVariant where
  ghcVariantL = id
  {-# INLINE ghcVariantL #-}

-- | Render a GHC variant to a String.
ghcVariantName :: GHCVariant -> String
ghcVariantName GHCStandard = "standard"
ghcVariantName GHCIntegerSimple = "integersimple"
ghcVariantName GHCNativeBignum = "int-native"
ghcVariantName (GHCCustom name) = "custom-" ++ name

-- | Render a GHC variant to a String suffix.
ghcVariantSuffix :: GHCVariant -> String
ghcVariantSuffix GHCStandard = ""
ghcVariantSuffix v = "-" ++ ghcVariantName v

-- | Parse GHC variant from a String.
parseGHCVariant :: (MonadThrow m) => String -> m GHCVariant
parseGHCVariant s =
  case stripPrefix "custom-" s of
    Just name -> pure (GHCCustom name)
    Nothing
      | s == "" -> pure GHCStandard
      | s == "standard" -> pure GHCStandard
      | s == "integersimple" -> pure GHCIntegerSimple
      | s == "int-native" -> pure GHCNativeBignum
      | otherwise -> pure (GHCCustom s)
