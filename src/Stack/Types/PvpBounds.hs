{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.PvpBounds
  ( PvpBounds (..)
  , PvpBoundsType (..)
  , pvpBoundsText
  , parsePvpBounds
  ) where
import qualified Data.Map as Map
import qualified Data.Text as T
import           Pantry.Internal.AesonExtended
                   ( FromJSON (..), ToJSON (..), withText
                   )
import           Stack.Prelude

-- | How PVP bounds should be added to .cabal files
data PvpBoundsType
  = PvpBoundsNone
  | PvpBoundsUpper
  | PvpBoundsLower
  | PvpBoundsBoth
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

data PvpBounds = PvpBounds
  { pbType :: !PvpBoundsType
  , pbAsRevision :: !Bool
  }
  deriving (Eq, Ord, Read, Show, Typeable)

pvpBoundsText :: PvpBoundsType -> Text
pvpBoundsText PvpBoundsNone = "none"
pvpBoundsText PvpBoundsUpper = "upper"
pvpBoundsText PvpBoundsLower = "lower"
pvpBoundsText PvpBoundsBoth = "both"

parsePvpBounds :: Text -> Either String PvpBounds
parsePvpBounds t = maybe err Right $ do
  (t', asRevision) <-
    case T.break (== '-') t of
      (x, "") -> Just (x, False)
      (x, "-revision") -> Just (x, True)
      _ -> Nothing
  x <- Map.lookup t' m
  Just PvpBounds
    { pbType = x
    , pbAsRevision = asRevision
    }
 where
  m = Map.fromList $ map (pvpBoundsText &&& id) [minBound..maxBound]
  err = Left $ "Invalid PVP bounds: " ++ T.unpack t

instance ToJSON PvpBounds where
  toJSON (PvpBounds typ asRevision) =
    toJSON (pvpBoundsText typ <> (if asRevision then "-revision" else ""))

instance FromJSON PvpBounds where
  parseJSON = withText "PvpBounds" (either fail pure . parsePvpBounds)
