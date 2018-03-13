{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Stack.Types.VersionIntervals
  ( VersionIntervals
  , toVersionRange
  , fromVersionRange
  , withinIntervals
  , unionVersionIntervals
  , intersectVersionIntervals
  ) where

import Stack.Types.Version
import qualified Distribution.Version as C
import Stack.Prelude

newtype VersionIntervals = VersionIntervals [VersionInterval]
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store VersionIntervals
instance NFData VersionIntervals

data VersionInterval = VersionInterval
  { viLowerVersion :: !Version
  , viLowerBound :: !Bound
  , viUpper :: !(Maybe (Version, Bound))
  }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store VersionInterval
instance NFData VersionInterval

data Bound = ExclusiveBound | InclusiveBound
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store Bound
instance NFData Bound

toVersionRange :: VersionIntervals -> C.VersionRange
toVersionRange = C.fromVersionIntervals . toCabal

fromVersionRange :: C.VersionRange -> VersionIntervals
fromVersionRange = fromCabal . C.toVersionIntervals

withinIntervals :: Version -> VersionIntervals -> Bool
withinIntervals v vi = C.withinIntervals (toCabalVersion v) (toCabal vi)

unionVersionIntervals :: VersionIntervals -> VersionIntervals -> VersionIntervals
unionVersionIntervals x y = fromCabal $ C.unionVersionIntervals
    (toCabal x)
    (toCabal y)

intersectVersionIntervals :: VersionIntervals -> VersionIntervals -> VersionIntervals
intersectVersionIntervals x y = fromCabal $ C.intersectVersionIntervals
    (toCabal x)
    (toCabal y)

toCabal :: VersionIntervals -> C.VersionIntervals
toCabal (VersionIntervals vi) =
  C.mkVersionIntervals $ map go vi
  where
    go (VersionInterval lowerV lowerB mupper) =
        ( C.LowerBound (toCabalVersion lowerV) (toCabalBound lowerB)
        , case mupper of
            Nothing -> C.NoUpperBound
            Just (v, b) -> C.UpperBound (toCabalVersion v) (toCabalBound b)
        )

fromCabal :: C.VersionIntervals -> VersionIntervals
fromCabal =
    VersionIntervals . map go . C.versionIntervals
  where
    go (C.LowerBound lowerV lowerB, upper) = VersionInterval
      { viLowerVersion = fromCabalVersion lowerV
      , viLowerBound = fromCabalBound lowerB
      , viUpper =
          case upper of
            C.NoUpperBound -> Nothing
            C.UpperBound v b -> Just (fromCabalVersion v, fromCabalBound b)
      }

toCabalBound :: Bound -> C.Bound
toCabalBound ExclusiveBound = C.ExclusiveBound
toCabalBound InclusiveBound = C.InclusiveBound

fromCabalBound :: C.Bound -> Bound
fromCabalBound C.ExclusiveBound = ExclusiveBound
fromCabalBound C.InclusiveBound = InclusiveBound
