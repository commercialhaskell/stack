-- A separate module so that we can contain all usage of deprecated identifiers here
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Distribution.Version.Extra
  ( hasUpper
  , hasLower
  ) where

import Distribution.Version (VersionRange (..))

-- | Does the version range have an upper bound?
hasUpper :: VersionRange -> Bool
hasUpper AnyVersion = False
hasUpper (ThisVersion _) = True
hasUpper (LaterVersion _) = False
hasUpper (EarlierVersion _) = True
hasUpper (WildcardVersion _) = True
hasUpper (UnionVersionRanges x y) = hasUpper x && hasUpper y
hasUpper (IntersectVersionRanges x y) = hasUpper x || hasUpper y
hasUpper (VersionRangeParens x) = hasUpper x

-- | Does the version range have a lower bound?
hasLower :: VersionRange -> Bool
hasLower AnyVersion = False
hasLower (ThisVersion _) = True
hasLower (LaterVersion _) = True
hasLower (EarlierVersion _) = False
hasLower (WildcardVersion _) = True
hasLower (UnionVersionRanges x y) = hasLower x && hasLower y
hasLower (IntersectVersionRanges x y) = hasLower x || hasLower y
hasLower (VersionRangeParens x) = hasLower x
