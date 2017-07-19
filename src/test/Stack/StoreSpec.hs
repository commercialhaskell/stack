{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wwarn #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.StoreSpec where

import qualified Data.ByteString as BS
import           Data.Containers (mapFromList, setFromList)
import           Data.Sequences (fromList)
import           Data.Store.Internal (StaticSize (..))
import           Data.Store.TH
import qualified Data.Vector.Unboxed as UV
import           GHC.TypeLits (KnownNat)
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany
import           Stack.Prelude
import           Stack.Types.Build
import           Stack.Types.PackageDump
import           Test.Hspec
import           Test.SmallCheck.Series

-- NOTE: these were copied from Data.Store. Should probably be moved to
-- smallcheck.

instance (Monad m, Serial m k, Serial m a, Ord k) => Serial m (Map k a) where
    series = fmap mapFromList series

instance (Monad m, Serial m k, Serial m a, Eq k, Hashable k) => Serial m (HashMap k a) where
    series = fmap mapFromList series

instance Monad m => Serial m Text where
    series = fmap fromList series

instance (Monad m, Serial m a, UV.Unbox a) => Serial m (UV.Vector a) where
    series = fmap fromList series

instance Monad m => Serial m BS.ByteString where
    series = fmap BS.pack series

instance (Monad m, Serial m a, Ord a) => Serial m (Set a) where
    series = fmap setFromList series

instance (Monad m, KnownNat n) => Serial m (StaticSize n BS.ByteString)

addMinAndMaxBounds :: forall a. (Bounded a, Eq a) => [a] -> [a]
addMinAndMaxBounds xs =
    (if (minBound :: a) `notElem` xs then [minBound] else []) ++
    (if (maxBound :: a) `notElem` xs && (maxBound :: a) /= minBound then maxBound : xs else xs)

$(do let ns = [ ''Int64, ''Word64, ''Word, ''Word8
              ]
         f n = [d| instance Monad m => Serial m $(conT n) where
                      series = generate (\_ -> addMinAndMaxBounds [0, 1]) |]
     concat <$> mapM f ns)

$(do let tys = [ ''InstalledCacheInner
               -- FIXME , ''PackageCache
               -- FIXME , ''LoadedSnapshot
               , ''BuildCache
               , ''ConfigCache
               ]
     ns <- reifyManyWithoutInstances ''Serial tys (`notElem` [''UV.Vector])
     let f n = [d| instance Monad m => Serial m $(conT n) |]
     concat <$> mapM f ns)

verbose :: Bool
verbose = False

spec :: Spec
spec = do
    describe "Roundtrips binary formats" $ do
        $(smallcheckManyStore False 6
            [ [t| InstalledCacheInner |]
            , [t| BuildCache |]
            ])
        -- Blows up with > 5
        {-
        $(smallcheckManyStore False 5
            [ -- FIXME [t| PackageCache |]
            -- FIXME , [t| LoadedSnapshot |]
            ])
        -}
        -- Blows up with > 4
        $(smallcheckManyStore False 4
            [ [t| ConfigCache |]
            ])
