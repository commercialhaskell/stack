{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import Data.Char
import Data.Maybe
import System.OsString
import System.OsString.Internal.Types
import qualified System.OsString.Posix as Posix
import qualified System.OsString.Windows as Windows
import Data.ByteString ( ByteString )
import qualified Data.ByteString as ByteString
import Test.Tasty.QuickCheck


instance Arbitrary OsString where
  arbitrary = fmap fromJust $ encodeUtf <$> listOf filepathChar

instance Arbitrary PosixString where
  arbitrary = fmap fromJust $ Posix.encodeUtf <$> listOf filepathChar

instance Arbitrary WindowsString where
  arbitrary = fmap fromJust $ Windows.encodeUtf <$> listOf filepathChar


newtype NonNullString = NonNullString { nonNullString :: String }
  deriving Show

instance Arbitrary NonNullString where
  arbitrary = NonNullString <$> listOf filepathChar

filepathChar :: Gen Char
filepathChar = arbitraryUnicodeChar `suchThat` (\c -> not (isNull c) && isValidUnicode c)
 where
  isNull = (== '\NUL')
  isValidUnicode c = case generalCategory c of
      Surrogate -> False
      NotAssigned -> False
      _ -> True


newtype NonNullAsciiString = NonNullAsciiString { nonNullAsciiString :: String }
  deriving Show

instance Arbitrary NonNullAsciiString where
  arbitrary = NonNullAsciiString <$> listOf filepathAsciiChar

filepathAsciiChar :: Gen Char
filepathAsciiChar = arbitraryASCIIChar `suchThat` (\c -> not (isNull c))
 where
  isNull = (== '\NUL')

newtype NonNullSurrogateString = NonNullSurrogateString { nonNullSurrogateString :: String }
  deriving Show

instance Arbitrary NonNullSurrogateString where
  arbitrary = NonNullSurrogateString <$> listOf filepathWithSurrogates

filepathWithSurrogates :: Gen Char
filepathWithSurrogates =
  frequency
    [(3, arbitraryASCIIChar),
     (1, arbitraryUnicodeChar),
     (1, arbitraryBoundedEnum)
    ]


instance Arbitrary ByteString where arbitrary = ByteString.pack <$> arbitrary
instance CoArbitrary ByteString where coarbitrary = coarbitrary . ByteString.unpack
