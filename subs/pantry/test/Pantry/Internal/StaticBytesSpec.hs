{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Pantry.Internal.StaticBytesSpec (spec) where

import RIO
import Pantry.Internal.StaticBytes
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

spec :: Spec
spec = do
  describe "ByteString" $ tests B.pack
  describe "Storable Vector" $ tests VS.fromList
  describe "Unboxed Vector" $ tests VU.fromList
  describe "Primitive Vector" $ tests VP.fromList

tests :: (Eq dbytes, Show dbytes, DynamicBytes dbytes) => ([Word8] -> dbytes) -> Spec
tests pack = do
  it "disallows 4 bytes" $ property $ \(w1,w2,w3,w4) ->
    toStaticExact (pack [w1,w2,w3,w4]) `shouldBe` (Left NotEnoughBytes :: Either StaticBytesException Bytes8)
  it "toStaticExact matches ByteString" $ property $ \(w1,w2,w3,w4) -> property $ \(w5,w6,w7,w8) -> do
    let octets =  [w1,w2,w3,w4,w5,w6,w7,w8]
        (expected :: Bytes8) = either impureThrow id $ toStaticExact (B.pack octets)
        actual = either impureThrow id $ toStaticExact (pack octets)
    actual `shouldBe` expected

  it "fromStatic round trips" $ property $ \(w1,w2,w3,w4) -> property $ \(w5,w6,w7,w8) -> do
    let octets = [w1,w2,w3,w4,w5,w6,w7,w8]
        v1 = pack octets
        (b8 :: Bytes8) = either impureThrow id $ toStaticExact v1
        v2 = fromStatic b8
    v2 `shouldBe` v1

  it "allows 8 bytes" $ property $ \(w1,w2,w3,w4) -> property $ \(w5,w6,w7,w8) -> do
    let bs = pack [w1,w2,w3,w4,w5,w6,w7,w8]
    case toStaticExact bs of
      Left e -> throwIO e
      Right b8 -> fromStatic (b8 :: Bytes8) `shouldBe` bs
    toStaticExact bs `shouldBe` (Left NotEnoughBytes :: Either StaticBytesException Bytes16)
  it "padding is the same as trailing nulls" $ property $ \(w1,w2,w3,w4) -> do
    let ws = [w1,w2,w3,w4]
        bs1 = pack $ ws ++ replicate 4 0
        bs2 = pack ws
    Right (toStaticPadTruncate bs2 :: Bytes8) `shouldBe` toStaticExact bs1

  prop "handles bytes16" $ \octets -> do
    let bs = pack $ take 16 octets
        (b16 :: Bytes16) = either impureThrow id $ toStaticPad bs
    fromStatic b16 `shouldBe` pack (take 16 (octets ++ replicate 16 0))

  it "spot check bytes16" $ forAll (replicateM 16 arbitrary) $ \ws -> do
    let bs = pack ws
        (b16 :: Bytes16) = either impureThrow id $ toStaticPad bs
    fromStatic b16 `shouldBe` pack ws

  prop "handles bytes32" $ \octets -> do
    let bs = pack $ take 32 octets
        (b32 :: Bytes32) = either impureThrow id $ toStaticPad bs
    fromStatic b32 `shouldBe` pack (take 32 (take 32 octets ++ replicate 32 0))

  prop "fuzz with encodeUtf8" $ \chars -> do
    let t = T.pack $ filter (/= '\0') chars
        bs = TE.encodeUtf8 t
        bs128 = pack $ B.unpack $ B.take 128 $ bs `B.append` B.replicate 128 0
        b128 = toStaticPadTruncate (pack $ B.unpack bs) :: Bytes128

    fromStatic b128 `shouldBe` bs128
