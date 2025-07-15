{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module OsPathSpec where

import Data.Maybe

import System.OsPath as OSP
import System.OsString.Internal.Types
import System.OsPath.Posix as Posix
import System.OsPath.Windows as Windows
import System.OsPath.Encoding
import qualified System.OsString.Internal.Types as OS
import System.OsString.Data.ByteString.Short ( toShort )
import System.OsString.Posix as PosixS hiding (map)
import System.OsString.Windows as WindowsS hiding (map)

import Control.Exception
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Test.QuickCheck.Classes.Base as QC
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding ( setFileSystemEncoding )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.DeepSeq
import Data.Bifunctor ( first )
import qualified Data.ByteString.Char8 as C
import qualified System.OsString.Data.ByteString.Short.Word16 as BS16
import qualified System.OsString.Data.ByteString.Short as SBS
import Data.Char ( ord )
import Data.Proxy ( Proxy(..) )
import Test.Tasty
import Test.Tasty.QuickCheck

import Arbitrary


fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b


tests :: TestTree
tests = testGroup "Abstract filepath" [
    testGroup "filepaths"
    [ testProperties "OSP"
      [ ("pack . unpack == id",
        property $ \ws@(OsString _) ->
          OSP.pack (OSP.unpack ws) === ws
        ),
        ("encodeUtf . decodeUtf == id",
          property $ \(NonNullString str) -> (OSP.decodeUtf . fromJust . OSP.encodeUtf) str == Just str)
      ],
      testProperties "Windows"
        [ ("pack . unpack == id (Windows)",
          property $ \ws@(WindowsString _) ->
          Windows.pack (Windows.unpack ws) === ws
          )
        , ("decodeUtf . encodeUtf == id",
          property $ \(NonNullString str) -> (Windows.decodeUtf . fromJust . Windows.encodeUtf) str == Just str)
        , ("encodeWith ucs2le . decodeWith ucs2le == id",
          property $ \(padEven -> bs) -> (Windows.encodeWith ucs2le . (\(Right r) -> r) . Windows.decodeWith ucs2le . OS.WS . toShort) bs
                 === Right (OS.WS . toShort $ bs))
        , ("decodeFS . encodeFS == id (Windows)",
          property $ \(NonNullString str) -> ioProperty $ do
            r1 <- Windows.encodeFS str
            r2 <- try @SomeException $ Windows.decodeFS r1
            r3 <- evaluate $ force $ first displayException r2
            pure (r3 === Right str)
            )
        , ("fromPlatformString* functions are equivalent under ASCII",
          property $ \(WindowsString . BS16.pack . map (fromIntegral . ord) . nonNullAsciiString -> str) -> ioProperty $ do
            r1         <- Windows.decodeFS str
            r2         <- Windows.decodeUtf str
            (Right r3) <- pure $ Windows.decodeWith (mkUTF16le TransliterateCodingFailure) str
            (Right r4) <- pure $ Windows.decodeWith (mkUTF16le RoundtripFailure) str
            (Right r5) <- pure $ Windows.decodeWith (mkUTF16le ErrorOnCodingFailure) str
            pure (    r1 === r2
                 .&&. r1 === r3
                 .&&. r1 === r4
                 .&&. r1 === r5
                 )
          )
        , ("toPlatformString* functions are equivalent under ASCII",
          property $ \(NonNullAsciiString str) -> ioProperty $ do
            r1         <- Windows.encodeFS str
            r2         <- Windows.encodeUtf str
            (Right r3) <- pure $ Windows.encodeWith (mkUTF16le TransliterateCodingFailure) str
            (Right r4) <- pure $ Windows.encodeWith (mkUTF16le RoundtripFailure) str
            (Right r5) <- pure $ Windows.encodeWith (mkUTF16le ErrorOnCodingFailure) str
            pure (    r1 === r2
                 .&&. r1 === r3
                 .&&. r1 === r4
                 .&&. r1 === r5
                 )
          )
        , ("Unit test toPlatformString*",
          property $ ioProperty $ do
            let str = "ABcK_(ツ123_&**"
            let expected = WindowsString $ BS16.pack [0x0041,0x0042,0x0063,0x004b,0x005f,0x0028,0x30c4,0x0031,0x0032,0x0033,0x005f,0x0026,0x002a,0x002a]
            r1         <-        Windows.encodeFS str
            r2         <-        Windows.encodeUtf str
            (Right r3) <- pure $ Windows.encodeWith (mkUTF16le TransliterateCodingFailure) str
            (Right r4) <- pure $ Windows.encodeWith (mkUTF16le RoundtripFailure) str
            (Right r5) <- pure $ Windows.encodeWith (mkUTF16le ErrorOnCodingFailure) str
            pure (    r1 === expected
                 .&&. r2 === expected
                 .&&. r3 === expected
                 .&&. r4 === expected
                 .&&. r5 === expected
                 )
          )
        , ("Unit test fromPlatformString*",
          property $ ioProperty $ do
            let bs = WindowsString $ BS16.pack [0x0041,0x0042,0x0063,0x004b,0x005f,0x0028,0x30c4,0x0031,0x0032,0x0033,0x005f,0x0026,0x002a,0x002a]
            let expected = "ABcK_(ツ123_&**"
            r1         <-        Windows.decodeFS bs
            r2         <-        Windows.decodeUtf bs
            (Right r3) <- pure $ Windows.decodeWith (mkUTF16le TransliterateCodingFailure) bs
            (Right r4) <- pure $ Windows.decodeWith (mkUTF16le RoundtripFailure) bs
            (Right r5) <- pure $ Windows.decodeWith (mkUTF16le ErrorOnCodingFailure) bs
            pure (    r1 === expected
                 .&&. r2 === expected
                 .&&. r3 === expected
                 .&&. r4 === expected
                 .&&. r5 === expected
                 )
          )
        ]
    , testProperties "Posix"
      [ ("decodeUtf . encodeUtf == id",
          property $ \(NonNullString str) -> (Posix.decodeUtf . fromJust . Posix.encodeUtf) str == Just str)
      , ("encodeWith ucs2le . decodeWith ucs2le == id (Posix)",
          property $ \(padEven -> bs) -> (Posix.encodeWith ucs2le . (\(Right r) -> r) . Posix.decodeWith ucs2le . OS.PS . toShort) bs === Right (OS.PS . toShort $ bs))
      , ("decodeFS . encodeFS == id",
          property $ \(NonNullString str) -> ioProperty $ do
            setFileSystemEncoding (mkUTF8 TransliterateCodingFailure)
            r1 <- Posix.encodeFS str
            r2 <- try @SomeException $ Posix.decodeFS r1
            r3 <- evaluate $ force $ first displayException r2
            pure (r3 === Right str)
            )
      , ("fromPlatformString* functions are equivalent under ASCII",
          property $ \(PosixString . SBS.toShort . C.pack . nonNullAsciiString -> str) -> ioProperty $ do
            r1         <-        Posix.decodeFS str
            r2         <-        Posix.decodeUtf str
            (Right r3) <- pure $ Posix.decodeWith (mkUTF8 TransliterateCodingFailure) str
            (Right r4) <- pure $ Posix.decodeWith (mkUTF8 RoundtripFailure) str
            (Right r5) <- pure $ Posix.decodeWith (mkUTF8 ErrorOnCodingFailure) str
            pure (    r1 === r2
                 .&&. r1 === r3
                 .&&. r1 === r4
                 .&&. r1 === r5
                 )
          )
      , ("toPlatformString* functions are equivalent under ASCII",
          property $ \(NonNullAsciiString str) -> ioProperty $ do
            r1         <-        Posix.encodeFS str
            r2         <-        Posix.encodeUtf str
            (Right r3) <- pure $ Posix.encodeWith (mkUTF8 TransliterateCodingFailure) str
            (Right r4) <- pure $ Posix.encodeWith (mkUTF8 RoundtripFailure) str
            (Right r5) <- pure $ Posix.encodeWith (mkUTF8 ErrorOnCodingFailure) str
            pure (    r1 === r2
                 .&&. r1 === r3
                 .&&. r1 === r4
                 .&&. r1 === r5
                 )
          )
      , ("Unit test toPlatformString*",
          property $ ioProperty $ do
            let str = "ABcK_(ツ123_&**"
            let expected = PosixString $ SBS.pack [0x41,0x42,0x63,0x4b,0x5f,0x28,0xe3,0x83,0x84,0x31,0x32,0x33,0x5f,0x26,0x2a,0x2a]
            r1         <-        Posix.encodeFS str
            r2         <-        Posix.encodeUtf str
            (Right r3) <- pure $ Posix.encodeWith (mkUTF8 TransliterateCodingFailure) str
            (Right r4) <- pure $ Posix.encodeWith (mkUTF8 RoundtripFailure) str
            (Right r5) <- pure $ Posix.encodeWith (mkUTF8 ErrorOnCodingFailure) str
            pure (    r1 === expected
                 .&&. r2 === expected
                 .&&. r3 === expected
                 .&&. r4 === expected
                 .&&. r5 === expected
                 )
          )
      , ("Unit test fromPlatformString*",
          property $ ioProperty $ do
            let bs = PosixString $ SBS.pack [0x41,0x42,0x63,0x4b,0x5f,0x28,0xe3,0x83,0x84,0x31,0x32,0x33,0x5f,0x26,0x2a,0x2a]
            let expected = "ABcK_(ツ123_&**"
            r1         <-        Posix.decodeFS bs
            r2         <-        Posix.decodeUtf bs
            (Right r3) <- pure $ Posix.decodeWith (mkUTF8 TransliterateCodingFailure) bs
            (Right r4) <- pure $ Posix.decodeWith (mkUTF8 RoundtripFailure) bs
            (Right r5) <- pure $ Posix.decodeWith (mkUTF8 ErrorOnCodingFailure) bs
            pure (    r1 === expected
                 .&&. r2 === expected
                 .&&. r3 === expected
                 .&&. r4 === expected
                 .&&. r5 === expected
                 )
          )
      , ("pack . unpack == id (Posix)",
          property $ \ws@(PosixString _) ->
            Posix.pack (Posix.unpack ws) === ws
          )
      ]
    ],
  testGroup "QuasiQuoter"
    [ testProperties "windows"
      [ ("QuasiQuoter (WindowsPath)",
        property $ do
          let bs = WindowsString $ BS16.pack [0x0041,0x0042,0x0063,0x004b,0x005f]
          let expected = [Windows.pstr|ABcK_|]
          bs === expected
        )
      , ("QuasiQuoter (WindowsString)",
        property $ do
          let bs = WindowsString $ BS16.pack [0x0041,0x0042,0x0063,0x004b,0x005f,0x0028,0x30c4,0x0031,0x0032,0x0033,0x005f,0x0026,0x002a,0x002a]
          let expected = [WindowsS.pstr|ABcK_(ツ123_&**|]
          bs === expected
        )
       ],
       testProperties "posix"
       [ ("QuasiQuoter (PosixPath)",
         property $ do
           let bs = PosixString $ SBS.pack [0x41,0x42,0x63,0x4b,0x5f]
           let expected = [Posix.pstr|ABcK_|]
           bs === expected
         )
       , ("QuasiQuoter (PosixString)",
          property $ do
            let bs = PosixString $ SBS.pack [0x41,0x42,0x63,0x4b,0x5f,0x28,0xe3,0x83,0x84,0x31,0x32,0x33,0x5f,0x26,0x2a,0x2a]
            let expected = [PosixS.pstr|ABcK_(ツ123_&**|]
            bs === expected
          )
       ]
    ],
   testProperties "Type laws"
     (QC.lawsProperties (QC.ordLaws (Proxy @OsPath))
      ++ QC.lawsProperties (QC.monoidLaws (Proxy @OsPath))

      ++ QC.lawsProperties (QC.ordLaws (Proxy @OsString))
      ++ QC.lawsProperties (QC.monoidLaws (Proxy @OsString))

      ++ QC.lawsProperties (QC.ordLaws (Proxy @WindowsString))
      ++ QC.lawsProperties (QC.monoidLaws (Proxy @WindowsString))

      ++ QC.lawsProperties (QC.ordLaws (Proxy @PosixString))
      ++ QC.lawsProperties (QC.monoidLaws (Proxy @PosixString))

      ++ QC.lawsProperties (QC.ordLaws (Proxy @PlatformString))
      ++ QC.lawsProperties (QC.monoidLaws (Proxy @PlatformString)))
  ]


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]
