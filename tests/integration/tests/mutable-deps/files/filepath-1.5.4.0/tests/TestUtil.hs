{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestUtil(
    module TestUtil,
    module Test.Tasty.QuickCheck,
    module Data.List,
    module Data.Maybe
    ) where

import Test.Tasty.QuickCheck hiding ((==>))
import Data.ByteString.Short (ShortByteString)
import Data.List
import Data.Maybe
import Control.Monad
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P
#ifdef GHC_MAKE
import qualified System.OsPath.Windows.Internal as AFP_W
import qualified System.OsPath.Posix.Internal as AFP_P
#else
import qualified System.OsPath.Windows as AFP_W
import qualified System.OsPath.Posix as AFP_P
import System.OsPath.Types
#endif
import System.OsString.Internal.Types
import System.OsString.Encoding.Internal
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import GHC.IO.Encoding.Failure


infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b


newtype QFilePathValidW = QFilePathValidW FilePath deriving Show

instance Arbitrary QFilePathValidW where
    arbitrary = fmap (QFilePathValidW . W.makeValid) arbitraryFilePath
    shrink (QFilePathValidW x) = shrinkValid QFilePathValidW W.makeValid x

newtype QFilePathValidP = QFilePathValidP FilePath deriving Show

instance Arbitrary QFilePathValidP where
    arbitrary = fmap (QFilePathValidP . P.makeValid) arbitraryFilePath
    shrink (QFilePathValidP x) = shrinkValid QFilePathValidP P.makeValid x

newtype QFilePath = QFilePath FilePath deriving Show

instance Arbitrary QFilePath where
    arbitrary = fmap QFilePath arbitraryFilePath
    shrink (QFilePath x) = shrinkValid QFilePath id x


-- | Generate an arbitrary FilePath use a few special (interesting) characters.
arbitraryFilePath :: Gen FilePath
arbitraryFilePath = sized $ \n -> do
    k <- choose (0,n)
    replicateM k $ elements "?./:\\a ;_"

-- | Shrink, but also apply a validity function. Try and make shorter, or use more
--   @a@ (since @a@ is pretty dull), but make sure you terminate even after valid.
shrinkValid :: (FilePath -> a) -> (FilePath -> FilePath) -> FilePath -> [a]
shrinkValid wrap valid o =
    [ wrap y
    | y <- map valid $ shrinkList (\x -> ['a' | x /= 'a']) o
    , length y < length o || (length y == length o && countA y > countA o)]
    where countA = length . filter (== 'a')

encodeUtf16LE :: String -> ShortByteString
encodeUtf16LE = either (error . show) id . encodeWithTE (mkUTF16le TransliterateCodingFailure)

encodeUtf8 :: String -> ShortByteString
encodeUtf8 = either (error . show) id . encodeWithTE (mkUTF8 TransliterateCodingFailure)

decodeUtf16LE :: ShortByteString -> String
decodeUtf16LE = either (error . show) id . decodeWithTE (mkUTF16le TransliterateCodingFailure)

decodeUtf8 :: ShortByteString -> String
decodeUtf8 = either (error . show) id . decodeWithTE (mkUTF8 TransliterateCodingFailure)

#ifdef GHC_MAKE
newtype QFilePathValidAFP_W = QFilePathValidAFP_W ShortByteString deriving Show

instance Arbitrary QFilePathValidAFP_W where
    arbitrary = fmap (QFilePathValidAFP_W . AFP_W.makeValid . encodeUtf16LE) arbitraryFilePath
    shrink (QFilePathValidAFP_W x) = shrinkValid (QFilePathValidAFP_W . encodeUtf16LE) (decodeUtf16LE . AFP_W.makeValid . encodeUtf16LE) (decodeUtf16LE x)

newtype QFilePathValidAFP_P = QFilePathValidAFP_P ShortByteString deriving Show

instance Arbitrary QFilePathValidAFP_P where
    arbitrary = fmap (QFilePathValidAFP_P . AFP_P.makeValid . encodeUtf8) arbitraryFilePath
    shrink (QFilePathValidAFP_P x) = shrinkValid (QFilePathValidAFP_P . encodeUtf8) (decodeUtf8 . AFP_P.makeValid . encodeUtf8) (decodeUtf8 x)

newtype QFilePathAFP_W = QFilePathAFP_W ShortByteString deriving Show
newtype QFilePathAFP_P = QFilePathAFP_P ShortByteString deriving Show

instance Arbitrary QFilePathAFP_W where
    arbitrary = fmap (QFilePathAFP_W . encodeUtf16LE) arbitraryFilePath
    shrink (QFilePathAFP_W x) = shrinkValid (QFilePathAFP_W . encodeUtf16LE) id (decodeUtf16LE x)

instance Arbitrary QFilePathAFP_P where
    arbitrary = fmap (QFilePathAFP_P . encodeUtf8) arbitraryFilePath
    shrink (QFilePathAFP_P x) = shrinkValid (QFilePathAFP_P . encodeUtf8) id (decodeUtf8 x)

newtype QFilePathsAFP_W = QFilePathsAFP_W [ShortByteString] deriving Show
newtype QFilePathsAFP_P = QFilePathsAFP_P [ShortByteString] deriving Show

instance Arbitrary QFilePathsAFP_W where
    arbitrary = fmap (QFilePathsAFP_W . fmap encodeUtf16LE) (listOf arbitraryFilePath)

instance Arbitrary QFilePathsAFP_P where
    arbitrary = fmap (QFilePathsAFP_P . fmap encodeUtf8) (listOf arbitraryFilePath)

#else


newtype QFilePathValidAFP_W = QFilePathValidAFP_W WindowsPath deriving Show

instance Arbitrary QFilePathValidAFP_W where
    arbitrary = fmap (QFilePathValidAFP_W . AFP_W.makeValid . WS . encodeUtf16LE) arbitraryFilePath
    shrink (QFilePathValidAFP_W x) = shrinkValid (QFilePathValidAFP_W . WS . encodeUtf16LE) (decodeUtf16LE . getWindowsString . AFP_W.makeValid . WS . encodeUtf16LE) (decodeUtf16LE . getWindowsString $ x)

newtype QFilePathValidAFP_P = QFilePathValidAFP_P PosixPath deriving Show

instance Arbitrary QFilePathValidAFP_P where
    arbitrary = fmap (QFilePathValidAFP_P . AFP_P.makeValid . PS . encodeUtf8) arbitraryFilePath
    shrink (QFilePathValidAFP_P x) = shrinkValid (QFilePathValidAFP_P . PS . encodeUtf8) (decodeUtf8 . getPosixString . AFP_P.makeValid . PS . encodeUtf8) (decodeUtf8 . getPosixString $ x)

newtype QFilePathAFP_W = QFilePathAFP_W WindowsPath deriving Show
newtype QFilePathAFP_P = QFilePathAFP_P PosixPath deriving Show

instance Arbitrary QFilePathAFP_W where
    arbitrary = fmap (QFilePathAFP_W . WS . encodeUtf16LE) arbitraryFilePath
    shrink (QFilePathAFP_W x) = shrinkValid (QFilePathAFP_W . WS . encodeUtf16LE) id (decodeUtf16LE . getWindowsString $ x)

instance Arbitrary QFilePathAFP_P where
    arbitrary = fmap (QFilePathAFP_P . PS . encodeUtf8) arbitraryFilePath
    shrink (QFilePathAFP_P x) = shrinkValid (QFilePathAFP_P . PS . encodeUtf8) id (decodeUtf8 . getPosixString $ x)

newtype QFilePathsAFP_W = QFilePathsAFP_W [WindowsPath] deriving Show
newtype QFilePathsAFP_P = QFilePathsAFP_P [PosixPath] deriving Show

instance Arbitrary QFilePathsAFP_W where
    arbitrary = fmap (QFilePathsAFP_W . fmap (WS . encodeUtf16LE)) (listOf arbitraryFilePath)

instance Arbitrary QFilePathsAFP_P where
    arbitrary = fmap (QFilePathsAFP_P . fmap (PS . encodeUtf8)) (listOf arbitraryFilePath)

instance Arbitrary WindowsChar where
  arbitrary = WW <$> arbitrary

instance Arbitrary PosixChar where
  arbitrary = PW <$> arbitrary
#endif

