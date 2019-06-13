
module TestUtil(
    (==>), QFilePath(..), QFilePathValidW(..), QFilePathValidP(..),
    module Test.QuickCheck,
    module Data.List,
    module Data.Maybe
    ) where

import Test.QuickCheck hiding ((==>))
import Data.List
import Data.Maybe
import Control.Monad
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P

infixr 0 ==>
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
