-- Stack rebuilds a benchmark when an indirect dependency changes.
--
-- See: https://github.com/commercialhaskell/stack/issues/2781

import StackTest
import System.Directory ( createDirectoryIfMissing )

main :: IO ()
main = do
  copy "myPackageB/v1/MyPackageB-v1.hs" "myPackageB/src/MyPackageB.hs"
  stack ["bench"]
  copy "myPackageB/v2/MyPackageB-v2.hs" "myPackageB/src/MyPackageB.hs"
  stack ["bench"]
