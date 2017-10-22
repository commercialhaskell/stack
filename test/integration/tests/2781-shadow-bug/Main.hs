import StackTest
import System.Directory

main :: IO ()
main = do
  createDirectoryIfMissing True "foo/src"
  readFile "foo/v1/Foo.hs" >>= writeFile "foo/src/Foo.hs"
  stack ["bench"]
  readFile "foo/v2/Foo.hs" >>= writeFile "foo/src/Foo.hs"
  stack ["bench"]
