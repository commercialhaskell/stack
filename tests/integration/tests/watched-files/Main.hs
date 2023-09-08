import StackTest
import Data.Foldable (for_)
import Control.Monad (unless)

main :: IO ()
main = for_ (words "foo bar baz bin") $ \x -> do
  writeFile "some-text-file.txt" x
  stackCheckStdout ["run"] $ \y ->
    unless (x == y) $ error $ concat
      [ "Expected: "
      , show x
      , "\nActual:  "
      , show y
      ]
