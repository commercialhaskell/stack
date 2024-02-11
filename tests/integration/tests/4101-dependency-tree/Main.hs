import Control.Monad (when, unless)
import StackTest
import System.Directory (getCurrentDirectory)
import Data.List (isPrefixOf)

main :: IO ()
main = unless isWindows $ do
  stackCheckStdout ["ls", "dependencies", "tree"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "├─┬ files 0.1.0.0"
                           , "│ ├─┬ base 4.18.2.0"
                           ]
    unless (expected `isPrefixOf` stdOut) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "tree", "--depth=1"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "├─┬ files 0.1.0.0"
                           , "│ ├── base 4.18.2.0"
                           , "│ ├── filelock 0.1.1.2"
                           , "│ ├── mtl 2.3.1"
                           , "│ └── subproject 0.1.0.0"
                           , "└─┬ subproject 0.1.0.0"
                           , "  └── base 4.18.2.0"
                           ]
    when (stdOut /= expected) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "tree", "subproject"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "└─┬ subproject 0.1.0.0"
                           , "  └─┬ base 4.18.2.0"
                           , "    ├─┬ ghc-bignum 1.3"
                           , "    │ └─┬ ghc-prim 0.10.0"
                           , "    │   └── rts 1.0.2"
                           , "    ├─┬ ghc-prim 0.10.0"
                           , "    │ └── rts 1.0.2"
                           , "    └── rts 1.0.2"
                           ]
    when (stdOut /= expected) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "json"] $ \stdOut -> do
    currdir <- getCurrentDirectory
    let expected =
          "[{\"dependencies\":[\"base\",\"bytestring\",\"filepath\",\"time\"]"
    unless (expected `isPrefixOf` stdOut) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]
