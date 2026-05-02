-- Stack can report the dependency tree of project packages.
--
-- See: https://github.com/commercialhaskell/stack/issues/4101

import           Control.Monad ( unless, when )
import           Data.List ( isPrefixOf )
import           StackTest
import           System.Directory ( getCurrentDirectory )

main :: IO ()
main = unless isWindows $ do
  stackCheckStdout ["ls", "dependencies", "tree"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "├─┬ myPackageA 0.0.0"
                           , "│ ├─┬ base 4.20.2.0"
                           ]
    unless (expected `isPrefixOf` stdOut) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "tree", "--depth=1"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "├─┬ myPackageA 0.0.0"
                           , "│ ├── base 4.20.2.0"
                           , "│ ├── filelock 0.1.1.2"
                           , "│ ├── mtl 2.3.1"
                           , "│ └── myPackageB 0.0.0"
                           , "└─┬ myPackageB 0.0.0"
                           , "  └── base 4.20.2.0"
                           ]
    when (stdOut /= expected) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "tree", "myPackageB"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "└─┬ myPackageB 0.0.0"
                           , "  └─┬ base 4.20.2.0"
                           , "    ├─┬ ghc-internal 9.1003.0"
                           , "    │ ├─┬ ghc-bignum 1.3"
                           , "    │ │ └─┬ ghc-prim 0.12.0"
                           , "    │ │   └── rts 1.0.2"
                           , "    │ ├─┬ ghc-prim 0.12.0"
                           , "    │ │ └── rts 1.0.2"
                           , "    │ └── rts 1.0.2"
                           , "    └─┬ ghc-prim 0.12.0"
                           , "      └── rts 1.0.2"
                           ]
    when (stdOut /= expected) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "json"] $ \stdOut -> do
    currdir <- getCurrentDirectory
    let expected =
          "[{\"dependencies\":[\"base\",\"bytestring\",\"filepath\",\"os-string\",\"time\"]"
    unless (expected `isPrefixOf` stdOut) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]
