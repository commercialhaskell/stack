import Control.Monad (when)
import StackTest

main :: IO ()
main = do
  stackCheckStdout ["ls", "dependencies", "--tree"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "└─┬ files mkVersion [0,1,0,0]"
                           , "  ├─┬ base mkVersion [4,10,1,0]"
                           , "  │ ├─┬ ghc-prim mkVersion [0,5,1,1]"
                           , "  │ │ └── rts mkVersion [1,0]"
                           , "  │ ├─┬ integer-gmp mkVersion [1,0,1,0]"
                           , "  │ │ └─┬ ghc-prim mkVersion [0,5,1,1]"
                           , "  │ │   └── rts mkVersion [1,0]"
                           , "  │ └── rts mkVersion [1,0]"
                           , "  └─┬ mtl mkVersion [2,2,2]"
                           , "    ├─┬ base mkVersion [4,10,1,0]"
                           , "    │ ├─┬ ghc-prim mkVersion [0,5,1,1]"
                           , "    │ │ └── rts mkVersion [1,0]"
                           , "    │ ├─┬ integer-gmp mkVersion [1,0,1,0]"
                           , "    │ │ └─┬ ghc-prim mkVersion [0,5,1,1]"
                           , "    │ │   └── rts mkVersion [1,0]"
                           , "    │ └── rts mkVersion [1,0]"
                           , "    └── transformers mkVersion [0,5,2,0]"
                           ]
    when (stdOut /= expected) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]

  stackCheckStdout ["ls", "dependencies", "--tree", "--depth=1"] $ \stdOut -> do
    let expected = unlines [ "Packages"
                           , "└─┬ files mkVersion [0,1,0,0]"
                           , "  ├── base mkVersion [4,10,1,0]"
                           , "  └── mtl mkVersion [2,2,2]"
                           ]
    when (stdOut /= expected) $
      error $ unlines [ "Expected:", expected, "Actual:", stdOut ]
