-- | Stack allows ignore-bounds to be applied to the Cabal files of specified
-- packages.

import           StackTest

main :: IO ()
main = do
  -- ignore-bounds and ignore-bounds-in
  stack ["-w", "stackA.yaml", "build"]
  -- ignore-bounds has precedence over allow-newer
  stack ["-w", "stackB.yaml", "build"]
  -- allow-newer and ignore-bounds-in
  stack ["-w", "stackC.yaml", "build"]
  -- ignore-bounds and allow-newer-deps
  stack ["-w", "stackD.yaml", "build"]
  -- ignore-bounds-in and allow-newer-deps combine
  stack ["-w", "stackE.yaml", "build"]
  -- ignore-bounds-in and allow-newer-deps combine
  stack ["-w", "stackF.yaml", "build"]
  -- Command line has precedence
  stack ["-w", "stackG.yaml", "build", "--ignore-bounds"]
  -- Command line has precedence
  stack ["-w", "stackG.yaml", "build", "--allow-newer"]
  -- ignore-newer has precedence on the command line
  stack ["-w", "stackG.yaml", "build", "--ignore-bounds", "--no-allow-newer"]
