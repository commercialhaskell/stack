-- Stack can install targets that are executables as compiler tools.

-- See: https://github.com/commercialhaskell/stack/issues/2643

import           Control.Monad ( unless )
import           StackTest
import           System.Directory ()

main :: IO ()
main = do
  -- Install myExeA executable normally:
  stack ["install",
         "--local-bin-path", "./bin",
         "--flag", "myPackage:build-myExeA"
        ]

  -- Install myExeB and myExeC executables as compiler tools, in alternative
  -- ways (build or install):
  stack ["build",
         "--local-bin-path", "./bin",
         "--copy-compiler-tool",
         "--flag", "myPackage:build-myExeB"
        ]
  stack ["install",
         "--local-bin-path", "./bin",
         "--copy-compiler-tool",
         "--flag", "myPackage:build-myExeC"
        ]

  -- Remove .stack-work/, so we can test if the installed ones exist:
  stackCleanFull

  -- myExeB and myExeC were installed as compiler tools and should work:
  stack ["exec", "--", "myExeB" <> exeExt]
  stack ["exec", "--", "myExeC" <> exeExt]

  -- myExeA was installed in .bin/, which is not on the PATH, and should not
  -- work.

  -- The Windows condition is because `stackCleanFull` may have failed (see
  -- issue #4936):
  unless isWindows $ stackErr ["exec", "--", "myExeA" <> exeExt]

  -- Check existences make sense:
  doesExist $ "./bin/myExeA" <> exeExt
  doesNotExist $ "./bin/myExeB" <> exeExt
  doesNotExist $ "./bin/myExeC" <> exeExt

  -- Check that this exists
  stack ["path", "--compiler-tools-bin"]
