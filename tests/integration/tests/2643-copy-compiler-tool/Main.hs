import StackTest
import System.Directory
import Control.Monad (unless)

main :: IO ()
main = do
  -- init
  removeFileIgnore "stack.yaml"
  removeDirIgnore ".stack-work"
  stack ["init", defaultSnapshotArg]

  -- place to throw some exes
  removeDirIgnore "binny"
  createDirectory "binny"

  -- check assumptions on exec and the build flags and clean
  stack ["build", "--flag", "copy-compiler-tool-test:build-baz"]
  stack ["exec", "--", "baz-exe" ++ exeExt]
  stackErr ["exec", "--", "bar-exe" ++ exeExt]
  stackCleanFull
  -- See #4936. The Windows condition is because `stackCleanFull` may have
  -- failed.
  unless isWindows $ stackErr ["exec", "--", "baz-exe" ++ exeExt]

  -- install one exe normally
  stack ["install",
         "--local-bin-path", "./binny",
         "--flag", "*:build-foo"
        ]

  -- and install two compiler-tools, opposite ways
  -- (build or install)
  stack ["build",
         "--local-bin-path", "./binny",
         "--copy-compiler-tool",
         "--flag", "*:build-bar"
        ]
  stack ["install",
         "--local-bin-path", "./binny",
         "--copy-compiler-tool",
         "--flag", "*:build-baz"
        ]

  -- nuke the built things that go in .stack-work/, so we can test if
  -- the installed ones exist for sure
  stackCleanFull

  -- bar and baz were installed as compiler tools, should work fine
  stack ["exec", "--", "bar-exe" ++ exeExt]
  stack ["exec", "--", "baz-exe" ++ exeExt]

  -- foo was installed as a normal exe (in .binny/, which can't be on PATH),
  -- so shouldn't
  -- See #4936. The Windows condition is because `stackCleanFull` may have
  -- failed.
  unless isWindows $ stackErr ["exec", "--", "foo-exe" ++ exeExt]

  -- check existences make sense
  doesExist $ "./binny/foo-exe" ++ exeExt
  doesNotExist $ "./binny/bar-exe" ++ exeExt
  doesNotExist $ "./binny/baz-exe" ++ exeExt

  -- just check that this exists
  stack ["path", "--compiler-tools-bin"]
