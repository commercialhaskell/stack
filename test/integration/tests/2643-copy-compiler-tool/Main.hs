import StackTest

main :: IO ()
main = do
  -- init
  stack ["init", defaultResolverArg]

  -- check assumptions on exec and the build flags and clean
  stack ["build", "--flag", "*:build-baz"]
  stack ["exec", "--", "baz-exe" ++ exeExt]
  stackErr ["exec", "--", "bar-exe" ++ exeExt]
  stack ["clean", "--full"]
  stackErr ["exec", "--", "baz-exe" ++ exeExt]

  -- install one exe normally and two compiler-tools, opposite ways
  -- (build or install)
  stack ["install", "--flag", "*:build-foo"]
  stack ["build", "--copy-compiler-tool", "--flag", "*:build-bar"]
  stack ["install", "--copy-compiler-tool", "--flag", "*:build-baz"]

  -- nuke the built things that go in .stack-work/, so we can test if
  -- the installed ones exist for sure
  stack ["clean", "--full"]

  -- bar and baz were installed as compiler tools, should work fine
  stack ["exec", "--", "bar-exe" ++ exeExt]
  stack ["exec", "--", "baz-exe" ++ exeExt]

  -- foo was installed as a normal exe (in .local/bin/), so shouldn't
  -- TODO: Check this in a more reliable fashion
  stackErr ["exec", "--", "foo-exe" ++ exeExt]

  -- TODO: check paths against `stack path`
