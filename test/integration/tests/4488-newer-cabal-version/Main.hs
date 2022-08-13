import StackTest

main :: IO ()
main = do
  -- Sibi: Commenting this because we are supporting latest cabal
  -- version with this change. You can uncomment it when a newer cabal
  -- release is made.
  -- https://cabal.readthedocs.io/en/3.6/file-format-changelog.html
  -- stackErr ["--stack-yaml", "stack-bad.yaml", "build", "--dry-run"]
  --
  stack ["--stack-yaml", "stack-good.yaml", "build", "--dry-run"]
