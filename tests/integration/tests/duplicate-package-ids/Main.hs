-- | Stack distinguises between a package in the package index and a project
-- package, even if they have the same name and version.

import           StackTest

main :: IO ()
main = do
  stack ["--stack-yaml", "stack1.yaml", "build", "acme-missiles"]
  stack ["unpack", "acme-missiles-0.3"]
  stack ["--stack-yaml", "stack2.yaml", "build"]
