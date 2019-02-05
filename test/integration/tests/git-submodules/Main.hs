import StackTest
import System.Directory (createDirectoryIfMissing,withCurrentDirectory)

main :: IO ()
main = do
    let
      gitInit = do
         runShell "git init ."
         runShell "git config user.name Test"
         runShell "git config user.email test@test.com"

    createDirectoryIfMissing True "tmpSubSubRepo"
    withCurrentDirectory "tmpSubSubRepo" $ do
      gitInit
      stack ["new", "pkg ", defaultResolverArg]
      runShell "git add pkg"
      runShell "git commit -m SubSubCommit"

    createDirectoryIfMissing True "tmpSubRepo"
    withCurrentDirectory "tmpSubRepo" $ do
      gitInit
      runShell "git submodule add ../tmpSubSubRepo sub"
      runShell "git commit -a -m SubCommit"

    createDirectoryIfMissing True "tmpRepo"
    withCurrentDirectory "tmpRepo" $ do
      gitInit
      runShell "git submodule add ../tmpSubRepo sub"
      runShell "git commit -a -m Commit"

    stack ["new", defaultResolverArg, "tmpPackage"]

    withCurrentDirectory "tmpPackage" $ do
      -- add git dependency on repo with recursive submodules
      runShell "echo 'extra-deps:' >> stack.yaml"
      runShell "echo \"- git: $(cd ../tmpRepo && pwd)\" >> stack.yaml"
      runShell "echo \"  commit: $(cd ../tmpRepo && git rev-parse HEAD)\" >> stack.yaml"
      runShell "echo '  subdir: sub/sub/pkg' >> stack.yaml"

      -- Setup the package
      stack ["setup"]

    -- cleanup
    removeDirIgnore "tmpRepo"
    removeDirIgnore "tmpSubRepo"
    removeDirIgnore "tmpSubSubRepo"
    removeDirIgnore "tmpPackage"
