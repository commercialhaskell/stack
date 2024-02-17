import StackTest
import System.Directory
         ( createDirectoryIfMissing, withCurrentDirectory, getCurrentDirectory )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )
import Data.List ( filter )
import System.IO ( hPutStrLn, withFile, IOMode (..) )
import Control.Monad ( when )

main :: IO ()
main = when isLinux $ do
    runShell "git config --global protocol.file.allow always"

    let
      gitInit = do
         runShell "git init ."
         runShell "git config user.name Test"
         runShell "git config user.email test@test.com"
         runShell "git config commit.gpgsign false"

    let withEmptyDir name inner = do
          removeDirIgnore name
          createDirectoryIfMissing True name
          withCurrentDirectory name inner

    withEmptyDir "tmpSubSubRepo" $ do
      gitInit
      stack ["new", "pkg ", defaultSnapshotArg]
      runShell "git add pkg"
      runShell "git commit -m SubSubCommit"

    withEmptyDir "tmpSubRepo" $ do
      gitInit
      runShell "git submodule add ../tmpSubSubRepo sub"
      runShell "git commit -a -m SubCommit"

    withEmptyDir "tmpRepo" $ do
      gitInit
      runShell "git submodule add ../tmpSubRepo sub"
      runShell "git commit -a -m Commit"

    removeDirIgnore "tmpPackage"
    stack ["new", defaultSnapshotArg, "tmpPackage"]

    curDir <- getCurrentDirectory
    let tmpRepoDir = curDir </> "tmpRepo"
    gitHead <- runWithCwd tmpRepoDir "git" ["rev-parse", "HEAD"]
    let gitHeadCommit = stripNewline gitHead

    withCurrentDirectory "tmpPackage" $ do
      -- add git dependency on repo with recursive submodules
      writeToStackFile (tmpRepoDir, gitHeadCommit)
      -- Setup the package
      stack ["setup"]

    -- cleanup
    removeDirIgnore "tmpRepo"
    removeDirIgnore "tmpSubRepo"
    removeDirIgnore "tmpSubSubRepo"
    removeDirIgnore "tmpPackage"

writeToStackFile :: (String, String) -> IO ()
writeToStackFile (tmpRepoDir, gitCommit) = do
  curDir <- getCurrentDirectory
  let stackFile = curDir </> "stack.yaml"
  let line1 = "extra-deps:"
      line2 = "- git: " ++ tmpRepoDir
      line3 = "  commit: " ++ gitCommit
      line4 = "  subdir: sub/sub/pkg"
  withFile stackFile AppendMode (\handle -> do
                                   hPutStrLn handle line1
                                   hPutStrLn handle line2
                                   hPutStrLn handle line3
                                   hPutStrLn handle line4
                                )

newline :: Char
newline = '\n'

stripNewline :: String -> String
stripNewline = filter (/= newline)
