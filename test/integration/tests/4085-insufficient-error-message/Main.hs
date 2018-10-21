import StackTest
import System.Environment (getEnv)
import System.Directory
import System.FilePath
import System.Process
import System.Exit

imageTag :: String
imageTag = "4085-fix"

buildDockerImageWithStackSource :: IO ExitCode
buildDockerImageWithStackSource = do
  stackSrc <- stackSrc
  testDir <- testDir
  (Nothing, Nothing, Nothing, ph) <- createProcess (
    proc "docker" ["build"
                  , "-f", testDir </> "Dockerfile"
                  , "-t", imageTag
                  , "."]
    ) {cwd = Just stackSrc}
  waitForProcess ph

main :: IO ()
main = do
  code <- buildDockerImageWithStackSource
  return ()
