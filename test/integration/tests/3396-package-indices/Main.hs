import           StackTest
import           System.Directory   (createDirectory)
import           System.Environment (getEnv)
import           System.FilePath    ((</>))

main :: IO ()
main = do
  home <- getEnv "HOME"
  createDirectory (home </> ".stack" </> "indices" </> "CustomIndex")
  copy "CustomIndex/01-index.tar" (home </> ".stack"  </> "indices" </> "CustomIndex" </> "01-index.tar")
  stack ["build"]
