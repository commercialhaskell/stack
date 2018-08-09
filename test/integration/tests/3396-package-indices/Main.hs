import           StackTest
import           System.Directory   (createDirectoryIfMissing)
import           System.Environment (getEnv, setEnv)
import           System.FilePath    ((</>))

main :: IO ()
main = do
  putStrLn "With pantry, non-Hackage Security indices are no longer supported, skipping test"
  {-
  home <- getEnv "HOME"
  setEnv "STACK_ROOT" (home </> ".stack") -- Needed for Windows
  createDirectoryIfMissing True (home </> ".stack" </> "indices" </> "CustomIndex")
  copy "CustomIndex/01-index.tar" (home </> ".stack"  </> "indices" </> "CustomIndex" </> "01-index.tar")
  stack ["build"]
  -}
