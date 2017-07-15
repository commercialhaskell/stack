import StackTest

import Control.Exception (bracket)
import qualified Data.ByteString as S

main :: IO ()
main = do
  -- we need to build all the executables first to be able to skip them later (see issue #3229)
  stack ["build"]
  bracket
    (S.readFile "app/MainFail.hs")
    (S.writeFile "app/MainFail.hs")
    (const $ do
        writeFile "app/MainFail.hs" "bdsf"
        stack ["build", "--test", "--bench", "--skip", "failing-test", "--skip", "failing-bench", "--skip", "failing-exe"]
        stack ["build", ":failing-test", ":failing-bench", ":exe", ":failing-exe", "--skip", "failing-test", "--skip", "failing-bench", "--skip", "failing-exe"])
