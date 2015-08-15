import Control.Concurrent
import StackTest

main :: IO ()
main = do
    copy "src/Unlisted_OK.hs" "src/Unlisted.hs"
    copy "embed_OK.txt" "embed.txt"
    stack ["build"]
    pause
    copy "src/Unlisted_FAIL.hs" "src/Unlisted.hs"
    stackErr ["build"]
    pause
    copy "src/Unlisted_OK.hs" "src/Unlisted.hs"
    stack ["build"]
    stack ["exec", "files-exe"]
    pause
    copy "embed_FAIL.txt" "embed.txt"
    stack ["build"]
    stackErr ["exec", "files-exe"]
    pause
    copy "embed_OK.txt" "embed.txt"
    stack ["build"]
    stack ["exec", "files-exe"]

pause = threadDelay 1000000
