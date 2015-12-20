import StackTest

main :: IO ()
main =
    if isWindows
        -- Disabled on Windows (see https://github.com/commercialhaskell/stack/issues/1337#issuecomment-166118678)
        then return ()
        else do
            stack ["build"]
            stack ["exec", "以-exe"]
