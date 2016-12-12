import StackTest

{-# ANN module "HLint: ignore Use unless" #-}
main :: IO ()
main =
    if isWindows
        then logInfo "Disabled on Windows (see https://github.com/commercialhaskell/stack/issues/1337#issuecomment-166118678)"
        else do
            stack ["build"]
            stack ["exec", "以-exe"]
