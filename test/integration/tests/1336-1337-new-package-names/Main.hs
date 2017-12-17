import StackTest
import Control.Monad
import System.Directory
import System.FilePath

{-# ANN module "HLint: ignore Use unless" #-}
main :: IO ()
main =
    if isWindows
        then logInfo "Disabled on Windows (see https://github.com/commercialhaskell/stack/issues/1337#issuecomment-166118678)"
        else do
            stack ["new", "1234a-4b-b4-abc-12b34"]
            doesExist "./1234a-4b-b4-abc-12b34/stack.yaml"
            stackErr ["new", "1234-abc"]
            doesNotExist "./1234-abc/stack.yaml"
            doesNotExist "./1234-abc"
            stackErr ["new", "1-abc"]
            stackErr ["new", "44444444444444"]
            stackErr ["new", "abc-1"]
            stackErr ["new", "444-ば日本-4本"]
            unless isMacOSX $ stack ["new", "ば日本-4本"]
            stack ["new", "אבהץש"]
            stack ["new", "ΔΘΩϬ"]
            doesExist "./ΔΘΩϬ/stack.yaml"
            doesExist "./ΔΘΩϬ/ΔΘΩϬ.cabal"
