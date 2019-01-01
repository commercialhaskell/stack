import Control.Monad (unless)
import StackTest
import Data.Set
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

main :: IO ()
main = do
  stackCheckStdout ["freeze"] $ \stdOut -> do
    let contents = fromList [
                    "resolver:",
                    "size: 527165",
                    "url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/19.yaml",
                    "sha256: 0116ad1779b20ad2c9d6620f172531f13b12bb69867e78f4277157e28865dfd4",
                    "extra-deps:",
                    "pantry-tree:",
                    "hackage: a50-0.5@sha256:b8dfcc13dcbb12e444128bb0e17527a2a7a9bd74ca9450d6f6862c4b394ac054,1491",
                    "size: 409",
                    "sha256: a7c6151a18b04afe1f13637627cad4deff91af51d336c4f33e95fc98c64c40d3"
                   ]
        isLeadingYamlSymbol c = c == '-'
        trim str = dropWhileEnd isSpace $ dropWhile (\x -> isSpace x || isLeadingYamlSymbol x) str
    let stdOutLines = fromList $ Prelude.map trim (lines stdOut)
    unless (stdOutLines == contents) $
      error $ concat [ "Expected: "
                     , show contents
                     , "\nActual: "
                     , show stdOutLines
                     ]
