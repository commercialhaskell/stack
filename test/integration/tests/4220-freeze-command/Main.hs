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
                    "size: 527200",
                    "url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/19.yaml",
                    "sha256: 16758b43c10c731bc142fdc5c005795db8338d7b4a28cd0af6730d739af2b306",
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
