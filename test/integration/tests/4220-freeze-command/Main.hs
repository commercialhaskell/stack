import Control.Monad (unless)
import StackTest

main :: IO ()
main = do
  stackCheckStdout ["freeze"] $ \stdOut -> do
    let expected = unlines
          [ "packages:"
          , "- ."
          , "extra-deps:"
          , "- hackage: a50-0.5@sha256:b8dfcc13dcbb12e444128bb0e17527a2a7a9bd74ca9450d6f6862c4b394ac054,1491"
          , "  pantry-tree:"
          , "    size: 409"
          , "    sha256: a7c6151a18b04afe1f13637627cad4deff91af51d336c4f33e95fc98c64c40d3"
          , "resolver:"
          , "  blob:"
          , "    size: 527165"
          , "    sha256: 0116ad1779b20ad2c9d6620f172531f13b12bb69867e78f4277157e28865dfd4"
          , "  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/11/19.yaml"
          ]
    unless (stdOut == expected) $
      error $ concat [ "Expected: "
                   , show expected
                   , "\nActual: "
                   , show stdOut
                   ]
