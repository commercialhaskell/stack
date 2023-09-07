import StackTest
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.List (isInfixOf)

main :: IO ()
main = do
  stackCleanFull
  let stackYamlFiles = words "stack-locals.yaml stack-everything.yaml stack-targets.yaml stack-name.yaml"
  for_ stackYamlFiles $ \stackYaml ->
    stackErrStderr ["build", "--stack-yaml", stackYaml] $ \str ->
      unless ("invalid option" `isInfixOf` str) $
      error "Configure option is not present"

  stack ["build", "--stack-yaml", "stack-locals.yaml", "acme-dont"]
  stack ["build", "--stack-yaml", "stack-targets.yaml", "acme-dont"]
  stackErr ["build", "--stack-yaml", "stack-name.yaml", "acme-dont"]
  stackErr ["build", "--stack-yaml", "stack-everything.yaml", "acme-dont"]
