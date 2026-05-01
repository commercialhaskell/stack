-- Stack dumps logs with GHC warnings for multi-package projects and
-- non-interleaved output.
--
-- See: https://github.com/commercialhaskell/stack/issues/2997

import Data.List ( isInfixOf )
import StackTest

main :: IO ()
main = do
  stackCheckStderr ["build", "--no-interleaved-output"] $ \str ->
    if "no type signature" `isInfixOf` str
      then pure ()
      else error "Warnings are not being shown"
