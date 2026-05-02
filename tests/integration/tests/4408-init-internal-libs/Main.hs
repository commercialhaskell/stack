-- Stack can initialise a project where a project package includes a private
-- named library (an internal library).
--
-- See: https://github.com/commercialhaskell/stack/issues/4408

import          StackTest

main :: IO ()
main = stack ["init"]
