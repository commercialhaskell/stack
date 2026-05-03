-- | Stack will build packages in the package index in the absence of a Stack
-- project-level configuration file (by referring to the configuration file in
-- the global-project directory in the Stack root).

import           StackTest

main :: IO ()
main = stack ["build", "acme-missiles-0.3"]
