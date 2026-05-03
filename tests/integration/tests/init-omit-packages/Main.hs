-- | Stack's init command provides an --omit-packages flag to avoid the problem
-- of bad project packages.

import           Control.Monad ( unless )
import           StackTest
import           System.IO ( readFile )

main :: IO ()
main = do
  stackErr ["init", "--snapshot", "lts-24.37"]
  stack ["init", "--snapshot", "lts-24.37", "--omit-packages"]
  contents <- lines <$> readFile "stack.yaml"
  unless ("#- bad" `elem` contents) $
    error "commented out 'bad' package was expected"
