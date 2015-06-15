-- | Args parser test suite.

module Stack.ArgsSpec where

import Control.Monad
import Options.Applicative.Args
import Test.Hspec

-- | Test spec.
spec :: Spec
spec =
    forM_
        tests
        (\(input,output) ->
              it input (parseArgsFromString input == output))

-- | Fairly comprehensive checks.
tests :: [(String, Either String [String])]
tests =
    [ ("x", Right ["x"])
    , ("x y z", Right ["x", "y", "z"])
    , ("aaa bbb ccc", Right ["aaa", "bbb", "ccc"])
    , ("    aaa    bbb    ccc   ", Right ["aaa", "bbb", "ccc"])
    , ("aaa\"", Left "unterminated string: endOfInput")
    , ("\"", Left "unterminated string: endOfInput")
    , ("\"\"", Right [""])
    , ("\"aaa", Left "unterminated string: endOfInput")
    , ("\"aaa\" bbb ccc \"ddd\"", Right ["aaa", "bbb", "ccc", "ddd"])
    , ("\"aa\\\"a\" bbb ccc \"ddd\"", Right ["aa\"a", "bbb", "ccc", "ddd"])
    , ("\"aa\\\"a\" bb\\b ccc \"ddd\"", Right ["aa\"a", "bb\\b", "ccc", "ddd"])
    , ("\"\" \"\" c", Right ["","","c"])]
