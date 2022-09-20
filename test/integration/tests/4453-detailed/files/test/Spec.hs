module Spec (tests) where

import Distribution.TestSuite

tests :: IO [Test]
tests = do
  pure [
      test "foo" Pass
    ]

test :: String -> Result -> Test
test name r = Test t
  where
    t = TestInstance {
        run = pure (Finished r)
      , name = name
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t
      }
