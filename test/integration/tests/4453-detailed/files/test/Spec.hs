module Spec (tests) where

import Distribution.TestSuite

tests :: IO [Test]
tests = do
  return [   
      test "foo" Pass
    , test "bar" (Fail "It did not work out!")
    ]

test :: String -> Result -> Test
test name r = Test t
  where          
    t = TestInstance {
        run = return (Finished r)
      , name = name
      , tags = []
      , options = []
      , setOption = \_ _ -> Right t
      }
