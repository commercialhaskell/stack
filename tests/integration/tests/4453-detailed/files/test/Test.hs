module Test
  ( tests
  ) where

import           Distribution.TestSuite
                   ( Progress (..), Result (..), Test (..), TestInstance (..) )

tests :: IO [Test]
tests = pure [ test "test" Pass ]

test :: String -> Result -> Test
test name r = Test testInstance
 where
  testInstance = TestInstance
    { run = pure (Finished r)
    , name = name
    , tags = []
    , options = []
    , setOption = \_ _ -> Right testInstance
    }
