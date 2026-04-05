import Data.Char (toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import StackTest
import Control.Monad (unless)

main :: IO ()
main = do
  stack ["test", "--no-run-tests"] -- pre-build to avoid counting build time in the test

  start <- getCurrentTime
  errRef <- newIORef ""
  stackErrStderr
    [ "test"
    , "--test-suite-timeout", "1"
    , "--test-suite-timeout-grace", "1"
    ]
    (writeIORef errRef)
  end <- getCurrentTime
  err <- readIORef errRef

  let errLower = map toLower err
      elapsedSecs :: Double
      elapsedSecs = realToFrac (diffUTCTime end start)

  logInfo $ "Elapsed time: " ++ show elapsedSecs ++ "s"

  unless ("timed out" `isInfixOf` errLower) $
    error "Expected test-suite timeout message in stderr output."

  if isWindows
    then unless (elapsedSecs < 5.0) $
      error $ "Expected timeout+grace run to finish quickly on Windows, took "
        ++ show elapsedSecs ++ "s"
    else unless (elapsedSecs > 1.5 && elapsedSecs < 5.0) $
      error $ "Expected timeout+grace run to take about timeout+grace on Unix, took "
        ++ show elapsedSecs ++ "s"
