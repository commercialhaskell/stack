-- Stack advises the use of allow-newer in a configuration file if the package
-- versions needed are not the version in the snapshot.
--
-- See: https://github.com/commercialhaskell/stack/issues/3685

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

planRecommendation :: String
planRecommendation = "To ignore all version constraints"

main :: IO ()
main =
  -- intero-0.1.23 chosen because it depends on ghc >=7.8 && <8.2.2.
  stackErrStderr ["install", "intero-0.1.23"] (expectMessage planRecommendation)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected a recommendation: \n" ++ show msg
