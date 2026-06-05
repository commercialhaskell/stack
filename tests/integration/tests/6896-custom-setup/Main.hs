-- | Stack can build:
--
-- * a project package with a custom @Setup.hs@ (see @myPackageA@);
--
-- * a project package with a custom @Setup.hs@ that depends on the main library
--   of another project package (see @myPackageB@ and @myPackageD@); and
--
-- * a project package with a custom @Setup.hs@ that depends on the public
--   sublibrary of another project package that does not have a main library
--   (see @myPackageC@ amd @myPackageE@).
--
-- See: https://github.com/commercialhaskell/stack/issues/6896

import           Control.Monad ( unless)
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr ["build", "myPackageA"] (expectMessage usingCustomA)
  stackCheckStderr ["build", "myPackageB"] (expectMessage usingCustomB)
  stackCheckStderr ["build", "myPackageC"] (expectMessage usingCustomC)

usingCustomA :: String
usingCustomA = "Using my custom Setup.hs for myPackageA"

usingCustomB :: String
usingCustomB = "messageD: Using my custom Setup.hs for myPackageB"

usingCustomC :: String
usingCustomC = "messageE: Using my custom Setup.hs for myPackageC"

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected output: \n" ++ show msg
