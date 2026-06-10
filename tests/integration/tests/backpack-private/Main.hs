import Control.Monad ( unless )
import StackTest

-- Test that a package using Backpack internally (private Backpack) builds.
-- lib:str-sig defines a Str signature (.hsig), lib:str-impl provides
-- a concrete Str module, and the main library depends on both so that
-- mix-in linking fills the signature.
main :: IO ()
-- On Windows, this test fails because Cabal-3.12.1.0's copy command cannot cope
-- with long paths. It appears it will be fixed in Cabal >= 3.18. We disable the
-- test on Windows for now.
main = unless isWindows $ do
  stack ["build"]
  -- Verify the built executable actually runs (Backpack instantiation worked)
  stack ["exec", "private-backpack-demo"]
