import StackTest

-- Test that a package using Backpack internally (private Backpack) builds.
-- lib:str-sig defines a Str signature (.hsig), lib:str-impl provides
-- a concrete Str module, and the main library depends on both so that
-- mix-in linking fills the signature.
main :: IO ()
main = do
  stack ["build"]
  -- Verify the built executable actually runs (Backpack instantiation worked)
  stack ["exec", "private-backpack-demo"]
