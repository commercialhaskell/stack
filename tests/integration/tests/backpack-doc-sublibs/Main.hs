import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- Regression test for the sub-libraries example in doc/topics/backpack.md.
-- The package shape is verbatim from the doc:
--   library str-sig    -- indefinite, signatures: Str
--   library str-text   -- concrete, exposed-modules: Str (backed by text)
--   library            -- main, depends on both with mixins
-- This exercises:
--   1. multi-library package routing (Hybrid: legacy non-split path)
--   2. signature instantiation within a single package
--   3. that the doc's str-sig stanza correctly includes build-depends: base
--      (without it, GHC can't load Prelude to compile the .hsig)
main :: IO ()
main = do
  stack ["build"]

  -- Confirm the build output mentions the signature and main library compiled.
  -- If the str-sig stanza loses its build-depends: base, the .hsig would fail
  -- with "Could not load module 'Prelude'" before any of these lines appear.
  stackCheckStderr ["clean"] $ const (pure ())
  stackCheckStderr ["build"] $ \err -> do
    let expect tag =
          unless (tag `isInfixOf` err) $
            error $ "Expected " ++ show tag ++ " in build output:\n" ++ err
    expect "Compiling Str[sig]"
    expect "Compiling MyModule"
