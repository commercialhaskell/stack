-- HLint configuration file

module HLint.HLint where

-- mimic default HLint.hs
-- https://github.com/ndmitchell/hlint/blob/master/data/HLint.hs
import "hint" HLint.Default
import "hint" HLint.Builtin.All

ignore "Use fewer imports" =
  System.Process.Read  -- Related to 'Hide post-AMP warnings' comment
  Stack.Exec  -- ifdef for System.Process.Read

-- Related to 'explicit pattern matching is clearer' comment
ignore "Use fromMaybe" =
  Stack.Types.Config.explicitSetupDeps

-- For clarity (related to do syntax)
ignore "Reduce duplication" =
  Network.HTTP.Download.VerifiedSpec
  Stack.PackageDumpSpec
  Stack.Types.StackT
  Stack.Docker

-- Not considered useful hints
ignore "Redundant do"
ignore "Use section"
ignore "Use camelCase"
ignore "Use list comprehension"
ignore "Redundant if"
ignore "Avoid lambda"
ignore "Eta reduce"
ignore "Use fmap"  -- specific to GHC 7.8 compat
ignore "Parse error"  -- we trust the compiler over HLint
ignore "Use ==" -- Creates infinite loops in `EQ` using expressions
ignore "Evaluate"
