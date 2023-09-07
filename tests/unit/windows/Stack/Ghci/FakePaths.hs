module Stack.Ghci.FakePaths
  ( defaultDrive
  ) where

-- | Helpers for writing fake paths for test suite for the GhciScript DSL. This
-- must be a separate module because it is used in Template Haskell splices.
defaultDrive :: FilePath
defaultDrive = "C:\\"
