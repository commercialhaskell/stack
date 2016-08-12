{-# LANGUAGE CPP #-}

-- | Helpers for writing fake paths for test suite for the GhciScript DSL.
-- This must be a separate module because it is used in Teplate Haskell splices.
module Stack.Ghci.PortableFakePaths where

defaultDrive :: FilePath
#ifdef WINDOWS
defaultDrive = "C:\\"
#else
defaultDrive = "/"
#endif
