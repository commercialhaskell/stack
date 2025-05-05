{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Options.ConfigEnvParser
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @config env@ command.
-}

module Stack.Options.ConfigEnvParser
  ( configCmdEnvParser
  ) where

import qualified Options.Applicative as OA
import           Options.Applicative.Builder.Extra ( boolFlags )
import           Stack.Prelude
import           Stack.Types.EnvSettings ( EnvSettings (..) )

-- | Parse command line arguments for Stack's @config env@ command.
configCmdEnvParser :: OA.Parser EnvSettings
configCmdEnvParser = EnvSettings
  <$> boolFlags True "locals" "include information about local packages" mempty
  <*> boolFlags True
        "ghc-package-path" "set GHC_PACKAGE_PATH environment variable" mempty
  <*> boolFlags True "stack-exe" "set STACK_EXE environment variable" mempty
  <*> boolFlags False
        "locale-utf8" "set the GHC_CHARENC environment variable to UTF-8" mempty
  <*> boolFlags False
        "keep-ghc-rts" "keep any GHCRTS environment variable" mempty
