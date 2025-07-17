{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Options.SetupParser
Description : Parse arguments for Stack's @setup@ command.
License     : BSD-3-Clause

Functions to parse command line arguments for Stack's @setup@ command.
-}

module Stack.Options.SetupParser
  ( setupOptsParser
  ) where

import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Options.Applicative.Builder.Extra as OA
import qualified Options.Applicative.Types as OA
import           Stack.Prelude
import           Stack.Types.SetupOpts ( SetupCmdOpts (..) )

-- | Parse command line arguments for Stack's @setup@ command.
setupOptsParser :: OA.Parser SetupCmdOpts
setupOptsParser = SetupCmdOpts
  <$> OA.optional (OA.argument readVersion
        (  OA.metavar "GHC_VERSION"
        <> OA.help "Version of GHC to install, e.g. 9.10.2. (default: install \
                   \the version implied by the snapshot)"
        ))
  <*> OA.boolFlags False
        "reinstall"
        "reinstalling GHC, even if available (incompatible with --system-ghc)."
        OA.idm
  <*> OA.optional (OA.strOption
        (  OA.long "ghc-bindist"
        <> OA.metavar "URL"
        <> OA.help "Alternate GHC binary distribution (requires custom \
                   \--ghc-variant)."
        ))
  <*> OA.many (OA.strOption
        (  OA.long "ghcjs-boot-options"
        <> OA.metavar "GHCJS_BOOT"
        <> OA.help "Additional ghcjs-boot options."
        ))
  <*> OA.boolFlags True
        "ghcjs-boot-clean"
        "Control if ghcjs-boot should have --clean option present."
        OA.idm
 where
  readVersion = do
    s <- OA.readerAsk
    case parseWantedCompiler ("ghc-" <> T.pack s) of
      Left _ ->
        case parseWantedCompiler (T.pack s) of
          Left _ -> OA.readerError $ "Invalid version: " ++ s
          Right x -> pure x
      Right x -> pure x
