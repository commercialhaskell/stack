{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Install GHC/GHCJS and Cabal.
module Stack.SetupCmd
  ( setup
  , setupParser
  , SetupCmdOpts (..)
  ) where

import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Options.Applicative.Builder.Extra as OA
import qualified Options.Applicative.Types as OA
import           Stack.Prelude
import           Stack.Setup ( SetupOpts (..), ensureCompilerAndMsys )
import           Stack.Types.Config
                   ( CompilerPaths (..), Config (..), HasBuildConfig
                   , HasConfig (..), HasGHCVariant
                   )
import           Stack.Types.Version ( VersionCheck )

data SetupCmdOpts = SetupCmdOpts
  { scoCompilerVersion :: !(Maybe WantedCompiler)
  , scoForceReinstall  :: !Bool
  , scoGHCBindistURL   :: !(Maybe String)
  , scoGHCJSBootOpts   :: ![String]
  , scoGHCJSBootClean  :: !Bool
  }

setupParser :: OA.Parser SetupCmdOpts
setupParser = SetupCmdOpts
  <$> OA.optional (OA.argument readVersion
        (  OA.metavar "GHC_VERSION"
        <> OA.help "Version of GHC to install, e.g. 7.10.2. The default is to \
                   \install the version implied by the resolver."
        ))
  <*> OA.boolFlags False
        "reinstall"
        "reinstalling GHC, even if available (incompatible with --system-ghc)"
        OA.idm
  <*> OA.optional (OA.strOption
        (  OA.long "ghc-bindist"
        <> OA.metavar "URL"
        <> OA.help "Alternate GHC binary distribution (requires custom \
                   \--ghc-variant)"
        ))
  <*> OA.many (OA.strOption
        (  OA.long "ghcjs-boot-options"
        <> OA.metavar "GHCJS_BOOT"
        <> OA.help "Additional ghcjs-boot options"
        ))
  <*> OA.boolFlags True
        "ghcjs-boot-clean"
        "Control if ghcjs-boot should have --clean option present"
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

setup ::
       (HasBuildConfig env, HasGHCVariant env)
    => SetupCmdOpts
    -> WantedCompiler
    -> VersionCheck
    -> Maybe (Path Abs File)
    -> RIO env ()
setup SetupCmdOpts{..} wantedCompiler compilerCheck mstack = do
  Config{..} <- view configL
  sandboxedGhc <- cpSandboxed . fst <$> ensureCompilerAndMsys SetupOpts
    { soptsInstallIfMissing = True
    , soptsUseSystem = configSystemGHC && not scoForceReinstall
    , soptsWantedCompiler = wantedCompiler
    , soptsCompilerCheck = compilerCheck
    , soptsStackYaml = mstack
    , soptsForceReinstall = scoForceReinstall
    , soptsSanityCheck = True
    , soptsSkipGhcCheck = False
    , soptsSkipMsys = configSkipMsys
    , soptsResolveMissingGHC = Nothing
    , soptsGHCBindistURL = scoGHCBindistURL
    }
  let compiler = case wantedCompiler of
        WCGhc _ -> "GHC"
        WCGhcGit{} -> "GHC (built from source)"
        WCGhcjs {} -> "GHCJS"
  if sandboxedGhc
    then logInfo $ "Stack will use a sandboxed " <> compiler <> " it installed."
    else logInfo $ "Stack will use the " <> compiler <> " on your PATH."
  logInfo "For more information on paths, see 'stack path' and 'stack exec env'."
  logInfo
    $  "To use this "
    <> compiler
    <> " and packages outside of a project, consider using:"
  logInfo "'stack ghc', 'stack ghci', 'stack runghc', or 'stack exec'."
