{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Install GHC/GHCJS and Cabal.
module Stack.SetupCmd
    ( setup
    , setupParser
    , SetupCmdOpts(..)
    ) where

import           Control.Applicative
import           Control.Monad.Logger ()
import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Options.Applicative.Builder.Extra as OA
import qualified Options.Applicative.Types as OA
import           Path
import           Stack.Prelude
import           Stack.Setup
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.Version

data SetupCmdOpts = SetupCmdOpts
    { scoCompilerVersion :: !(Maybe (CompilerVersion 'CVWanted))
    , scoForceReinstall  :: !Bool
    , scoUpgradeCabal    :: !(Maybe UpgradeTo)
    , scoSetupInfoYaml   :: !String
    , scoGHCBindistURL   :: !(Maybe String)
    , scoGHCJSBootOpts   :: ![String]
    , scoGHCJSBootClean  :: !Bool
    }

setupYamlCompatParser :: OA.Parser String
setupYamlCompatParser = stackSetupYaml <|> setupInfoYaml
    where stackSetupYaml = OA.strOption (
               OA.long "stack-setup-yaml"
            <> OA.help "DEPRECATED: Use 'setup-info-yaml' instead"
            <> OA.metavar "URL"
            <> OA.hidden )
          setupInfoYaml  = OA.strOption (
               OA.long "setup-info-yaml"
            <> OA.help "Alternate URL or absolute path for stack dependencies"
            <> OA.metavar "URL"
            <> OA.value defaultSetupInfoYaml )

cabalUpgradeParser :: OA.Parser UpgradeTo
cabalUpgradeParser = Specific <$> version' <|> latestParser
    where
        versionReader = do
            s <- OA.readerAsk
            case parseVersion (T.pack s) of
                Nothing -> OA.readerError $ "Invalid version: " ++ s
                Just v  -> return v
        version' = OA.option versionReader (
            OA.long "install-cabal"
         <> OA.metavar "VERSION"
         <> OA.help "Install a specific version of Cabal" )
        latestParser = OA.flag' Latest (
            OA.long "upgrade-cabal"
         <> OA.help "DEPRECATED Install latest version of Cabal globally" )

setupParser :: OA.Parser SetupCmdOpts
setupParser = SetupCmdOpts
    <$> OA.optional (OA.argument readVersion
            (OA.metavar "GHC_VERSION" <>
             OA.help ("Version of GHC to install, e.g. 7.10.2. " ++
                      "The default is to install the version implied by the resolver.")))
    <*> OA.boolFlags False
            "reinstall"
            "reinstalling GHC, even if available (incompatible with --system-ghc)"
            OA.idm
    <*> OA.optional cabalUpgradeParser
    <*> setupYamlCompatParser
    <*> OA.optional (OA.strOption
            (OA.long "ghc-bindist"
           <> OA.metavar "URL"
           <> OA.help "Alternate GHC binary distribution (requires custom --ghc-variant)"))
    <*> OA.many (OA.strOption
            (OA.long "ghcjs-boot-options"
           <> OA.metavar "GHCJS_BOOT"
           <> OA.help "Additional ghcjs-boot options"))
    <*> OA.boolFlags True
            "ghcjs-boot-clean"
            "Control if ghcjs-boot should have --clean option present"
            OA.idm
  where
    readVersion = do
        s <- OA.readerAsk
        case parseCompilerVersion ("ghc-" <> T.pack s) of
            Nothing ->
                case parseCompilerVersion (T.pack s) of
                    Nothing -> OA.readerError $ "Invalid version: " ++ s
                    Just x -> return x
            Just x -> return x

setup
    :: (HasConfig env, HasGHCVariant env)
    => SetupCmdOpts
    -> CompilerVersion 'CVWanted
    -> VersionCheck
    -> Maybe (Path Abs File)
    -> RIO env ()
setup SetupCmdOpts{..} wantedCompiler compilerCheck mstack = do
    Config{..} <- view configL
    (_, _, sandboxedGhc) <- ensureCompiler SetupOpts
        { soptsInstallIfMissing = True
        , soptsUseSystem = configSystemGHC && not scoForceReinstall
        , soptsWantedCompiler = wantedCompiler
        , soptsCompilerCheck = compilerCheck
        , soptsStackYaml = mstack
        , soptsForceReinstall = scoForceReinstall
        , soptsSanityCheck = True
        , soptsSkipGhcCheck = False
        , soptsSkipMsys = configSkipMsys
        , soptsUpgradeCabal = scoUpgradeCabal
        , soptsResolveMissingGHC = Nothing
        , soptsSetupInfoYaml = scoSetupInfoYaml
        , soptsGHCBindistURL = scoGHCBindistURL
        , soptsGHCJSBootOpts = scoGHCJSBootOpts ++ ["--clean" | scoGHCJSBootClean]
        }
    let compiler = case wantedCompiler of
            GhcVersion _ -> "GHC"
            GhcjsVersion {} -> "GHCJS"
    if sandboxedGhc
        then logInfo $ "stack will use a sandboxed " <> compiler <> " it installed"
        else logInfo $ "stack will use the " <> compiler <> " on your PATH"
    logInfo "For more information on paths, see 'stack path' and 'stack exec env'"
    logInfo $ "To use this " <> compiler <> " and packages outside of a project, consider using:"
    logInfo "stack ghc, stack ghci, stack runghc, or stack exec"
