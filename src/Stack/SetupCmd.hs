{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Install GHC/GHCJS and Cabal.
module Stack.SetupCmd
    ( setup
    , setupParser
    , SetupCmdOpts(..)
    ) where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Options.Applicative.Builder.Extra as OA
import qualified Options.Applicative.Types as OA
import           Path
import           Prelude -- silence redundant import warnings
import           Stack.Setup
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.StackT
import           Stack.Types.Version

data SetupCmdOpts = SetupCmdOpts
    { scoCompilerVersion :: !(Maybe CompilerVersion)
    , scoForceReinstall  :: !Bool
    , scoUpgradeCabal    :: !Bool
    , scoSetupInfoYaml   :: !String
    , scoGHCBindistURL   :: !(Maybe String)
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
    <*> OA.boolFlags False
            "upgrade-cabal"
            "installing the newest version of the Cabal library globally"
            OA.idm
    <*> setupYamlCompatParser
    <*> OA.optional (OA.strOption
            (OA.long "ghc-bindist"
           <> OA.metavar "URL"
           <> OA.help "Alternate GHC binary distribution (requires custom --ghc-variant)"))
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
    :: (StackM env m, HasConfig env, HasGHCVariant env)
    => SetupCmdOpts
    -> CompilerVersion
    -> VersionCheck
    -> Maybe (Path Abs File)
    -> m ()
setup SetupCmdOpts{..} wantedCompiler compilerCheck mstack = do
    Config{..} <- view configL
    mpaths <- fst <$> ensureCompiler SetupOpts
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
        }
    let compiler = case wantedCompiler of
            GhcVersion _ -> "GHC"
            GhcjsVersion {} -> "GHCJS"
    case mpaths of
        Nothing -> $logInfo $ "stack will use the " <> compiler <> " on your PATH"
        Just _ -> $logInfo $ "stack will use a sandboxed " <> compiler <> " it installed"
    $logInfo "For more information on paths, see 'stack path' and 'stack exec env'"
    $logInfo $ "To use this " <> compiler <> " and packages outside of a project, consider using:"
    $logInfo "stack ghc, stack ghci, stack runghc, or stack exec"
