{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Options.Completion
Description : Completers for command line arguments.
License     : BSD-3-Clause

Completers for command line arguments or arguments of command line options.
-}

module Stack.Options.Completion
  ( ghcOptsCompleter
  , targetCompleter
  , flagCompleter
  , projectExeCompleter
  ) where

import           Data.Char ( isSpace )
import           Data.List ( isPrefixOf )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.PackageDescription as C
import           Options.Applicative ( Completer, mkCompleter )
import           Options.Applicative.Builder.Extra ( unescapeBashArg )
import           Stack.Constants ( ghcShowOptionsOutput )
import           Stack.Options.GlobalParser ( globalOptsFromMonoid )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig
                   , withRunnerGlobal
                   )
import           Stack.Prelude
import           Stack.Types.BuildConfig ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.Config ( Config (..) )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.NamedComponent ( renderPkgComponent )
import           Stack.Types.SourceMap ( SMWanted (..), ppComponents, ppGPD )

-- | A completer for @--ghc-options@ or @--ghci-options@.
ghcOptsCompleter :: Completer
ghcOptsCompleter = mkCompleter $ \inputRaw -> pure $
  let input = unescapeBashArg inputRaw
      (curArgReversed, otherArgsReversed) = break isSpace (reverse input)
      curArg = reverse curArgReversed
      otherArgs = reverse otherArgsReversed
  in  if null curArg
        then []
        else
          map (otherArgs ++) $
          filter (curArg `isPrefixOf`) ghcShowOptionsOutput

-- TODO: Ideally this would pay attention to --stack-yaml, may require
-- changes to optparse-applicative.

buildConfigCompleter :: (String -> RIO EnvConfig [String]) -> Completer
buildConfigCompleter inner = mkCompleter $ \inputRaw -> do
  let input = unescapeBashArg inputRaw
  case input of
    -- If it looks like a flag, skip this more costly completion.
    ('-': _) -> pure []
    _ -> do
      -- We do not need to specify the name of the current Stack executable, as
      -- it was invoked, or the path to the current Stack executable, as
      -- withDefaultEnvConfig does not need either.
      go' <- globalOptsFromMonoid "" Nothing False mempty
      let go = go' { logLevel = LevelOther "silent" }
      withRunnerGlobal go $ withConfig NoReexec $ withDefaultEnvConfig $ inner input

-- | A completer for components of project packages.
targetCompleter :: Completer
targetCompleter = buildConfigCompleter $ \input -> do
  packages <- view $ buildConfigL . to (.smWanted.project)
  comps <- for packages ppComponents
  pure $
    concatMap
      (filter (input `isPrefixOf`) . allComponentNames)
      (Map.toList comps)
 where
  allComponentNames (name, comps) =
    map (T.unpack . renderPkgComponent . (name,)) (Set.toList comps)

-- | A completer for Cabal flags of project packages.
flagCompleter :: Completer
flagCompleter = buildConfigCompleter $ \input -> do
  bconfig <- view buildConfigL
  gpds <- for bconfig.smWanted.project ppGPD
  let wildcardFlags
        = nubOrd
        $ concatMap (\(name, gpd) ->
            map (\fl -> "*:" ++ flagString name fl) (C.genPackageFlags gpd))
        $ Map.toList gpds
      normalFlags
        = concatMap (\(name, gpd) ->
            map (\fl -> packageNameString name ++ ":" ++ flagString name fl)
                (C.genPackageFlags gpd))
        $ Map.toList gpds
      flagString name fl =
        let flname = C.unFlagName $ C.flagName fl
        in  (if flagEnabled name fl then "-" else "") ++ flname
      prjFlags =
        case bconfig.config.project of
          PCProject (p, _) -> p.flagsByPkg
          PCGlobalProject -> mempty
          PCNoProject _ -> mempty
      flagEnabled name fl =
        fromMaybe (C.flagDefault fl) $
        Map.lookup (C.flagName fl) $
        Map.findWithDefault Map.empty name prjFlags
  pure $ filter (input `isPrefixOf`) $
    case input of
      ('*' : ':' : _) -> wildcardFlags
      ('*' : _) -> wildcardFlags
      _ -> normalFlags

-- | A completer for executable components of project packages.
projectExeCompleter :: Completer
projectExeCompleter = buildConfigCompleter $ \input -> do
  packages <- view $ buildConfigL . to (.smWanted.project)
  gpds <- Map.traverseWithKey (const ppGPD) packages
  pure
    $ filter (input `isPrefixOf`)
    $ nubOrd
    $ concatMap
        (map (C.unUnqualComponentName . fst) . C.condExecutables)
        gpds
