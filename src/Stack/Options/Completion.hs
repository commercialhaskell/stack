{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

buildConfigCompleter ::
     (String -> RIO EnvConfig [String])
  -> Completer
buildConfigCompleter inner = mkCompleter $ \inputRaw -> do
  let input = unescapeBashArg inputRaw
  case input of
    -- If it looks like a flag, skip this more costly completion.
    ('-': _) -> pure []
    _ -> do
      go' <- globalOptsFromMonoid False mempty
      let go = go' { globalLogLevel = LevelOther "silent" }
      withRunnerGlobal go $ withConfig NoReexec $ withDefaultEnvConfig $ inner input

targetCompleter :: Completer
targetCompleter = buildConfigCompleter $ \input -> do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  comps <- for packages ppComponents
  pure $
    concatMap
      (filter (input `isPrefixOf`) . allComponentNames)
      (Map.toList comps)
 where
  allComponentNames (name, comps) =
    map (T.unpack . renderPkgComponent . (name,)) (Set.toList comps)

flagCompleter :: Completer
flagCompleter = buildConfigCompleter $ \input -> do
  bconfig <- view buildConfigL
  gpds <- for (smwProject $ bcSMWanted bconfig) ppGPD
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
        case configProject (bcConfig bconfig) of
          PCProject (p, _) -> projectFlags p
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

projectExeCompleter :: Completer
projectExeCompleter = buildConfigCompleter $ \input -> do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  gpds <- Map.traverseWithKey (const ppGPD) packages
  pure
    $ filter (input `isPrefixOf`)
    $ nubOrd
    $ concatMap
        (map (C.unUnqualComponentName . fst) . C.condExecutables)
        gpds
