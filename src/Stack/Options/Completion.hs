{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Stack.Options.Completion
    ( ghcOptsCompleter
    , targetCompleter
    , flagCompleter
    , projectExeCompleter
    ) where

import           Data.Char (isSpace)
import           Data.List (isPrefixOf)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.PackageDescription as C
import qualified Distribution.Types.UnqualComponentName as C
import           Options.Applicative
import           Options.Applicative.Builder.Extra
import           Stack.Build.Target (NeedTargets(..))
import           Stack.Constants (ghcShowOptionsOutput)
import           Stack.Options.GlobalParser (globalOptsFromMonoid)
import           Stack.Runners (loadConfigWithOpts)
import           Stack.Prelude
import           Stack.Setup
import           Stack.Types.Config
import           Stack.Types.NamedComponent
import           Stack.Types.SourceMap

ghcOptsCompleter :: Completer
ghcOptsCompleter = mkCompleter $ \inputRaw -> return $
    let input = unescapeBashArg inputRaw
        (curArgReversed, otherArgsReversed) = break isSpace (reverse input)
        curArg = reverse curArgReversed
        otherArgs = reverse otherArgsReversed
     in if null curArg then [] else
         map (otherArgs ++) $
         filter (curArg `isPrefixOf`) ghcShowOptionsOutput

-- TODO: Ideally this would pay attention to --stack-yaml, may require
-- changes to optparse-applicative.

buildConfigCompleter
    :: (String -> RIO EnvConfig [String])
    -> Completer
buildConfigCompleter inner = mkCompleter $ \inputRaw -> do
    let input = unescapeBashArg inputRaw
    case input of
        -- If it looks like a flag, skip this more costly completion.
        ('-': _) -> return []
        _ -> do
            go' <- globalOptsFromMonoid False mempty
            let go = go' { globalLogLevel = LevelOther "silent" }
            loadConfigWithOpts go $ \lc -> do
              bconfig <- liftIO $ lcLoadBuildConfig lc (globalCompiler go)
              envConfig <- runRIO bconfig (setupEnv AllowNoTargets defaultBuildOptsCLI Nothing)
              runRIO envConfig (inner input)

targetCompleter :: Completer
targetCompleter = buildConfigCompleter $ \input -> do
  packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
  comps <- for packages ppComponents
  pure
    $ filter (input `isPrefixOf`)
    $ concatMap allComponentNames
    $ Map.toList comps
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
             in (if flagEnabled name fl then "-" else "") ++ flname
        flagEnabled name fl =
            fromMaybe (C.flagDefault fl) $
            Map.lookup (C.flagName fl) $
            Map.findWithDefault Map.empty name (error "bcFlags bconfig")
    return $ filter (input `isPrefixOf`) $
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
        (\gpd -> map
          (C.unUnqualComponentName . fst)
          (C.condExecutables gpd)
        )
        gpds
