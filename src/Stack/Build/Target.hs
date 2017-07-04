{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
-- | Parsing command line targets
module Stack.Build.Target
    ( -- * Types
      ComponentName
    , UnresolvedComponent (..)
    , RawTarget (..)
    , LocalPackageView (..)
    , SimpleTarget (..)
    , NeedTargets (..)
      -- * Parsers
    , parseRawTarget
    , parseTargets
    ) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad.IO.Unlift
import           Data.Either (partitionEithers)
import           Data.Foldable
import           Data.List.Extra (groupSort)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Path
import           Path.Extra (rejectMissingDir)
import           Path.IO
import           Prelude hiding (concat, concatMap) -- Fix redundant import warnings
import           Stack.Types.Config
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.GhcPkgId

-- | The name of a component, which applies to executables, test suites, and benchmarks
type ComponentName = Text

newtype RawInput = RawInput { unRawInput :: Text }

-- | Either a fully resolved component, or a component name that could be
-- either an executable, test, or benchmark
data UnresolvedComponent
    = ResolvedComponent !NamedComponent
    | UnresolvedComponent !ComponentName
    deriving (Show, Eq, Ord)

-- | Raw command line input, without checking against any databases or list of
-- locals. Does not deal with directories
data RawTarget (a :: RawTargetType) where
    RTPackageComponent :: !PackageName -> !UnresolvedComponent -> RawTarget a
    RTComponent :: !ComponentName -> RawTarget a
    RTPackage :: !PackageName -> RawTarget a
    RTPackageIdentifierRevision :: !PackageIdentifierRevision -> RawTarget 'HasIdents

deriving instance Show (RawTarget a)
deriving instance Eq (RawTarget a)

data RawTargetType = HasIdents | NoIdents

-- | If this function returns @Nothing@, the input should be treated as a
-- directory.
parseRawTarget :: Text -> Maybe (RawTarget 'HasIdents)
parseRawTarget t =
        (RTPackageIdentifierRevision <$> parsePackageIdentifierRevision t)
    <|> (RTPackage <$> parsePackageNameFromString s)
    <|> (RTComponent <$> T.stripPrefix ":" t)
    <|> parsePackageComponent
  where
    s = T.unpack t

    parsePackageComponent =
        case T.splitOn ":" t of
            [pname, "lib"]
                | Just pname' <- parsePackageNameFromString (T.unpack pname) ->
                    Just $ RTPackageComponent pname' $ ResolvedComponent CLib
            [pname, cname]
                | Just pname' <- parsePackageNameFromString (T.unpack pname) ->
                    Just $ RTPackageComponent pname' $ UnresolvedComponent cname
            [pname, typ, cname]
                | Just pname' <- parsePackageNameFromString (T.unpack pname)
                , Just wrapper <- parseCompType typ ->
                    Just $ RTPackageComponent pname' $ ResolvedComponent $ wrapper cname
            _ -> Nothing

    parseCompType t' =
        case t' of
            "exe" -> Just CExe
            "test" -> Just CTest
            "bench" -> Just CBench
            _ -> Nothing

-- | Same as @parseRawTarget@, but also takes directories into account.
parseRawTargetDirs :: MonadIO m
                   => Path Abs Dir -- ^ current directory
                   -> Map PackageName LocalPackageView
                   -> Text
                   -> m (Either Text [(RawInput, RawTarget 'HasIdents)])
parseRawTargetDirs root locals t =
    case parseRawTarget t of
        Just rt -> return $ Right [(ri, rt)]
        Nothing -> do
            mdir <- liftIO $ forgivingAbsence (resolveDir root (T.unpack t))
              >>= rejectMissingDir
            case mdir of
                Nothing -> return $ Left $ "Directory not found: " `T.append` t
                Just dir ->
                    case mapMaybe (childOf dir) $ Map.toList locals of
                        [] -> return $ Left $
                            "No local directories found as children of " `T.append`
                            t
                        names -> return $ Right $ map ((ri, ) . RTPackage) names
  where
    ri = RawInput t

    childOf dir (name, lpv) =
        if dir == lpvRoot lpv || isParentOf dir (lpvRoot lpv)
            then Just name
            else Nothing

data SimpleTarget
    = STUnknown
    | STNonLocal
    | STLocalComps !(Set NamedComponent)
    | STLocalAll
    deriving (Show, Eq, Ord)

-- | Given the snapshot information and the local packages (both
-- project and dependencies), figure out the appropriate 'RawTarget'
-- and any added local dependencies based on specified package
-- identifiers.
resolveIdents :: Map PackageName (LoadedPackageInfo GhcPkgId) -- ^ globals
              -> Map PackageName (LoadedPackageInfo (PackageLocationIndex FilePath)) -- ^ snapshot
              -> Map PackageName Version -- ^ local dependencies
              -> Map PackageName LocalPackageView -- ^ names and locations of project packages
              -> (RawInput, RawTarget 'HasIdents)
              -> Either Text ((RawInput, RawTarget 'NoIdents), Map PackageName Version)
resolveIdents _ _ _ _ (ri, RTPackageComponent x y) = Right ((ri, RTPackageComponent x y), Map.empty)
resolveIdents _ _ _ _ (ri, RTComponent x) = Right ((ri, RTComponent x), Map.empty)
resolveIdents _ _ _ _ (ri, RTPackage x) = Right ((ri, RTPackage x), Map.empty)
resolveIdents _ _ _ _ (_ri, RTPackageIdentifierRevision (PackageIdentifierRevision _ (Just _cfi))) =
    Left "Cabal file revision information should not be passed on the command line,\nplease add in your snapshot or stack.yaml configuration instead"
resolveIdents globals snap deps locals (ri, RTPackageIdentifierRevision (PackageIdentifierRevision (PackageIdentifier name version) Nothing)) =
    fmap ((ri, RTPackage name), ) newDeps
  where
    newDeps =
        case (Map.member name locals, mfound) of
            -- Error if it matches a local package, pkg idents not
            -- supported for local.
            (True, _) -> Left $ T.concat
                [ packageNameText name
                , " target has a specific version number, but it is a local package."
                , "\nTo avoid confusion, we will not install the specified version or build the local one."
                , "\nTo build the local package, specify the target without an explicit version."
                ]
            -- Specified the same package identifier as we already
            -- have, so nothing to add.
            (_, Just foundVersion) | foundVersion == version -> Right Map.empty
            -- Otherwise, if there is no specified version or a
            -- mismatch, add an extra dep.
            _ -> Right $ Map.singleton name version
    mfound = asum (map (Map.lookup name) [deps, lpiVersion <$> snap, lpiVersion <$> globals])

-- | Convert a 'RawTarget' without any package identifiers into a
-- 'SimpleTarget', if possible. This will deal with things like
-- checking for correct components.
resolveRawTarget :: Map PackageName (LoadedPackageInfo GhcPkgId) -- ^ globals
                 -> Map PackageName (LoadedPackageInfo (PackageLocationIndex FilePath)) -- ^ snapshot
                 -> Map PackageName Version -- ^ local extras
                 -> Map PackageName LocalPackageView -- ^ locals
                 -> (RawInput, RawTarget 'NoIdents)
                 -> Either Text (PackageName, (RawInput, SimpleTarget))
resolveRawTarget globals snap deps locals (ri, rt) =
    go rt
  where
    go (RTPackageComponent name ucomp) =
        case Map.lookup name locals of
            Nothing -> Left $ T.pack $ "Unknown local package: " ++ packageNameString name
            Just lpv ->
                case ucomp of
                    ResolvedComponent comp
                        | comp `Set.member` lpvComponents lpv ->
                            Right (name, (ri, STLocalComps $ Set.singleton comp))
                        | otherwise -> Left $ T.pack $ concat
                            [ "Component "
                            , show comp
                            , " does not exist in package "
                            , packageNameString name
                            ]
                    UnresolvedComponent comp ->
                        case filter (isCompNamed comp) $ Set.toList $ lpvComponents lpv of
                            [] -> Left $ T.concat
                                [ "Component "
                                , comp
                                , " does not exist in package "
                                , T.pack $ packageNameString name
                                ]
                            [x] -> Right (name, (ri, STLocalComps $ Set.singleton x))
                            matches -> Left $ T.concat
                                [ "Ambiguous component name "
                                , comp
                                , " for package "
                                , T.pack $ packageNameString name
                                , ": "
                                , T.pack $ show matches
                                ]
    go (RTComponent cname) =
        let allPairs = concatMap
                (\(name, lpv) -> map (name,) $ Set.toList $ lpvComponents lpv)
                (Map.toList locals)
         in case filter (isCompNamed cname . snd) allPairs of
                [] -> Left $ cname `T.append` " doesn't seem to be a local target. Run 'stack ide targets' for a list of available targets"
                [(name, comp)] ->
                    Right (name, (ri, STLocalComps $ Set.singleton comp))
                matches -> Left $ T.concat
                    [ "Ambiugous component name "
                    , cname
                    , ", matches: "
                    , T.pack $ show matches
                    ]

    go (RTPackage name) =
        case Map.lookup name locals of
            Just _lpv -> Right (name, (ri, STLocalAll))
            Nothing
              | Map.member name deps ||
                Map.member name snap ||
                Map.member name globals -> Right (name, (ri, STNonLocal))
              | otherwise -> Right (name, (ri, STUnknown))

isCompNamed :: Text -> NamedComponent -> Bool
isCompNamed _ CLib = False
isCompNamed t1 (CExe t2) = t1 == t2
isCompNamed t1 (CTest t2) = t1 == t2
isCompNamed t1 (CBench t2) = t1 == t2

simplifyTargets :: [(PackageName, (RawInput, SimpleTarget))]
                -> ([Text], Map PackageName SimpleTarget)
simplifyTargets =
    foldMap go . collect
  where
    go :: (PackageName, NonEmpty (RawInput, SimpleTarget))
       -> ([Text], Map PackageName SimpleTarget)
    go (name, (_, st) :| []) = ([], Map.singleton name st)
    go (name, pairs) =
        case partitionEithers $ map (getLocalComp . snd) (NonEmpty.toList pairs) of
            ([], comps) -> ([], Map.singleton name $ STLocalComps $ Set.unions comps)
            _ ->
                let err = T.pack $ concat
                        [ "Overlapping targets provided for package "
                        , packageNameString name
                        , ": "
                        , show $ map (unRawInput . fst) (NonEmpty.toList pairs)
                        ]
                 in ([err], Map.empty)

    collect :: Ord a => [(a, b)] -> [(a, NonEmpty b)]
    collect = map (second NonEmpty.fromList) . groupSort

    getLocalComp (STLocalComps comps) = Right comps
    getLocalComp _ = Left ()

-- | Need targets, e.g. `stack build` or allow none?
data NeedTargets
    = NeedTargets
    | AllowNoTargets

-- | Given the snapshot and local package information from the config
-- files and a list of command line targets, calculate additional
-- local dependencies needed and the simplified view of targets that
-- we actually want to build.
parseTargets :: MonadIO m
             => NeedTargets -- ^ need at least one target?
             -> Bool -- ^ using implicit global project? used for better error reporting
             -> Map PackageName (LoadedPackageInfo GhcPkgId) -- ^ globals
             -> Map PackageName (LoadedPackageInfo (PackageLocationIndex FilePath)) -- ^ snapshot
             -> Map PackageName Version -- ^ local dependencies
             -> Map PackageName LocalPackageView -- ^ names and locations of project packages
             -> Path Abs Dir -- ^ current directory
             -> [Text] -- ^ command line targets
             -> m (Map PackageName Version, Map PackageName SimpleTarget)
parseTargets needTargets implicitGlobal globals snap deps locals currDir textTargets' = do
    let textTargets =
            if null textTargets'
                then map (T.pack . packageNameString) (Map.keys locals)
                else textTargets'
    erawTargets <- mapM (parseRawTargetDirs currDir locals) textTargets

    let (errs1, rawTargets) = partitionEithers erawTargets
        -- When specific package identifiers are provided, treat these
        -- as extra-deps.
        (errs2, unzip -> (rawTargets', newDeps)) = partitionEithers $
            map (resolveIdents globals snap deps locals) $ concat rawTargets
        -- Find targets that specify components in the local packages,
        -- otherwise find package targets in snap and extra-deps.
        (errs3, targetTypes) = partitionEithers $
            map (resolveRawTarget globals snap deps locals) rawTargets'
        (errs4, targets) = simplifyTargets targetTypes
        errs = concat [errs1, errs2, errs3, errs4]

    if null errs
        then if Map.null targets
                 then case needTargets of
                        AllowNoTargets -> return (Map.empty, Map.empty)
                        NeedTargets
                            | null textTargets' && implicitGlobal -> throwIO $ TargetParseException
                                ["The specified targets matched no packages.\nPerhaps you need to run 'stack init'?"]
                            | null textTargets' && Map.null locals -> throwIO $ TargetParseException
                                ["The project contains no local packages (packages not marked with 'extra-dep')"]
                            | otherwise -> throwIO $ TargetParseException
                                ["The specified targets matched no packages"]
                 else return (Map.unions newDeps, targets)
        else throwIO $ TargetParseException errs
