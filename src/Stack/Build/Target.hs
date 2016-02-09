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
import           Control.Arrow          (second)
import           Control.Monad.Catch    (MonadCatch, throwM)
import           Control.Monad.IO.Class
import           Data.Either            (partitionEithers)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Data.Monoid
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Path
import           Path.IO
import           Prelude -- Fix redundant import warnings
import           Stack.Types

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
    RTPackageIdentifier :: !PackageIdentifier -> RawTarget 'HasIdents

deriving instance Show (RawTarget a)
deriving instance Eq (RawTarget a)
deriving instance Ord (RawTarget a)

data RawTargetType = HasIdents | NoIdents

-- | If this function returns @Nothing@, the input should be treated as a
-- directory.
parseRawTarget :: Text -> Maybe (RawTarget 'HasIdents)
parseRawTarget t =
        (RTPackageIdentifier <$> parsePackageIdentifierFromString s)
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

-- | A view of a local package needed for resolving components
data LocalPackageView = LocalPackageView
    { lpvVersion    :: !Version
    , lpvRoot       :: !(Path Abs Dir)
    , lpvCabalFP    :: !(Path Abs File)
    , lpvComponents :: !(Set NamedComponent)
    , lpvExtraDep   :: !Bool
    }

-- | Same as @parseRawTarget@, but also takes directories into account.
parseRawTargetDirs :: (MonadIO m, MonadCatch m)
                   => Path Abs Dir -- ^ current directory
                   -> Map PackageName LocalPackageView
                   -> Text
                   -> m (Either Text [(RawInput, RawTarget 'HasIdents)])
parseRawTargetDirs root locals t =
    case parseRawTarget t of
        Just rt -> return $ Right [(ri, rt)]
        Nothing -> do
            mdir <- forgivingAbsence (resolveDir root (T.unpack t))
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
        if (dir == lpvRoot lpv || isParentOf dir (lpvRoot lpv)) && not (lpvExtraDep lpv)
            then Just name
            else Nothing

data SimpleTarget
    = STUnknown
    | STNonLocal
    | STLocalComps !(Set NamedComponent)
    | STLocalAll
    deriving (Show, Eq, Ord)

resolveIdents :: Map PackageName Version -- ^ snapshot
              -> Map PackageName Version -- ^ extra deps
              -> Map PackageName LocalPackageView
              -> (RawInput, RawTarget 'HasIdents)
              -> Either Text ((RawInput, RawTarget 'NoIdents), Map PackageName Version)
resolveIdents _ _ _ (ri, RTPackageComponent x y) = Right ((ri, RTPackageComponent x y), Map.empty)
resolveIdents _ _ _ (ri, RTComponent x) = Right ((ri, RTComponent x), Map.empty)
resolveIdents _ _ _ (ri, RTPackage x) = Right ((ri, RTPackage x), Map.empty)
resolveIdents snap extras locals (ri, RTPackageIdentifier (PackageIdentifier name version)) =
    case mfound of
        Just (foundPlace, foundVersion) | foundVersion /= version -> Left $ T.pack $ concat
            [ "Specified target version "
            , versionString version
            , " for package "
            , packageNameString name
            , " does not match "
            , foundPlace
            , " version "
            , versionString foundVersion
            ]
        _ -> Right
            ( (ri, RTPackage name)
            , case mfound of
                -- Add to extra deps since we didn't have it already
                Nothing -> Map.singleton name version
                -- Already had it, don't add to extra deps
                Just _ -> Map.empty
            )
  where
    mfound = mlocal <|> mextra <|> msnap

    mlocal = (("local", ) . lpvVersion) <$> Map.lookup name locals
    mextra = ("extra-deps", ) <$> Map.lookup name extras
    msnap = ("snapshot", ) <$> Map.lookup name snap

resolveRawTarget :: Map PackageName Version -- ^ snapshot
                 -> Map PackageName Version -- ^ extra deps
                 -> Map PackageName LocalPackageView
                 -> (RawInput, RawTarget 'NoIdents)
                 -> Either Text (PackageName, (RawInput, SimpleTarget))
resolveRawTarget snap extras locals (ri, rt) =
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
                [] -> Left $ "Could not find a component named " `T.append` cname
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
            Nothing ->
                case Map.lookup name extras of
                    Just _ -> Right (name, (ri, STNonLocal))
                    Nothing ->
                        case Map.lookup name snap of
                            Just _ -> Right (name, (ri, STNonLocal))
                            Nothing -> Right (name, (ri, STUnknown))

isCompNamed :: Text -> NamedComponent -> Bool
isCompNamed _ CLib = False
isCompNamed t1 (CExe t2) = t1 == t2
isCompNamed t1 (CTest t2) = t1 == t2
isCompNamed t1 (CBench t2) = t1 == t2

simplifyTargets :: [(PackageName, (RawInput, SimpleTarget))]
                -> ([Text], Map PackageName SimpleTarget)
simplifyTargets =
    mconcat . map go . Map.toList . Map.fromListWith (++) . fmap (second return)
  where
    go :: (PackageName, [(RawInput, SimpleTarget)])
       -> ([Text], Map PackageName SimpleTarget)
    go (_, []) = error "Stack.Build.Target.simplifyTargets: the impossible happened"
    go (name, [(_, st)]) = ([], Map.singleton name st)
    go (name, pairs) =
        case partitionEithers $ map (getLocalComp . snd) pairs of
            ([], comps) -> ([], Map.singleton name $ STLocalComps $ Set.unions comps)
            _ ->
                let err = T.pack $ concat
                        [ "Overlapping targets provided for package "
                        , packageNameString name
                        , ": "
                        , show $ map (unRawInput . fst) pairs
                        ]
                 in ([err], Map.empty)

    getLocalComp (STLocalComps comps) = Right comps
    getLocalComp _ = Left ()

-- | Need targets, e.g. `stack build` or allow none?
data NeedTargets
    = NeedTargets
    | AllowNoTargets

parseTargets :: (MonadCatch m, MonadIO m)
             => NeedTargets -- ^ need at least one target
             -> Bool -- ^ using implicit global project?
             -> Map PackageName Version -- ^ snapshot
             -> Map PackageName Version -- ^ extra deps
             -> Map PackageName LocalPackageView
             -> Path Abs Dir -- ^ current directory
             -> [Text] -- ^ command line targets
             -> m (Map PackageName Version, Map PackageName SimpleTarget)
parseTargets needTargets implicitGlobal snap extras locals currDir textTargets' = do
    let textTargets =
            if null textTargets'
                then map (T.pack . packageNameString) $ Map.keys $ Map.filter (not . lpvExtraDep) locals
                else textTargets'
    erawTargets <- mapM (parseRawTargetDirs currDir locals) textTargets

    let (errs1, rawTargets) = partitionEithers erawTargets
        (errs2, unzip -> (rawTargets', newExtras)) = partitionEithers $
            map (resolveIdents snap extras locals) $ concat rawTargets
        (errs3, targetTypes) = partitionEithers $
            map (resolveRawTarget snap extras locals) rawTargets'
        (errs4, targets) = simplifyTargets targetTypes
        errs = concat [errs1, errs2, errs3, errs4]

    if null errs
        then if Map.null targets
                 then case needTargets of
                        AllowNoTargets ->
                            return (Map.empty, Map.empty)
                        NeedTargets ->
                            throwM $ TargetParseException
                              $ if implicitGlobal
                                  then ["The specified targets matched no packages.\nPerhaps you need to run 'stack init'?"]
                                  else ["The specified targets matched no packages"]
                 else return (Map.unions newExtras, targets)
        else throwM $ TargetParseException errs
