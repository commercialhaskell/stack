{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
-- | Shared types for various stackage packages.
module Stack.Types.BuildPlan
    ( -- * Types
      BuildPlan (..)
    , PackagePlan (..)
    , PackageConstraints (..)
    , TestState (..)
    , SystemInfo (..)
    , Maintainer (..)
    , ExeName (..)
    , SimpleDesc (..)
    , Snapshots (..)
    , DepInfo (..)
    , Component (..)
    , SnapName (..)
    , MiniBuildPlan (..)
    , MiniPackageInfo (..)
    , renderSnapName
    , parseSnapName
    ) where

import           Control.Applicative
import           Control.Arrow                   ((&&&))
import           Control.Exception               (Exception)
import           Control.Monad.Catch             (MonadThrow, throwM)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  object, withObject, withText,
                                                  (.!=), (.:), (.:?), (.=))
import           Data.Binary.VersionTagged
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HashMap
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import           Data.Set                        (Set)
import           Data.String                     (IsString, fromString)
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text                       as T
import Data.Text.Read (decimal)
import           Data.Time                       (Day)
import qualified Data.Traversable                as T
import           Data.Typeable                   (TypeRep, Typeable, typeOf)
import           Data.Vector                     (Vector)
import           Distribution.System             (Arch, OS (..))
import qualified Distribution.Text               as DT
import qualified Distribution.Version            as C
import           GHC.Generics                    (Generic)
import           Prelude -- Fix AMP warning
import           Safe (readMay)
import           Stack.Types.Compiler
import           Stack.Types.FlagName
import           Stack.Types.PackageName
import           Stack.Types.Version

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Show, Eq, Ord)

data BuildPlan = BuildPlan
    { bpSystemInfo  :: SystemInfo
    , bpTools       :: Vector (PackageName, Version)
    , bpPackages    :: Map PackageName PackagePlan
    , bpGithubUsers :: Map Text (Set Text)
    }
    deriving (Show, Eq)

instance ToJSON BuildPlan where
    toJSON BuildPlan {..} = object
        [ "system-info" .= bpSystemInfo
        , "tools" .= fmap goTool bpTools
        , "packages" .= bpPackages
        , "github-users" .= bpGithubUsers
        ]
      where
        goTool (k, v) = object
            [ "name" .= k
            , "version" .= v
            ]
instance FromJSON BuildPlan where
    parseJSON = withObject "BuildPlan" $ \o -> do
        bpSystemInfo <- o .: "system-info"
        bpTools <- o .: "tools" >>= T.mapM goTool
        bpPackages <- o .: "packages"
        bpGithubUsers <- o .:? "github-users" .!= mempty
        return BuildPlan {..}
      where
        goTool = withObject "Tool" $ \o -> (,)
            <$> o .: "name"
            <*> o .: "version"

data PackagePlan = PackagePlan
    { ppVersion     :: Version
    , ppGithubPings :: Set Text
    , ppUsers       :: Set PackageName
    , ppConstraints :: PackageConstraints
    , ppDesc        :: SimpleDesc
    }
    deriving (Show, Eq)

instance ToJSON PackagePlan where
    toJSON PackagePlan {..} = object
        [ "version"      .= ppVersion
        , "github-pings" .= ppGithubPings
        , "users"        .= ppUsers
        , "constraints"  .= ppConstraints
        , "description"  .= ppDesc
        ]
instance FromJSON PackagePlan where
    parseJSON = withObject "PackageBuild" $ \o -> do
        ppVersion <- o .: "version"
        ppGithubPings <- o .:? "github-pings" .!= mempty
        ppUsers <- o .:? "users" .!= mempty
        ppConstraints <- o .: "constraints"
        ppDesc <- o .: "description"
        return PackagePlan {..}

display :: DT.Text a => a -> Text
display = fromString . DT.display

simpleParse :: (MonadThrow m, DT.Text a, Typeable a) => Text -> m a
simpleParse orig = withTypeRep $ \rep ->
    case DT.simpleParse str of
        Nothing -> throwM (ParseFailedException rep (pack str))
        Just v  -> return v
  where
    str = unpack orig

    withTypeRep :: Typeable a => (TypeRep -> m a) -> m a
    withTypeRep f =
        res
      where
        res = f (typeOf (unwrap res))

        unwrap :: m a -> a
        unwrap _ = error "unwrap"

data BuildPlanTypesException
    = ParseSnapNameException Text
    | ParseFailedException TypeRep Text
    deriving Typeable
instance Exception BuildPlanTypesException
instance Show BuildPlanTypesException where
    show (ParseSnapNameException t) = "Invalid snapshot name: " ++ T.unpack t
    show (ParseFailedException rep t) =
        "Unable to parse " ++ show t ++ " as " ++ show rep

data PackageConstraints = PackageConstraints
    { pcVersionRange     :: VersionRange
    , pcMaintainer       :: Maybe Maintainer
    , pcTests            :: TestState
    , pcHaddocks         :: TestState
    , pcBuildBenchmarks  :: Bool
    , pcFlagOverrides    :: Map FlagName Bool
    , pcEnableLibProfile :: Bool
    }
    deriving (Show, Eq)
instance ToJSON PackageConstraints where
    toJSON PackageConstraints {..} = object $ addMaintainer
        [ "version-range" .= display pcVersionRange
        , "tests" .= pcTests
        , "haddocks" .= pcHaddocks
        , "build-benchmarks" .= pcBuildBenchmarks
        , "flags" .= pcFlagOverrides
        , "library-profiling" .= pcEnableLibProfile
        ]
      where
        addMaintainer = maybe id (\m -> (("maintainer" .= m):)) pcMaintainer
instance FromJSON PackageConstraints where
    parseJSON = withObject "PackageConstraints" $ \o -> do
        pcVersionRange <- (o .: "version-range")
                      >>= either (fail . show) return . simpleParse
        pcTests <- o .: "tests"
        pcHaddocks <- o .: "haddocks"
        pcBuildBenchmarks <- o .: "build-benchmarks"
        pcFlagOverrides <- o .: "flags"
        pcMaintainer <- o .:? "maintainer"
        pcEnableLibProfile <- fmap (fromMaybe True) (o .:? "library-profiling")
        return PackageConstraints {..}

data TestState = ExpectSuccess
               | ExpectFailure
               | Don'tBuild -- ^ when the test suite will pull in things we don't want
    deriving (Show, Eq, Ord, Bounded, Enum)

testStateToText :: TestState -> Text
testStateToText ExpectSuccess = "expect-success"
testStateToText ExpectFailure = "expect-failure"
testStateToText Don'tBuild    = "do-not-build"

instance ToJSON TestState where
    toJSON = toJSON . testStateToText
instance FromJSON TestState where
    parseJSON = withText "TestState" $ \t ->
        case HashMap.lookup t states of
            Nothing -> fail $ "Invalid state: " ++ unpack t
            Just v -> return v
      where
        states = HashMap.fromList
               $ map (\x -> (testStateToText x, x)) [minBound..maxBound]

data SystemInfo = SystemInfo
    { siCompilerVersion :: CompilerVersion
    , siOS              :: OS
    , siArch            :: Arch
    , siCorePackages    :: Map PackageName Version
    , siCoreExecutables :: Set ExeName
    }
    deriving (Show, Eq, Ord)
instance ToJSON SystemInfo where
    toJSON SystemInfo {..} = object $
        (case siCompilerVersion of
            GhcVersion version -> "ghc-version" .= version
            _ -> "compiler-version" .= siCompilerVersion) :
        [ "os" .= display siOS
        , "arch" .= display siArch
        , "core-packages" .= siCorePackages
        , "core-executables" .= siCoreExecutables
        ]
instance FromJSON SystemInfo where
    parseJSON = withObject "SystemInfo" $ \o -> do
        let helper name = (o .: name) >>= either (fail . show) return . simpleParse
        ghcVersion <- o .:? "ghc-version"
        compilerVersion <- o .:? "compiler-version"
        siCompilerVersion <-
            case (ghcVersion, compilerVersion) of
                (Just _, Just _) -> fail "can't have both compiler-version and ghc-version fields"
                (Just ghc, _) -> return (GhcVersion ghc)
                (_, Just compiler) -> return compiler
                _ -> fail "expected field \"ghc-version\" or \"compiler-version\" not present"
        siOS <- helper "os"
        siArch <- helper "arch"
        siCorePackages <- o .: "core-packages"
        siCoreExecutables <- o .: "core-executables"
        return SystemInfo {..}

newtype Maintainer = Maintainer { unMaintainer :: Text }
    deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON, IsString)

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString, Generic, Binary, NFData)
instance HasStructuralInfo ExeName
instance ToJSON ExeName where
    toJSON = toJSON . unExeName
instance FromJSON ExeName where
    parseJSON = withText "ExeName" $ return . ExeName

-- | A simplified package description that tracks:
--
-- * Package dependencies
--
-- * Build tool dependencies
--
-- * Provided executables
--
-- It has fully resolved all conditionals
data SimpleDesc = SimpleDesc
    { sdPackages     :: Map PackageName DepInfo
    , sdTools        :: Map ExeName DepInfo
    , sdProvidedExes :: Set ExeName
    , sdModules      :: Set Text
    -- ^ modules exported by the library
    }
    deriving (Show, Eq)
instance Monoid SimpleDesc where
    mempty = SimpleDesc mempty mempty mempty mempty
    mappend (SimpleDesc a b c d) (SimpleDesc w x y z) = SimpleDesc
        (Map.unionWith (<>) a w)
        (Map.unionWith (<>) b x)
        (c <> y)
        (d <> z)
instance ToJSON SimpleDesc where
    toJSON SimpleDesc {..} = object
        [ "packages" .= sdPackages
        , "tools" .= sdTools
        , "provided-exes" .= sdProvidedExes
        , "modules" .= sdModules
        ]
instance FromJSON SimpleDesc where
    parseJSON = withObject "SimpleDesc" $ \o -> do
        sdPackages <- o .: "packages"
        sdTools <- o .: "tools"
        sdProvidedExes <- o .: "provided-exes"
        sdModules <- o .: "modules"
        return SimpleDesc {..}

data DepInfo = DepInfo
    { diComponents :: Set Component
    , diRange      :: VersionRange
    }
    deriving (Show, Eq)

instance Monoid DepInfo where
    mempty = DepInfo mempty C.anyVersion
    DepInfo a x `mappend` DepInfo b y = DepInfo
        (mappend a b)
        (C.intersectVersionRanges x y)
instance ToJSON DepInfo where
    toJSON DepInfo {..} = object
        [ "components" .= diComponents
        , "range" .= display diRange
        ]
instance FromJSON DepInfo where
    parseJSON = withObject "DepInfo" $ \o -> do
        diComponents <- o .: "components"
        diRange <- o .: "range" >>= either (fail . show) return . simpleParse
        return DepInfo {..}

data Component = CompLibrary
               | CompExecutable
               | CompTestSuite
               | CompBenchmark
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

compToText :: Component -> Text
compToText CompLibrary = "library"
compToText CompExecutable = "executable"
compToText CompTestSuite = "test-suite"
compToText CompBenchmark = "benchmark"

instance ToJSON Component where
    toJSON = toJSON . compToText
instance FromJSON Component where
    parseJSON = withText "Component" $ \t -> maybe
        (fail $ "Invalid component: " ++ unpack t)
        return
        (HashMap.lookup t comps)
      where
        comps = HashMap.fromList $ map (compToText &&& id) [minBound..maxBound]

-- | Convert a 'SnapName' into its short representation, e.g. @lts-2.8@,
-- @nightly-2015-03-05@.
renderSnapName :: SnapName -> Text
renderSnapName (LTS x y) = T.pack $ concat ["lts-", show x, ".", show y]
renderSnapName (Nightly d) = T.pack $ "nightly-" ++ show d

-- | Parse the short representation of a 'SnapName'.
parseSnapName :: MonadThrow m => Text -> m SnapName
parseSnapName t0 =
    case lts <|> nightly of
        Nothing -> throwM $ ParseSnapNameException t0
        Just sn -> return sn
  where
    lts = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, t2) <- Just $ decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ decimal t3
        return $ LTS x y
    nightly = do
        t1 <- T.stripPrefix "nightly-" t0
        Nightly <$> readMay (T.unpack t1)

-- | Most recent Nightly and newest LTS version per major release.
data Snapshots = Snapshots
    { snapshotsNightly :: !Day
    , snapshotsLts     :: !(IntMap Int)
    }
    deriving Show
instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \o -> Snapshots
        <$> (o .: "nightly" >>= parseNightly)
        <*> (fmap IntMap.unions
                $ mapM (parseLTS . snd)
                $ filter (isLTS . fst)
                $ HashMap.toList o)
      where
        parseNightly t =
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS _ _) -> fail "Unexpected LTS value"
                Right (Nightly d) -> return d

        isLTS = ("lts-" `T.isPrefixOf`)

        parseLTS = withText "LTS" $ \t ->
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS x y) -> return $ IntMap.singleton x y
                Right (Nightly _) -> fail "Unexpected nightly value"

instance ToJSON a => ToJSON (Map ExeName a) where
  toJSON = toJSON . Map.mapKeysWith const unExeName
instance FromJSON a => FromJSON (Map ExeName a) where
    parseJSON = fmap (Map.mapKeysWith const ExeName) . parseJSON

-- | A simplified version of the 'BuildPlan' + cabal file.
data MiniBuildPlan = MiniBuildPlan
    { mbpCompilerVersion :: !CompilerVersion
    , mbpPackages :: !(Map PackageName MiniPackageInfo)
    }
    deriving (Generic, Show, Eq)
instance Binary MiniBuildPlan
instance NFData MiniBuildPlan
instance HasStructuralInfo MiniBuildPlan
instance HasSemanticVersion MiniBuildPlan

-- | Information on a single package for the 'MiniBuildPlan'.
data MiniPackageInfo = MiniPackageInfo
    { mpiVersion :: !Version
    , mpiFlags :: !(Map FlagName Bool)
    , mpiPackageDeps :: !(Set PackageName)
    , mpiToolDeps :: !(Set Text)
    -- ^ Due to ambiguity in Cabal, it is unclear whether this refers to the
    -- executable name, the package name, or something else. We have to guess
    -- based on what's available, which is why we store this is an unwrapped
    -- 'Text'.
    , mpiExes :: !(Set ExeName)
    -- ^ Executables provided by this package
    , mpiHasLibrary :: !Bool
    -- ^ Is there a library present?
    }
    deriving (Generic, Show, Eq)
instance Binary MiniPackageInfo
instance HasStructuralInfo MiniPackageInfo
instance NFData MiniPackageInfo
