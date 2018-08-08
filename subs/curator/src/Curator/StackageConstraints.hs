{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- | Deal with the @build-constraints.yaml@ format used by
-- @commercialhaskell/stackage@.
module Curator.StackageConstraints
  ( loadStackageConstraints
  ) where

import Pantry
import Curator.Types
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Distribution.Types.VersionRange (VersionRange, anyVersion, intersectVersionRanges, normaliseVersionRange)
import Data.Yaml
import Distribution.Text (simpleParse)

data SC = SC
  { scGhcVersion :: !Version
  -- ^ GHC version to use

  , scPackages :: !(Map PackageName (Set Maintainer, [VersionRange]))
  -- ^ Packages to include

  , scFlags :: !(Map PackageName (Map FlagName Bool))
  -- ^ Flags for those packages

  -- FIXME let's see if we can work around this with changes to the Docker image
  -- , scConfigureArgs :: !(Map PackageName [Text])

  , scSkippedBuilds :: !(Set PackageName)
  -- ^ Include the package in the snapshot, but don't build
  -- it. Intended for Windows-specific packages.

  , scSkippedTests :: !(Set PackageName)
  -- ^ Don't even try to build the tests, for out-of-bounds dependencies

  , scExpectedTestFailures :: !(Set PackageName)
  -- ^ Test suites which are expected to fail. Run them, but don't
  -- error out if they fail.

  , scSkippedBenchmarks :: !(Set PackageName)
  -- ^ Like 'scSkippedTests'

  , scExpectedBenchmarkFailures :: !(Set PackageName)
  -- ^ Like 'scExepctedTestFailures'

  , scExpectedHaddockFailures :: !(Set PackageName)
  -- ^ Haddocks don't build successfully

  , scSkippedHaddocks :: !(Set PackageName)
  -- ^ Sometimes Haddock is really flaky

  -- FIXME deal with all of the github-users and ping logic

  , scTellMeWhenItsReleased :: !(Map PackageName Version)

  , scHide :: !(Set PackageName)

  , scNoRevisions :: !(Set PackageName)

  , scNonParallelBuilds :: !(Set PackageName)
  }
  deriving Show

instance FromJSON SC where
  parseJSON = withObject "StackageConstraints" $ \o -> do
    CabalString scGhcVersion <- o .: "ghc-version"
    scPackages <- convertPackages <$> o .: "packages"
    scFlags <- fmap unCabalStringMap . unCabalStringMap <$> o .: "package-flags"

    scSkippedBuilds <- Set.map unCabalString <$> o .: "skipped-builds"

    scSkippedTests <- Set.map unCabalString <$> o .: "skipped-tests"
    scSkippedBenchmarks <- Set.map unCabalString <$> o .: "skipped-benchmarks"
    scSkippedHaddocks <- Set.map unCabalString <$> o .: "skipped-haddocks"

    scExpectedTestFailures <- Set.map unCabalString <$> o .: "expected-test-failures"
    scExpectedBenchmarkFailures <- Set.map unCabalString <$> o .: "expected-benchmark-failures"
    scExpectedHaddockFailures <- Set.map unCabalString <$> o .: "expected-haddock-failures"

    scHide <- Set.map unCabalString <$> o .: "hide"
    scNoRevisions <- Set.map unCabalString <$> o .: "no-revisions"
    scTellMeWhenItsReleased <-
          mconcat
        . map (\(CabalString (PackageIdentifier name version)) -> Map.singleton name version)
      <$> o .: "tell-me-when-its-released"
    scNonParallelBuilds <- Set.map unCabalString <$> o .: "non-parallel-builds"

    pure SC {..}

data PackageRange = PackageRange !PackageName !(Maybe VersionRange)
instance FromJSON PackageRange where
  parseJSON = withText "PackageRange" $ \t -> do
    let s = T.unpack t
    maybe (fail $ "Invalid PackageRange: " ++ s) pure $ do
      let (nameT, T.strip -> rangeT) = T.break (== ' ') t
      name <- simpleParse $ T.unpack nameT
      mrange <-
        if T.null rangeT
          then Just Nothing
          else fmap Just $ simpleParse $ T.unpack rangeT
      pure $ PackageRange name mrange

convertPackages
  :: Map Maintainer [PackageRange]
  -> Map PackageName (Set Maintainer, [VersionRange])
convertPackages =
    Map.fromListWith combine . concatMap go . Map.toList
  where
    go (maintainer, prs) = map
      (\(PackageRange name mrange) ->
         ( name
         , ( Set.singleton maintainer
           , maybeToList mrange
           )
         )
      )
      prs

    combine (a, x) (b, y) = (a <> b, x <> y)

loadStackageConstraints :: FilePath -> RIO env Constraints
loadStackageConstraints = decodeFileThrow >=> convert

convert :: SC -> RIO env Constraints
convert sc0 = do
  let (sc1, packages, errs) =
        foldl'
          go
          (sc0, mempty, [])
          (Map.toList (scPackages sc0))
  unless (null errs) $ error $ unlines errs
  -- check that all of the fields are empty now
  pure Constraints
    { consGhcVersion = scGhcVersion sc1
    , consPackages = packages
    }
  where
    go :: (SC, Map PackageName PackageConstraints, [String])
       -> (PackageName, (Set Maintainer, [VersionRange]))
       -> (SC, Map PackageName PackageConstraints, [String])
    go (sc1, m, errs) (name, (maintainers, ranges)) =
      case res of
        Left e -> (sc2, m, e : errs)
        Right pc -> (sc2, Map.insert name pc m, errs)
      where
        sc2 = sc1
          { scTellMeWhenItsReleased = Map.delete name $ scTellMeWhenItsReleased sc1
          , scNoRevisions = Set.delete name $ scNoRevisions sc1
          , scFlags = Map.delete name $ scFlags sc1
          , scSkippedBuilds = Set.delete name $ scSkippedBuilds sc1
          , scNonParallelBuilds = Set.delete name $ scNonParallelBuilds sc1
          , scExpectedTestFailures = Set.delete name $ scExpectedTestFailures sc1
          , scSkippedTests = Set.delete name $ scSkippedTests sc1
          , scExpectedBenchmarkFailures = Set.delete name $ scExpectedBenchmarkFailures sc1
          , scSkippedBenchmarks = Set.delete name $ scSkippedBenchmarks sc1
          , scExpectedHaddockFailures = Set.delete name $ scExpectedHaddockFailures sc1
          , scSkippedHaddocks = Set.delete name $ scSkippedHaddocks sc1
          , scHide = Set.delete name $ scHide sc1
          }
        res = do
          tests <-
            case (Set.member name $ scExpectedTestFailures sc1, Set.member name $ scSkippedTests sc1) of
              (False, False) -> Right CAExpectSuccess
              (True, False) -> Right CAExpectFailure
              (False, True) -> Right CASkip
              (True, True) -> Right CASkip -- Left $ "Cannot skip and expect test failure: " ++ displayC name

          benchmarks <-
            case (Set.member name $ scExpectedBenchmarkFailures sc1, Set.member name $ scSkippedBenchmarks sc1) of
              (False, False) -> Right CAExpectSuccess
              (True, False) -> Right CAExpectFailure
              (False, True) -> Right CASkip
              (True, True) -> Right CASkip -- Left $ "Cannot skip and expect benchmark failure: " ++ displayC name

          haddock <-
            case (Set.member name $ scExpectedHaddockFailures sc1, Set.member name $ scSkippedHaddocks sc1) of
              (False, False) -> Right CAExpectSuccess
              (True, False) -> Right CAExpectFailure
              (False, True) -> Right CASkip
              (True, True) -> Right CASkip -- Left $ "Cannot skip and expect haddock failure: " ++ displayC name

          Right PackageConstraints
            { pcMaintainers = maintainers
            , pcSource = PSHackage $ HackageSource
                { hsRange =
                    case ranges of
                      [] -> Nothing
                      r:rs -> Just $ foldl' intersectVersionRanges r rs
                , hsRequiredLatest = Map.lookup name (scTellMeWhenItsReleased sc1)
                , hsRevisions =
                    if Set.member name (scNoRevisions sc1)
                      then NoRevisions
                      else UseRevisions
                }
            , pcFlags = fromMaybe mempty $ Map.lookup name $ scFlags sc1
            , pcSkipBuild = Set.member name $ scSkippedBuilds sc1
            , pcNonParallelBuild = Set.member name $ scNonParallelBuilds sc1
            , pcTests = tests
            , pcBenchmarks = benchmarks
            , pcHaddock = haddock
            , pcHide = Set.member name $ scHide sc1
            }
