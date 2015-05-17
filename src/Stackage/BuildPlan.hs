{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Resolving a build plan for a set of packages in a given Stackage
-- snapshot.

module Stackage.BuildPlan
    ( SnapName (..)
    , renderSnapName
    , parseSnapName
    , BuildPlanException (..)
    , Snapshots (..)
    , getSnapshots
    , allPackages
    , loadBuildPlan
    ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson (FromJSON (..))
import           Data.Set (Set)
import           Stackage.Config
import           Stackage.PackageIdentifier
import           "stackage-types" Stackage.Types       (BuildPlan, bpPackages, bpSystemInfo, display, ppVersion, siCorePackages, siGhcVersion)
import Path
import Stackage.Package
import Stackage.PackageName
import Stackage.FlagName
import           Control.Applicative                   ((<$>), (<*>), (<|>))
import           Control.Exception                     (Exception, assert)
import           Control.Monad                         (unless)
import           Control.Monad.Catch                   (MonadThrow, throwM)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (MonadLogger, logDebug)
import           Control.Monad.Trans.Resource          (runResourceT)
import           Data.Aeson                            (FromJSON (..),
                                                        withObject, withText,
                                                        (.:))
import           Data.Aeson.Parser                     (json')
import           Data.Aeson.Types                      (parseEither)
import qualified Data.ByteString                       as S
import           Data.Conduit                          (($$))
import           Data.Conduit.Attoparsec               (sinkParser)
import qualified Data.Conduit.Binary                   as CB
import qualified Data.HashMap.Strict                   as HM
import           Data.IntMap                           (IntMap)
import qualified Data.IntMap                           as IntMap
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (mapMaybe)
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (decodeUtf8With)
import           Data.Text.Encoding.Error              (lenientDecode)
import           Data.Text.Read                        (decimal)
import           Data.Time                             (Day)
import           Data.Typeable                         (Typeable)
import           Data.Version                          (Version)
import           Data.Yaml                             (decodeFileEither)
import           Distribution.Compiler                 (CompilerFlavor (GHC))
import           Distribution.InstalledPackageInfo     (PError)
import           Distribution.PackageDescription       (GenericPackageDescription, genPackageFlags, flagManual, flagName, flagDefault)
import           Distribution.PackageDescription.Parse (ParseResult (..),
                                                        parsePackageDescription)
import           Distribution.System                   (buildArch, buildOS)
import           Distribution.Version                  (VersionRange,
                                                        intersectVersionRanges,
                                                        withinRange)
import           Network.HTTP.Client                   (Manager, parseUrl,
                                                        responseBody,
                                                        withResponse)
import           Network.HTTP.Client.Conduit           (bodyReaderSource)
import           Safe                                  (readMay)
import           "stackage-types" Stackage.Types       (BuildPlan, bpPackages, bpSystemInfo, display, ppVersion, siCorePackages, siGhcVersion)
import           System.Directory                      (createDirectoryIfMissing,
                                                        getAppUserDataDirectory,
                                                        getDirectoryContents)
import           System.FilePath                       (takeDirectory,
                                                        takeExtension, (<.>))
import qualified System.FilePath as FP

data BuildPlanException
    = ParseSnapNameException Text
    | GetSnapshotsException String
    deriving (Show, Typeable)
instance Exception BuildPlanException

-- | Includes things like flags, the current package database
-- location, etc.
data BuildPlanConfig

resolveBuildPlan :: (MonadLogger m,MonadIO m,MonadThrow m)
                 => Config
                 -> BuildPlanConfig
                 -> Set PackageIdentifier
                 -> m (Set PackageIdentifier)
resolveBuildPlan = undefined

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Show, Eq, Ord)

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

-- | Download the 'Snapshots' value from stackage.org.
getSnapshots :: MonadIO m => Manager -> m Snapshots
getSnapshots man = liftIO $ withResponse req man $ \res -> do
    val <- bodyReaderSource (responseBody res) $$ sinkParser json'
    case parseEither parseJSON val of
        Left e -> throwM $ GetSnapshotsException e
        Right x -> return x
  where
    req = "https://www.stackage.org/download/snapshots.json"

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
                $ mapM parseLTS
                $ map snd
                $ filter (isLTS . fst)
                $ HM.toList o)
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

-- | Load the 'BuildPlan' for the given snapshot. Will load from a local copy
-- if available, otherwise downloading from Github.
loadBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m)
              => Manager
              -> SnapName
              -> m BuildPlan
loadBuildPlan man name = do
    stackage <- liftIO $ getAppUserDataDirectory "stackage"
    let fp = stackage FP.</> "build-plan" FP.</> T.unpack (renderSnapName name) <.> "yaml"
    $logDebug $ "Decoding build plan from: " <> T.pack fp
    eres <- liftIO $ decodeFileEither fp
    case eres of
        Right bp -> return bp
        Left e -> do
            $logDebug $ "Decoding failed: " <> T.pack (show e)
            liftIO $ createDirectoryIfMissing True $ takeDirectory fp
            req <- parseUrl $ T.unpack url
            $logDebug $ "Downloading build plan from: " <> url
            liftIO $ withResponse req man $ \res ->
                   runResourceT
                 $ bodyReaderSource (responseBody res)
                $$ CB.sinkFile fp
            liftIO (decodeFileEither fp) >>= either throwM return
  where
    url = T.concat
        [ "https://raw.githubusercontent.com/fpco/"
        , reponame
        , "/master/"
        , renderSnapName name
        , ".yaml"
        ]
    reponame =
        case name of
            LTS _ _ -> "lts-haskell"
            Nightly _ -> "stackage-nightly"

-- | Get all packages present in the given build plan, including both core and
-- non-core.
allPackages :: BuildPlan -> Map PackageName Version
allPackages bp =
    Map.mapKeysWith const fromCabalPackageName $
    siCorePackages (bpSystemInfo bp) <>
    fmap ppVersion (bpPackages bp)
