{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE TemplateHaskell    #-}
module Stack.CreateConfigFile
    ( SnapName (..)
    , checkBuildPlan
    , findBuildPlan
    , checkDeps
    ) where

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
import           Stackage.BuildPlan.Types
import           System.Directory                      (createDirectoryIfMissing,
                                                        getAppUserDataDirectory,
                                                        getDirectoryContents)
import           System.FilePath                       (takeDirectory,
                                                        takeExtension, (<.>))
import qualified System.FilePath as FP
import Stackage.BuildPlan
import Stackage.PackageVersion

-- | Find the set of @FlagName@s necessary to get the given
-- @GenericPackageDescription@ to compile against the given @BuildPlan@. Will
-- only modify non-manual flags, and will prefer default values for flags.
-- Returns @Nothing@ if no combination exists.
checkBuildPlan :: (MonadLogger m, MonadThrow m, MonadIO m)
               => SnapName -- ^ used only for debugging purposes
               -> BuildPlan
               -> Path Abs File -- ^ cabal file path, used only for debugging purposes
               -> GenericPackageDescription
               -> m (Maybe [FlagName])
checkBuildPlan name bp cabalfp gpd = do
    $logDebug $ "Checking against build plan " <> renderSnapName name
    loop flagOptions
  where
    loop [] = return Nothing
    loop (flags:rest) = do
        pkg <- resolvePackage pkgConfig cabalfp gpd
        passes <- checkDeps flags (packageDeps pkg) packages
        if passes
            then return $ Just $ map fst $ filter snd $ Map.toList flags
            else loop rest
      where
        pkgConfig = PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = flags
            , packageConfigGhcVersion = ghcVersion
            }

    ghcVersion = siGhcVersion $ bpSystemInfo bp

    flagName' = fromCabalFlagName . flagName

    flagOptions = map Map.fromList $ mapM getOptions $ genPackageFlags gpd
    getOptions f
        | flagManual f = [(flagName' f, flagDefault f)]
        | flagDefault f =
            [ (flagName' f, True)
            , (flagName' f, False)
            ]
        | otherwise =
            [ (flagName' f, False)
            , (flagName' f, True)
            ]
    packages = allPackages bp

-- | Checks if the given package dependencies can be satisfied by the given set
-- of packages. Will fail if a package is either missing or has a version
-- outside of the version range.
checkDeps :: MonadLogger m
          => Map FlagName Bool -- ^ used only for debugging purposes
          -> Map PackageName VersionRange
          -> Map PackageName PackageVersion
          -> m Bool
checkDeps flags deps packages = do
    let errs = mapMaybe go $ Map.toList deps
    if null errs
        then return True
        else do
            $logDebug $ "Checked against following flags: " <> T.pack (show flags)
            mapM_ $logDebug errs
            return False
  where
    go :: (PackageName, VersionRange) -> Maybe Text
    go (name, range) =
        case Map.lookup name packages of
            Nothing -> Just $ "Package not present: " <> packageNameText name
            Just v
                | withinRange (toCabalVersion v) range -> Nothing
                | otherwise -> Just $ T.concat
                    [ packageNameText name
                    , " version available: "
                    , packageVersionText v
                    , " does not match "
                    , versionRangeText range
                    ]

-- | Find a snapshot and set of flags that is compatible with the given
-- 'GenericPackageDescription'. Returns 'Nothing' if no such snapshot is found.
findBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m)
              => Manager
              -> Path Abs File
              -> GenericPackageDescription
              -> m (Maybe (SnapName, [FlagName]))
findBuildPlan manager cabalfp gpd = do
    snapshots <- liftIO $ getSnapshots manager
    let names =
            map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            ++ [Nightly $ snapshotsNightly snapshots]
        loop [] = return Nothing
        loop (name:names') = do
            bp <- loadBuildPlan manager name
            mflags <- checkBuildPlan name bp cabalfp gpd
            case mflags of
                Nothing -> loop names'
                Just flags -> return $ Just (name, flags)
    loop names
