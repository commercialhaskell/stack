{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE TemplateHaskell    #-}
module Stack.CreateConfigFile
    ( SnapName (..)
    , getCabalFileName
    , parseCabalFile
    , findBuildPlan
    , CreateConfigFileException (..)
    ) where

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
import           Distribution.Package                  (Dependency (Dependency), PackageName (PackageName))
import           Distribution.PackageDescription       (CondTree, CondTree (..),
                                                        Condition (..), ConfVar,
                                                        ConfVar (..), FlagName, GenericPackageDescription,
                                                        condBenchmarks,
                                                        condExecutables,
                                                        condLibrary,
                                                        condTestSuites,
                                                        flagDefault, flagManual,
                                                        flagName,
                                                        genPackageFlags)
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
                                                        takeExtension, (<.>),
                                                        (</>))

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Show, Eq, Ord)

renderSnapName :: SnapName -> Text
renderSnapName (LTS x y) = T.pack $ concat ["lts-", show x, ".", show y]
renderSnapName (Nightly d) = T.pack $ "nightly-" ++ show d

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

data CreateConfigFileException
    = NoCabalFileFound FilePath
    | MultipleCabalFilesFound FilePath [FilePath]
    | ParseSnapNameException Text
    | GetSnapshotsException String
    | BadCabalFile FilePath PError
    deriving (Show, Typeable)
instance Exception CreateConfigFileException

getSnapshots :: MonadIO m => Manager -> m Snapshots
getSnapshots man = liftIO $ withResponse req man $ \res -> do
    val <- bodyReaderSource (responseBody res) $$ sinkParser json'
    case parseEither parseJSON val of
        Left e -> throwM $ GetSnapshotsException e
        Right x -> return x
  where
    req = "https://www.stackage.org/download/snapshots.json"

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

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
getCabalFileName
    :: (MonadThrow m, MonadIO m)
    => FilePath -- ^ project directory
    -> m FilePath
getCabalFileName dir = do
    files <- liftIO $ filter isCabal <$> getDirectoryContents dir
    case files of
        [] -> throwM $ NoCabalFileFound dir
        [x] -> return $ dir </> x
        _:_ -> throwM $ MultipleCabalFilesFound dir files
  where
    isCabal fp = takeExtension fp == ".cabal"

-- | Parse a cabal file.
parseCabalFile :: (MonadThrow m, MonadIO m, MonadLogger m)
               => FilePath
               -> m GenericPackageDescription
parseCabalFile fp = do
    $logDebug $ "Parsing cabal file: " <> T.pack fp
    bs <- liftIO $ S.readFile fp
    let t = decodeUtf8With lenientDecode bs
    case parsePackageDescription $ T.unpack t of
        ParseFailed e -> throwM $ BadCabalFile fp e
        ParseOk ws x -> do
            unless (null ws) $ $logDebug $ T.concat
                [ "Warnings when parsing "
                , T.pack fp
                , ": "
                , T.pack $ show ws
                ]
            return x

loadBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m)
              => Manager
              -> SnapName
              -> m BuildPlan
loadBuildPlan man name = do
    stackage <- liftIO $ getAppUserDataDirectory "stackage"
    let fp = stackage </> "build-plan" </> T.unpack (renderSnapName name) <.> "yaml"
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

checkBuildPlan :: MonadLogger m
               => SnapName
               -> BuildPlan
               -> GenericPackageDescription
               -> m (Maybe (Map FlagName Bool))
checkBuildPlan name bp gpd = do
    $logDebug $ "Checking against build plan " <> renderSnapName name
    loop flagOptions
  where
    loop [] = return Nothing
    loop (flags:rest) = do
        passes <- checkDeps flags deps packages
        if passes
            then return $ Just flags
            else loop rest
      where
        deps = flattenDeps True True gpd ghcVersion flags

    ghcVersion = siGhcVersion $ bpSystemInfo bp

    flagOptions = map Map.fromList $ mapM getOptions $ genPackageFlags gpd
    getOptions f
        | flagManual f = [(flagName f, flagDefault f)]
        | flagDefault f =
            [ (flagName f, True)
            , (flagName f, False)
            ]
        | otherwise =
            [ (flagName f, False)
            , (flagName f, True)
            ]
    packages = allPackages bp

checkDeps :: MonadLogger m
          => Map FlagName Bool
          -> Map PackageName VersionRange
          -> Map PackageName Version
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
    go (name@(PackageName name'), range) =
        case Map.lookup name packages of
            Nothing -> Just $ "Package not present: " <> T.pack name'
            Just v
                | withinRange v range -> Nothing
                | otherwise -> Just $ T.concat
                    [ "Version available: "
                    , display v
                    , " does not match "
                    , display range
                    ]

flattenDeps :: Bool -- ^ include tests?
            -> Bool -- ^ include benchmarks?
            -> GenericPackageDescription
            -> Version -- ^ GHC version
            -> Map FlagName Bool
            -> Map PackageName VersionRange
flattenDeps tests benchs gpd ghcVersion flags = combine
    [ maybe Map.empty (goTree ghcVersion flags) (condLibrary gpd)
    , combine $ map (goTree ghcVersion flags . snd) (condExecutables gpd)
    , if tests
        then combine $ map (goTree ghcVersion flags . snd) (condTestSuites gpd)
        else Map.empty
    , if benchs
        then combine $ map (goTree ghcVersion flags . snd) (condBenchmarks gpd)
        else Map.empty
    ]
  where
    combine = Map.unionsWith intersectVersionRanges

goTree :: Version -- ^ GHC version
       -> Map FlagName Bool
       -> CondTree ConfVar [Dependency] ignored
       -> Map PackageName VersionRange
goTree ghcVersion flags =
    goTree'
  where
    goTree' (CondNode _ deps comps) =
        Map.unionsWith intersectVersionRanges
            $ goDeps deps
            : map goComps comps

    goDeps = Map.fromList . map (\(Dependency k v) -> (k, v))

    goComps (cond, onTrue, onFalse)
        | checkCond cond = goTree' onTrue
        | otherwise = maybe Map.empty goTree' onFalse

    checkCond (Var (OS os)) = os == buildOS
    checkCond (Var (Arch arch)) = arch == buildArch
    checkCond (Var (Flag f)) =
        case Map.lookup f flags of
            Nothing -> assert False False
            Just x -> x
    checkCond (Var (Impl flavor range)) =
        flavor == GHC && withinRange ghcVersion range

    checkCond (Lit b) = b
    checkCond (CNot c) = not $ checkCond c
    checkCond (COr x y) = checkCond x || checkCond y
    checkCond (CAnd x y) = checkCond x && checkCond y

allPackages :: BuildPlan -> Map PackageName Version
allPackages bp =
    siCorePackages (bpSystemInfo bp) <>
    fmap ppVersion (bpPackages bp)

-- | Find a snapshot and set of flags that is compatible with the given
-- 'GenericPackageDescription'. Returns 'Nothing' if no such snapshot is found.
findBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m)
              => Manager
              -> GenericPackageDescription
              -> m (Maybe (SnapName, Map FlagName Bool))
findBuildPlan manager gpd = do
    snapshots <- liftIO $ getSnapshots manager
    let names =
            map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            ++ [Nightly $ snapshotsNightly snapshots]
        loop [] = return Nothing
        loop (name:names') = do
            bp <- loadBuildPlan manager name
            mflags <- checkBuildPlan name bp gpd
            case mflags of
                Nothing -> loop names'
                Just flags -> return $ Just (name, flags)
    loop names

{- Sample usage:

main :: IO ()
main = runStdoutLoggingT $ do
    gpd <- getCabalFileName "." >>= parseCabalFile
    manager <- liftIO $ newManager tlsManagerSettings
    findBuildPlan manager gpd >>= liftIO . print

-}
