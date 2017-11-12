{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Ls
  ( lsCmd
  , lsParser
  ) where

import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson
import qualified Data.Aeson.Types as A
import qualified Data.List as L
import Data.Monoid
import Data.Text
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Network.HTTP.Simple
       (addRequestHeader, getResponseBody, httpJSON, parseRequest,
        setRequestManager)
import Network.HTTP.Types.Header (hAccept)
import qualified Options.Applicative as OA
import Path
import Stack.Runners (withBuildConfig)
import Stack.Types.Config
import System.Console.ANSI
import System.Directory (listDirectory)
import Network.HTTP.Client.TLS (getGlobalManager)

data LsView
    = Local
    | Remote
    deriving (Show, Eq, Ord)

data SnapshotType
    = Lts
    | Nightly
    deriving (Show, Eq, Ord)

data LsCmds = LsSnapshot SnapshotOpts deriving (Eq, Show, Ord)

data SnapshotOpts = SnapshotOpts {
      soptViewType :: LsView
    , soptLtsSnapView :: Bool      
    , soptNightlySnapView :: Bool
} deriving (Eq, Show, Ord)

data LsCmdOpts = LsCmdOpts
    { lsView :: LsCmds
    } deriving (Eq, Show, Ord)

lsParser :: OA.Parser LsCmdOpts
lsParser = LsCmdOpts <$> OA.hsubparser lsSnapCmd

lsCmdOptsParser :: OA.Parser LsCmds
lsCmdOptsParser = fmap LsSnapshot lsViewSnapCmd

lsViewSnapCmd :: OA.Parser SnapshotOpts
lsViewSnapCmd = SnapshotOpts <$> OA.hsubparser (lsViewRemoteCmd <> lsViewLocalCmd) <*> OA.switch (OA.long "some-flag" <> OA.help "Set the flag") <*> OA.switch (OA.long "ome-flag" <> OA.help "Set the flag")

lsSnapCmd :: OA.Mod OA.CommandFields LsCmds
lsSnapCmd = OA.command "snapshots" (OA.info lsCmdOptsParser (OA.progDesc "View local snapshot"))



-- <*>
--     OA.switch
--         (OA.long "lts" <> OA.short 'l' <> OA.help "Only show lts snapshots") <*>
--     OA.switch
--         (OA.long "nightly" <> OA.short 'n' <>
--          OA.help "Only show nightly snapshots") <*>
--     OA.option
--         OA.auto
--         (OA.long "package" <> OA.help "Show packages list" <> OA.short 'p' <>
--          OA.value Nothing <>
--          OA.metavar "RESOLVER")



data Snapshot = Snapshot
    { snapId :: Text
    , snapTitle :: Text
    , snapTime :: Text
    } deriving (Show, Eq, Ord)

data SnapshotData = SnapshotData
    { _snapTotalCounts :: Integer
    , snaps :: [[Snapshot]]
    } deriving (Show, Eq, Ord)

instance FromJSON Snapshot where
    parseJSON o@(Array _) = parseSnapshot o
    parseJSON _ = mempty

instance FromJSON SnapshotData where
    parseJSON (Object s) =
        SnapshotData <$> s .: "totalCount" <*> s .: "snapshots"
    parseJSON _ = mempty

data Package = Package
    { pkgSynopsis :: Text
    , pkgVersion :: Text
    , pkgName :: Text
    , _pkgCore :: Bool
    } deriving (Show, Eq, Ord)

data PackageSnapshot = PackageSnapshot
    { pkgSnapName :: Text
    , pkgSnapCreated :: Text
    , pkgSnapGhc :: Text
    } deriving (Show, Eq, Ord)

instance FromJSON PackageSnapshot where
    parseJSON (Object s) =
        PackageSnapshot <$> s .: "name" <*> s .: "created" <*> s .: "ghc"
    parseJSON _ = mempty

instance FromJSON Package where
    parseJSON (Object s) =
        Package <$> s .: "synopsis" <*> s .: "version" <*> s .: "name" <*>
        s .: "isCore"
    parseJSON _ = mempty

data Packages = Packages
    { pkgPackages :: [Package]
    , pkgSnapshot :: PackageSnapshot
    } deriving (Show, Eq, Ord)

instance FromJSON Packages where
    parseJSON (Object s) = Packages <$> s .: "packages" <*> s .: "snapshot"
    parseJSON _ = mempty

toSnapshot :: [Value] -> Snapshot
toSnapshot [String sid, String stitle, String stime] =
    Snapshot
    { snapId = sid
    , snapTitle = stitle
    , snapTime = stime
    }
toSnapshot val = throw $ ParseFailure val

newtype LsException =
    ParseFailure [Value]
    deriving (Show, Typeable)

instance Exception LsException

parseSnapshot :: Value -> A.Parser Snapshot
parseSnapshot =
    A.withArray "array of snapshot" (\val -> return $ toSnapshot (V.toList val))

displaySnap :: Snapshot -> IO ()
displaySnap snapshot = do
    T.putStrLn $ "Resolver name: " <> snapId snapshot
    T.putStrLn $ snapTitle snapshot
    putStrLn ""

displayTime :: Snapshot -> IO ()
displayTime snapshot = do
    setSGR [SetColor Foreground Dull Green]
    T.putStrLn $ snapTime snapshot
    setSGR [Reset]
    putStrLn ""

displaySingleSnap :: [Snapshot] -> IO ()
displaySingleSnap snapshots =
    case snapshots of
        [] -> return ()
        (x:xs) -> do
            displayTime x
            displaySnap x
            mapM_ displaySnap xs

displaySnapshotData :: SnapshotData -> IO ()
displaySnapshotData sdata =
    case L.reverse $ snaps sdata of
        [] -> return ()
        xs -> mapM_ displaySingleSnap xs

filterSnapshotData :: SnapshotData -> SnapshotType -> SnapshotData
filterSnapshotData sdata stype =
    sdata
    { snaps = filterSnapData
    }
  where
    snapdata = snaps sdata
    filterSnapData =
        case stype of
            Lts ->
                L.map (\s -> L.filter (\x -> "lts" `isPrefixOf` snapId x) s) snapdata
            Nightly ->
                L.map
                    (\s -> L.filter (\x -> "nightly" `isPrefixOf` snapId x) s)
                    snapdata

displayLocalSnapshot :: [String] -> IO ()
displayLocalSnapshot xs = mapM_ putStrLn xs

handleLocal
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
    => LsCmdOpts -> m ()
handleLocal lsOpts = do
    (instRoot :: Path Abs Dir) <- installationRootDeps
    let snapRootDir = parent $ parent instRoot
    snapData' <- liftIO $ listDirectory $ toFilePath snapRootDir
    let snapData = L.sort snapData'
    case (lsView lsOpts) of
      LsSnapshot SnapshotOpts{..} -> 
          case (soptLtsSnapView, soptNightlySnapView) of
            (True, False) ->
                liftIO $ displayLocalSnapshot $ L.filter (L.isPrefixOf "lts") snapData
            (False, True) ->
                liftIO $ displayLocalSnapshot $ L.filter (L.isPrefixOf "night") snapData
            _ -> liftIO $ displayLocalSnapshot snapData

lsCmd :: LsCmdOpts -> GlobalOpts -> IO ()
lsCmd lsOpts go = do
    case lsView lsOpts of
      LsSnapshot SnapshotOpts{..} -> case soptViewType of
        Local -> withBuildConfig go (handleLocal lsOpts)
        Remote -> do
            req <- parseRequest urlInfo
            mgr <- getGlobalManager
            let req' =
                    setRequestManager mgr $
                    addRequestHeader hAccept "application/json" req
            result <- httpJSON req'
            let snapData = getResponseBody result
            case (soptLtsSnapView, soptNightlySnapView) of
                (True, False) ->
                    liftIO $ displaySnapshotData $ filterSnapshotData snapData Lts
                (False, True) ->
                    liftIO $ displaySnapshotData $ filterSnapshotData snapData Nightly
                _ -> liftIO $ displaySnapshotData snapData
  where
    urlInfo = "https://www.stackage.org/snapshots"

lsViewLocalCmd :: OA.Mod OA.CommandFields LsView
lsViewLocalCmd =
    OA.command
        "local"
        (OA.info (pure Remote) (OA.progDesc "View local snapshot"))

lsViewRemoteCmd :: OA.Mod OA.CommandFields LsView
lsViewRemoteCmd =
    OA.command
        "remote"
        (OA.info (pure Remote) (OA.progDesc "View remote snapshot"))
