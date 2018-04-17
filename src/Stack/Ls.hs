{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Ls
  ( lsCmd
  , lsParser
  , listDependenciesCmd
  ) where

import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad (when)
import Data.Aeson
import Stack.Types.Runner
import qualified Data.Aeson.Types as A
import qualified Data.List as L
import Data.Monoid
import Data.Text hiding (pack, intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Network.HTTP.StackClient (httpJSON)
import Network.HTTP.Simple
       (addRequestHeader, getResponseBody, parseRequest,
        setRequestManager)
import Network.HTTP.Types.Header (hAccept)
import qualified Options.Applicative as OA
import Options.Applicative ((<|>))
import Path
import Stack.Runners (withBuildConfig, withBuildConfigDot)
import Stack.Types.Config
import Stack.Dot
import Stack.Options.DotParser (listDepsOptsParser)
import System.Process.PagerEditor (pageText)
import System.Directory (listDirectory)
import System.IO (stderr, hPutStrLn)
import Network.HTTP.Client.TLS (getGlobalManager)

data LsView
    = Local
    | Remote
    deriving (Show, Eq, Ord)

data SnapshotType
    = Lts
    | Nightly
    deriving (Show, Eq, Ord)

data LsCmds
    = LsSnapshot SnapshotOpts
    | LsDependencies ListDepsOpts

data SnapshotOpts = SnapshotOpts
    { soptViewType :: LsView
    , soptLtsSnapView :: Bool
    , soptNightlySnapView :: Bool
    } deriving (Eq, Show, Ord)

newtype LsCmdOpts = LsCmdOpts
    { lsView :: LsCmds
    }

lsParser :: OA.Parser LsCmdOpts
lsParser = LsCmdOpts <$> OA.hsubparser (lsSnapCmd <> lsDepsCmd)

lsCmdOptsParser :: OA.Parser LsCmds
lsCmdOptsParser = LsSnapshot <$> lsViewSnapCmd

lsDepOptsParser :: OA.Parser LsCmds
lsDepOptsParser = LsDependencies <$> listDepsOptsParser

lsViewSnapCmd :: OA.Parser SnapshotOpts
lsViewSnapCmd =
    SnapshotOpts <$>
    (OA.hsubparser (lsViewRemoteCmd <> lsViewLocalCmd) <|> pure Local) <*>
    OA.switch
        (OA.long "lts" <> OA.short 'l' <> OA.help "Only show lts snapshots") <*>
    OA.switch
        (OA.long "nightly" <> OA.short 'n' <>
         OA.help "Only show nightly snapshots")

lsSnapCmd :: OA.Mod OA.CommandFields LsCmds
lsSnapCmd =
    OA.command
        "snapshots"
        (OA.info
             lsCmdOptsParser
             (OA.progDesc "View local snapshot (default option)"))

lsDepsCmd :: OA.Mod OA.CommandFields LsCmds
lsDepsCmd =
    OA.command
        "dependencies"
        (OA.info lsDepOptsParser (OA.progDesc "View the dependencies"))

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
parseSnapshot = A.withArray "array of snapshot" (return . toSnapshot . V.toList)

displayTime :: Snapshot -> [Text]
displayTime Snapshot {..} = [snapTime]

displaySnap :: Snapshot -> [Text]
displaySnap Snapshot {..} =
    ["Resolver name: " <> snapId, "\n" <> snapTitle <> "\n\n"]

displaySingleSnap :: [Snapshot] -> Text
displaySingleSnap snapshots =
    case snapshots of
        [] -> mempty
        (x:xs) ->
            let snaps =
                    displayTime x <> ["\n\n"] <> displaySnap x <>
                    L.concatMap displaySnap xs
            in T.concat snaps

renderData :: Bool -> Text -> IO ()
renderData True content = pageText content
renderData False content = T.putStr content

displaySnapshotData :: Bool -> SnapshotData -> IO ()
displaySnapshotData term sdata =
    case L.reverse $ snaps sdata of
        [] -> return ()
        xs ->
            let snaps = T.concat $ L.map displaySingleSnap xs
            in renderData term snaps

filterSnapshotData :: SnapshotData -> SnapshotType -> SnapshotData
filterSnapshotData sdata stype =
    sdata
    { snaps = filterSnapData
    }
  where
    snapdata = snaps sdata
    filterSnapData =
        case stype of
            Lts -> L.map (L.filter (\x -> "lts" `isPrefixOf` snapId x)) snapdata
            Nightly ->
                L.map (L.filter (\x -> "nightly" `isPrefixOf` snapId x)) snapdata

displayLocalSnapshot :: Bool -> [String] -> IO ()
displayLocalSnapshot term xs = renderData term (localSnaptoText xs)

localSnaptoText :: [String] -> Text
localSnaptoText xs = T.intercalate "\n" $ L.map T.pack xs

handleLocal
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
    => LsCmdOpts -> m ()
handleLocal lsOpts = do
    (instRoot :: Path Abs Dir) <- installationRootDeps
    isStdoutTerminal <- view terminalL
    let snapRootDir = parent $ parent instRoot
    snapData' <- liftIO $ listDirectory $ toFilePath snapRootDir
    let snapData = L.sort snapData'
    case lsView lsOpts of
        LsSnapshot SnapshotOpts {..} ->
            case (soptLtsSnapView, soptNightlySnapView) of
                (True, False) ->
                    liftIO $
                    displayLocalSnapshot isStdoutTerminal $
                    L.filter (L.isPrefixOf "lts") snapData
                (False, True) ->
                    liftIO $
                    displayLocalSnapshot isStdoutTerminal $
                    L.filter (L.isPrefixOf "night") snapData
                _ -> liftIO $ displayLocalSnapshot isStdoutTerminal snapData
        LsDependencies _ -> return ()

handleRemote
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
    => LsCmdOpts -> m ()
handleRemote lsOpts = do
    req <- liftIO $ parseRequest urlInfo
    mgr <- liftIO getGlobalManager
    isStdoutTerminal <- view terminalL
    let req' =
            setRequestManager mgr $
            addRequestHeader hAccept "application/json" req
    result <- httpJSON req'
    let snapData = getResponseBody result
    case lsView lsOpts of
        LsSnapshot SnapshotOpts {..} ->
            case (soptLtsSnapView, soptNightlySnapView) of
                (True, False) ->
                    liftIO $
                    displaySnapshotData isStdoutTerminal $
                    filterSnapshotData snapData Lts
                (False, True) ->
                    liftIO $
                    displaySnapshotData isStdoutTerminal $
                    filterSnapshotData snapData Nightly
                _ -> liftIO $ displaySnapshotData isStdoutTerminal snapData
        LsDependencies _ -> return ()
  where
    urlInfo = "https://www.stackage.org/snapshots"

lsCmd :: LsCmdOpts -> GlobalOpts -> IO ()
lsCmd lsOpts go =
    case lsView lsOpts of
        LsSnapshot SnapshotOpts {..} ->
            case soptViewType of
                Local -> withBuildConfig go (handleLocal lsOpts)
                Remote -> withBuildConfig go (handleRemote lsOpts)
        LsDependencies depOpts -> listDependenciesCmd False depOpts go

-- | List the dependencies
listDependenciesCmd :: Bool -> ListDepsOpts -> GlobalOpts -> IO ()
listDependenciesCmd deprecated opts go = do
    when
        deprecated
        (hPutStrLn
             stderr
             "DEPRECATED: Use ls dependencies instead. Will be removed in next major version.")
    withBuildConfigDot (listDepsDotOpts opts) go $ listDependencies opts

lsViewLocalCmd :: OA.Mod OA.CommandFields LsView
lsViewLocalCmd =
    OA.command
        "local"
        (OA.info (pure Local) (OA.progDesc "View local snapshot"))

lsViewRemoteCmd :: OA.Mod OA.CommandFields LsView
lsViewRemoteCmd =
    OA.command
        "remote"
        (OA.info (pure Remote) (OA.progDesc "View remote snapshot"))
