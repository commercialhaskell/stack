{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Types and functions related to Stack's @ls@ command.
module Stack.Ls
  ( LsCmdOpts (..)
  , LsCmds (..)
  , SnapshotOpts (..)
  , ListStylesOpts (..)
  , ListToolsOpts (..)
  , LsView (..)
  , lsCmd
  ) where

import           Data.Aeson ( FromJSON, Value (..), (.:) )
import           Data.Array.IArray ( (//), elems )
import           Distribution.Package ( mkPackageName )
import qualified Data.Aeson.Types as A
import qualified Data.List as L
import           Data.Text hiding ( filter, intercalate, pack, reverse )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Network.HTTP.StackClient
                   ( addRequestHeader, hAccept, httpJSON, getResponseBody
                   , parseRequest
                   )
import           Path ( parent )
import           RIO.List ( sort )
import           Stack.Constants ( osIsWindows )
import           Stack.Dot ( ListDepsOpts, listDependencies )
import           Stack.Prelude hiding ( Nightly, Snapshot )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Setup.Installed
                   ( Tool (..), filterTools, listInstalled, toolString )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.EnvConfig ( installationRootDeps )
import           Stack.Types.Runner ( HasRunner, Runner, terminalL )
import           System.Console.ANSI.Codes
                   ( SGR (Reset), setSGRCode, sgrToCode )
import           System.Process.Pager ( pageText )
import           System.Directory ( listDirectory )
import           System.IO ( putStrLn )

-- | Type representing exceptions thrown by functions exported by the "Stack.Ls"
-- module.
newtype LsException
  = ParseFailure [Value]
  deriving (Show, Typeable)

instance Exception LsException where
  displayException (ParseFailure val) =
    "Error: [S-3421]\n"
    ++ "Failure to parse values as a snapshot: "
    ++ show val

-- | Type representing subcommands for the @stack ls snapshots@ command.
data LsView
  = Local
  | Remote
  deriving (Eq, Ord, Show)

-- | Type representing Stackage snapshot types.
data SnapshotType
  = Lts
    -- ^ Stackage LTS Haskell
  | Nightly
    -- ^ Stackage Nightly
  deriving (Eq, Ord, Show)

-- | Type representing command line options for the @stack ls snapshots@
-- command.
data SnapshotOpts = SnapshotOpts
  { soptViewType :: LsView
  , soptLtsSnapView :: Bool
  , soptNightlySnapView :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Type representing command line options for the @stack ls stack-colors@ and
-- @stack ls stack-colours@ commands.
data ListStylesOpts = ListStylesOpts
  { coptBasic   :: Bool
  , coptSGR     :: Bool
  , coptExample :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Type representing command line options for the @stack ls tools@ command.
newtype ListToolsOpts
  = ListToolsOpts { toptFilter  :: String }

-- | Type representing subcommands for the @stack ls@ command.
data LsCmds
  = LsSnapshot SnapshotOpts
  | LsDependencies ListDepsOpts
  | LsStyles ListStylesOpts
  | LsTools ListToolsOpts

-- | Type representing command line options for the @stack ls@ command.
newtype LsCmdOpts
  = LsCmdOpts { lsView :: LsCmds }

data Snapshot = Snapshot
  { snapId :: Text
  , snapTitle :: Text
  , snapTime :: Text
  }
  deriving (Eq, Ord, Show)

data SnapshotData = SnapshotData
  { _snapTotalCounts :: Integer
  , snaps :: [[Snapshot]]
  }
  deriving (Eq, Ord, Show)

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
toSnapshot val = impureThrow $ ParseFailure val

parseSnapshot :: Value -> A.Parser Snapshot
parseSnapshot = A.withArray "array of snapshot" (pure . toSnapshot . V.toList)

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
      in  T.concat snaps

renderData :: Bool -> Text -> IO ()
renderData True content = pageText content
renderData False content = T.putStr content

displaySnapshotData :: Bool -> SnapshotData -> IO ()
displaySnapshotData term sdata =
  case L.reverse $ snaps sdata of
    [] -> pure ()
    xs ->
      let snaps = T.concat $ L.map displaySingleSnap xs
      in  renderData term snaps

filterSnapshotData :: SnapshotData -> SnapshotType -> SnapshotData
filterSnapshotData sdata stype =
  sdata { snaps = filterSnapData }
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

handleLocal :: LsCmdOpts -> RIO Runner ()
handleLocal lsOpts = do
  (instRoot :: Path Abs Dir) <-
    withConfig YesReexec $ withDefaultEnvConfig installationRootDeps
  isStdoutTerminal <- view terminalL
  let parentInstRoot = parent instRoot
      snapRootDir
        | osIsWindows = parentInstRoot
        | otherwise   = parent parentInstRoot
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
    LsDependencies _ -> pure ()
    LsStyles _ -> pure ()
    LsTools _ -> pure ()

handleRemote :: HasRunner env => LsCmdOpts -> RIO env ()
handleRemote lsOpts = do
  req <- liftIO $ parseRequest urlInfo
  isStdoutTerminal <- view terminalL
  let req' = addRequestHeader hAccept "application/json" req
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
    LsDependencies _ -> pure ()
    LsStyles _ -> pure ()
    LsTools _ -> pure ()
 where
  urlInfo = "https://www.stackage.org/snapshots"

lsCmd :: LsCmdOpts -> RIO Runner ()
lsCmd lsOpts =
  case lsView lsOpts of
    LsSnapshot SnapshotOpts {..} ->
      case soptViewType of
        Local -> handleLocal lsOpts
        Remote -> handleRemote lsOpts
    LsDependencies depOpts -> listDependencies depOpts
    LsStyles stylesOpts -> withConfig NoReexec $ listStylesCmd stylesOpts
    LsTools toolsOpts -> withConfig NoReexec $ listToolsCmd toolsOpts

-- | List Stack's output styles
listStylesCmd :: ListStylesOpts -> RIO Config ()
listStylesCmd opts = do
  lc <- ask
  -- This is the same test as is used in Stack.Types.Runner.withRunner
  let useColor = view useColorL lc
      styles = elems $ defaultStyles // stylesUpdate (view stylesUpdateL lc)
      isComplex = not (coptBasic opts)
      showSGR = isComplex && coptSGR opts
      showExample = isComplex && coptExample opts && useColor
      styleReports = L.map (styleReport showSGR showExample) styles
  liftIO $
    T.putStrLn $ T.intercalate (if isComplex then "\n" else ":") styleReports
 where
  styleReport :: Bool -> Bool -> StyleSpec -> Text
  styleReport showSGR showExample (k, sgrs) = k <> "=" <> codes
    <> (if showSGR then sgrsList else mempty)
    <> (if showExample then example else mempty)
   where
    codes = T.intercalate ";" (L.map (fromString . show) $
              L.concatMap sgrToCode sgrs)
    sgrsList = " [" <> T.intercalate ", " (L.map (fromString . show) sgrs)
               <> "]"
    example = " " <> ansi <> "Example" <> reset
    ansi = fromString $ setSGRCode sgrs
    reset = fromString $ setSGRCode [Reset]

-- | List Stack's installed tools, sorted (see instance of 'Ord' for 'Tool').
listToolsCmd :: ListToolsOpts -> RIO Config ()
listToolsCmd opts = do
  localPrograms <- view $ configL.to configLocalPrograms
  installed <- sort <$> listInstalled localPrograms
  let wanted = case toptFilter opts of
        [] -> installed
        "ghc-git" -> [t | t@(ToolGhcGit _ _) <- installed]
        pkgName -> filtered pkgName installed
  liftIO $ mapM_ (putStrLn . toolString) wanted
 where
  filtered pkgName installed = Tool <$>
      filterTools (mkPackageName pkgName) (const True) installed
