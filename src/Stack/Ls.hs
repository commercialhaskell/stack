{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Ls
  ( lsCmd
  , lsParser
  ) where

import           Data.Aeson
import           Data.Array.IArray ( (//), elems )
import           Distribution.Package ( mkPackageName )
import qualified Data.Aeson.Types as A
import qualified Data.List as L
import           Data.Text hiding ( filter, intercalate, pack, reverse )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Network.HTTP.StackClient
                   ( httpJSON, addRequestHeader, getResponseBody, parseRequest
                   , hAccept
                   )
import qualified Options.Applicative as OA
import           Options.Applicative ( idm )
import           Options.Applicative.Builder.Extra ( boolFlags )
import           Path
import           RIO.List ( sort )
import           Stack.Constants ( osIsWindows, globalFooter )
import           Stack.Dot
import           Stack.Prelude hiding ( Snapshot (..), SnapName (..) )
import           Stack.Runners
import           Stack.Options.DotParser ( listDepsOptsParser )
import           Stack.Setup.Installed
                   ( Tool (..), filterTools, listInstalled, toolString )
import           Stack.Types.Config
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
    | LsStyles ListStylesOpts
    | LsTools ListToolsOpts

data SnapshotOpts = SnapshotOpts
    { soptViewType :: LsView
    , soptLtsSnapView :: Bool
    , soptNightlySnapView :: Bool
    }
    deriving (Eq, Show, Ord)

data ListStylesOpts = ListStylesOpts
    { coptBasic   :: Bool
    , coptSGR     :: Bool
    , coptExample :: Bool
    }
    deriving (Eq, Ord, Show)

newtype ListToolsOpts = ListToolsOpts
    { toptFilter  :: String
    }

newtype LsCmdOpts = LsCmdOpts
    { lsView :: LsCmds
    }

lsParser :: OA.Parser LsCmdOpts
lsParser = LsCmdOpts
    <$> OA.hsubparser (lsSnapCmd <> lsDepsCmd <> lsStylesCmd <> lsToolsCmd)

lsCmdOptsParser :: OA.Parser LsCmds
lsCmdOptsParser = LsSnapshot <$> lsViewSnapCmd

lsDepOptsParser :: OA.Parser LsCmds
lsDepOptsParser = LsDependencies <$> listDepsOptsParser

lsStylesOptsParser :: OA.Parser LsCmds
lsStylesOptsParser = LsStyles <$> listStylesOptsParser

lsToolsOptsParser :: OA.Parser LsCmds
lsToolsOptsParser = LsTools <$> listToolsOptsParser

listStylesOptsParser :: OA.Parser ListStylesOpts
listStylesOptsParser = ListStylesOpts
    <$> boolFlags False
                  "basic"
                  "a basic report of the styles used. The default is a fuller \
                  \one"
                  idm
    <*> boolFlags True
                  "sgr"
                  "the provision of the equivalent SGR instructions (provided \
                  \by default). Flag ignored for a basic report"
                  idm
    <*> boolFlags True
                  "example"
                  "the provision of an example of the applied style (provided \
                  \by default for colored output). Flag ignored for a basic \
                  \report"
                  idm

listToolsOptsParser :: OA.Parser ListToolsOpts
listToolsOptsParser = ListToolsOpts
    <$> OA.strOption
            ( OA.long "filter"
           <> OA.metavar "TOOL_NAME"
           <> OA.value ""
           <> OA.help "Filter by a tool name (eg 'ghc', 'ghc-git' or 'msys2') \
                      \- case sensitive. The default is no filter"
            )

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
lsSnapCmd = OA.command "snapshots" $
  OA.info lsCmdOptsParser $
       OA.progDesc "View snapshots (local by default)"
    <> OA.footer localSnapshotMsg

lsDepsCmd :: OA.Mod OA.CommandFields LsCmds
lsDepsCmd = OA.command "dependencies" $
    OA.info lsDepOptsParser $
         OA.progDesc "View the dependencies"
      <> OA.footer globalFooter

lsStylesCmd :: OA.Mod OA.CommandFields LsCmds
lsStylesCmd =
    OA.command
        "stack-colors"
        (OA.info lsStylesOptsParser
                 (OA.progDesc "View Stack's output styles"))
    <>
    OA.command
        "stack-colours"
        (OA.info lsStylesOptsParser
                 (OA.progDesc "View Stack's output styles (alias for \
                              \'stack-colors')"))

lsToolsCmd :: OA.Mod OA.CommandFields LsCmds
lsToolsCmd =
    OA.command
        "tools"
        (OA.info lsToolsOptsParser (OA.progDesc "View Stack's installed tools"))

data Snapshot = Snapshot
    { snapId :: Text
    , snapTitle :: Text
    , snapTime :: Text
    }
    deriving (Show, Eq, Ord)

data SnapshotData = SnapshotData
    { _snapTotalCounts :: Integer
    , snaps :: [[Snapshot]]
    }
    deriving (Show, Eq, Ord)

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

handleLocal :: LsCmdOpts -> RIO Runner ()
handleLocal lsOpts = do
    (instRoot :: Path Abs Dir) <- withConfig YesReexec $ withDefaultEnvConfig installationRootDeps
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

lsViewLocalCmd :: OA.Mod OA.CommandFields LsView
lsViewLocalCmd = OA.command "local" $
  OA.info (pure Local) $
       OA.progDesc "View local snapshots"
    <> OA.footer localSnapshotMsg

lsViewRemoteCmd :: OA.Mod OA.CommandFields LsView
lsViewRemoteCmd = OA.command "remote" $
  OA.info (pure Remote) $
       OA.progDesc "View remote snapshots"
    <> OA.footer pagerMsg

pagerMsg :: String
pagerMsg =
  "On a terminal, uses a pager, if one is available. Respects the PAGER \
  \environment variable (subject to that, prefers pager 'less' to 'more')."

localSnapshotMsg :: String
localSnapshotMsg =
  "A local snapshot is identified by a hash code. " <> pagerMsg

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
    liftIO $ T.putStrLn $ T.intercalate (if isComplex then "\n" else ":") styleReports
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
