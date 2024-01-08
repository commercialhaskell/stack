{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and functions related to Stack's @ls@ command.
module Stack.Ls
  ( LsCmdOpts (..)
  , LsCmds (..)
  , SnapshotOpts (..)
  , LsView (..)
  , ListDepsOpts (..)
  , ListDepsFormat (..)
  , ListDepsFormatOpts (..)
  , ListStylesOpts (..)
  , ListToolsOpts (..)
  , lsCmd
  ) where

import           Data.Aeson ( FromJSON, Value (..), (.:), encode )
import           Data.Array.IArray ( (//), elems )
import qualified Data.ByteString.Lazy.Char8 as LBC8
import           Distribution.Package ( mkPackageName )
import qualified Data.Aeson.Types as A
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text ( isPrefixOf )
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
import           Stack.DependencyGraph ( createPrunedDependencyGraph )
import           Stack.Prelude hiding ( Nightly, Snapshot )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Setup.Installed
                   ( Tool (..), filterTools, listInstalled, toolString )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.DependencyTree
                   ( DependencyTree (..), DotPayload (..), licenseText
                   , versionText
                   )
import           Stack.Types.DotOpts ( DotOpts (..) )
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

-- | Type representing command line options for the @stack ls@ command.
newtype LsCmdOpts
  = LsCmdOpts { lsView :: LsCmds }

-- | Type representing subcommands for the @stack ls@ command.
data LsCmds
  = LsSnapshot SnapshotOpts
  | LsDependencies ListDepsOpts
  | LsStyles ListStylesOpts
  | LsTools ListToolsOpts

-- | Type representing command line options for the @stack ls snapshots@
-- command.
data SnapshotOpts = SnapshotOpts
  { soptViewType :: LsView
  , soptLtsSnapView :: Bool
  , soptNightlySnapView :: Bool
  }
  deriving (Eq, Ord, Show)

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

data ListDepsOpts = ListDepsOpts
  { listDepsFormat :: !ListDepsFormat
    -- ^ Format of printing dependencies
  , listDepsDotOpts :: !DotOpts
    -- ^ The normal dot options.
  }

data ListDepsFormat
  = ListDepsText ListDepsFormatOpts
  | ListDepsTree ListDepsFormatOpts
  | ListDepsJSON
  | ListDepsConstraints

data ListDepsFormatOpts = ListDepsFormatOpts
  { listDepsSep :: !Text
    -- ^ Separator between the package name and details.
  , listDepsLicense :: !Bool
    -- ^ Print dependency licenses instead of versions.
  }

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

data Snapshot = Snapshot
  { snapId :: Text
  , snapTitle :: Text
  , snapTime :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Snapshot where
  parseJSON o@(Array _) = parseSnapshot o
  parseJSON _ = mempty

data SnapshotData = SnapshotData
  { _snapTotalCounts :: Integer
  , snaps :: [[Snapshot]]
  }
  deriving (Eq, Ord, Show)

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
displayTime snap = [snap.snapTime]

displaySnap :: Snapshot -> [Text]
displaySnap snap =
  ["Resolver name: " <> snap.snapId, "\n" <> snap.snapTitle <> "\n\n"]

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
    LsSnapshot sopt ->
      case (sopt.soptLtsSnapView, sopt.soptNightlySnapView) of
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
    LsSnapshot sopt ->
      case (sopt.soptLtsSnapView, sopt.soptNightlySnapView) of
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
    LsSnapshot sopt ->
      case sopt.soptViewType of
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
  localPrograms <- view $ configL . to configLocalPrograms
  installed <- sort <$> listInstalled localPrograms
  let wanted = case toptFilter opts of
        [] -> installed
        "ghc-git" -> [t | t@(ToolGhcGit _ _) <- installed]
        pkgName -> filtered pkgName installed
  liftIO $ mapM_ (putStrLn . toolString) wanted
 where
  filtered pkgName installed = Tool <$>
      filterTools (mkPackageName pkgName) (const True) installed

listDependencies :: ListDepsOpts -> RIO Runner ()
listDependencies opts = do
  let dotOpts = listDepsDotOpts opts
  (pkgs, resultGraph) <- createPrunedDependencyGraph dotOpts
  liftIO $ case listDepsFormat opts of
    ListDepsTree treeOpts ->
      T.putStrLn "Packages"
      >> printTree treeOpts dotOpts 0 [] (treeRoots opts pkgs) resultGraph
    ListDepsJSON -> printJSON pkgs resultGraph
    ListDepsText textOpts ->
      void $ Map.traverseWithKey (go "" textOpts) (snd <$> resultGraph)
    ListDepsConstraints -> do
      let constraintOpts = ListDepsFormatOpts " ==" False
      T.putStrLn "constraints:"
      void $ Map.traverseWithKey (go "  , " constraintOpts)
                                 (snd <$> resultGraph)
 where
  go prefix lineOpts name payload =
    T.putStrLn $ prefix <> listDepsLine lineOpts name payload

treeRoots :: ListDepsOpts -> Set PackageName -> Set PackageName
treeRoots opts projectPackages' =
  let targets = dotTargets $ listDepsDotOpts opts
  in  if null targets
        then projectPackages'
        else Set.fromList $ map (mkPackageName . T.unpack) targets

printTree ::
     ListDepsFormatOpts
  -> DotOpts
  -> Int
  -> [Int]
  -> Set PackageName
  -> Map PackageName (Set PackageName, DotPayload)
  -> IO ()
printTree opts dotOpts depth remainingDepsCounts packages dependencyMap =
  F.sequence_ $ Seq.mapWithIndex go (toSeq packages)
 where
  toSeq = Seq.fromList . Set.toList
  go index name =
    let newDepsCounts = remainingDepsCounts ++ [Set.size packages - index - 1]
    in  case Map.lookup name dependencyMap of
          Just (deps, payload) -> do
            printTreeNode opts dotOpts depth newDepsCounts deps payload name
            if Just depth == dotDependencyDepth dotOpts
              then pure ()
              else printTree opts dotOpts (depth + 1) newDepsCounts deps
                     dependencyMap
          -- TODO: Define this behaviour, maybe pure an error?
          Nothing -> pure ()

printTreeNode ::
     ListDepsFormatOpts
  -> DotOpts
  -> Int
  -> [Int]
  -> Set PackageName
  -> DotPayload
  -> PackageName
  -> IO ()
printTreeNode opts dotOpts depth remainingDepsCounts deps payload name =
  let remainingDepth = fromMaybe 999 (dotDependencyDepth dotOpts) - depth
      hasDeps = not $ null deps
  in  T.putStrLn $
        treeNodePrefix "" remainingDepsCounts hasDeps remainingDepth <> " " <>
        listDepsLine opts name payload

treeNodePrefix :: Text -> [Int] -> Bool -> Int -> Text
treeNodePrefix t [] _ _      = t
treeNodePrefix t [0] True  0 = t <> "└──"
treeNodePrefix t [_] True  0 = t <> "├──"
treeNodePrefix t [0] True  _ = t <> "└─┬"
treeNodePrefix t [_] True  _ = t <> "├─┬"
treeNodePrefix t [0] False _ = t <> "└──"
treeNodePrefix t [_] False _ = t <> "├──"
treeNodePrefix t (0:ns) d remainingDepth = treeNodePrefix (t <> "  ") ns d remainingDepth
treeNodePrefix t (_:ns) d remainingDepth = treeNodePrefix (t <> "│ ") ns d remainingDepth

listDepsLine :: ListDepsFormatOpts -> PackageName -> DotPayload -> Text
listDepsLine opts name payload =
  T.pack (packageNameString name) <> listDepsSep opts <>
  payloadText opts payload

payloadText :: ListDepsFormatOpts -> DotPayload -> Text
payloadText opts payload =
  if listDepsLicense opts
    then licenseText payload
    else versionText payload

printJSON ::
     Set PackageName
  -> Map PackageName (Set PackageName, DotPayload)
  -> IO ()
printJSON pkgs dependencyMap =
  LBC8.putStrLn $ encode $ DependencyTree pkgs dependencyMap
