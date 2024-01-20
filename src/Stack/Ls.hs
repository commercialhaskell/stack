{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
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
  , ListDepsTextFilter (..)
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
  { viewType :: LsView
  , ltsSnapView :: Bool
  , nightlySnapView :: Bool
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
  { format :: !ListDepsFormat
    -- ^ Format of printing dependencies
  , dotOpts :: !DotOpts
    -- ^ The normal dot options.
  }

data ListDepsFormat
  = ListDepsText ListDepsFormatOpts [ListDepsTextFilter]
  | ListDepsTree ListDepsFormatOpts
  | ListDepsJSON
  | ListDepsConstraints

data ListDepsFormatOpts = ListDepsFormatOpts
  { sep :: !Text
    -- ^ Separator between the package name and details.
  , license :: !Bool
    -- ^ Print dependency licenses instead of versions.
  }

-- | Type representing items to filter the results of @stack ls dependencies@.
data ListDepsTextFilter
  = FilterPackage PackageName
    -- ^ Item is a package name.
  | FilterLocals
    -- ^ Item represents all local packages.

-- | Type representing command line options for the @stack ls stack-colors@ and
-- @stack ls stack-colours@ commands.
data ListStylesOpts = ListStylesOpts
  { basic   :: Bool
  , sgr     :: Bool
  , example :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Type representing command line options for the @stack ls tools@ command.
newtype ListToolsOpts
  = ListToolsOpts { toptFilter  :: String }

data Snapshot = Snapshot
  { snapId :: Text
  , title :: Text
  , time :: Text
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
toSnapshot [String snapId, String title, String time] =
  Snapshot
    { snapId
    , title
    , time
    }
toSnapshot val = impureThrow $ ParseFailure val

parseSnapshot :: Value -> A.Parser Snapshot
parseSnapshot = A.withArray "array of snapshot" (pure . toSnapshot . V.toList)

displayTime :: Snapshot -> [Text]
displayTime snap = [snap.time]

displaySnap :: Snapshot -> [Text]
displaySnap snap =
  ["Resolver name: " <> snap.snapId, "\n" <> snap.title <> "\n\n"]

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
  case L.reverse sdata.snaps of
    [] -> pure ()
    xs ->
      let snaps = T.concat $ L.map displaySingleSnap xs
      in  renderData term snaps

filterSnapshotData :: SnapshotData -> SnapshotType -> SnapshotData
filterSnapshotData sdata stype =
  sdata { snaps = filterSnapData }
 where
  snapdata = sdata.snaps
  filterSnapData =
    case stype of
      Lts -> L.map (L.filter (\x -> "lts" `isPrefixOf` x.snapId)) snapdata
      Nightly ->
        L.map (L.filter (\x -> "nightly" `isPrefixOf` x.snapId)) snapdata

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
  case lsOpts.lsView of
    LsSnapshot sopt ->
      case (sopt.ltsSnapView, sopt.nightlySnapView) of
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
  case lsOpts.lsView of
    LsSnapshot sopt ->
      case (sopt.ltsSnapView, sopt.nightlySnapView) of
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
  case lsOpts.lsView of
    LsSnapshot sopt ->
      case sopt.viewType of
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
      isComplex = not opts.basic
      showSGR = isComplex && opts.sgr
      showExample = isComplex && opts.example && useColor
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
  localPrograms <- view $ configL . to (.localPrograms)
  installed <- sort <$> listInstalled localPrograms
  let wanted = case opts.toptFilter of
        [] -> installed
        "ghc-git" -> [t | t@(ToolGhcGit _ _) <- installed]
        pkgName -> filtered pkgName installed
  liftIO $ mapM_ (putStrLn . toolString) wanted
 where
  filtered pkgName installed = Tool <$>
      filterTools (mkPackageName pkgName) (const True) installed

listDependencies :: ListDepsOpts -> RIO Runner ()
listDependencies opts = do
  let dotOpts = opts.dotOpts
  (pkgs, resultGraph) <- createPrunedDependencyGraph dotOpts
  liftIO $ case opts.format of
    ListDepsTree treeOpts ->
      T.putStrLn "Packages"
      >> printTree treeOpts dotOpts 0 [] (treeRoots opts pkgs) resultGraph
    ListDepsJSON -> printJSON pkgs resultGraph
    ListDepsText textOpts listDepsTextFilters -> do
      let resultGraph' = Map.filterWithKey p resultGraph
          p k _ =
            Set.notMember k (exclude (Set.toList pkgs) listDepsTextFilters)
      void $ Map.traverseWithKey (go "" textOpts) (snd <$> resultGraph')
     where
      exclude :: [PackageName] -> [ListDepsTextFilter] -> Set PackageName
      exclude locals = Set.fromList . exclude' locals

      exclude' :: [PackageName] -> [ListDepsTextFilter] -> [PackageName]
      exclude' _ [] = []
      exclude' locals (f:fs) = case f of
        FilterPackage pkgName -> pkgName : exclude' locals fs
        FilterLocals -> locals <> exclude' locals fs
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
  let targets = opts.dotOpts.dotTargets
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
            if Just depth == dotOpts.dependencyDepth
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
  let remainingDepth = fromMaybe 999 dotOpts.dependencyDepth - depth
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
  T.pack (packageNameString name) <> opts.sep <>
  payloadText opts payload

payloadText :: ListDepsFormatOpts -> DotPayload -> Text
payloadText opts payload =
  if opts.license
    then licenseText payload
    else versionText payload

printJSON ::
     Set PackageName
  -> Map PackageName (Set PackageName, DotPayload)
  -> IO ()
printJSON pkgs dependencyMap =
  LBC8.putStrLn $ encode $ DependencyTree pkgs dependencyMap
