{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.PackageDump
License     : BSD-3-Clause
-}

module Stack.PackageDump
  ( Line
  , eachSection
  , eachPair
  , DumpPackage (..)
  , conduitDumpPackage
  , ghcPkgDump
  , ghcPkgDescribe
  , ghcPkgField
  , sinkMatching
  , pruneDeps
  ) where

import           Control.Monad.Extra ( whenJust )
import           Data.Attoparsec.Args ( EscapingMode (..), argsParser )
import           Data.Attoparsec.Text as P
import           Data.Conduit ( await, leftover, toConsumer, yield )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.Pretty as C
import qualified Distribution.Text as C
import           Distribution.Types.MungedPackageName
                   ( decodeCompatPackageName )
import           Path.Extra ( toFilePathNoTrailingSep )
import           RIO.Process ( HasProcessContext )
import qualified RIO.Text as T
import           Stack.Component ( fromCabalName )
import           Stack.GhcPkg ( createDatabase )
import           Stack.Prelude
import           Stack.Types.CompilerPaths ( GhcPkgExe (..), HasCompiler (..) )
import           Stack.Types.ComponentUtils ( unqualCompFromText )
import           Stack.Types.DumpPackage ( DumpPackage (..), SublibDump (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, parseGhcPkgId )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.PackageDump" module.
data PackageDumpException
  = MissingSingleField Text (Map Text [Line])
  | Couldn'tParseField Text [Line]
  deriving (Show, Typeable)

instance Exception PackageDumpException where
  displayException (MissingSingleField name values) = unlines $
    concat
      [ "Error: [S-4257]\n"
      , "Expected single value for field name "
      , show name
      , " when parsing ghc-pkg dump output:"
      ]
    : map (\(k, v) -> "    " ++ show (k, v)) (Map.toList values)
  displayException (Couldn'tParseField name ls) = concat
    [ "Error: [S-2016]\n"
    , "Couldn't parse the field "
    , show name
    , " from lines: "
    , show ls
    , "."
    ]

-- | Call @ghc-pkg dump@ with appropriate flags and stream to the given sink,
-- using either the global package database or the given package databases.
ghcPkgDump ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> [Path Abs Dir]
     -- ^ A list of package databases. If empty, use the global package
     -- database.
  -> ConduitM Text Void (RIO env) a
     -- ^ Sink.
  -> RIO env a
ghcPkgDump pkgexe = ghcPkgCmdArgs pkgexe ["dump"]

-- | Call @ghc-pkg describe@ with appropriate flags and stream to the given
-- sink, using either the global package database or the given package
-- databases.
ghcPkgDescribe ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> PackageName
  -> [Path Abs Dir]
     -- ^ A list of package databases. If empty, use the global package
     -- database.
  -> ConduitM Text Void (RIO env) a
     -- ^ Sink.
  -> RIO env a
ghcPkgDescribe pkgexe pkgName' = ghcPkgCmdArgs
  pkgexe
  ["describe", "--simple-output", packageNameString pkgName']

-- | Call @ghc-pkg field@ with appropriate flags and stream to the given sink,
-- using the given package database. Throws 'RIO.Process.ExitCodeException' if
-- the process fails (for example, if the package is not found in the package
-- database or the field is not found in the package's *.conf file).
ghcPkgField ::
     (HasCompiler env, HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> Path Abs Dir
     -- ^ A package database.
  -> MungedPackageId
     -- ^ A munged package identifier.
  -> String
     -- ^ A field name.
  -> ConduitM Text Void (RIO env) a
     -- ^ Sink.
  -> RIO env a
ghcPkgField pkgexe pkgDb mungedPkgId fieldName = ghcPkgCmdArgs
  pkgexe
  ["field", C.prettyShow mungedPkgId, fieldName, "--simple-output" ]
  [pkgDb]

-- | Call @ghc-pkg@ and stream to the given sink, using the either the global
-- package database or the given package databases.
ghcPkgCmdArgs ::
     (HasProcessContext env, HasTerm env)
  => GhcPkgExe
  -> [String]
     -- ^ A list of commands.
  -> [Path Abs Dir]
     -- ^ A list of package databases. If empty, use the global package
     -- database.
  -> ConduitM Text Void (RIO env) a
     -- ^ Sink.
  -> RIO env a
ghcPkgCmdArgs pkgexe@(GhcPkgExe pkgPath) cmd mpkgDbs sink = do
  case reverse mpkgDbs of
      (pkgDb:_) -> createDatabase pkgexe pkgDb -- TODO maybe use some retry logic instead?
      _ -> pure ()
  -- https://github.com/haskell/process/issues/251
  snd <$> sinkProcessStderrStdout (toFilePath pkgPath) args CL.sinkNull sink'
 where
  args = concat
    [ case mpkgDbs of
          [] -> ["--global", "--no-user-package-db"]
          _ ->   "--user"
               : "--no-user-package-db"
               : concatMap
                   (\pkgDb -> ["--package-db", toFilePathNoTrailingSep pkgDb])
                   mpkgDbs
    , cmd
    , ["--expand-pkgroot"]
    ]
  sink' = CT.decodeUtf8 .| sink

-- | Prune a list of possible packages down to those whose dependencies are met.
--
-- * id uniquely identifies an item
--
-- * There can be multiple items per name
pruneDeps ::
     (Ord name, Ord id)
  => (id -> name) -- ^ extract the name from an id
  -> (item -> id) -- ^ the id of an item
  -> (item -> [id]) -- ^ get the dependencies of an item
  -> (item -> item -> item) -- ^ choose the desired of two possible items
  -> [item] -- ^ input items
  -> Map name item
pruneDeps getName getId getDepends chooseBest =
    Map.fromList
  . fmap (getName . getId &&& id)
  . loop Set.empty Set.empty []
 where
  loop foundIds usedNames foundItems dps =
    case partitionEithers $ map depsMet dps of
      ([], _) -> foundItems
      (s', dps') ->
        let foundIds' = Map.fromListWith chooseBest s'
            foundIds'' = Set.fromList $ map getId $ Map.elems foundIds'
            usedNames' = Map.keysSet foundIds'
            foundItems' = Map.elems foundIds'
        in  loop
              (Set.union foundIds foundIds'')
              (Set.union usedNames usedNames')
              (foundItems ++ foundItems')
              (catMaybes dps')
   where
    depsMet dp
      | name `Set.member` usedNames = Right Nothing
      | all (`Set.member` foundIds) (getDepends dp) = Left (name, dp)
      | otherwise = Right $ Just dp
     where
      id' = getId dp
      name = getName id'

-- | Find the package IDs matching the given constraints with all dependencies installed.
-- Packages not mentioned in the provided @Map@ are allowed to be present too.
sinkMatching ::
     Monad m
  => Map PackageName Version -- ^ allowed versions
  -> ConduitM DumpPackage o m (Map PackageName DumpPackage)
sinkMatching allowed =
    Map.fromList
  . map (pkgName . (.packageIdent) &&& id)
  . Map.elems
  . pruneDeps
      id
      (.ghcPkgId)
      (.depends)
      const -- Could consider a better comparison in the future
  <$> (CL.filter (isAllowed . (.packageIdent)) .| CL.consume)
 where
  isAllowed (PackageIdentifier name version) =
    case Map.lookup name allowed of
      Just version' | version /= version' -> False
      _ -> True

-- | Convert a stream of bytes into a stream of @DumpPackage@s
conduitDumpPackage :: MonadThrow m
                   => ConduitM Text DumpPackage m ()
conduitDumpPackage = (.| CL.catMaybes) $ eachSection $ do
  pairs <- eachPair (\k -> (k, ) <$> CL.consume) .| CL.consume
  let m = Map.fromList pairs
  let parseS k =
        case Map.lookup k m of
          Just [v] -> pure v
          _ -> throwM $ MissingSingleField k m
      -- Can't fail: if not found, same as an empty list. See:
      -- https://github.com/commercialhaskell/stack/issues/182
      parseM k = Map.findWithDefault [] k m

      parseDepend :: MonadThrow m => Text -> m (Maybe GhcPkgId)
      parseDepend "builtin_rts" = pure Nothing
      parseDepend bs = Just <$> parseGhcPkgId bs'
       where
        (bs', _builtinRts) =
          case stripSuffixText " builtin_rts" bs of
            Nothing ->
              case stripPrefixText "builtin_rts " bs of
                Nothing -> (bs, False)
                Just x -> (x, True)
            Just x -> (x, True)
  case Map.lookup "id" m of
    Just ["builtin_rts"] -> pure Nothing
    _ -> do
      name <- parseS "name" >>= parsePackageNameThrowing . T.unpack
      version <- parseS "version" >>= parseVersionThrowing . T.unpack
      ghcPkgId <- parseS "id" >>= parseGhcPkgId

      -- if a package has no modules, these won't exist
      let libDirKey = "library-dirs"
          libraries = parseM "hs-libraries"
          exposedModules = parseM "exposed-modules"
          exposed = parseM "exposed"
          license =
            case parseM "license" of
              [licenseText] -> C.simpleParse (T.unpack licenseText)
              _ -> Nothing
      depends <- mapMaybeM parseDepend $ concatMap T.words $ parseM "depends"

      -- Handle sub-libraries by recording the name of the parent library
      -- If name of parent library is missing, this is not a sub-library.
      let maybePackageName :: Maybe PackageName =
            parseS "package-name" >>= parsePackageNameThrowing . T.unpack
          maybeLibName = parseS "lib-name"
          getLibNameFromLegacyName = case decodeCompatPackageName name of
            MungedPackageName _parentPackageName (LSubLibName libName') ->
              fromCabalName libName'
            MungedPackageName _parentPackageName _ -> ""
          libName =
            maybe getLibNameFromLegacyName unqualCompFromText maybeLibName
          sublib = flip SublibDump libName <$> maybePackageName
          parseQuoted key =
            case mapM (P.parseOnly (argsParser NoEscaping)) val of
              Left{} -> throwM (Couldn'tParseField key val)
              Right dirs -> pure (concat dirs)
           where
            val = parseM key
      libDirs <- parseQuoted libDirKey
      haddockInterfaces <- parseQuoted "haddock-interfaces"
      haddockHtml <- listToMaybe <$> parseQuoted "haddock-html"
      pure $ Just DumpPackage
        { ghcPkgId
        , packageIdent = PackageIdentifier name version
        , sublib
        , license
        , libDirs
        , libraries = T.words $ T.unwords libraries
        , hasExposedModules = not (null libraries || null exposedModules)
        -- Strip trailing commas from ghc package exposed-modules (looks buggy
        -- to me...). Then try to parse the module names.
        , exposedModules =
              Set.fromList
            $ mapMaybe (C.simpleParse . T.unpack . T.dropSuffix ",")
            $ T.words
            $ T.unwords exposedModules
        , depends
        , haddockInterfaces
        , haddockHtml
        , isExposed = exposed == ["True"]
        }

stripPrefixText :: Text -> Text -> Maybe Text
stripPrefixText x y
  | x `T.isPrefixOf` y = Just $ T.drop (T.length x) y
  | otherwise = Nothing

stripSuffixText :: Text -> Text -> Maybe Text
stripSuffixText x y
  | x `T.isSuffixOf` y = Just $ T.take (T.length y - T.length x) y
  | otherwise = Nothing

-- | A single line of input, not including line endings
type Line = Text

-- | Apply the given Sink to each section of output, broken by a single line containing ---
eachSection ::
     Monad m
  => ConduitM Line Void m a
  -> ConduitM Text a m ()
eachSection inner = CL.map (T.filter (/= '\r')) .| CT.lines .| start
 where
  peekText = await >>= maybe (pure Nothing) (\bs ->
    if T.null bs
      then peekText
      else leftover bs >> pure (Just bs))

  start = peekText >>= maybe (pure ()) (const go)

  go = do
    x <- toConsumer $ takeWhileC (/= "---") .| inner
    yield x
    CL.drop 1
    start

-- | Grab each key/value pair
eachPair ::
     Monad m
  => (Text -> ConduitM Line Void m a)
  -> ConduitM Line a m ()
eachPair inner = start
 where
  start = await >>= maybe (pure ()) start'

  start' bs1 = toConsumer (valSrc .| inner key) >>= yield >> start
   where
    (key, bs2) = T.break (== ':') bs1
    (spaces, bs3) = T.span (== ' ') $ T.drop 1 bs2
    ind = T.length key + 1 + T.length spaces

    valSrc
      | T.null bs3 = noIndent
      | otherwise = yield bs3 >> loopIndent ind

  noIndent = do
    mx <- await
    whenJust mx $ \bs -> do
      let (spaces, val) = T.span (== ' ') bs
      if T.length spaces == 0
        then leftover val
        else do
          yield val
          loopIndent (T.length spaces)

  loopIndent i = loop
   where
    loop = await >>= maybe (pure ()) go

    go bs
      | T.length spaces == i && T.all (== ' ') spaces =
          yield val >> loop
      | otherwise = leftover bs
     where
      (spaces, val) = T.splitAt i bs

-- | General purpose utility
takeWhileC :: Monad m => (a -> Bool) -> ConduitM a a m ()
takeWhileC f = loop
 where
  loop = await >>= maybe (pure ()) go

  go x
    | f x = yield x >> loop
    | otherwise = leftover x
