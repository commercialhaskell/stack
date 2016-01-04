{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Stack.Solver
    ( checkResolverSpec
    , cabalPackagesCheck
    , findCabalFiles
    , solveExtraDeps
    , solveResolverSpec
    ) where

import           Control.Applicative
import           Control.Exception (assert)
import           Control.Exception.Enclosed  (tryIO)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Aeson.Extended         (object, (.=), toJSON, logJSONWarnings)
import qualified Data.ByteString             as S
import           Data.Either
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (isSuffixOf, intercalate)
import           Data.List.Extra             (groupSortOn)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Text.Encoding.Error    (lenientDecode)
import qualified Data.Text.Lazy              as LT
import           Data.Text.Lazy.Encoding     (decodeUtf8With)
import qualified Data.Yaml                   as Yaml
import qualified Distribution.PackageDescription as C
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.Find                   (findFiles)
import           Path.IO                     (parseRelAsAbsDir)
import           Prelude
import           Stack.BuildPlan
import           Stack.Package               (printCabalFileWarning
                                             , readPackageUnresolved)
import           Stack.Setup
import           Stack.Setup.Installed
import           Stack.Types
import           Stack.Types.Internal        ( HasTerminal
                                             , HasReExec
                                             , HasLogLevel)
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing,
                                              getTemporaryDirectory,
                                              makeRelativeToCurrentDirectory)
import qualified System.FilePath             as FP
import           System.IO.Temp              (withSystemTempDirectory)
import           System.Process.Read

data ConstraintType = Constraint | Preference deriving (Eq)

cabalSolver :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadReader env m, HasConfig env)
            => EnvOverride
            -> [Path Abs Dir] -- ^ cabal files
            -> ConstraintType
            -> Map PackageName Version -- ^ constraints
            -> Map PackageName (Map FlagName Bool) -- ^ user-specified flags
            -> [String] -- ^ additional arguments
            -> m (Maybe (Map PackageName (Version, Map FlagName Bool)))
cabalSolver menv cabalfps constraintType constraints userFlags cabalArgs = withSystemTempDirectory "cabal-solver" $ \dir -> do
    configLines <- getCabalConfig dir constraintType constraints
    let configFile = dir FP.</> "cabal.config"
    liftIO $ S.writeFile configFile $ encodeUtf8 $ T.unlines configLines

    -- Run from a temporary directory to avoid cabal getting confused by any
    -- sandbox files, see:
    -- https://github.com/commercialhaskell/stack/issues/356
    --
    -- In theory we could use --ignore-sandbox, but not all versions of cabal
    -- support it.
    tmpdir <- liftIO getTemporaryDirectory >>= parseRelAsAbsDir

    let args = ("--config-file=" ++ configFile)
             : "install"
             : "--enable-tests"
             : "--enable-benchmarks"
             : "--dry-run"
             : "--only-dependencies"
             : "--reorder-goals"
             : "--max-backjumps=-1"
             : "--package-db=clear"
             : "--package-db=global"
             : cabalArgs ++
               toConstraintArgs userFlags ++
               fmap toFilePath cabalfps

    catch (liftM Just (readProcessStdout (Just tmpdir) menv "cabal" args))
          (\e@(ReadProcessException _ _ _ err) -> do
              let errMsg = decodeUtf8With lenientDecode err
              if LT.isInfixOf "Could not resolve dependencies" errMsg
              then do
                  $logInfo "Attempt failed."
                  $logInfo "\n>>>> Cabal errors begin"
                  $logInfo $ LT.toStrict errMsg
                             <> "<<<< Cabal errors end\n"
                  return Nothing
              else throwM e)
    >>= maybe (return Nothing) parseCabalOutput

  where
    parseCabalOutput bs = do
        let ls = drop 1
               $ dropWhile (not . T.isPrefixOf "In order, ")
               $ T.lines
               $ decodeUtf8 bs
            (errs, pairs) = partitionEithers $ map parseLine ls
        if null errs
          then return $ Just (Map.fromList pairs)
          else error $ "Could not parse cabal-install output: " ++ show errs

    parseLine t0 = maybe (Left t0) Right $ do
        -- Sample output to parse:
        -- text-1.2.1.1 (latest: 1.2.2.0) -integer-simple (via: parsec-3.1.9) (new package))
        -- An ugly parser to extract module id and flags
        let t1 = T.concat $
                 [ T.takeWhile (/= '(')
                 ,   (T.takeWhile (/= '('))
                   . (T.drop 1)
                   . (T.dropWhile (/= ')'))
                 ] <*> [t0]

        ident':flags' <- Just $ T.words t1
        PackageIdentifier name version <-
            parsePackageIdentifierFromString $ T.unpack ident'
        flags <- mapM parseFlag flags'
        Just (name, (version, Map.fromList flags))
    parseFlag t0 = do
        flag <- parseFlagNameFromString $ T.unpack t1
        return (flag, enabled)
      where
        (t1, enabled) =
            case T.stripPrefix "-" t0 of
                Nothing ->
                    case T.stripPrefix "+" t0 of
                        Nothing -> (t0, True)
                        Just x -> (x, True)
                Just x -> (x, False)
    toConstraintArgs userFlagMap =
        [formatFlagConstraint package flag enabled
            | (package, fs) <- Map.toList userFlagMap
            , (flag, enabled) <- Map.toList fs]

    formatFlagConstraint package flag enabled =
        let sign = if enabled then '+' else '-'
        in
        "--constraint=" ++ unwords [packageNameString package, sign : flagNameString flag]

getCabalConfig :: (MonadLogger m, MonadReader env m, HasConfig env, MonadIO m, MonadThrow m)
               => FilePath -- ^ temp dir
               -> ConstraintType
               -> Map PackageName Version -- ^ constraints
               -> m [Text]
getCabalConfig dir constraintType constraints = do
    indices <- asks $ configPackageIndices . getConfig
    remotes <- mapM goIndex indices
    let cache = T.pack $ "remote-repo-cache: " ++ dir
    return $ cache : remotes ++ map goConstraint (Map.toList constraints)
  where
    goIndex index = do
        src <- configPackageIndex $ indexName index
        let dstdir = dir FP.</> T.unpack (indexNameText $ indexName index)
            dst = dstdir FP.</> "00-index.tar"
        liftIO $ void $ tryIO $ do
            createDirectoryIfMissing True dstdir
            copyFile (toFilePath src) dst
        return $ T.concat
            [ "remote-repo: "
            , indexNameText $ indexName index
            , ":http://0.0.0.0/fake-url"
            ]

    goConstraint (name, version) = T.concat
        [ (if constraintType == Constraint
           then "constraint: "
           else "preference: ")
        , T.pack $ packageNameString name
        , "=="
        , T.pack $ versionString version
        ]

setupCompiler
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => CompilerVersion
    -> m (Maybe ExtraDirs)
setupCompiler compiler = do
    let msg = Just $ T.concat
          [ "Compiler version (" <> compilerVersionText compiler <> ") "
          , "required by your resolver specification cannot be found.\n\n"
          , "Please use '--install-ghc' command line switch to automatically "
          , "install the compiler or '--system-ghc' to use a suitable "
          , "compiler available on your PATH." ]

    config <- asks getConfig
    mpaths <- ensureCompiler SetupOpts
        { soptsInstallIfMissing  = configInstallGHC config
        , soptsUseSystem         = configSystemGHC config
        , soptsWantedCompiler    = compiler
        , soptsCompilerCheck     = configCompilerCheck config

        , soptsStackYaml         = Nothing
        , soptsForceReinstall    = False
        , soptsSanityCheck       = False
        , soptsSkipGhcCheck      = False
        , soptsSkipMsys          = configSkipMsys config
        , soptsUpgradeCabal      = False
        , soptsResolveMissingGHC = msg
        , soptsStackSetupYaml    = defaultStackSetupYaml
        , soptsGHCBindistURL     = Nothing
        }

    return mpaths

setupCabalEnv
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => CompilerVersion
    -> m EnvOverride
setupCabalEnv compiler = do
    mpaths <- setupCompiler compiler
    menv0 <- getMinimalEnvOverride
    envMap <- removeHaskellEnvVars
              <$> augmentPathMap (maybe [] edBins mpaths)
                                 (unEnvOverride menv0)
    platform <- asks getPlatform
    menv <- mkEnvOverride platform envMap

    mcabal <- findExecutable menv "cabal"
    case mcabal of
        Nothing -> throwM SolverMissingCabalInstall
        Just _ -> return ()

    mver <- getSystemCompiler menv (whichCompiler compiler)
    case mver of
        Just (version, _) ->
            $logInfo $ "Using compiler: " <> compilerVersionText version
        Nothing -> error "Failed to determine compiler version. \
                         \This is most likely a bug."
    return menv

solveResolverSpec
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File  -- ^ stack.yaml file location
    -> [Path Abs Dir] -- ^ package dirs containing cabal files
    -> Map PackageName Version -- ^ local packages to build
    -> ( Resolver
       , Map PackageName (Map FlagName Bool)
       , Map PackageName Version)
    -> m (Maybe ( Resolver
               , Map PackageName (Map FlagName Bool)
               , Map PackageName Version))
solveResolverSpec stackYaml cabalDirs srcPackages
                  (resolver, flags, extraPackages) = do
    $logInfo $ "Using resolver: " <> resolverName resolver
    (compilerVer, snapPackages) <- getResolverMiniPlan resolver
    menv <- setupCabalEnv compilerVer

    let assertVer v = assert ((not . null . versionString) v) $ return ()
    mapM_ assertVer snapPackages
    mapM_ assertVer extraPackages

    -- Note - The order in Map.union below is important.
    -- We prefer extraPackages over the snapshot packages.
    let externalPackages = Map.union extraPackages snapPackages
        -- We cannot have the snapshot versions of source packages as
        -- constraints.
        constraints = Map.difference externalPackages srcPackages
        solver t = cabalSolver menv cabalDirs t constraints flags $
                          ["-v"] -- TODO make it conditional on debug
                       ++ ["--ghcjs" | (whichCompiler compilerVer) == Ghcjs]

    let srcNames = (T.intercalate " and ") $
          ["packages from " <> resolverName resolver
              | not (Map.null snapPackages)] ++
          [T.pack ((show $ Map.size extraPackages) <> " external packages")
              | not (Map.null extraPackages)]

    $logInfo "Asking cabal to calculate a build plan..."
    unless (Map.null constraints)
        ($logInfo $ "Trying with " <> srcNames <> " as hard constraints...")

    mdeps <- solver Constraint
    mdeps' <- case mdeps of
        Nothing | not (Map.null constraints) -> do
            $logInfo $ "Retrying with " <> srcNames <> " as preferences..."
            solver Preference
        _ -> return mdeps

    case mdeps' of
      Just pairs -> do
        let versiondiff (v, f) v' = if v == v' then Nothing else Just (v, f)
            newPairs = Map.differenceWith versiondiff pairs snapPackages

        $logInfo $ "Successfully determined a build plan with "
                 <> T.pack (show $ Map.size newPairs)
                 <> " external dependencies."

        return $ Just ( resolver
               , Map.filter (not . Map.null) (fmap snd pairs)
               , fmap fst newPairs)
      Nothing -> do
        $logInfo $ "Failed to arrive at a workable build plan using "
                   <> resolverName resolver <> " resolver."
        return Nothing
    where
      getResolverMiniPlan (ResolverSnapshot snapName) = do
          mbp <- loadMiniBuildPlan snapName
          return (mbpCompilerVersion mbp, fmap mpiVersion (mbpPackages mbp))

      getResolverMiniPlan (ResolverCompiler compiler) =
          return (compiler, Map.empty)

      -- FIXME instead of passing the stackYaml dir we should maintain
      -- the file URL in the custom resolver always relative to stackYaml.
      getResolverMiniPlan (ResolverCustom _ url) = do
          mbp <- parseCustomMiniBuildPlan stackYaml url
          return (mbpCompilerVersion mbp, fmap mpiVersion (mbpPackages mbp))

-- | Given a bundle of packages and a resolver, check the resolver with respect
-- to the packages and return how well the resolver satisfies the depndencies
-- of the packages. If 'flags' is passed as 'Nothing' then flags are chosen
-- automatically.

checkResolverSpec
    :: ( MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m
       , HasHttpManager env, HasConfig env, HasGHCVariant env
       , MonadBaseControl IO m)
    => [C.GenericPackageDescription]
    -> Maybe (Map PackageName (Map FlagName Bool))
    -> Resolver
    -> m BuildPlanCheck
checkResolverSpec gpds flags resolver = do
    case resolver of
      ResolverSnapshot name -> checkSnapBuildPlan gpds flags name
      ResolverCompiler _ -> return $ BuildPlanCheckPartial Map.empty Map.empty
      -- TODO support custom resolver for stack init
      ResolverCustom _ _ -> return $ BuildPlanCheckPartial Map.empty Map.empty

findCabalFiles :: MonadIO m => Bool -> Path Abs Dir -> m [Path Abs File]
findCabalFiles recurse dir =
    liftIO $ findFiles dir isCabal (\subdir -> recurse && not (isIgnored subdir))
  where
    isCabal path = ".cabal" `isSuffixOf` toFilePath path

    isIgnored path = FP.dropTrailingPathSeparator (toFilePath (dirname path))
                     `Set.member` ignoredDirs

-- | Special directories that we don't want to traverse for .cabal files
ignoredDirs :: Set FilePath
ignoredDirs = Set.fromList
    [ ".git"
    , "dist"
    , ".stack-work"
    ]

-- | Do some basic checks on a list of cabal file paths to be used for creating
-- stack config, print some informative and error messages and if all is ok
-- return @GenericPackageDescription@ list.
cabalPackagesCheck
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
     => [Path Abs File]
     -> String
     -> String
     -> m [C.GenericPackageDescription]
cabalPackagesCheck cabalfps noPkgMsg dupPkgFooter = do
    when (null cabalfps) $
        error noPkgMsg

    relpaths <- mapM makeRel cabalfps
    $logInfo $ "Using cabal packages:"
    $logInfo $ T.pack (formatGroup relpaths)

    when (dupGroups relpaths /= []) $
        error $ "Duplicate cabal package names cannot be used in a single "
        <> "stack project. Following duplicates were found:\n"
        <> intercalate "\n" (dupGroups relpaths)
        <> "\n"
        <> dupPkgFooter

    (warnings,gpds) <- fmap unzip (mapM readPackageUnresolved cabalfps)
    zipWithM_ (mapM_ . printCabalFileWarning) cabalfps warnings
    return gpds

    where
        makeRel         = liftIO . makeRelativeToCurrentDirectory . toFilePath
        groups          = filter ((> 1) . length) . groupSortOn (FP.takeFileName)
        dupGroups       = (map formatGroup) . groups
        formatPath path = "- " <> path <> "\n"
        formatGroup     = concat . (map formatPath)

-- | Determine missing extra-deps
solveExtraDeps
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasEnvConfig env, HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Bool -- ^ modify stack.yaml?
    -> m ()
solveExtraDeps modStackYaml = do
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig

    let stackYaml = bcStackYaml bconfig
    relStackYaml <- liftIO $ makeRelativeToCurrentDirectory
                           $ toFilePath stackYaml

    $logInfo $ "Using configuration file: " <> T.pack relStackYaml
    let cabalDirs = Map.keys $ envConfigPackages econfig
        noPkgMsg = "No cabal packages found in " <> relStackYaml <>
                   ". Please add at least one directory containing a .cabal \
                   \file. You can also use 'stack init' to automatically \
                   \generate the config file."
        dupPkgFooter = "Please remove the directories containing duplicate \
                       \entries from '" <> relStackYaml <> "'."

    cabalfps  <- liftM concat (mapM (findCabalFiles False) cabalDirs)
    gpds <- cabalPackagesCheck cabalfps noPkgMsg dupPkgFooter

    let oldFlags     = bcFlags bconfig
        oldExtraDeps = bcExtraDeps bconfig
        resolver     = bcResolver bconfig

    result <- checkResolverSpec gpds (Just oldFlags) resolver
    resolverSpec <- case result of
        BuildPlanCheckFail _ _ -> throwM $ ResolverMismatch resolver
        BuildPlanCheckOk flags -> return $ Just (resolver, flags, Map.empty)
        BuildPlanCheckPartial _ _ -> solveResolverSpec stackYaml cabalDirs
                                                       (gpdPackages gpds)
                                                       ( resolver
                                                       , oldFlags
                                                       , oldExtraDeps)

    (flags, extraDeps) <- case resolverSpec of
        Nothing -> throwM (SolverGiveUp Nothing)
        Just (_, f, e) -> return (f, e)

    let
        vDiff v v' = if v == v' then Nothing else Just v
        depsDiff = Map.differenceWith vDiff
        newDeps  = depsDiff extraDeps oldExtraDeps
        goneDeps = depsDiff oldExtraDeps extraDeps

        fDiff f f' = if f == f' then Nothing else Just f
        flagsDiff  = Map.differenceWith fDiff
        newFlags   = flagsDiff flags oldFlags
        goneFlags  = flagsDiff oldFlags flags

        changed =    any (not . Map.null) [newDeps, goneDeps]
                  || any (not . Map.null) [newFlags, goneFlags]

    if changed then do
        $logInfo $ "The following changes will be made to "
                   <> T.pack relStackYaml <> ":"

        printFlags newFlags  "New"
        printDeps  newDeps   "New"

        printFlags goneFlags "Deleted"
        printDeps  goneDeps  "Deleted"

        if modStackYaml then do
            writeStackYaml stackYaml extraDeps flags
            $logInfo $ "Updated " <> T.pack relStackYaml
        else do
            $logInfo $ "To automatically update " <> T.pack relStackYaml
                       <> ", rerun with '--modify-stack-yaml'"
     else
        $logInfo $ "No changes needed to " <> T.pack relStackYaml

    where
        printFlags fl msg = do
            when ((not . Map.null) fl) $ do
                $logInfo ""
                $logInfo $ T.pack msg <> " flags:"
                $logInfo $ decodeUtf8 $ Yaml.encode $ object ["flags" .= fl]

        printDeps deps msg = do
            when ((not . Map.null) deps) $ do
                $logInfo ""
                $logInfo $ T.pack msg <> " dependencies:"
                $logInfo $ decodeUtf8 $ Yaml.encode $ object $
                        [("extra-deps" .= map fromTuple (Map.toList deps))]

        writeStackYaml path deps fl = do
            let fp = toFilePath path
            obj <- liftIO (Yaml.decodeFileEither fp) >>= either throwM return
            (ProjectAndConfigMonoid _ _, warnings) <-
                liftIO (Yaml.decodeFileEither fp) >>= either throwM return
            logJSONWarnings fp warnings
            let obj' =
                    HashMap.insert "extra-deps"
                        (toJSON $ map fromTuple $ Map.toList deps)
                  $ HashMap.insert ("flags" :: Text) (toJSON fl) obj
            liftIO $ Yaml.encodeFile fp obj'
