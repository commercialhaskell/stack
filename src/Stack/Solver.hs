{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Stack.Solver
    ( checkResolverSpec
    , cabalPackagesCheck
    , findCabalFiles
    , getResolverConstraints
    , mergeConstraints
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
import           Data.Aeson.Extended         ( WithJSONWarnings(..), object, (.=), toJSON
                                             , logJSONWarnings)
import qualified Data.ByteString             as S
import           Data.Either
import           Data.Function               (on)
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   ( (\\), isSuffixOf, intercalate
                                             , minimumBy, isPrefixOf)
import           Data.List.Extra             (groupSortOn)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, isNothing, mapMaybe)
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
import qualified Distribution.Package        as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text           as C
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.Find                   (findFiles)
import           Path.IO                     hiding (findExecutable, findFiles)
import           Prelude
import           Safe                        (headMay)
import           Stack.BuildPlan
import           Stack.Constants             (stackDotYaml)
import           Stack.Package               (printCabalFileWarning
                                             , hpack
                                             , readPackageUnresolved)
import           Stack.Setup
import           Stack.Setup.Installed
import           Stack.Types
import           Stack.Types.Internal        ( HasTerminal
                                             , HasReExec
                                             , HasLogLevel)
import qualified System.Directory            as D
import qualified System.FilePath             as FP
import           System.Process.Read

data ConstraintType = Constraint | Preference deriving (Eq)
type ConstraintSpec = Map PackageName (Version, Map FlagName Bool)

cabalSolver :: (MonadIO m, MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadReader env m, HasConfig env)
            => EnvOverride
            -> [Path Abs Dir] -- ^ cabal files
            -> ConstraintType
            -> ConstraintSpec -- ^ src constraints
            -> ConstraintSpec -- ^ dep constraints
            -> [String] -- ^ additional arguments
            -> m (Either [PackageName] ConstraintSpec)
cabalSolver menv cabalfps constraintType
            srcConstraints depConstraints cabalArgs =
  withSystemTempDir "cabal-solver" $ \dir' -> do

    let versionConstraints = fmap fst depConstraints
        dir = toFilePath dir'
    configLines <- getCabalConfig dir constraintType versionConstraints
    let configFile = dir FP.</> "cabal.config"
    liftIO $ S.writeFile configFile $ encodeUtf8 $ T.unlines configLines

    -- Run from a temporary directory to avoid cabal getting confused by any
    -- sandbox files, see:
    -- https://github.com/commercialhaskell/stack/issues/356
    --
    -- In theory we could use --ignore-sandbox, but not all versions of cabal
    -- support it.
    tmpdir <- getTempDir

    let args = ("--config-file=" ++ configFile)
             : "install"
             : "--enable-tests"
             : "--enable-benchmarks"
             : "--dry-run"
             : "--reorder-goals"
             : "--max-backjumps=-1"
             : "--package-db=clear"
             : "--package-db=global"
             : cabalArgs ++
               toConstraintArgs (flagConstraints constraintType) ++
               fmap toFilePath cabalfps

    catch (liftM Right (readProcessStdout (Just tmpdir) menv "cabal" args))
          (\ex -> case ex of
              ReadProcessException _ _ _ err -> return $ Left err
              _ -> throwM ex)
    >>= either parseCabalErrors parseCabalOutput

  where
    errCheck = T.isInfixOf "Could not resolve dependencies"
    cabalBuildErrMsg e =
               ">>>> Cabal errors begin\n"
            <> e
            <> "<<<< Cabal errors end\n"

    parseCabalErrors err = do
        let errExit e = error $ "Could not parse cabal-install errors:\n\n"
                              ++ cabalBuildErrMsg (T.unpack e)
            msg = LT.toStrict $ decodeUtf8With lenientDecode err

        if errCheck msg then do
            $logInfo "Attempt failed.\n"
            $logInfo $ cabalBuildErrMsg msg
            let pkgs = parseConflictingPkgs msg
                mPkgNames = map (C.simpleParse . T.unpack) pkgs
                pkgNames  = map (fromCabalPackageName . C.pkgName)
                                (catMaybes mPkgNames)

            when (any isNothing mPkgNames) $ do
                  $logInfo $ "*** Only some package names could be parsed: " <>
                      (T.pack (intercalate ", " (map show pkgNames)))
                  error $ T.unpack $
                       "*** User packages involved in cabal failure: "
                       <> (T.intercalate ", " $ parseConflictingPkgs msg)

            if pkgNames /= [] then do
                  return $ Left pkgNames
            else errExit msg
        else errExit msg

    parseConflictingPkgs msg =
        let ls = dropWhile (not . errCheck) $ T.lines msg
            select s = ((T.isPrefixOf "trying:" s)
                      || (T.isPrefixOf "next goal:" s))
                      && (T.isSuffixOf "(user goal)" s)
            pkgName =   (take 1)
                      . T.words
                      . (T.drop 1)
                      . (T.dropWhile (/= ':'))
        in concat $ map pkgName (filter select ls)

    parseCabalOutput bs = do
        let ls = drop 1
               $ dropWhile (not . T.isPrefixOf "In order, ")
               $ T.lines
               $ decodeUtf8 bs
            (errs, pairs) = partitionEithers $ map parseLine ls
        if null errs
          then return $ Right (Map.fromList pairs)
          else error $ "The following lines from cabal-install output could \
                       \not be parsed: \n"
                       ++ (T.unpack (T.intercalate "\n" errs))

    -- An ugly parser to extract module id and flags
    parseLine :: Text -> Either Text (PackageName, (Version, Map FlagName Bool))
    parseLine t0 = maybe (Left t0) Right $ do
        -- Sample outputs to parse:
        -- text-1.2.1.1 (latest: 1.2.2.0) -integer-simple (via: parsec-3.1.9) (new package))
        -- hspec-snap-1.0.0.0 *test (via: servant-snap-0.5) (new package)
        -- time-locale-compat-0.1.1.1 -old-locale (via: http-api-data-0.2.2) (new package))

        if (not $ T.null t0) then do
            ident':rest <- Just $ T.words t0
            PackageIdentifier name version <-
                parsePackageIdentifierFromString $ T.unpack ident'

            nextWord <- headMay rest
            rest' <- case T.head nextWord of
                '(' -> Just $ dropWhile (")" `T.isSuffixOf`) rest
                '*' -> Just $ dropWhile ("*" `T.isPrefixOf`) rest
                '+' -> Just rest
                '-' -> Just rest
                _ -> Nothing

            let fl = takeWhile (not . ("(" `T.isPrefixOf`)) rest'
            flags <- mapM parseFlag fl
            Just (name, (version, Map.fromList flags))
        else Nothing

    parseFlag :: Text -> Maybe (FlagName, Bool)
    parseFlag t0 = do
        (fl, st) <- case T.head t0 of
                '-' -> Just $ (T.tail t0, False)
                '+' -> Just $ (T.tail t0, True)
                _   -> Nothing
        if (not $ T.null fl) then do
            flag <- parseFlagNameFromString $ T.unpack fl
            return (flag, st)
        else Nothing

    toConstraintArgs userFlagMap =
        [formatFlagConstraint package flag enabled
            | (package, fs) <- Map.toList userFlagMap
            , (flag, enabled) <- Map.toList fs]

    formatFlagConstraint package flag enabled =
        let sign = if enabled then '+' else '-'
        in
        "--constraint=" ++ unwords [packageNameString package, sign : flagNameString flag]

    -- Note the order of the Map union is important
    -- We override a package in snapshot by a src package
    flagConstraints Constraint = fmap snd (Map.union srcConstraints
                                           depConstraints)
    -- Even when using preferences we want to
    -- keep the src package flags unchanged
    -- TODO - this should be done only for manual flags.
    flagConstraints Preference = fmap snd srcConstraints

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
            D.createDirectoryIfMissing True dstdir
            D.copyFile (toFilePath src) dst
        return $ T.concat
            [ "remote-repo: "
            , indexNameText $ indexName index
            , ":http://0.0.0.0/fake-url"
            ]

    goConstraint (name, version) =
        assert (not . null . versionString $ version) $
            T.concat
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

-- | Merge two separate maps, one defining constraints on package versions and
-- the other defining package flagmap, into a single map of version and flagmap
-- tuples.
mergeConstraints
    :: Map PackageName v
    -> Map PackageName (Map p f)
    -> Map PackageName (v, Map p f)
mergeConstraints = Map.mergeWithKey
    -- combine entry in both maps
    (\_ v f -> Just (v, f))
    -- convert entry in first map only
    (fmap (flip (,) Map.empty))
    -- convert entry in second map only
    (\m -> if Map.null m then Map.empty
           else error "Bug: An entry in flag map must have a corresponding \
                      \entry in the version map")

-- | Given a resolver, user package constraints (versions and flags) and extra
-- dependency constraints determine what extra dependencies are required
-- outside the resolver snapshot and the specified extra dependencies.
--
-- First it tries by using the snapshot and the input extra dependencies
-- as hard constraints, if no solution is arrived at by using hard
-- constraints it then tries using them as soft constraints or preferences.
--
-- It returns either conflicting packages when no solution is arrived at
-- or the solution in terms of src package flag settings and extra
-- dependencies.
solveResolverSpec
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File  -- ^ stack.yaml file location
    -> [Path Abs Dir] -- ^ package dirs containing cabal files
    -> ( Resolver
       , ConstraintSpec
       , ConstraintSpec) -- ^ ( resolver
                         --   , src package constraints
                         --   , extra dependency constraints )
    -> m (Either [PackageName] (ConstraintSpec , ConstraintSpec))
       -- ^ (Conflicting packages
       --    (resulting src package specs, external dependency specs))

solveResolverSpec stackYaml cabalDirs
                  (resolver, srcConstraints, extraConstraints) = do
    $logInfo $ "Using resolver: " <> resolverName resolver
    (compilerVer, snapConstraints) <- getResolverConstraints stackYaml resolver
    menv <- setupCabalEnv compilerVer

    let -- Note - The order in Map.union below is important.
        -- We want to override snapshot with extra deps
        depConstraints = Map.union extraConstraints snapConstraints
        -- Make sure to remove any user packages from the dep constraints
        -- There are two reasons for this:
        -- 1. We do not want snapshot versions to override the sources
        -- 2. Sources may have blank versions leading to bad cabal constraints
        depOnlyConstraints = Map.difference depConstraints srcConstraints
        solver t = cabalSolver menv cabalDirs t
                               srcConstraints depOnlyConstraints $
                          ["-v"] -- TODO make it conditional on debug
                       ++ ["--ghcjs" | (whichCompiler compilerVer) == Ghcjs]

    let srcNames = (T.intercalate " and ") $
          ["packages from " <> resolverName resolver
              | not (Map.null snapConstraints)] ++
          [T.pack ((show $ Map.size extraConstraints) <> " external packages")
              | not (Map.null extraConstraints)]

    $logInfo "Asking cabal to calculate a build plan..."
    unless (Map.null depOnlyConstraints)
        ($logInfo $ "Trying with " <> srcNames <> " as hard constraints...")

    eresult <- solver Constraint
    eresult' <- case eresult of
        Left _ | not (Map.null depOnlyConstraints) -> do
            $logInfo $ "Retrying with " <> srcNames <> " as preferences..."
            solver Preference
        _ -> return eresult

    case eresult' of
        Right deps -> do
            let
                -- All src package constraints returned by cabal.
                -- Flags may have changed.
                srcs = Map.intersection deps srcConstraints
                inSnap = Map.intersection deps snapConstraints
                -- All packages which are in the snapshot but cabal solver
                -- returned versions or flags different from the snapshot.
                inSnapChanged = Map.differenceWith diffConstraints
                                                   inSnap snapConstraints
                -- Packages neither in snapshot, nor srcs
                extra = Map.difference deps (Map.union srcConstraints
                                                       snapConstraints)
                external = Map.union inSnapChanged extra

            -- Just in case.
            -- If cabal output contains versions of user packages, those
            -- versions better be the same as those in our cabal file i.e.
            -- cabal should not be solving using versions from external
            -- indices.
            let outVers  = fmap fst srcs
                inVers   = fmap fst srcConstraints
                bothVers = Map.intersectionWith (\v1 v2 -> (v1, v2))
                                                inVers outVers
            when (not $ outVers `Map.isSubmapOf` inVers) $ do
                let msg = "Error: user package versions returned by cabal \
                          \solver are not the same as the versions in the \
                          \cabal files:\n"
                -- TODO We can do better in formatting the message
                error $ T.unpack $ msg
                        <> (showItems $ map show (Map.toList bothVers))

            $logInfo $ "Successfully determined a build plan with "
                     <> T.pack (show $ Map.size external)
                     <> " external dependencies."

            return $ Right (srcs, external)
        Left x -> do
            $logInfo $ "*** Failed to arrive at a workable build plan."
            return $ Left x
    where
        -- Think of the first map as the deps reported in cabal output and
        -- the second as the snapshot packages

        -- Note: For flags we only require that the flags in cabal output be a
        -- subset of the snapshot flags. This is to avoid a false difference
        -- reporting due to any spurious flags in the build plan which will
        -- always be absent in the cabal output.
        diffConstraints
            :: (Eq v, Eq a, Ord k)
            => (v, Map k a) -> (v, Map k a) -> Maybe (v, Map k a)
        diffConstraints (v, f) (v', f')
            | (v == v') && (f `Map.isSubmapOf` f') = Nothing
            | otherwise              = Just (v, f)

-- | Given a resolver (snpashot, compiler or custom resolver)
-- return the compiler version, package versions and packages flags
-- for that resolver.
getResolverConstraints
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
    => Path Abs File
    -> Resolver
    -> m (CompilerVersion,
          Map PackageName (Version, Map FlagName Bool))
getResolverConstraints stackYaml resolver =
    case resolver of
        ResolverSnapshot snapName -> do
            mbp <- loadMiniBuildPlan snapName
            return (mbpCompilerVersion mbp, mbpConstraints mbp)
        ResolverCustom _ url -> do
            -- FIXME instead of passing the stackYaml dir we should maintain
            -- the file URL in the custom resolver always relative to stackYaml.
            mbp <- parseCustomMiniBuildPlan stackYaml url
            return (mbpCompilerVersion mbp, mbpConstraints mbp)
        ResolverCompiler compiler ->
            return (compiler, Map.empty)
    where
      mpiConstraints mpi = (mpiVersion mpi, mpiFlags mpi)
      mbpConstraints mbp = fmap mpiConstraints (mbpPackages mbp)

-- | Given a bundle of user packages, flag constraints on those packages and a
-- resolver, determine if the resolver fully, partially or fails to satisfy the
-- dependencies of the user packages.
--
-- If the package flags are passed as 'Nothing' then flags are chosen
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
      ResolverCompiler {} -> return $ BuildPlanCheckPartial Map.empty Map.empty
      -- TODO support custom resolver for stack init
      ResolverCustom {} -> return $ BuildPlanCheckPartial Map.empty Map.empty

-- | Finds all files with a .cabal extension under a given directory. If
-- a `hpack` `package.yaml` file exists, this will be used to generate a cabal
-- file.
-- Subdirectories can be included depending on the @recurse@ parameter.
findCabalFiles :: MonadIO m => Bool -> Path Abs Dir -> m [Path Abs File]
findCabalFiles recurse dir = liftIO $ do
    findFiles dir isHpack subdirFilter >>= mapM_ (hpack . parent)
    findFiles dir isCabal subdirFilter
  where
    subdirFilter subdir = recurse && not (isIgnored subdir)
    isHpack = (== "package.yaml")     . toFilePath . filename
    isCabal = (".cabal" `isSuffixOf`) . toFilePath

    isIgnored path = "." `isPrefixOf` dirName || dirName `Set.member` ignoredDirs
      where
        dirName = FP.dropTrailingPathSeparator (toFilePath (dirname path))

-- | Special directories that we don't want to traverse for .cabal files
ignoredDirs :: Set FilePath
ignoredDirs = Set.fromList
    [ "dist"
    ]

-- | Perform some basic checks on a list of cabal files to be used for creating
-- stack config. It checks for duplicate package names, package name and
-- cabal file name mismatch and reports any issues related to those.
--
-- If no error occurs it returns filepath and @GenericPackageDescription@s
-- pairs as well as any filenames for duplicate packages not included in the
-- pairs.
cabalPackagesCheck
    :: ( MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadMask m
       , MonadReader env m, HasConfig env , HasGHCVariant env
       , HasHttpManager env , HasLogLevel env , HasReExec env
       , HasTerminal env)
     => [Path Abs File]
     -> String
     -> Maybe String
     -> m ( Map PackageName (Path Abs File, C.GenericPackageDescription)
          , [Path Abs File])
cabalPackagesCheck cabalfps noPkgMsg dupErrMsg = do
    when (null cabalfps) $
        error noPkgMsg

    relpaths <- mapM prettyPath cabalfps
    $logInfo $ "Using cabal packages:"
    $logInfo $ T.pack (formatGroup relpaths)

    (warnings, gpds) <- fmap unzip (mapM readPackageUnresolved cabalfps)
    zipWithM_ (mapM_ . printCabalFileWarning) cabalfps warnings

    -- package name cannot be empty or missing otherwise
    -- it will result in cabal solver failure.
    -- stack requires packages name to match the cabal file name
    -- Just the latter check is enough to cover both the cases

    let packages  = zip cabalfps gpds
        getNameMismatchPkg (fp, gpd)
            | (show . gpdPackageName) gpd /= (FP.takeBaseName . toFilePath) fp
                = Just fp
            | otherwise = Nothing
        nameMismatchPkgs = mapMaybe getNameMismatchPkg packages

    when (nameMismatchPkgs /= []) $ do
        rels <- mapM prettyPath nameMismatchPkgs
        error $ "Package name as defined in the .cabal file must match the \
                \.cabal file name.\n\
                \Please fix the following packages and try again:\n"
                <> (formatGroup rels)

    let dupGroups = filter ((> 1) . length)
                            . groupSortOn (gpdPackageName . snd)
        dupAll    = concat $ dupGroups packages

        -- Among duplicates prefer to include the ones in upper level dirs
        pathlen     = length . FP.splitPath . toFilePath . fst
        getmin      = minimumBy (compare `on` pathlen)
        dupSelected = map getmin (dupGroups packages)
        dupIgnored  = dupAll \\ dupSelected
        unique      = packages \\ dupIgnored

    when (dupIgnored /= []) $ do
        dups <- mapM (mapM (prettyPath. fst)) (dupGroups packages)
        $logWarn $ T.pack $
            "Following packages have duplicate package names:\n"
            <> intercalate "\n" (map formatGroup dups)
        case dupErrMsg of
          Nothing -> $logWarn $ T.pack $
                 "Packages with duplicate names will be ignored.\n"
              <> "Packages in upper level directories will be preferred.\n"
          Just msg -> error msg

    return (Map.fromList
            $ map (\(file, gpd) -> (gpdPackageName gpd,(file, gpd))) unique
           , map fst dupIgnored)

formatGroup :: [String] -> String
formatGroup = concatMap (\path -> "- " <> path <> "\n")

reportMissingCabalFiles :: (MonadIO m, MonadThrow m, MonadLogger m)
  => [Path Abs File]   -- ^ Directories to scan
  -> Bool              -- ^ Whether to scan sub-directories
  -> m ()
reportMissingCabalFiles cabalfps includeSubdirs = do
    allCabalfps <- findCabalFiles includeSubdirs =<< getCurrentDir

    relpaths <- mapM prettyPath (allCabalfps \\ cabalfps)
    unless (null relpaths) $ do
        $logWarn $ "The following packages are missing from the config:"
        $logWarn $ T.pack (formatGroup relpaths)

-- TODO Currently solver uses a stack.yaml in the parent chain when there is
-- no stack.yaml in the current directory. It should instead look for a
-- stack yaml only in the current directory and suggest init if there is
-- none available. That will make the behavior consistent with init and provide
-- a correct meaning to a --ignore-subdirs option if implemented.

-- | Verify the combination of resolver, package flags and extra
-- dependencies in an existing stack.yaml and suggest changes in flags or
-- extra dependencies so that the specified packages can be compiled.
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
    relStackYaml <- toFilePath <$> makeRelativeToCurrentDir stackYaml

    $logInfo $ "Using configuration file: " <> T.pack relStackYaml
    let cabalDirs = Map.keys $ envConfigPackages econfig
        noPkgMsg = "No cabal packages found in " <> relStackYaml <>
                   ". Please add at least one directory containing a .cabal \
                   \file. You can also use 'stack init' to automatically \
                   \generate the config file."
        dupPkgFooter = "Please remove the directories containing duplicate \
                       \entries from '" <> relStackYaml <> "'."

    cabalfps  <- liftM concat (mapM (findCabalFiles False) cabalDirs)
    -- TODO when solver supports --ignore-subdirs option pass that as the
    -- second argument here.
    reportMissingCabalFiles cabalfps True
    (bundle, _) <- cabalPackagesCheck cabalfps noPkgMsg (Just dupPkgFooter)

    let gpds              = Map.elems $ fmap snd bundle
        oldFlags          = bcFlags bconfig
        oldExtraVersions  = bcExtraDeps bconfig
        resolver          = bcResolver bconfig
        oldSrcs           = gpdPackages gpds
        oldSrcFlags       = Map.intersection oldFlags oldSrcs
        oldExtraFlags     = Map.intersection oldFlags oldExtraVersions

        srcConstraints    = mergeConstraints oldSrcs oldSrcFlags
        extraConstraints  = mergeConstraints oldExtraVersions oldExtraFlags

    resolverResult <- checkResolverSpec gpds (Just oldSrcFlags) resolver
    resultSpecs <- case resolverResult of
        BuildPlanCheckOk flags ->
            return $ Just ((mergeConstraints oldSrcs flags), Map.empty)
        BuildPlanCheckPartial {} -> do
            eres <- solveResolverSpec stackYaml cabalDirs
                              (resolver, srcConstraints, extraConstraints)
            -- TODO Solver should also use the init code to ignore incompatible
            -- packages
            return $ either (const Nothing) Just eres
        BuildPlanCheckFail {} ->
            throwM $ ResolverMismatch resolver (show resolverResult)

    (srcs, edeps) <- case resultSpecs of
        Nothing -> throwM (SolverGiveUp giveUpMsg)
        Just x -> return x

    let
        flags = removeSrcPkgDefaultFlags gpds (fmap snd (Map.union srcs edeps))
        versions = fmap fst edeps

        vDiff v v' = if v == v' then Nothing else Just v
        versionsDiff = Map.differenceWith vDiff
        newVersions  = versionsDiff versions oldExtraVersions
        goneVersions = versionsDiff oldExtraVersions versions

        fDiff f f' = if f == f' then Nothing else Just f
        flagsDiff  = Map.differenceWith fDiff
        newFlags   = flagsDiff flags oldFlags
        goneFlags  = flagsDiff oldFlags flags

        changed =    any (not . Map.null) [newVersions, goneVersions]
                  || any (not . Map.null) [newFlags, goneFlags]

    if changed then do
        $logInfo ""
        $logInfo $ "The following changes will be made to "
                   <> T.pack relStackYaml <> ":"

        -- TODO print whether resolver changed from previous
        $logInfo $ "* Resolver is " <> resolverName resolver

        printFlags newFlags  "* Flags to be added"
        printDeps  newVersions   "* Dependencies to be added"

        printFlags goneFlags "* Flags to be deleted"
        printDeps  goneVersions  "* Dependencies to be deleted"

        -- TODO backup the old config file
        if modStackYaml then do
            writeStackYaml stackYaml resolver versions flags
            $logInfo $ "Updated " <> T.pack relStackYaml
        else do
            $logInfo $ "To automatically update " <> T.pack relStackYaml
                       <> ", rerun with '--update-config'"
     else
        $logInfo $ "No changes needed to " <> T.pack relStackYaml

    where
        indent t = T.unlines $ fmap ("    " <>) (T.lines t)

        printFlags fl msg = do
            when ((not . Map.null) fl) $ do
                $logInfo $ T.pack msg
                $logInfo $ indent $ decodeUtf8 $ Yaml.encode
                                  $ object ["flags" .= fl]

        printDeps deps msg = do
            when ((not . Map.null) deps) $ do
                $logInfo $ T.pack msg
                $logInfo $ indent $ decodeUtf8 $ Yaml.encode $ object $
                        [("extra-deps" .= map fromTuple (Map.toList deps))]

        writeStackYaml path res deps fl = do
            let fp = toFilePath path
            obj <- liftIO (Yaml.decodeFileEither fp) >>= either throwM return
            WithJSONWarnings (ProjectAndConfigMonoid _ _) warnings <-
                liftIO (Yaml.decodeFileEither fp) >>= either throwM return
            logJSONWarnings fp warnings
            let obj' =
                    HashMap.insert "extra-deps"
                        (toJSON $ map fromTuple $ Map.toList deps)
                  $ HashMap.insert ("flags" :: Text) (toJSON fl)
                  $ HashMap.insert ("resolver" :: Text) (toJSON (resolverName res)) obj
            liftIO $ Yaml.encodeFile fp obj'

        giveUpMsg = concat
            [ "    - Update external packages with 'stack update' and try again.\n"
            , "    - Tweak " <> toFilePath stackDotYaml <> " and try again\n"
            , "        - Remove any unnecessary packages.\n"
            , "        - Add any missing remote packages.\n"
            , "        - Add extra dependencies to guide solver.\n"
            ]

prettyPath
    :: forall r t m. (MonadIO m, RelPath (Path r t) ~ Path Rel t, AnyPath (Path r t))
    => Path r t -> m String
prettyPath path = do
    eres <- liftIO $ try $ makeRelativeToCurrentDir path
    return $ case eres of
        Left (_ :: PathParseException) -> toFilePath path
        Right res -> toFilePath (res :: Path Rel t)
