{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Stack.Solver
    ( cabalPackagesCheck
    , findCabalDirs
    , getResolverConstraints
    , mergeConstraints
    , solveExtraDeps
    , solveResolverSpec
    -- * Internal - for tests
    , parseCabalOutputLine
    ) where

import           Stack.Prelude hiding (Display (..))
import           Data.Aeson.Extended         (object, (.=), toJSON)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isSpace)
import           Data.Conduit.Process.Typed (eceStderr)
import qualified Data.HashMap.Strict as HashMap
import           Data.List                   ( (\\), isSuffixOf
                                             , minimumBy, isPrefixOf
                                             , intersperse)
import           Data.List.Extra (groupSortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Tuple (swap)
import qualified Data.Yaml as Yaml
import qualified Distribution.Package as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.Text as C
import           Path
import           Path.Find (findFiles)
import           Path.IO hiding (findExecutable, findFiles, withSystemTempDir)
import qualified RIO
import           Stack.Build.Target (gpdVersion)
import           Stack.BuildPlan
import           Stack.Config (getLocalPackages, loadConfigYaml)
import           Stack.Constants (stackDotYaml, wiredInPackages)
import           Stack.PrettyPrint
import           Stack.Setup
import           Stack.Setup.Installed
import           Stack.Snapshot (loadSnapshot)
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.PackageIdentifier
import           Stack.Types.Version
import qualified System.Directory as D
import qualified System.FilePath as FP
import           RIO.Process
import           Text.Regex.Applicative.Text (match, sym, psym, anySym, few)

import qualified Data.Text.Normalize as T ( normalize , NormalizationMode(NFC) )

data ConstraintType = Constraint | Preference deriving (Eq)
type ConstraintSpec = Map PackageName (Version, Map FlagName Bool)

cabalSolver :: HasConfig env
            => [Path Abs Dir] -- ^ cabal files
            -> ConstraintType
            -> ConstraintSpec -- ^ src constraints
            -> ConstraintSpec -- ^ dep constraints
            -> [String] -- ^ additional arguments
            -> RIO env (Either [PackageName] ConstraintSpec)
cabalSolver cabalfps constraintType
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

    try ( withWorkingDir (toFilePath tmpdir)
        $ proc "cabal" args readProcess_
        )
        >>= either
          (parseCabalErrors . eceStderr)
          (parseCabalOutput . BL.toStrict . fst)

  where
    errCheck = T.isInfixOf "Could not resolve dependencies"
    linesNoCR = map stripCR . T.lines
    cabalBuildErrMsg e =
               ">>>> Cabal errors begin\n"
            <> e
            <> "<<<< Cabal errors end\n"

    parseCabalErrors err = do
        let errExit e = error $ "Could not parse cabal-install errors:\n\n"
                              ++ cabalBuildErrMsg (T.unpack e)
            msg = decodeUtf8With lenientDecode $ toStrictBytes err

        if errCheck msg then do
            logInfo "Attempt failed.\n"
            logInfo $ RIO.display $ cabalBuildErrMsg msg
            let pkgs = parseConflictingPkgs msg
                mPkgNames = map (C.simpleParse . T.unpack) pkgs
                pkgNames  = map C.pkgName (catMaybes mPkgNames)

            when (any isNothing mPkgNames) $ do
                  logInfo $ "*** Only some package names could be parsed: " <>
                      mconcat (intersperse ", " (map (fromString . packageNameString) pkgNames))
                  error $ T.unpack $
                       "*** User packages involved in cabal failure: "
                       <> T.intercalate ", " (parseConflictingPkgs msg)

            if pkgNames /= [] then do
                  return $ Left pkgNames
            else errExit msg
        else errExit msg

    parseConflictingPkgs msg =
        let ls = dropWhile (not . errCheck) $ linesNoCR msg
            select s = (T.isPrefixOf "trying:" s
                      || T.isPrefixOf "next goal:" s)
                      && T.isSuffixOf "(user goal)" s
            pkgName' =  take 1
                      . T.words
                      . T.drop 1
                      . T.dropWhile (/= ':')
        in concatMap pkgName' (filter select ls)

    parseCabalOutput bs = do
        let ls = drop 1
               $ dropWhile (not . T.isPrefixOf "In order, ")
               $ linesNoCR
               $ decodeUtf8With lenientDecode bs
            (errs, pairs) = partitionEithers $ map parseCabalOutputLine ls
        if null errs
          then return $ Right (Map.fromList pairs)
          else error $ "The following lines from cabal-install output could \
                       \not be parsed: \n"
                       ++ T.unpack (T.intercalate "\n" errs)

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


    -- An ugly parser to extract module id and flags
parseCabalOutputLine :: Text -> Either Text (PackageName, (Version, Map FlagName Bool))
parseCabalOutputLine t0 = maybe (Left t0) Right . join .  match re $ t0
    -- Sample outputs to parse:
    -- text-1.2.1.1 (latest: 1.2.2.0) -integer-simple (via: parsec-3.1.9) (new package))
    -- hspec-snap-1.0.0.0 *test (via: servant-snap-0.5) (new package)
    -- time-locale-compat-0.1.1.1 -old-locale (via: http-api-data-0.2.2) (new package))
    -- flowdock-rest-0.2.0.0 -aeson-compat *test (via: haxl-fxtra-0.0.0.0) (new package)
  where
    re = mk <$> some (psym $ not . isSpace) <*> many (lexeme reMaybeFlag)

    reMaybeFlag =
        (\s -> Just (True, s))  <$ sym '+' <*> some (psym $ not . isSpace) <|>
        (\s -> Just (False, s)) <$ sym '-' <*> some (psym $ not . isSpace) <|>
        Nothing <$ sym '*' <* some (psym $ not . isSpace) <|>
        Nothing <$ sym '(' <* few anySym <* sym ')'

    mk :: String -> [Maybe (Bool, String)] -> Maybe (PackageName, (Version, Map FlagName Bool))
    mk ident fl = do
        PackageIdentifier name version <-
            parsePackageIdentifierThrowing ident
        fl' <- (traverse . traverse) parseFlagNameThrowing $ catMaybes fl
        return (name, (version, Map.fromList $ map swap fl'))

    lexeme r = some (psym isSpace) *> r

getCabalConfig :: HasConfig env
               => FilePath -- ^ temp dir
               -> ConstraintType
               -> Map PackageName Version -- ^ constraints
               -> RIO env [Text]
getCabalConfig dir constraintType constraints = do
    src <- view $ hackageIndexTarballL.to toFilePath
    let dstdir = dir FP.</> "hackage"
        -- NOTE: see https://github.com/commercialhaskell/stack/issues/2888
        -- for why we are pretending that a 01-index.tar is actually a
        -- 00-index.tar file.
        dst0 = dstdir FP.</> "00-index.tar"
        dst1 = dstdir FP.</> "01-index.tar"
    liftIO $ void $ tryIO $ do
        D.createDirectoryIfMissing True dstdir
        D.copyFile src dst0
        D.copyFile src dst1

    let cache = T.pack $ "remote-repo-cache: " ++ dir
        remote = "remote-repo: hackage:http://0.0.0.0/fake-url"
    return $ cache : remote : map goConstraint (Map.toList constraints)
  where
    goConstraint (name, version) =
        assert (not . null . versionString $ version) $
            T.concat
              [ if constraintType == Constraint
                   || name `Set.member` wiredInPackages
                then "constraint: "
                else "preference: "
              , T.pack $ packageNameString name
              , "=="
              , T.pack $ versionString version
              ]

setupCompiler
    :: (HasConfig env, HasGHCVariant env)
    => WantedCompiler
    -> RIO env (Maybe ExtraDirs)
setupCompiler compiler = do
    let msg = Just $ utf8BuilderToText $
          "Compiler version (" <> RIO.display compiler <> ") " <>
          "required by your resolver specification cannot be found.\n\n" <>
          "Please use '--install-ghc' command line switch to automatically " <>
          "install the compiler or '--system-ghc' to use a suitable " <>
          "compiler available on your PATH."

    config <- view configL
    (dirs, _, _) <- ensureCompiler SetupOpts
        { soptsInstallIfMissing  = configInstallGHC config
        , soptsUseSystem         = configSystemGHC config
        , soptsWantedCompiler    = compiler
        , soptsCompilerCheck     = configCompilerCheck config
        , soptsStackYaml         = Nothing
        , soptsForceReinstall    = False
        , soptsSanityCheck       = False
        , soptsSkipGhcCheck      = False
        , soptsSkipMsys          = configSkipMsys config
        , soptsUpgradeCabal      = Nothing
        , soptsResolveMissingGHC = msg
        , soptsSetupInfoYaml     = defaultSetupInfoYaml
        , soptsGHCBindistURL     = Nothing
        , soptsGHCJSBootOpts     = ["--clean"]
        }
    return dirs

-- | Runs the given inner command with an updated configuration that
-- has the desired GHC on the PATH.
setupCabalEnv
    :: (HasConfig env, HasGHCVariant env)
    => WantedCompiler
    -> (ActualCompiler -> RIO env a)
    -> RIO env a
setupCabalEnv compiler inner = do
  mpaths <- setupCompiler compiler
  menv0 <- view processContextL
  envMap <- either throwM (return . removeHaskellEnvVars)
              $ augmentPathMap (toFilePath <$> maybe [] edBins mpaths)
                               (view envVarsL menv0)
  menv <- mkProcessContext envMap
  withProcessContext menv $ do
    mcabal <- getCabalInstallVersion
    case mcabal of
        Nothing -> throwM SolverMissingCabalInstall
        Just version
            | version < $(mkVersion "1.24") -> prettyWarn $
                "Installed version of cabal-install (" <>
                fromString (versionString version) <>
                ") doesn't support custom-setup clause, and so may not yield correct results." <> line <>
                "To resolve this, install a newer version via 'stack install cabal-install'." <> line
            | version >= $(mkVersion "1.25") -> prettyWarn $
                "Installed version of cabal-install (" <>
                fromString (versionString version) <>
                ") is newer than stack has been tested with.  If you run into difficulties, consider downgrading." <> line
            | otherwise -> return ()

    mver <- getSystemCompiler (whichCompiler (wantedToActual compiler))
    version <- case mver of
        Just (version, _) -> do
            logInfo $ "Using compiler: " <> RIO.display version
            return version
        Nothing -> error "Failed to determine compiler version. \
                         \This is most likely a bug."

    inner version

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
    (fmap (, Map.empty))
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
    :: (HasConfig env, HasGHCVariant env)
    => [Path Abs Dir] -- ^ package dirs containing cabal files
    -> ( SnapshotDef
       , ConstraintSpec
       , ConstraintSpec) -- ^ ( resolver
                         --   , src package constraints
                         --   , extra dependency constraints )
    -> RIO env
         (Either [PackageName] (ConstraintSpec , ConstraintSpec))
       -- ^ (Conflicting packages
       --    (resulting src package specs, external dependency specs))

solveResolverSpec cabalDirs
                  (sd, srcConstraints, extraConstraints) = do
  logInfo $ "Using resolver: " <> RIO.display (sdResolverName sd)
  let wantedCompilerVersion = sdWantedCompilerVersion sd
  setupCabalEnv wantedCompilerVersion $ \compilerVersion -> do
    (compilerVer, snapConstraints) <- getResolverConstraints (Just compilerVersion) sd

    let -- Note - The order in Map.union below is important.
        -- We want to override snapshot with extra deps
        depConstraints = Map.union extraConstraints snapConstraints
        -- Make sure to remove any user packages from the dep constraints
        -- There are two reasons for this:
        -- 1. We do not want snapshot versions to override the sources
        -- 2. Sources may have blank versions leading to bad cabal constraints
        depOnlyConstraints = Map.difference depConstraints srcConstraints
        solver t = cabalSolver cabalDirs t srcConstraints depOnlyConstraints $
                     "-v" : -- TODO make it conditional on debug
                     ["--ghcjs" | whichCompiler compilerVer == Ghcjs]

    let srcNames = T.intercalate " and " $
          ["packages from " <> sdResolverName sd
              | not (Map.null snapConstraints)] ++
          [T.pack (show (Map.size extraConstraints) <> " external packages")
              | not (Map.null extraConstraints)]

    logInfo "Asking cabal to calculate a build plan..."
    unless (Map.null depOnlyConstraints)
        (logInfo $ "Trying with " <> RIO.display srcNames <> " as hard constraints...")

    eresult <- solver Constraint
    eresult' <- case eresult of
        Left _ | not (Map.null depOnlyConstraints) -> do
            logInfo $ "Retrying with " <> RIO.display srcNames <> " as preferences..."
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

                           -- If a package appears in both the
                           -- snapshot and locally, we don't want to
                           -- include it in extra-deps. This makes
                           -- sure we filter out such packages. See:
                           -- https://github.com/commercialhaskell/stack/issues/3533

                                    `Map.difference` srcConstraints

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
            unless (outVers `Map.isSubmapOf` inVers) $ do
                let msg = "Error: user package versions returned by cabal \
                          \solver are not the same as the versions in the \
                          \cabal files:\n"
                -- TODO We can do better in formatting the message
                error $ T.unpack $ msg
                        <> showItems (map show (Map.toList bothVers))

            logInfo $ "Successfully determined a build plan with "
                     <> displayShow (Map.size external)
                     <> " external dependencies."

            return $ Right (srcs, external)
        Left x -> do
            logInfo "*** Failed to arrive at a workable build plan."
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
    :: (HasConfig env, HasGHCVariant env)
    => Maybe ActualCompiler -- ^ actually installed compiler
    -> SnapshotDef
    -> RIO env
         (ActualCompiler,
          Map PackageName (Version, Map FlagName Bool))
getResolverConstraints mcompilerVersion sd = do
    ls <- loadSnapshot mcompilerVersion sd
    return (lsCompilerVersion ls, lsConstraints ls)
  where
    lpiConstraints lpi = (lpiVersion lpi, lpiFlags lpi)
    lsConstraints ls = Map.union
      (Map.map lpiConstraints (lsPackages ls))
      (Map.map lpiConstraints (lsGlobals ls))

-- | Finds all directories with a .cabal file or an hpack
-- package.yaml.  Subdirectories can be included depending on the
-- @recurse@ parameter.
findCabalDirs
  :: HasConfig env
  => Bool -> Path Abs Dir -> RIO env (Set (Path Abs Dir))
findCabalDirs recurse dir =
    Set.fromList . map parent
    <$> liftIO (findFiles dir isHpackOrCabal subdirFilter)
  where
    subdirFilter subdir = recurse && not (isIgnored subdir)
    isHpack = (== "package.yaml")     . toFilePath . filename
    isCabal = (".cabal" `isSuffixOf`) . toFilePath
    isHpackOrCabal x = isHpack x || isCabal x

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
    :: (HasConfig env, HasGHCVariant env)
     => [Path Abs Dir]
     -> String
     -> Maybe String
     -> RIO env
          ( Map PackageName (Path Abs File, C.GenericPackageDescription)
          , [Path Abs File])
cabalPackagesCheck cabaldirs noPkgMsg dupErrMsg = do
    when (null cabaldirs) $
        error noPkgMsg

    relpaths <- mapM prettyPath cabaldirs
    logInfo "Using cabal packages:"
    logInfo $ formatGroup relpaths

    packages <- map (\(x, y) -> (y, x)) <$>
                mapM (flip loadCabalFilePath YesPrintWarnings)
                cabaldirs

    -- package name cannot be empty or missing otherwise
    -- it will result in cabal solver failure.
    -- stack requires packages name to match the cabal file name
    -- Just the latter check is enough to cover both the cases

    let normalizeString = T.unpack . T.normalize T.NFC . T.pack
        getNameMismatchPkg (fp, gpd)
            | (normalizeString . packageNameString . gpdPackageName) gpd /= (normalizeString . FP.takeBaseName . toFilePath) fp
                = Just fp
            | otherwise = Nothing
        nameMismatchPkgs = mapMaybe getNameMismatchPkg packages

    when (nameMismatchPkgs /= []) $ do
        rels <- mapM prettyPath nameMismatchPkgs
        error $ "Package name as defined in the .cabal file must match the \
                \.cabal file name.\n\
                \Please fix the following packages and try again:\n"
                <> T.unpack (utf8BuilderToText (formatGroup rels))

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
        logWarn $
            "Following packages have duplicate package names:\n" <>
            mconcat (intersperse "\n" (map formatGroup dups))
        case dupErrMsg of
          Nothing -> logWarn $
                 "Packages with duplicate names will be ignored.\n"
              <> "Packages in upper level directories will be preferred.\n"
          Just msg -> error msg

    return (Map.fromList
            $ map (\(file, gpd) -> (gpdPackageName gpd,(file, gpd))) unique
           , map fst dupIgnored)

formatGroup :: [String] -> Utf8Builder
formatGroup = foldMap (\path -> "- " <> fromString path <> "\n")

reportMissingCabalFiles
  :: HasConfig env
  => [Path Abs File]   -- ^ Directories to scan
  -> Bool              -- ^ Whether to scan sub-directories
  -> RIO env ()
reportMissingCabalFiles cabalfps includeSubdirs = do
    allCabalDirs <- findCabalDirs includeSubdirs =<< getCurrentDir

    relpaths <- mapM prettyPath
              $ Set.toList
              $ allCabalDirs `Set.difference` Set.fromList (map parent cabalfps)
    unless (null relpaths) $ do
        logWarn "The following packages are missing from the config:"
        logWarn $ formatGroup relpaths

-- TODO Currently solver uses a stack.yaml in the parent chain when there is
-- no stack.yaml in the current directory. It should instead look for a
-- stack yaml only in the current directory and suggest init if there is
-- none available. That will make the behavior consistent with init and provide
-- a correct meaning to a --ignore-subdirs option if implemented.

-- | Verify the combination of resolver, package flags and extra
-- dependencies in an existing stack.yaml and suggest changes in flags or
-- extra dependencies so that the specified packages can be compiled.
solveExtraDeps
    :: forall env. HasEnvConfig env
    => Bool -- ^ modify stack.yaml?
    -> RIO env ()
solveExtraDeps modStackYaml = do
    bconfig <- view buildConfigL

    let stackYaml = bcStackYaml bconfig
    relStackYaml <- prettyPath stackYaml

    logInfo $ "Using configuration file: " <> fromString relStackYaml
    lp <- getLocalPackages
    let packages = lpProject lp
    let noPkgMsg = "No cabal packages found in " <> relStackYaml <>
                   ". Please add at least one directory containing a .cabal \
                   \file. You can also use 'stack init' to automatically \
                   \generate the config file."
        dupPkgFooter = "Please remove the directories containing duplicate \
                       \entries from '" <> relStackYaml <> "'."

        cabalDirs = map lpvRoot    $ Map.elems packages
        cabalfps  = map lpvCabalFP $ Map.elems packages
    -- TODO when solver supports --ignore-subdirs option pass that as the
    -- second argument here.
    reportMissingCabalFiles cabalfps True
    (bundle, _) <- cabalPackagesCheck cabalDirs noPkgMsg (Just dupPkgFooter)

    let gpds              = Map.elems $ fmap snd bundle
        oldFlags          = bcFlags bconfig
        oldExtraVersions  = Map.map (gpdVersion . fst) (lpDependencies lp)
        sd                = bcSnapshotDef bconfig
        resolver          = sdResolver sd
        oldSrcs           = gpdPackages gpds
        oldSrcFlags       = Map.intersection oldFlags oldSrcs
        oldExtraFlags     = Map.intersection oldFlags oldExtraVersions

        srcConstraints    = mergeConstraints oldSrcs oldSrcFlags
        extraConstraints  = mergeConstraints oldExtraVersions oldExtraFlags

    actualCompiler <- view actualCompilerVersionL
    resolverResult <- checkSnapBuildPlan gpds (Just oldSrcFlags) sd (Just actualCompiler)
    resultSpecs <- case resolverResult of
        BuildPlanCheckOk flags ->
            return $ Just (mergeConstraints oldSrcs flags, Map.empty)
        BuildPlanCheckPartial {} ->
            either (const Nothing) Just <$>
            solveResolverSpec cabalDirs (sd, srcConstraints, extraConstraints)
            -- TODO Solver should also use the init code to ignore incompatible
            -- packages
        BuildPlanCheckFail {} ->
            throwM $ ResolverMismatch IsSolverCmd (sdResolverName sd) (show resolverResult)

    (srcs, edeps) <- case resultSpecs of
        Nothing -> throwM (SolverGiveUp giveUpMsg)
        Just x -> return x

    mOldResolver <- view $ configL.to (fmap (projectResolver . fst) . configMaybeProject)

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
                  || any (/= resolver) mOldResolver

    if changed then do
        logInfo ""
        logInfo $ "The following changes will be made to "
                   <> fromString relStackYaml <> ":"

        printResolver mOldResolver resolver

        printFlags newFlags  "* Flags to be added"
        printDeps  newVersions   "* Dependencies to be added"

        printFlags goneFlags "* Flags to be deleted"
        printDeps  goneVersions  "* Dependencies to be deleted"

        -- TODO backup the old config file
        if modStackYaml then do
            writeStackYaml stackYaml resolver versions flags
            logInfo $ "Updated " <> fromString relStackYaml
        else do
            logInfo $ "To automatically update " <> fromString relStackYaml
                       <> ", rerun with '--update-config'"
     else
        logInfo $ "No changes needed to " <> fromString relStackYaml

    where
        indentLines t = T.unlines $ fmap ("    " <>) (T.lines t)

        printResolver mOldRes res = do
            forM_ mOldRes $ \oldRes ->
                when (res /= oldRes) $ do
                    logInfo $
                        "* Resolver changes from " <>
                        RIO.display oldRes <>
                        " to " <>
                        RIO.display res

        printFlags fl msg = do
            unless (Map.null fl) $ do
                logInfo $ fromString msg
                logInfo $ RIO.display $ indentLines $ decodeUtf8 $ Yaml.encode
                                       $ object ["flags" .= toCabalStringMap (fmap toCabalStringMap fl)]

        printDeps deps msg = do
            unless (Map.null deps) $ do
                logInfo $ fromString msg
                logInfo $ RIO.display $ indentLines $ decodeUtf8 $ Yaml.encode $ object
                        ["extra-deps" .= map (CabalString . uncurry PackageIdentifier) (Map.toList deps)]

        writeStackYaml
          :: Path Abs File
          -> SnapshotLocation
          -> Map PackageName Version
          -> Map PackageName (Map FlagName Bool)
          -> RIO env ()
        writeStackYaml path res deps fl = do
            let fp = toFilePath path
            obj <- liftIO (Yaml.decodeFileEither fp) >>= either throwM return
            -- Check input file and show warnings
            _ <- loadConfigYaml (parseProjectAndConfigMonoid (parent path)) path
            let obj' =
                    HashMap.insert "extra-deps"
                        (toJSON $ map (CabalString . uncurry PackageIdentifier) $ Map.toList deps)
                  $ HashMap.insert ("flags" :: Text) (toJSON $ toCabalStringMap $ toCabalStringMap <$> fl)
                  $ HashMap.insert ("resolver" :: Text) (toJSON res) obj
            liftIO $ Yaml.encodeFile fp obj'

        giveUpMsg = concat
            [ "    - Update external packages with 'stack update' and try again.\n"
            , "    - Tweak " <> toFilePath stackDotYaml <> " and try again\n"
            , "        - Remove any unnecessary packages.\n"
            , "        - Add any missing remote packages.\n"
            , "        - Add extra dependencies to guide solver.\n"
            , "        - Adjust resolver.\n"
            ]

prettyPath
    :: forall r t m. (MonadIO m, RelPath (Path r t) ~ Path Rel t, AnyPath (Path r t))
    => Path r t -> m String
prettyPath path = do
    eres <- liftIO $ try $ makeRelativeToCurrentDir path
    return $ case eres of
        Left (_ :: PathException) -> toFilePath path
        Right res -> toFilePath (res :: Path Rel t)
