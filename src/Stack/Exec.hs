{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Exec
Description : Types and function related to Stack's @exec@, @ghc@, @run@,
              @runghc@ and @runhaskell@ commands.
License     : BSD-3-Clause

Types and function related to Stack's @exec@, @ghc@, @run@, @runghc@ and
@runhaskell@ commands.
-}

module Stack.Exec
  ( ExecOpts (..)
  , SpecialExecCmd (..)
  , ExecOptsExtra (..)
  , execCmd
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Types.PackageName ( unPackageName )
import           RIO.NonEmpty ( head, nonEmpty )
import           RIO.Process ( exec )
import           Stack.Build ( build )
import           Stack.Build.Target
                   ( NeedTargets (..), RawTarget (..), parseRawTarget )
import           Stack.GhcPkg ( findGhcPkgField )
import           Stack.Setup ( withNewLocalBuildTargets )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig, withEnvConfig )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.BuildOptsCLI
                   ( BuildOptsCLI (..), defaultBuildOptsCLI )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..), getGhcPkgExe )
import           Stack.Types.ComponentUtils
                   ( unqualCompFromString, unqualCompToText )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.NamedComponent ( NamedComponent (..), isCExe )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.SourceMap ( SMWanted (..), ppComponents )
import           System.Directory ( withCurrentDirectory )
import           System.FilePath ( isValid )

-- | Type representing exceptions thrown by functions in the "Stack.Exec"
-- module.
newtype ExecException
  = InvalidPathForExec FilePath
  deriving (Show, Typeable)

instance Exception ExecException where
  displayException (InvalidPathForExec path) = concat
    [ "Error: [S-1541]\n"
    , "Got an invalid '--cwd' argument for 'stack exec' ("
    , path
    , ")."
    ]

-- | Type representing \'pretty\' exceptions thrown by functions in the
-- "Stack.Exec" module.
data ExecPrettyException
  = PackageIdNotFoundBug !String
  | ExecutableToRunNotFound
  | NoPackageIdReportedBug
  | InvalidExecTargets ![Text]
  deriving (Show, Typeable)

instance Pretty ExecPrettyException where
  pretty (PackageIdNotFoundBug name) = bugPrettyReport "[S-8251]" $
    fillSep
      [ flow "Could not find the package id of the package"
      , style Target (fromString name) <> "."
      ]
  pretty ExecutableToRunNotFound =
       "[S-2483]"
    <> line
    <> flow "No executables found."
  pretty NoPackageIdReportedBug = bugPrettyReport "S-8600" $
    flow "execCmd: findGhcPkgField returned Just \"\"."
  pretty (InvalidExecTargets targets) =
       "[S-7371]"
    <> line
    <> fillSep
         [ flow "The following are invalid"
         , style Shell "--package"
         , "values for"
         , style Shell (flow "stack ghc") <> ","
         , style Shell (flow "stack runghc") <> ","
         , "or"
         , style Shell (flow "stack runhaskell") <> ":"
         ]
    <> line
    <> bulletedList (map (style Target . string . T.unpack) targets )

instance Exception ExecPrettyException

-- | Type representing Stack's execution commands.
data SpecialExecCmd
  = ExecCmd String
    -- ^ @stack exec@ command.
  | ExecRun
    -- ^ @stack run@ command.
  | ExecGhc
    -- ^ @stack ghc@ command.
  | ExecRunGhc
    -- ^ @stack runghc@ or @stack runhaskell@ command.
  deriving (Eq, Show)

-- | Type representing extra Stack options for Stack's execution commands.
data ExecOptsExtra = ExecOptsExtra
  { envSettings :: !EnvSettings
  , packages :: ![String]
  , rtsOptions :: ![String]
  , cwd :: !(Maybe FilePath)
  }
  deriving Show

-- | Type representing options for Stack's execution commands.
data ExecOpts = ExecOpts
  { cmd :: !SpecialExecCmd
  , args :: ![String]
  , extra :: !ExecOptsExtra
  }
  deriving Show

-- | Type representing valid targets for @--package@ option.
data ExecTarget = ExecTarget PackageName (Maybe Version)

-- | The function underlying Stack's @exec@, @ghc@, @run@, @runghc@ and
-- @runhaskell@ commands. Execute a command.
execCmd :: ExecOpts -> RIO Runner ()
execCmd opts =
  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
    let (errs, execTargets) = partitionEithers $ map fromTarget targets
    unless (null errs) $ prettyThrowM $ InvalidExecTargets errs
    unless (null execTargets) $ build Nothing

    config <- view configL
    menv <- liftIO $ config.processContextSettings eo.envSettings
    withProcessContext menv $ do
      -- Add RTS options to arguments
      let argsWithRts args = if null eo.rtsOptions
                  then args :: [String]
                  else args ++ ["+RTS"] ++ eo.rtsOptions ++ ["-RTS"]
      (cmd, args) <- case (opts.cmd, argsWithRts opts.args) of
        (ExecCmd cmd, args) -> pure (cmd, args)
        (ExecRun, args) -> getRunCmd args
        (ExecGhc, args) -> getGhcCmd execTargets args
        (ExecRunGhc, args) -> getRunGhcCmd execTargets args

      runWithPath eo.cwd $ exec cmd args
 where
  eo = opts.extra

  targets = concatMap (T.words . T.pack) eo.packages
  boptsCLI = defaultBuildOptsCLI { targetsCLI = targets }

  fromTarget :: Text -> Either Text ExecTarget
  fromTarget target =
    case parseRawTarget target >>= toExecTarget of
      Nothing -> Left target
      Just execTarget -> Right execTarget

  toExecTarget :: RawTarget -> Maybe ExecTarget
  toExecTarget (RTPackageComponent _ _) = Nothing
  toExecTarget (RTComponent _) = Nothing
  toExecTarget (RTPackage name) = Just $ ExecTarget name Nothing
  toExecTarget (RTPackageIdentifier (PackageIdentifier name pkgId)) =
    Just $ ExecTarget name (Just pkgId)

  -- return the package-id of the first package in GHC_PACKAGE_PATH
  getPkgId (ExecTarget pkgName _) = do
    let name = unPackageName pkgName
    pkg <- getGhcPkgExe
    mId <- findGhcPkgField pkg [] name "id"
    case mId of
      Just i -> maybe
        (prettyThrowIO NoPackageIdReportedBug)
        (pure . head)
        (nonEmpty $ words $ T.unpack i)
      -- should never happen as we have already installed the packages
      _      -> prettyThrowIO (PackageIdNotFoundBug name)

  getPkgOpts pkgs =
    map ("-package-id=" ++) <$> mapM getPkgId pkgs

  getRunCmd args = do
    packages <- view $ buildConfigL . to (.smWanted.project)
    pkgComponents <- for (Map.elems packages) ppComponents
    let executables = concatMap (filter isCExe . Set.toList) pkgComponents
        (exe, args') = case args of
          [] -> (firstExe, args)
          x:xs -> let matchesExecutable y = y == CExe (unqualCompFromString x)
                  in  case L.find matchesExecutable executables of
                        Nothing -> (firstExe, args)
                        argExe -> (argExe, xs)
         where
          firstExe = listToMaybe executables
    case exe of
      Just (CExe exe') -> do
        let textExeName = unqualCompToText exe'
        withNewLocalBuildTargets [T.cons ':' textExeName] $ build Nothing
        pure (T.unpack textExeName, args')
      _ -> prettyThrowIO ExecutableToRunNotFound

  getGhcCmd pkgs args = do
    pkgopts <- getPkgOpts pkgs
    compiler <- view $ compilerPathsL . to (.compiler)
    pure (toFilePath compiler, pkgopts ++ args)

  getRunGhcCmd pkgs args = do
    pkgopts <- getPkgOpts pkgs
    interpret <- view $ compilerPathsL . to (.interpreter)
    pure (toFilePath interpret, pkgopts ++ args)

  runWithPath :: Maybe FilePath -> RIO EnvConfig () -> RIO EnvConfig ()
  runWithPath path callback = case path of
    Nothing -> callback
    Just p | not (isValid p) -> throwIO $ InvalidPathForExec p
    Just p -> withUnliftIO $ \ul -> withCurrentDirectory p $ unliftIO ul callback
