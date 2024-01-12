{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and function related to Stack's @exec@, @ghc@, @run@, @runghc@ and
-- @runhaskell@ commands.
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
import           RIO.NonEmpty ( head, nonEmpty )
import           RIO.Process ( exec )
import           Stack.Build ( build )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.GhcPkg ( findGhcPkgField )
import           Stack.Setup ( withNewLocalBuildTargets )
import           Stack.Types.NamedComponent ( NamedComponent (..), isCExe )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig, withEnvConfig )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.BuildOpts
                   ( BuildOptsCLI (..), defaultBuildOptsCLI )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..), getGhcPkgExe )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.EnvConfig ( EnvConfig )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
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

instance Exception ExecPrettyException

-- Type representing Stack's execution commands.
data SpecialExecCmd
  = ExecCmd String
  | ExecRun
  | ExecGhc
  | ExecRunGhc
  deriving (Eq, Show)

data ExecOptsExtra = ExecOptsExtra
  { eoEnvSettings :: !EnvSettings
  , eoPackages :: ![String]
  , eoRtsOptions :: ![String]
  , eoCwd :: !(Maybe FilePath)
  }
  deriving Show

-- Type representing options for Stack's execution commands.
data ExecOpts = ExecOpts
  { eoCmd :: !SpecialExecCmd
  , eoArgs :: ![String]
  , eoExtra :: !ExecOptsExtra
  }
  deriving Show

-- | The function underlying Stack's @exec@, @ghc@, @run@, @runghc@ and
-- @runhaskell@ commands. Execute a command.
execCmd :: ExecOpts -> RIO Runner ()
execCmd opts =
  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
    unless (null targets) $ build Nothing

    config <- view configL
    menv <- liftIO $ config.configProcessContextSettings eo.eoEnvSettings
    withProcessContext menv $ do
      -- Add RTS options to arguments
      let argsWithRts args = if null eo.eoRtsOptions
                  then args :: [String]
                  else args ++ ["+RTS"] ++ eo.eoRtsOptions ++ ["-RTS"]
      (cmd, args) <- case (opts.eoCmd, argsWithRts opts.eoArgs) of
        (ExecCmd cmd, args) -> pure (cmd, args)
        (ExecRun, args) -> getRunCmd args
        (ExecGhc, args) -> getGhcCmd eo.eoPackages args
        (ExecRunGhc, args) -> getRunGhcCmd eo.eoPackages args

      runWithPath eo.eoCwd $ exec cmd args
 where
  eo = opts.eoExtra

  targets = concatMap words eo.eoPackages
  boptsCLI = defaultBuildOptsCLI
             { boptsCLITargets = map T.pack targets
             }

  -- return the package-id of the first package in GHC_PACKAGE_PATH
  getPkgId name = do
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
    packages <- view $ buildConfigL . to (.bcSMWanted.smwProject)
    pkgComponents <- for (Map.elems packages) ppComponents
    let executables = concatMap (filter isCExe . Set.toList) pkgComponents
    let (exe, args') = case args of
          [] -> (firstExe, args)
          x:xs -> case L.find (\y -> y == CExe (T.pack x)) executables of
            Nothing -> (firstExe, args)
            argExe -> (argExe, xs)
         where
          firstExe = listToMaybe executables
    case exe of
      Just (CExe exe') -> do
        withNewLocalBuildTargets [T.cons ':' exe'] $ build Nothing
        pure (T.unpack exe', args')
      _ -> prettyThrowIO ExecutableToRunNotFound

  getGhcCmd pkgs args = do
    pkgopts <- getPkgOpts pkgs
    compiler <- view $ compilerPathsL . to (.cpCompiler)
    pure (toFilePath compiler, pkgopts ++ args)

  getRunGhcCmd pkgs args = do
    pkgopts <- getPkgOpts pkgs
    interpret <- view $ compilerPathsL . to (.cpInterpreter)
    pure (toFilePath interpret, pkgopts ++ args)

  runWithPath :: Maybe FilePath -> RIO EnvConfig () -> RIO EnvConfig ()
  runWithPath path callback = case path of
    Nothing -> callback
    Just p | not (isValid p) -> throwIO $ InvalidPathForExec p
    Just p -> withUnliftIO $ \ul -> withCurrentDirectory p $ unliftIO ul callback
