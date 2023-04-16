{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           RIO.Process ( exec )
import           Stack.Build ( build )
import           Stack.Build.Target ( NeedTargets (..) )
import           Stack.GhcPkg ( findGhcPkgField )
import           Stack.Setup ( withNewLocalBuildTargets )
import           Stack.Types.NamedComponent ( NamedComponent (..), isCExe )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig, withEnvConfig )
import           Stack.Types.Config
                   ( BuildConfig (..), BuildOptsCLI (..), CompilerPaths (..)
                   , EnvConfig, EnvSettings (..), Runner
                   , buildConfigL, compilerPathsL, configL, configProcessContextSettings, defaultBuildOptsCLI
                   , getGhcPkgExe, ppComponents
                   )
import           Stack.Types.SourceMap ( SMWanted (..) )
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
  deriving (Show, Typeable)

instance Pretty ExecPrettyException where
  pretty (PackageIdNotFoundBug name) = bugPrettyReport "[S-8251]" $
    "Could not find the package id of the package" <+>
      style Target (fromString name)
    <> "."
  pretty ExecutableToRunNotFound =
       "[S-2483]"
    <> line
    <> flow "No executables found."

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
execCmd ExecOpts {..} =
  withConfig YesReexec $ withEnvConfig AllowNoTargets boptsCLI $ do
    unless (null targets) $ build Nothing

    config <- view configL
    menv <- liftIO $ configProcessContextSettings config eoEnvSettings
    withProcessContext menv $ do
      -- Add RTS options to arguments
      let argsWithRts args = if null eoRtsOptions
                  then args :: [String]
                  else args ++ ["+RTS"] ++ eoRtsOptions ++ ["-RTS"]
      (cmd, args) <- case (eoCmd, argsWithRts eoArgs) of
        (ExecCmd cmd, args) -> pure (cmd, args)
        (ExecRun, args) -> getRunCmd args
        (ExecGhc, args) -> getGhcCmd eoPackages args
        (ExecRunGhc, args) -> getRunGhcCmd eoPackages args

      runWithPath eoCwd $ exec cmd args
 where
  ExecOptsExtra {..} = eoExtra

  targets = concatMap words eoPackages
  boptsCLI = defaultBuildOptsCLI
             { boptsCLITargets = map T.pack targets
             }

  -- return the package-id of the first package in GHC_PACKAGE_PATH
  getPkgId name = do
    pkg <- getGhcPkgExe
    mId <- findGhcPkgField pkg [] name "id"
    case mId of
      Just i -> pure (L.head $ words (T.unpack i))
      -- should never happen as we have already installed the packages
      _      -> prettyThrowIO (PackageIdNotFoundBug name)

  getPkgOpts pkgs =
    map ("-package-id=" ++) <$> mapM getPkgId pkgs

  getRunCmd args = do
    packages <- view $ buildConfigL.to (smwProject . bcSMWanted)
    pkgComponents <- for (Map.elems packages) ppComponents
    let executables = filter isCExe $ concatMap Set.toList pkgComponents
    let (exe, args') = case args of
                       []   -> (firstExe, args)
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
    compiler <- view $ compilerPathsL.to cpCompiler
    pure (toFilePath compiler, pkgopts ++ args)

  getRunGhcCmd pkgs args = do
    pkgopts <- getPkgOpts pkgs
    interpret <- view $ compilerPathsL.to cpInterpreter
    pure (toFilePath interpret, pkgopts ++ args)

  runWithPath :: Maybe FilePath -> RIO EnvConfig () -> RIO EnvConfig ()
  runWithPath path callback = case path of
    Nothing -> callback
    Just p | not (isValid p) -> throwIO $ InvalidPathForExec p
    Just p -> withUnliftIO $ \ul -> withCurrentDirectory p $ unliftIO ul callback
