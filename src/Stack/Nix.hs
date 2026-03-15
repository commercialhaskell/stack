{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Nix
Description : Run commands in a nix-shell.
License     : BSD-3-Clause

Run commands in a nix-shell.
-}

module Stack.Nix
  ( nixCmdName
  , nixHelpOptName
  , runShellAndExit
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as S8
import           Path.IO ( resolveFile )
import           RIO.Process
                   ( HasProcessContext (..), exec, proc, processContextL
                   , readProcess
                   )
import           Stack.Config ( getInContainer, withBuildConfig )
import           Stack.Config.Nix ( nixCompiler, nixCompilerVersion )
import           Stack.Constants
                   ( inContainerEnvVar, inNixShellEnvVar
                   , platformVariantEnvVar
                   )
import           Stack.Prelude
import           Stack.Types.BuildConfig ( wantedCompilerVersionL )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), configProjectRoot )
import           Stack.Types.Docker ( reExecArgName )
import           Stack.Types.Runner ( viewExecutablePath )
import           Stack.Types.Nix ( NixOpts (..) )
import           Stack.Types.Version ( showStackVersion )
import           System.Environment ( getArgs, lookupEnv )
import qualified System.FilePath as F

-- | Type representing  \'pretty\' exceptions thrown by functions exported by
-- the "Stack.Nix" module.
data NixPrettyException
  = CannotDetermineProjectRoot
    -- ^ Can't determine the project root (location of the shell file if any).
  | NixInstantiateCommandFailure ![String] !S8.ByteString
  deriving Show

instance Pretty NixPrettyException where
  pretty CannotDetermineProjectRoot =
    "[S-7384]"
    <> line
    <> flow "Cannot determine project root directory."
  pretty (NixInstantiateCommandFailure args e) =
    "[S-1264]"
    <> line
    <> fillSep
         [ flow "While using"
         , style Shell "nix-instantiate" <> ","
         , flow "with arguments:"
         ]
    <> line
    <> bulletedList (map (style Shell . string) args)
    <> blankLine
    <> flow "Stack encountered the following error:"
    <> blankLine
    <> string (T.unpack . textDisplay . displayBytesUtf8 $ S8.toStrict e)

instance Exception NixPrettyException

-- | Execute @nix-shell@, replacing the current process.
runShellAndExit :: RIO Config void
runShellAndExit = do
  inContainer <- getInContainer -- TODO we can probably assert that this is False based on Stack.Runners now
  origArgs <- liftIO getArgs
  let args | inContainer = origArgs  -- internal-re-exec version already passed
             -- first stack when restarting in the container
           | otherwise =
               ("--" ++ reExecArgName ++ "=" ++ showStackVersion) : origArgs
  exePath <- toFilePath <$> viewExecutablePath
  config <- view configL
  envOverride <- view processContextL
  local (set processContextL envOverride) $ do
    let cmnd = escape exePath
        args' = map escape args

    mshellFile <- case configProjectRoot config of
      Just projectRoot ->
        traverse (resolveFile projectRoot) config.nix.initFile
      Nothing -> pure Nothing

    -- This will never result in double loading the build config, since:
    --
    -- 1. This function explicitly takes a Config, not a HasConfig
    --
    -- 2. This function ends up exiting before running other code
    -- (thus the void return type)
    compilerVersion <- withBuildConfig $ view wantedCompilerVersionL

    ghc <- either prettyThrowIO pure $ nixCompiler compilerVersion
    ghcVersion <- either prettyThrowIO pure $ nixCompilerVersion compilerVersion
    let pkgsInConfig = config.nix.packages
        -- It appears that cacert needs to be specified in order for
        -- crypton-x509-system >= 1.6.8 to work with Stack's Nix integration:
        pkgs = pkgsInConfig ++ [ghc, "git", "gcc", "gmp", "cacert"]
        pkgsStr = "[" <> T.intercalate " " pkgs <> "]"
        pureShell = config.nix.pureShell
        addGCRoots = config.nix.addGCRoots
        nixopts = case mshellFile of
          Just fp ->
            [ toFilePath fp
            , "--arg"
            , "ghc"
            , "with (import <nixpkgs> {}); " ++ T.unpack ghc
            , "--argstr", "ghcVersion", T.unpack ghcVersion
            ]
          Nothing ->
            [ "-E"
            , T.unpack $ T.concat
                [ "with (import <nixpkgs> {}); "
                , "let inputs = ",pkgsStr,"; "
                ,     "libPath = lib.makeLibraryPath inputs; "
                ,     "stackExtraArgs = lib.concatMap (pkg: "
                ,     "[ ''--extra-lib-dirs=${lib.getLib pkg}/lib'' "
                ,     "  ''--extra-include-dirs=${lib.getDev pkg}/include'' ]"
                ,     ") inputs; in "
                , "runCommand ''myEnv'' { "
                , "buildInputs = lib.optional stdenv.isLinux glibcLocales ++ inputs; "
                  -- glibcLocales is necessary on Linux to avoid warnings about
                  -- GHC being incapable to set the locale.
                , T.pack platformVariantEnvVar <> "=''nix''; "
                , T.pack inNixShellEnvVar <> "=1; "
                , if inContainer
                     -- If shell is pure, this env var would not
                     -- be seen by stack inside nix
                     then T.pack inContainerEnvVar <> "=1; "
                     else ""
                , "LD_LIBRARY_PATH = libPath;"
                  -- LD_LIBRARY_PATH is set because for now it's needed by
                  -- builds using Template Haskell
                , "STACK_IN_NIX_EXTRA_ARGS = stackExtraArgs; "
                  -- overriding default locale so Unicode output using base
                  -- won't be broken
                , "LANG=\"en_US.UTF-8\";"
                , "} \"\""
                ]
            ]
        instantiateArgs = concat
          [ if addGCRoots
              then [ "--indirect"
                   , "--add-root"
                   , toFilePath
                             config.workDir
                       F.</> "nix-gc-symlinks"
                       F.</> "gc-root"
                   ]
              else []
          , nixopts
          ,  map T.unpack config.nix.instantiateOptions
          , [ "--show-trace" ]
          ]
        shellArgs = concat
          [ [ "--pure" | pureShell ]
          , map T.unpack config.nix.shellOptions
          , [ "--run", unwords (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args') ]
            -- Using --run instead of --command so we cannot end up in the
            -- nix-shell if stack build is Ctrl-C'd
          ]
    pathVar <- liftIO $ lookupEnv "PATH"
    logDebug $ "PATH is: " <> displayShow pathVar
    logDebug $
         "Using a nix-shell environment "
      <> ( case mshellFile of
             Just path ->
                  "from file: "
               <> fromString (toFilePath path)
             Nothing ->
                  "with nix packages: "
               <> display (T.intercalate ", " pkgs)
         )
    runNixShellExec instantiateArgs shellArgs

runNixShellExec
  :: (HasProcessContext env, HasLogFunc env)
  => [String]
     -- ^ nix-instantiate args
  -> [String]
     -- ^ nix-shell args
  -> RIO env a
runNixShellExec instantiateArgs shellArgs = do
  (ec, out, err) <- proc "nix-instantiate" instantiateArgs readProcess
  case ec of
    ExitFailure _ ->
      -- As of Nix 2.34, there appears to be no way to capture coloured output
      prettyThrowIO (NixInstantiateCommandFailure instantiateArgs err)
    ExitSuccess -> do
      let drvPath = case S8.lines out of
            [] -> error "error"
            -- nix-instantiate prints the .drv path
            (drvPath' : _) -> S8.unpack drvPath'
      exec "nix-shell" (drvPath : shellArgs)

-- | Shell-escape quotes inside the string and enclose it in quotes.
escape :: String -> String
escape str =
     "'"
  ++ foldr (\c -> if c == '\'' then ("'\"'\"'"++) else (c:)) "" str
  ++ "'"

-- | Command-line argument for "nix".
nixCmdName :: String
nixCmdName = "nix"

-- | Command-line option to show only @--nix-*@ options.
nixHelpOptName :: String
nixHelpOptName = nixCmdName ++ "-help"
