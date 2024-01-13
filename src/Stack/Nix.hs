{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Run commands in a nix-shell
module Stack.Nix
  ( nixCmdName
  , nixHelpOptName
  , runShellAndExit
  ) where

import qualified Data.Text as T
import           Path.IO ( resolveFile )
import           RIO.Process ( exec, processContextL )
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
import           Stack.Types.Nix ( NixOpts (..) )
import           Stack.Types.Version ( showStackVersion )
import           System.Environment ( getArgs, getExecutablePath, lookupEnv )
import qualified System.FilePath as F

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Nix" module.
data NixException
  = CannotDetermineProjectRoot
    -- ^ Can't determine the project root (location of the shell file if any).
  deriving (Show, Typeable)

instance Exception NixException where
  displayException CannotDetermineProjectRoot =
    "Error: [S-7384]\n"
    ++ "Cannot determine project root directory."

runShellAndExit :: RIO Config void
runShellAndExit = do
  inContainer <- getInContainer -- TODO we can probably assert that this is False based on Stack.Runners now
  origArgs <- liftIO getArgs
  let args | inContainer = origArgs  -- internal-re-exec version already passed
             -- first stack when restarting in the container
           | otherwise =
               ("--" ++ reExecArgName ++ "=" ++ showStackVersion) : origArgs
  exePath <- liftIO getExecutablePath
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

    ghc <- either throwIO pure $ nixCompiler compilerVersion
    ghcVersion <- either throwIO pure $ nixCompilerVersion compilerVersion
    let pkgsInConfig = config.nix.packages
        pkgs = pkgsInConfig ++ [ghc, "git", "gcc", "gmp"]
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

        fullArgs = concat
          [ [ "--pure" | pureShell ]
          , if addGCRoots
              then [ "--indirect"
                   , "--add-root"
                   , toFilePath
                             config.workDir
                       F.</> "nix-gc-symlinks"
                       F.</> "gc-root"
                   ]
              else []
          , map T.unpack config.nix.shellOptions
          , nixopts
          , ["--run", unwords (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args')]
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
    exec "nix-shell" fullArgs

-- | Shell-escape quotes inside the string and enclose it in quotes.
escape :: String -> String
escape str =
     "'"
  ++ foldr (\c -> if c == '\'' then ("'\"'\"'"++) else (c:)) "" str
  ++ "'"

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

nixHelpOptName :: String
nixHelpOptName = nixCmdName ++ "-help"
