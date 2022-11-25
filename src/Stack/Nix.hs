{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (nixCmdName
  ,nixHelpOptName
  ,runShellAndExit
  ) where

import           Stack.Prelude
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Path.IO
import qualified Paths_stack as Meta
import           Stack.Config (getInContainer, withBuildConfig)
import           Stack.Config.Nix (nixCompiler, nixCompilerVersion)
import           Stack.Constants (platformVariantEnvVar,inNixShellEnvVar,inContainerEnvVar)
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Nix
import           System.Environment (getArgs,getExecutablePath,lookupEnv)
import qualified System.FilePath  as F
import           RIO.Process (processContextL, exec)

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
                ("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) : origArgs
   exePath <- liftIO getExecutablePath
   config <- view configL
   envOverride <- view processContextL
   local (set processContextL envOverride) $ do
     let cmnd = escape exePath
         args' = map escape args

     mshellFile <- case configProjectRoot config of
         Just projectRoot ->
             traverse (resolveFile projectRoot) $ nixInitFile (configNix config)
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
     let pkgsInConfig = nixPackages (configNix config)
         pkgs = pkgsInConfig ++ [ghc, "git", "gcc", "gmp"]
         pkgsStr = "[" <> T.intercalate " " pkgs <> "]"
         pureShell = nixPureShell (configNix config)
         addGCRoots = nixAddGCRoots (configNix config)
         nixopts = case mshellFile of
           Just fp -> [toFilePath fp
                      ,"--arg", "ghc", "with (import <nixpkgs> {}); " ++ T.unpack ghc
                      ,"--argstr", "ghcVersion", T.unpack ghcVersion]
           Nothing -> ["-E", T.unpack $ T.concat
                              ["with (import <nixpkgs> {}); "
                              ,"let inputs = ",pkgsStr,"; "
                              ,    "libPath = lib.makeLibraryPath inputs; "
                              ,    "stackExtraArgs = lib.concatMap (pkg: "
                              ,    "[ ''--extra-lib-dirs=${lib.getLib pkg}/lib'' "
                              ,    "  ''--extra-include-dirs=${lib.getDev pkg}/include'' ]"
                              ,    ") inputs; in "
                              ,"runCommand ''myEnv'' { "
                              ,"buildInputs = lib.optional stdenv.isLinux glibcLocales ++ inputs; "
                              ,T.pack platformVariantEnvVar <> "=''nix''; "
                              ,T.pack inNixShellEnvVar <> "=1; "
                              ,if inContainer
                                  -- If shell is pure, this env var would not
                                  -- be seen by stack inside nix
                                  then T.pack inContainerEnvVar <> "=1; "
                                  else ""
                              ,"LD_LIBRARY_PATH = libPath;"  -- LD_LIBRARY_PATH is set because for now it's
                               -- needed by builds using Template Haskell
                              ,"STACK_IN_NIX_EXTRA_ARGS = stackExtraArgs; "
                               -- overriding default locale so Unicode output using base won't be broken
                              ,"LANG=\"en_US.UTF-8\";"
                              ,"} \"\""]]
                    -- glibcLocales is necessary on Linux to avoid warnings about GHC being incapable to set the locale.
         fullArgs = concat [if pureShell then ["--pure"] else []
                           ,if addGCRoots then ["--indirect", "--add-root"
                                               ,toFilePath (configWorkDir config)
                                                F.</> "nix-gc-symlinks" F.</> "gc-root"] else []
                           ,map T.unpack (nixShellOptions (configNix config))
                           ,nixopts
                           ,["--run", unwords (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args')]
                           ]
                           -- Using --run instead of --command so we cannot
                           -- end up in the nix-shell if stack build is Ctrl-C'd
     pathVar <- liftIO $ lookupEnv "PATH"
     logDebug $ "PATH is: " <> displayShow pathVar
     logDebug $
       "Using a nix-shell environment " <> (case mshellFile of
            Just path -> "from file: " <> fromString (toFilePath path)
            Nothing -> "with nix packages: " <> display (T.intercalate ", " pkgs))
     exec "nix-shell" fullArgs

-- | Shell-escape quotes inside the string and enclose it in quotes.
escape :: String -> String
escape str = "'" ++ foldr (\c -> if c == '\'' then
                                   ("'\"'\"'"++)
                                 else (c:)) "" str
                 ++ "'"

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

nixHelpOptName :: String
nixHelpOptName = nixCmdName ++ "-help"
