{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (reexecWithOptionalShell
  ,nixCmdName
  ,nixHelpOptName
  ) where

import           Stack.Prelude
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Lens.Micro (set)
import           Path.IO
import qualified Paths_stack as Meta
import           Stack.Config (getInNixShell, getInContainer)
import           Stack.Config.Nix (nixCompiler)
import           Stack.Constants (platformVariantEnvVar,inNixShellEnvVar,inContainerEnvVar)
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Nix
import           Stack.Types.Runner
import           Stack.Types.Compiler
import           System.Environment (getArgs,getExecutablePath,lookupEnv)
import qualified System.FilePath  as F
import           RIO.Process (processContextL, exec)

-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
reexecWithOptionalShell
    :: HasConfig env
    => Maybe (Path Abs Dir)
    -> IO (CompilerVersion 'CVWanted)
    -> IO ()
    -> RIO env ()
reexecWithOptionalShell mprojectRoot getCompilerVersion inner =
  do config <- view configL
     inShell <- getInNixShell
     inContainer <- getInContainer
     isReExec <- view reExecL
     let getCmdArgs = do
           origArgs <- liftIO getArgs
           let args | inContainer = origArgs  -- internal-re-exec version already passed
                      -- first stack when restarting in the container
                    | otherwise =
                        ("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) : origArgs
           exePath <- liftIO getExecutablePath
           return (exePath, args)
     if nixEnable (configNix config) && not inShell && (not isReExec || inContainer)
        then runShellAndExit mprojectRoot getCompilerVersion getCmdArgs
        else liftIO inner


runShellAndExit
    :: HasConfig env
    => Maybe (Path Abs Dir)
    -> IO (CompilerVersion 'CVWanted)
    -> RIO env (String, [String])
    -> RIO env ()
runShellAndExit mprojectRoot getCompilerVersion getCmdArgs = do
   config <- view configL
   envOverride <- view processContextL
   local (set processContextL envOverride) $ do
     (cmnd,args) <- fmap (escape *** map escape) getCmdArgs
     mshellFile <-
         traverse (resolveFile (fromMaybeProjectRoot mprojectRoot)) $
         nixInitFile (configNix config)
     compilerVersion <- liftIO getCompilerVersion
     inContainer <- getInContainer
     ghc <- either throwIO return $ nixCompiler compilerVersion
     let pkgsInConfig = nixPackages (configNix config)
         pkgs = pkgsInConfig ++ [ghc, "git", "gcc"]
         pkgsStr = "[" <> T.intercalate " " pkgs <> "]"
         pureShell = nixPureShell (configNix config)
         addGCRoots = nixAddGCRoots (configNix config)
         nixopts = case mshellFile of
           Just fp -> [toFilePath fp, "--arg", "ghc"
                      ,"with (import <nixpkgs> {}); " ++ T.unpack ghc]
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
                              ,"} \"\""]]
                    -- glibcLocales is necessary on Linux to avoid warnings about GHC being incapable to set the locale.
         fullArgs = concat [if pureShell then ["--pure"] else []
                           ,if addGCRoots then ["--indirect", "--add-root"
                                               ,toFilePath (configWorkDir config)
                                                F.</> "nix-gc-symlinks" F.</> "gc-root"] else []
                           ,map T.unpack (nixShellOptions (configNix config))
                           ,nixopts
                           ,["--run", unwords (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args)]
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

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot = fromMaybe (impureThrow CannotDetermineProjectRoot)

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

nixHelpOptName :: String
nixHelpOptName = nixCmdName ++ "-help"

-- | Exceptions thrown by "Stack.Nix".
data StackNixException
  = CannotDetermineProjectRoot
    -- ^ Can't determine the project root (location of the shell file if any).
  deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show CannotDetermineProjectRoot =
    "Cannot determine project root directory."
