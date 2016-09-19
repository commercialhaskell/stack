{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (reexecWithOptionalShell
  ,nixCmdName
  ,nixHelpOptName
  ) where

import           Control.Arrow ((***))
import           Control.Exception (Exception,throw)
import           Control.Monad hiding (mapM)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logDebug)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           Data.Typeable (Typeable)
import           Data.Version (showVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import qualified Paths_stack as Meta
import           Prelude hiding (mapM) -- Fix redundant import warnings
import           Stack.Config (getInNixShell, getInContainer)
import           Stack.Config.Nix (nixCompiler)
import           Stack.Constants (platformVariantEnvVar,inNixShellEnvVar,inContainerEnvVar)
import           Stack.Exec (exec)
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Nix
import           Stack.Types.Compiler
import           Stack.Types.Internal
import           System.Environment (getArgs,getExecutablePath,lookupEnv)
import qualified System.FilePath  as F
import           System.Process.Read (getEnvOverride)

-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
reexecWithOptionalShell
    :: M env m
    => Maybe (Path Abs Dir)
    -> IO CompilerVersion
    -> IO ()
    -> m ()
reexecWithOptionalShell mprojectRoot getCompilerVersion inner =
  do config <- asks getConfig
     inShell <- getInNixShell
     inContainer <- getInContainer
     isReExec <- asks getReExec
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
    :: M env m
    => Maybe (Path Abs Dir)
    -> IO CompilerVersion
    -> m (String, [String])
    -> m ()
runShellAndExit mprojectRoot getCompilerVersion getCmdArgs = do
     config <- asks getConfig
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- fmap (escape *** map escape) getCmdArgs
     mshellFile <-
         traverse (resolveFile (fromMaybeProjectRoot mprojectRoot)) $
         nixInitFile (configNix config)
     compilerVersion <- liftIO getCompilerVersion
     inContainer <- getInContainer
     let pkgsInConfig = nixPackages (configNix config)
         ghc = nixCompiler compilerVersion
         pkgs = pkgsInConfig ++ [ghc]
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
                              ,    "stackExtraArgs = lib.concatStrings ("
                              ,      "lib.foldl' (acc: p: acc ++ [\" --extra-lib-dirs \" p]) [] "
                              ,        "(lib.splitString '':'' libPath) ++ "
                              ,      "lib.foldl' (acc: p: acc ++ [\" --extra-include-dirs \" p]) [] "
                              ,        "(lib.splitString '':'' (lib.makeSearchPathOutput ''dev'' ''include'' inputs))"
                              ,    "); in "
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
                           ,["--run", intercalate " " (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args)]
                           ]
                           -- Using --run instead of --command so we cannot
                           -- end up in the nix-shell if stack build is Ctrl-C'd
     pathVar <- liftIO $ lookupEnv "PATH"
     $logDebug $ "PATH is: " <> T.pack (show pathVar)
     $logDebug $
       "Using a nix-shell environment " <> (case mshellFile of
            Just path -> "from file: " <> T.pack (toFilePath path)
            Nothing -> "with nix packages: " <> T.intercalate ", " pkgs)
     exec envOverride "nix-shell" fullArgs

-- | Shell-escape quotes inside the string and enclose it in quotes.
escape :: String -> String
escape str = "'" ++ foldr (\c -> if c == '\'' then
                                   ("'\"'\"'"++)
                                 else (c:)) "" str
                 ++ "'"

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot = fromMaybe (throw CannotDetermineProjectRoot)

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

type M env m =
  (MonadIO m
  ,MonadReader env m
  ,MonadLogger m
  ,MonadBaseControl IO m
  ,MonadMask m
  ,HasConfig env
  ,HasTerminal env
  ,HasReExec env
  ,HasHttpManager env
  )
