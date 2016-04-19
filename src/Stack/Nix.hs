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

import           Control.Applicative
import           Control.Arrow ((***))
import           Control.Exception (Exception,throw)
import           Control.Monad hiding (mapM)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logDebug)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Char (toUpper)
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
import           Stack.Config (makeConcreteResolver)
import           Stack.Config.Nix (nixCompiler)
import           Stack.Constants (stackProgName,platformVariantEnvVar)
import           Stack.Docker (reExecArgName)
import           Stack.Exec (exec)
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (lookupEnv,getArgs,getExecutablePath)
import           System.Process.Read (getEnvOverride)

-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
reexecWithOptionalShell
    :: M env m
    => Maybe (Path Abs Dir)
    -> Maybe AbstractResolver
    -> Maybe CompilerVersion
    -> IO ()
    -> m ()
reexecWithOptionalShell mprojectRoot maresolver mcompiler inner =
  do config <- asks getConfig
     inShell <- getInShell
     isReExec <- asks getReExec
     if nixEnable (configNix config) && not inShell && not isReExec
       then runShellAndExit mprojectRoot maresolver mcompiler getCmdArgs
       else liftIO inner
  where
    getCmdArgs = do
        args <-
            fmap
                (("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) :)
                (liftIO getArgs)
        exePath <- liftIO getExecutablePath
        return (exePath, args)

runShellAndExit
    :: M env m
    => Maybe (Path Abs Dir)
    -> Maybe AbstractResolver
    -> Maybe CompilerVersion
    -> m (String, [String])
    -> m ()
runShellAndExit mprojectRoot maresolver mcompiler getCmdArgs = do
     config <- asks getConfig
     mresolver <- mapM makeConcreteResolver maresolver
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- fmap (escape *** map escape) getCmdArgs
     mshellFile <-
         traverse (resolveFile (fromMaybeProjectRoot mprojectRoot)) $
         nixInitFile (configNix config)
     let pkgsInConfig = nixPackages (configNix config)
         pkgs = pkgsInConfig ++ [nixCompiler config mresolver mcompiler]
         pureShell = nixPureShell (configNix config)
         nixopts = case mshellFile of
           Just fp -> [toFilePath fp]
           Nothing -> ["-E", T.unpack $ T.intercalate " " $ concat
                              [["with (import <nixpkgs> {});"
                               ,"runCommand \"myEnv\" {"
                               ,"buildInputs=lib.optional stdenv.isLinux glibcLocales ++ ["],pkgs,["];"
                               ,T.pack platformVariantEnvVar <> "=''nix'';"
                               ,T.pack inShellEnvVar <> "=1;"
                               ,"STACK_IN_NIX_EXTRA_ARGS=''"]
                               ,      (map (\p -> T.concat
                                                  ["--extra-lib-dirs=${",p,"}/lib"
                                                  ," --extra-include-dirs=${",p,"}/include "])
                                           pkgs), ["'' ;"
                               ,"} \"\""]]]
                    -- glibcLocales is necessary on Linux to avoid warnings about GHC being incapable to set the locale.
         fullArgs = concat [if pureShell then ["--pure"] else [],
                            map T.unpack (nixShellOptions (configNix config))
                           ,nixopts
                           ,["--run", intercalate " " (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args)]
                           ]
                           -- Using --run instead of --command so we cannot
                           -- end up in the nix-shell if stack build is Ctrl-C'd
     $logDebug $
         "Using a nix-shell environment " <> (case mshellFile of
            Just path -> "from file: " <> (T.pack (toFilePath path))
            Nothing -> "with nix packages: " <> (T.intercalate ", " pkgs))
     exec envOverride "nix-shell" fullArgs

-- | Shell-escape quotes inside the string and enclose it in quotes.
escape :: String -> String
escape str = "'" ++ foldr (\c -> if c == '\'' then
                                   ("'\"'\"'"++)
                                 else (c:)) "" str
                 ++ "'"

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot = fromMaybe (throw CannotDetermineProjectRootException)

-- | 'True' if we are currently running inside a Nix.
getInShell :: (MonadIO m) => m Bool
getInShell = liftIO (isJust <$> lookupEnv inShellEnvVar)

-- | Environment variable used to indicate stack is running in container.
-- although we already have STACK_IN_NIX_EXTRA_ARGS that is set in the same conditions,
-- it can happen that STACK_IN_NIX_EXTRA_ARGS is set to empty.
inShellEnvVar :: String
inShellEnvVar = concat [map toUpper stackProgName,"_IN_NIXSHELL"]

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

nixHelpOptName :: String
nixHelpOptName = nixCmdName ++ "-help"

-- | Exceptions thrown by "Stack.Nix".
data StackNixException
  = CannotDetermineProjectRootException
    -- ^ Can't determine the project root (location of the shell file if any).
  deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show CannotDetermineProjectRootException =
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
