{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,nixCompiler
       ,StackNixException(..)
       ) where

import Control.Applicative
import Control.Monad (join, when)
import qualified Data.Text as T
import Data.Maybe
import Data.Typeable
import Distribution.System (OS (..))
import Stack.Types
import Control.Exception.Lifted
import Control.Monad.Catch (throwM,MonadCatch)
import Prelude

-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: (Monad m, MonadCatch m)
    => NixOptsMonoid
    -> OS
    -> m NixOpts
nixOptsFromMonoid NixOptsMonoid{..} os = do
    let nixEnable = fromMaybe nixMonoidDefaultEnable nixMonoidEnable
        defaultPure = case os of
          OSX -> False
          _ -> True
        nixPureShell = fromMaybe defaultPure nixMonoidPureShell
        nixPackages = fromMaybe [] nixMonoidPackages
        nixInitFile = nixMonoidInitFile
        nixShellOptions = fromMaybe [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromMaybe [] nixMonoidPath)
    when (not (null nixPackages) && isJust nixInitFile) $
       throwM NixCannotUseShellFileAndPackagesException
    return NixOpts{..}
  where prefixAll p (x:xs) = p : x : prefixAll p xs
        prefixAll _ _      = []

nixCompiler :: Config -> Maybe Resolver -> Maybe CompilerVersion -> T.Text
nixCompiler config resolverOverride compilerOverride =
  let mproject = fst <$> configMaybeProject config
      mresolver = resolverOverride <|> fmap projectResolver mproject
      mcompiler = compilerOverride <|> join (fmap projectCompiler mproject)
      nixCompilerFromVersion v = T.filter (/= '.') $ T.append (T.pack "haskell.compiler.ghc") (versionText v)
  in case (mresolver, mcompiler)  of
       (_, Just (GhcVersion v)) -> nixCompilerFromVersion v
       (Just (ResolverCompiler (GhcVersion v)), _) -> nixCompilerFromVersion v
       (Just (ResolverSnapshot (LTS x y)), _) ->
         T.pack ("haskell.packages.lts-" ++ show x ++ "_" ++ show y ++ ".ghc")
       _ -> T.pack "ghc"

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
