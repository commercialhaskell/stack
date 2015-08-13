{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Stack.Types.Compiler where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Stack.Types.Version

-- | Specifies a compiler and its version number(s).
--
-- Note that despite having this datatype, stack isn't in a hurry to
-- support compilers other than GHC.
data CompilerVersion
    = GhcVersion {-# UNPACK #-} !Version
    deriving (Show, Eq, Ord)

parseCompilerVersion :: T.Text -> Maybe CompilerVersion
parseCompilerVersion t
    | Just t' <- T.stripPrefix "ghc-" t
    , Just v <- parseVersionFromString $ T.unpack t'
        = Just (GhcVersion v)
    | otherwise
        = Nothing

compilerVersionName :: CompilerVersion -> T.Text
compilerVersionName (GhcVersion vghc) =
    "ghc-" <> versionText vghc

isWantedCompiler :: VersionCheck -> CompilerVersion -> CompilerVersion -> Bool
isWantedCompiler check (GhcVersion wanted) (GhcVersion actual) =
    checkVersion check wanted actual
