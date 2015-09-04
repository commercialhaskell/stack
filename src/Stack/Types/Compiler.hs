{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Stack.Types.Compiler where

import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Aeson
import           Data.Binary (Binary)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Stack.Types.Version

-- | Variety of compiler to use.
data WhichCompiler
    = Ghc
    | Ghcjs
    deriving (Show, Eq, Ord)

-- | Specifies a compiler and its version number(s).
--
-- Note that despite having this datatype, stack isn't in a hurry to
-- support compilers other than GHC.
--
-- NOTE: updating this will change its binary serialization. The
-- version number in the 'BinarySchema' instance for 'MiniBuildPlan'
-- should be updated.
data CompilerVersion
    = GhcVersion {-# UNPACK #-} !Version
    | GhcjsVersion
        {-# UNPACK #-} !Version -- GHCJS version
        {-# UNPACK #-} !Version -- GHC version
    deriving (Generic, Show, Eq, Ord)
instance Binary CompilerVersion
instance NFData CompilerVersion where
    rnf = genericRnf
instance ToJSON CompilerVersion where
    toJSON = toJSON . compilerVersionName
instance FromJSON CompilerVersion where
    parseJSON (String t) = maybe (fail "Failed to parse compiler version") return (parseCompilerVersion t)
    parseJSON _ = fail "Invalid CompilerVersion, must be String"

parseCompilerVersion :: T.Text -> Maybe CompilerVersion
parseCompilerVersion t
    | Just t' <- T.stripPrefix "ghc-" t
    , Just v <- parseVersionFromString $ T.unpack t'
        = Just (GhcVersion v)
    | Just t' <- T.stripPrefix "ghcjs-" t
    , [tghcjs, tghc] <- T.splitOn "_ghc-" t'
    , Just vghcjs <- parseVersionFromString $ T.unpack tghcjs
    , Just vghc <- parseVersionFromString $ T.unpack tghc
        = Just (GhcjsVersion vghcjs vghc)
    | otherwise
        = Nothing

compilerVersionName :: CompilerVersion -> T.Text
compilerVersionName (GhcVersion vghc) =
    "ghc-" <> versionText vghc
compilerVersionName (GhcjsVersion vghcjs vghc) =
    "ghcjs-" <> versionText vghcjs <> "_ghc-" <> versionText vghc

whichCompiler :: CompilerVersion -> WhichCompiler
whichCompiler GhcVersion {} = Ghc
whichCompiler GhcjsVersion {} = Ghcjs

isWantedCompiler :: VersionCheck -> CompilerVersion -> CompilerVersion -> Bool
isWantedCompiler check (GhcVersion wanted) (GhcVersion actual) =
    checkVersion check wanted actual
isWantedCompiler check (GhcjsVersion wanted wantedGhc) (GhcjsVersion actual actualGhc) =
    checkVersion check wanted actual && checkVersion check wantedGhc actualGhc
isWantedCompiler _ _ _ = False

compilerExeName :: WhichCompiler -> String
compilerExeName Ghc = "ghc"
compilerExeName Ghcjs = "ghcjs"

haddockExeName :: WhichCompiler -> String
haddockExeName Ghc = "haddock"
haddockExeName Ghcjs = "haddock-ghcjs"
