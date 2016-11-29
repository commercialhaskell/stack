{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Stack.Types.Compiler where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Data
import           Data.Monoid ((<>))
import           Data.Store (Store)
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
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store CompilerVersion
instance NFData CompilerVersion
instance ToJSON CompilerVersion where
    toJSON = toJSON . compilerVersionText
instance FromJSON CompilerVersion where
    parseJSON (String t) = maybe (fail "Failed to parse compiler version") return (parseCompilerVersion t)
    parseJSON _ = fail "Invalid CompilerVersion, must be String"
instance FromJSONKey CompilerVersion where
    fromJSONKey = FromJSONKeyTextParser $ \k ->
        case parseCompilerVersion k of
            Nothing -> fail $ "Failed to parse CompilerVersion " ++ T.unpack k
            Just parsed -> return parsed

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

compilerVersionText :: CompilerVersion -> T.Text
compilerVersionText (GhcVersion vghc) =
    "ghc-" <> versionText vghc
compilerVersionText (GhcjsVersion vghcjs vghc) =
    "ghcjs-" <> versionText vghcjs <> "_ghc-" <> versionText vghc

compilerVersionString :: CompilerVersion -> String
compilerVersionString = T.unpack . compilerVersionText

whichCompiler :: CompilerVersion -> WhichCompiler
whichCompiler GhcVersion {} = Ghc
whichCompiler GhcjsVersion {} = Ghcjs

isWantedCompiler :: VersionCheck -> CompilerVersion -> CompilerVersion -> Bool
isWantedCompiler check (GhcVersion wanted) (GhcVersion actual) =
    checkVersion check wanted actual
isWantedCompiler check (GhcjsVersion wanted wantedGhc) (GhcjsVersion actual actualGhc) =
    checkVersion check wanted actual && checkVersion check wantedGhc actualGhc
isWantedCompiler _ _ _ = False

getGhcVersion :: CompilerVersion -> Version
getGhcVersion (GhcVersion v) = v
getGhcVersion (GhcjsVersion _ v) = v

compilerExeName :: WhichCompiler -> String
compilerExeName Ghc = "ghc"
compilerExeName Ghcjs = "ghcjs"

haddockExeName :: WhichCompiler -> String
haddockExeName Ghc = "haddock"
haddockExeName Ghcjs = "haddock-ghcjs"
