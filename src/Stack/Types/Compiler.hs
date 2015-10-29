{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Stack.Types.Compiler where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary.VersionTagged (Binary, HasStructuralInfo)
import           Data.Map (Map)
import qualified Data.Map as Map
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
instance HasStructuralInfo CompilerVersion
instance NFData CompilerVersion
instance ToJSON CompilerVersion where
    toJSON = toJSON . compilerVersionText
instance FromJSON CompilerVersion where
    parseJSON (String t) = maybe (fail "Failed to parse compiler version") return (parseCompilerVersion t)
    parseJSON _ = fail "Invalid CompilerVersion, must be String"
instance FromJSON a => FromJSON (Map CompilerVersion a) where
    -- TODO: Dedupe with similar code in Stack.Types.Version?
    --
    -- Maybe this ought to be abstracted into a 'JSONKey' class, so that a
    -- fully generic definition for Map can be provided.
    parseJSON val = do
        m <- parseJSON val
        fmap Map.fromList $ mapM go $ Map.toList m
      where
        go (k, v) = do
            let mparsed = parseCompilerVersion (T.pack k)
            case mparsed of
                Nothing -> fail $ "Failed to parse CompilerVersion " ++ k
                Just parsed -> return (parsed, v)

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
