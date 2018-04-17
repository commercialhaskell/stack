{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Stack.Types.Compiler where

import           Data.Aeson
import           Data.Data
import qualified Data.Text as T
import           Stack.Prelude
import           Stack.Types.Version

-- | Variety of compiler to use.
data WhichCompiler
    = Ghc
    | Ghcjs
    deriving (Show, Eq, Ord)

-- | Whether the compiler version given is the wanted version (what
-- the stack.yaml file, snapshot file, or --resolver argument
-- request), or the actual installed GHC. Depending on the matching
-- requirements, these values could be different.
data CVType = CVWanted | CVActual

-- | Specifies a compiler and its version number(s).
--
-- Note that despite having this datatype, stack isn't in a hurry to
-- support compilers other than GHC.
data CompilerVersion (cvType :: CVType)
    = GhcVersion {-# UNPACK #-} !Version
    | GhcjsVersion
        {-# UNPACK #-} !Version -- GHCJS version
        {-# UNPACK #-} !Version -- GHC version
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store (CompilerVersion a)
instance NFData (CompilerVersion a)
instance Display (CompilerVersion a) where
    display = display . compilerVersionText
instance ToJSON (CompilerVersion a) where
    toJSON = toJSON . compilerVersionText
instance FromJSON (CompilerVersion a) where
    parseJSON (String t) = maybe (fail "Failed to parse compiler version") return (parseCompilerVersion t)
    parseJSON _ = fail "Invalid CompilerVersion, must be String"
instance FromJSONKey (CompilerVersion a) where
    fromJSONKey = FromJSONKeyTextParser $ \k ->
        case parseCompilerVersion k of
            Nothing -> fail $ "Failed to parse CompilerVersion " ++ T.unpack k
            Just parsed -> return parsed

actualToWanted :: CompilerVersion 'CVActual -> CompilerVersion 'CVWanted
actualToWanted (GhcVersion x) = GhcVersion x
actualToWanted (GhcjsVersion x y) = GhcjsVersion x y

wantedToActual :: CompilerVersion 'CVWanted -> CompilerVersion 'CVActual
wantedToActual (GhcVersion x) = GhcVersion x
wantedToActual (GhcjsVersion x y) = GhcjsVersion x y

parseCompilerVersion :: T.Text -> Maybe (CompilerVersion a)
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

compilerVersionText :: CompilerVersion a -> T.Text
compilerVersionText (GhcVersion vghc) =
    "ghc-" <> versionText vghc
compilerVersionText (GhcjsVersion vghcjs vghc) =
    "ghcjs-" <> versionText vghcjs <> "_ghc-" <> versionText vghc

compilerVersionString :: CompilerVersion a -> String
compilerVersionString = T.unpack . compilerVersionText

whichCompiler :: CompilerVersion a -> WhichCompiler
whichCompiler GhcVersion {} = Ghc
whichCompiler GhcjsVersion {} = Ghcjs

isWantedCompiler :: VersionCheck -> CompilerVersion 'CVWanted -> CompilerVersion 'CVActual -> Bool
isWantedCompiler check (GhcVersion wanted) (GhcVersion actual) =
    checkVersion check wanted actual
isWantedCompiler check (GhcjsVersion wanted wantedGhc) (GhcjsVersion actual actualGhc) =
    checkVersion check wanted actual && checkVersion check wantedGhc actualGhc
isWantedCompiler _ _ _ = False

getGhcVersion :: CompilerVersion a -> Version
getGhcVersion (GhcVersion v) = v
getGhcVersion (GhcjsVersion _ v) = v

compilerExeName :: WhichCompiler -> String
compilerExeName Ghc = "ghc"
compilerExeName Ghcjs = "ghcjs"

haddockExeName :: WhichCompiler -> String
haddockExeName Ghc = "haddock"
haddockExeName Ghcjs = "haddock-ghcjs"
