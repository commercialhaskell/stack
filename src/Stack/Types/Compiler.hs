{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Stack.Types.Compiler
  ( ActualCompiler (..)
  , WhichCompiler (..)
  , getGhcVersion
  , whichCompiler
  , compilerExeName
  , compilerVersionString
  , parseCompilerVersion
  , haddockExeName
  ) where

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

-- | Specifies a compiler and its version number(s).
--
-- Note that despite having this datatype, stack isn't in a hurry to
-- support compilers other than GHC.
data ActualCompiler
    = ACGhc {-# UNPACK #-} !Version
    | ACGhcjs
        {-# UNPACK #-} !Version -- GHCJS version
        {-# UNPACK #-} !Version -- GHC version
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store ActualCompiler
instance NFData ActualCompiler
instance Display ActualCompiler where
    display = display . compilerVersionText
instance ToJSON ActualCompiler where
    toJSON = toJSON . compilerVersionText
instance FromJSON ActualCompiler where
    parseJSON (String t) = maybe (fail "Failed to parse compiler version") return (parseCompilerVersion t)
    parseJSON _ = fail "Invalid CompilerVersion, must be String"
instance FromJSONKey ActualCompiler where
    fromJSONKey = FromJSONKeyTextParser $ \k ->
        case parseCompilerVersion k of
            Nothing -> fail $ "Failed to parse CompilerVersion " ++ T.unpack k
            Just parsed -> return parsed

actualToWanted :: ActualCompiler -> WantedCompiler
actualToWanted (ACGhc x) = WCGhc x
actualToWanted (ACGhcjs x y) = WCGhcjs x y

wantedToActual :: WantedCompiler -> ActualCompiler
wantedToActual (WCGhc x) = ACGhc x
wantedToActual (WCGhcjs x y) = ACGhcjs x y

-- FIXME remove
parseCompilerVersion :: T.Text -> Maybe ActualCompiler
parseCompilerVersion t
    | Just t' <- T.stripPrefix "ghc-" t
    , Just v <- parseVersion $ T.unpack t'
        = Just (ACGhc v)
    | Just t' <- T.stripPrefix "ghcjs-" t
    , [tghcjs, tghc] <- T.splitOn "_ghc-" t'
    , Just vghcjs <- parseVersion $ T.unpack tghcjs
    , Just vghc <- parseVersion $ T.unpack tghc
        = Just (ACGhcjs vghcjs vghc)
    | otherwise
        = Nothing

compilerVersionText :: ActualCompiler -> T.Text -- FIXME remove, should be in pantry only
compilerVersionText (ACGhc vghc) =
    "ghc-" <> displayC vghc
compilerVersionText (ACGhcjs vghcjs vghc) =
    "ghcjs-" <> displayC vghcjs <> "_ghc-" <> displayC vghc

compilerVersionString :: ActualCompiler -> String
compilerVersionString = T.unpack . compilerVersionText

whichCompiler :: ActualCompiler -> WhichCompiler
whichCompiler ACGhc{} = Ghc
whichCompiler ACGhcjs{} = Ghcjs

isWantedCompiler :: VersionCheck -> WantedCompiler -> ActualCompiler -> Bool
isWantedCompiler check (WCGhc wanted) (ACGhc actual) =
    checkVersion check wanted actual
isWantedCompiler check (WCGhcjs wanted wantedGhc) (ACGhcjs actual actualGhc) =
    checkVersion check wanted actual && checkVersion check wantedGhc actualGhc
isWantedCompiler _ _ _ = False

getGhcVersion :: ActualCompiler -> Version
getGhcVersion (ACGhc v) = v
getGhcVersion (ACGhcjs _ v) = v

compilerExeName :: WhichCompiler -> String
compilerExeName Ghc = "ghc"
compilerExeName Ghcjs = "ghcjs"

haddockExeName :: WhichCompiler -> String
haddockExeName Ghc = "haddock"
haddockExeName Ghcjs = "haddock-ghcjs"
