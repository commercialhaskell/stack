{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Stack.Types.Compiler
  ( ActualCompiler (..)
  , WhichCompiler (..)
  , getGhcVersion
  , whichCompiler
  , compilerVersionText
  , compilerVersionString
  , isWantedCompiler
  , wantedToActual
  , actualToWanted
  , parseActualCompiler
  ) where

import           Data.Aeson
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
    = ACGhc !Version
    | ACGhcjs
        !Version -- GHCJS version
        !Version -- GHC version
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance NFData ActualCompiler
instance Display ActualCompiler where
    display (ACGhc x) = display (WCGhc x)
    display (ACGhcjs x y) = display (WCGhcjs x y)
instance ToJSON ActualCompiler where
    toJSON = toJSON . compilerVersionText
instance FromJSON ActualCompiler where
    parseJSON (String t) = either (const $ fail "Failed to parse compiler version") return (parseActualCompiler t)
    parseJSON _ = fail "Invalid CompilerVersion, must be String"
instance FromJSONKey ActualCompiler where
    fromJSONKey = FromJSONKeyTextParser $ \k ->
        case parseActualCompiler k of
            Left _ -> fail $ "Failed to parse CompilerVersion " ++ T.unpack k
            Right parsed -> return parsed

wantedToActual :: WantedCompiler -> ActualCompiler
wantedToActual (WCGhc x) = ACGhc x
wantedToActual (WCGhcjs x y) = ACGhcjs x y

actualToWanted :: ActualCompiler -> WantedCompiler
actualToWanted (ACGhc x) = WCGhc x
actualToWanted (ACGhcjs x y) = WCGhcjs x y

parseActualCompiler :: T.Text -> Either PantryException ActualCompiler
parseActualCompiler = fmap wantedToActual . parseWantedCompiler

compilerVersionText :: ActualCompiler -> T.Text
compilerVersionText = utf8BuilderToText . display

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
