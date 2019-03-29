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
  , CompilerRepository (..)
  , defaultCompilerRepository
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
import           Data.Text (Text)
import           Stack.Prelude
import           Stack.Types.Version
import           Distribution.Version (mkVersion)

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
    | ACGhcGit !Text !Text
    | ACGhcjs
        !Version -- GHCJS version
        !Version -- GHC version
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance NFData ActualCompiler
instance Display ActualCompiler where
    display (ACGhc x) = display (WCGhc x)
    display (ACGhcjs x y) = display (WCGhcjs x y)
    display (ACGhcGit x y) = display (WCGhcGit x y)
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
wantedToActual (WCGhcGit x y) = ACGhcGit x y

actualToWanted :: ActualCompiler -> WantedCompiler
actualToWanted (ACGhc x) = WCGhc x
actualToWanted (ACGhcjs x y) = WCGhcjs x y
actualToWanted (ACGhcGit x y) = WCGhcGit x y

parseActualCompiler :: T.Text -> Either PantryException ActualCompiler
parseActualCompiler = fmap wantedToActual . parseWantedCompiler

compilerVersionText :: ActualCompiler -> T.Text
compilerVersionText = utf8BuilderToText . display

compilerVersionString :: ActualCompiler -> String
compilerVersionString = T.unpack . compilerVersionText

whichCompiler :: ActualCompiler -> WhichCompiler
whichCompiler ACGhc{} = Ghc
whichCompiler ACGhcGit{} = Ghc
whichCompiler ACGhcjs{} = Ghcjs

isWantedCompiler :: VersionCheck -> WantedCompiler -> ActualCompiler -> Bool
isWantedCompiler check (WCGhc wanted) (ACGhc actual) =
    checkVersion check wanted actual
isWantedCompiler check (WCGhcjs wanted wantedGhc) (ACGhcjs actual actualGhc) =
    checkVersion check wanted actual && checkVersion check wantedGhc actualGhc
isWantedCompiler _check (WCGhcGit wCommit wFlavour) (ACGhcGit aCommit aFlavour) =
    wCommit == aCommit && wFlavour == aFlavour
isWantedCompiler _ _ _ = False

getGhcVersion :: ActualCompiler -> Version
getGhcVersion (ACGhc v) = v
getGhcVersion (ACGhcjs _ v) = v
getGhcVersion (ACGhcGit _ _) =
   -- We can't return the actual version without running the installed ghc.
   -- For now we assume that users of ghc-git use it with a recent commit so we
   -- return a version far in the future. This disables our hacks for older
   -- versions and passes version checking when we use newer features.
   mkVersion [999,0,0]

-- | Repository containing the compiler sources
newtype CompilerRepository
  = CompilerRepository Text
  deriving (Show)

instance FromJSON CompilerRepository where
  parseJSON = withText "CompilerRepository" (return . CompilerRepository)

defaultCompilerRepository :: CompilerRepository
defaultCompilerRepository = CompilerRepository "https://gitlab.haskell.org/ghc/ghc.git"
