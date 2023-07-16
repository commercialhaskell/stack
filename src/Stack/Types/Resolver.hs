{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Stack.Types.Resolver
  ( AbstractResolver (..)
  , readAbstractResolver
  , Snapshots (..)
  ) where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types
                   ( FromJSON, parseJSON, withObject, withText )
import           Data.Aeson.WarningParser ( (.:) )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import           Data.Text.Read ( decimal )
import           Data.Time ( Day )
import           Options.Applicative ( ReadM )
import qualified Options.Applicative.Types as OA
import           Stack.Prelude

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Types.Resolver" module.
data TypesResolverException
  = ParseResolverException !Text
  | FilepathInDownloadedSnapshot !Text
  deriving (Show, Typeable)

instance Exception TypesResolverException where
  displayException (ParseResolverException t) = concat
    [ "Error: [S-8787]\n"
    , "Invalid resolver value: "
    , T.unpack t
    , ". Possible valid values include lts-2.12, nightly-YYYY-MM-DD, \
      \ghc-7.10.2, and ghcjs-0.1.0_ghc-7.10.2. See \
      \https://www.stackage.org/snapshots for a complete list."
    ]
  displayException (FilepathInDownloadedSnapshot url) = unlines
    [ "Error: [S-4865]"
    , "Downloaded snapshot specified a 'resolver: { location: filepath }' "
    , "field, but filepaths are not allowed in downloaded snapshots.\n"
    , "Filepath specified: " ++ T.unpack url
    ]

-- | Either an actual resolver value, or an abstract description of one (e.g.,
-- latest nightly).
data AbstractResolver
  = ARLatestNightly
  | ARLatestLTS
  | ARLatestLTSMajor !Int
  | ARResolver !RawSnapshotLocation
  | ARGlobal

instance Show AbstractResolver where
  show = T.unpack . utf8BuilderToText . display

instance Display AbstractResolver where
  display ARLatestNightly = "nightly"
  display ARLatestLTS = "lts"
  display (ARLatestLTSMajor x) = "lts-" <> display x
  display (ARResolver usl) = display usl
  display ARGlobal = "global"

readAbstractResolver :: ReadM (Unresolved AbstractResolver)
readAbstractResolver = do
  s <- OA.readerAsk
  case s of
    "global" -> pure $ pure ARGlobal
    "nightly" -> pure $ pure ARLatestNightly
    "lts" -> pure $ pure ARLatestLTS
    'l':'t':'s':'-':x | Right (x', "") <- decimal $ T.pack x ->
        pure $ pure $ ARLatestLTSMajor x'
    _ -> pure $ ARResolver <$> parseRawSnapshotLocation (T.pack s)

-- | Most recent Nightly and newest LTS version per major release.
data Snapshots = Snapshots
  { snapshotsNightly :: !Day
  , snapshotsLts     :: !(IntMap Int)
  }
  deriving Show

instance FromJSON Snapshots where
  parseJSON = withObject "Snapshots" $ \o -> Snapshots
    <$> (o .: "nightly" >>= parseNightly)
    <*> fmap IntMap.unions (mapM (parseLTS . snd)
          $ filter (isLTS . Key.toText . fst)
          $ KeyMap.toList o)
   where
    parseNightly t =
      case parseSnapName t of
        Left e -> fail $ displayException e
        Right (LTS _ _) -> fail "Unexpected LTS value"
        Right (Nightly d) -> pure d

    isLTS = ("lts-" `T.isPrefixOf`)

    parseLTS = withText "LTS" $ \t ->
      case parseSnapName t of
        Left e -> fail $ displayException e
        Right (LTS x y) -> pure $ IntMap.singleton x y
        Right (Nightly _) -> fail "Unexpected nightly value"
