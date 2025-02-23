{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoFieldSelectors     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Stack.Types.Snapshot
  ( AbstractSnapshot (..)
  , readAbstractSnapshot
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
-- "Stack.Types.Snapshot" module.
data TypesSnapshotException
  = ParseSnapshotException !Text
  | FilepathInDownloadedSnapshot !Text
  deriving (Show, Typeable)

instance Exception TypesSnapshotException where
  displayException (ParseSnapshotException t) = concat
    [ "Error: [S-8787]\n"
    , "Invalid snapshot value: "
    , T.unpack t
    , ". Possible valid values include lts-2.12, nightly-YYYY-MM-DD, \
      \ghc-7.10.2, and ghcjs-0.1.0_ghc-7.10.2. See \
      \https://www.stackage.org/snapshots for a complete list."
    ]
  displayException (FilepathInDownloadedSnapshot url) = unlines
    [ "Error: [S-4865]"
    , "Downloaded snapshot specified 'snapshot: { location: filepath }', "
    , "but filepaths are not allowed in downloaded snapshots.\n"
    , "Filepath specified: " ++ T.unpack url
    ]

-- | Either an actual snapshot value, or an abstract description of one (e.g.,
-- latest nightly).
data AbstractSnapshot
  = ASLatestNightly
  | ASLatestLTS
  | ASLatestLTSMajor !Int
  | ASSnapshot !RawSnapshotLocation
  | ASGlobal

instance Show AbstractSnapshot where
  show = T.unpack . utf8BuilderToText . display

instance Display AbstractSnapshot where
  display ASLatestNightly = "nightly"
  display ASLatestLTS = "lts"
  display (ASLatestLTSMajor x) = "lts-" <> display x
  display (ASSnapshot usl) = display usl
  display ASGlobal = "global"

instance FromJSON (Unresolved AbstractSnapshot) where
  parseJSON = withText "Unresolved AbstractSnapshot" $ \t ->
    pure $ parseAbstractSnapshot $ T.unpack t

readAbstractSnapshot :: ReadM (Unresolved AbstractSnapshot)
readAbstractSnapshot = parseAbstractSnapshot <$> OA.readerAsk

parseAbstractSnapshot :: String -> Unresolved AbstractSnapshot
parseAbstractSnapshot s = case s of
  "global" -> pure ASGlobal
  "nightly" -> pure ASLatestNightly
  "lts" -> pure ASLatestLTS
  'l':'t':'s':'-':x | Right (x', "") <- decimal $ T.pack x ->
      pure $ ASLatestLTSMajor x'
  _ ->ASSnapshot <$> parseRawSnapshotLocation (T.pack s)

-- | Most recent Nightly and newest LTS version per major release.
data Snapshots = Snapshots
  { nightly :: !Day
  , lts     :: !(IntMap Int)
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
