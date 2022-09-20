{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Stack.Types.Resolver
  (AbstractResolver(..)
  ,readAbstractResolver
  ,Snapshots (..)
  ) where

import           Pantry.Internal.AesonExtended
                 (FromJSON, parseJSON,
                  withObject, (.:), withText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Data.Time (Day)
import           Options.Applicative (ReadM)
import qualified Options.Applicative.Types as OA
import           Stack.Prelude

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

data BuildPlanTypesException
    = ParseResolverException !Text
    | FilepathInDownloadedSnapshot !Text
    deriving Typeable
instance Exception BuildPlanTypesException
instance Show BuildPlanTypesException where
    show (ParseResolverException t) = concat
        [ "Invalid resolver value: "
        , T.unpack t
        , ". Possible valid values include lts-2.12, nightly-YYYY-MM-DD, ghc-7.10.2, and ghcjs-0.1.0_ghc-7.10.2. "
        , "See https://www.stackage.org/snapshots for a complete list."
        ]
    show (FilepathInDownloadedSnapshot url) = unlines
        [ "Downloaded snapshot specified a 'resolver: { location: filepath }' "
        , "field, but filepaths are not allowed in downloaded snapshots.\n"
        , "Filepath specified: " ++ T.unpack url
        ]

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
                Left e -> fail $ show e
                Right (LTS _ _) -> fail "Unexpected LTS value"
                Right (Nightly d) -> pure d

        isLTS = ("lts-" `T.isPrefixOf`)

        parseLTS = withText "LTS" $ \t ->
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS x y) -> pure $ IntMap.singleton x y
                Right (Nightly _) -> fail "Unexpected nightly value"
