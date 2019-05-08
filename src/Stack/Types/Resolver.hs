{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Stack.Types.Resolver
  (AbstractResolver(..)
  ,readAbstractResolver
  ,SnapName(..)
  ,Snapshots (..)
  ,renderSnapName
  ,parseSnapName
  ) where

import           Pantry.Internal.AesonExtended
                 (FromJSON, parseJSON,
                  withObject, (.:), withText)
import qualified Data.HashMap.Strict as HashMap
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

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Generic, Typeable, Show, Data, Eq)
instance NFData SnapName
instance Display SnapName where
  display = display . renderSnapName

data BuildPlanTypesException
    = ParseSnapNameException !Text
    | ParseResolverException !Text
    | FilepathInDownloadedSnapshot !Text
    deriving Typeable
instance Exception BuildPlanTypesException
instance Show BuildPlanTypesException where
    show (ParseSnapNameException t) = "Invalid snapshot name: " ++ T.unpack t
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

-- | Convert a 'SnapName' into its short representation, e.g. @lts-2.8@,
-- @nightly-2015-03-05@.
renderSnapName :: SnapName -> Text
renderSnapName (LTS x y) = T.pack $ concat ["lts-", show x, ".", show y]
renderSnapName (Nightly d) = T.pack $ "nightly-" ++ show d

-- | Parse the short representation of a 'SnapName'.
parseSnapName :: MonadThrow m => Text -> m SnapName
parseSnapName t0 =
    case lts <|> nightly of
        Nothing -> throwM $ ParseSnapNameException t0
        Just sn -> return sn
  where
    lts = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, t2) <- Just $ decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ decimal t3
        return $ LTS x y
    nightly = do
        t1 <- T.stripPrefix "nightly-" t0
        Nightly <$> readMaybe (T.unpack t1)

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
                $ filter (isLTS . fst)
                $ HashMap.toList o)
      where
        parseNightly t =
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS _ _) -> fail "Unexpected LTS value"
                Right (Nightly d) -> return d

        isLTS = ("lts-" `T.isPrefixOf`)

        parseLTS = withText "LTS" $ \t ->
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS x y) -> return $ IntMap.singleton x y
                Right (Nightly _) -> fail "Unexpected nightly value"
