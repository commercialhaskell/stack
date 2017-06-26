{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Stack.Types.Resolver
  (Resolver
  ,IsLoaded(..)
  ,LoadedResolver
  ,ResolverWith(..)
  ,parseResolverText
  ,resolverDirName
  ,resolverName
  ,customResolverHash
  ,toResolverNotLoaded
  ,AbstractResolver(..)
  ,readAbstractResolver
  ,SnapName(..)
  ,Snapshots (..)
  ,renderSnapName
  ,parseSnapName
  ,SnapshotHash (..)
  ,trimmedSnapshotHash
  ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Monad.Catch (MonadThrow, throwM, Exception)
import           Data.Aeson.Extended
                 (ToJSON, toJSON, FromJSON, parseJSON, object,
                  WithJSONWarnings(..), Value(String, Object), (.=),
                  noJSONWarnings, (..:), withObjectWarnings, withObject, (.:),
                  withText)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Monoid.Extra
import           Data.Store (Store)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Read (decimal)
import           Data.Time (Day)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Options.Applicative (ReadM)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Prelude
import           Safe (readMay)
import           Stack.Types.Compiler

data IsLoaded = Loaded | NotLoaded

type LoadedResolver = ResolverWith SnapshotHash
type Resolver = ResolverWith ()

-- TODO: once GHC 8.0 is the lowest version we support, make these into
-- actual haddock comments...

-- | How we resolve which dependencies to install given a set of packages.
data ResolverWith hash
    = ResolverSnapshot !SnapName
    -- ^ Use an official snapshot from the Stackage project, either an
    -- LTS Haskell or Stackage Nightly.

    | ResolverCompiler !CompilerVersion
    -- ^ Require a specific compiler version, but otherwise provide no
    -- build plan. Intended for use cases where end user wishes to
    -- specify all upstream dependencies manually, such as using a
    -- dependency solver.

    | ResolverCustom !Text !Text !hash
    -- ^ A custom resolver based on the given name and URL. When a URL is
    -- provided, its contents must be completely immutable. Filepaths are
    -- always loaded. This constructor is used before the build-plan has
    -- been loaded, as we do not yet know the custom snapshot's hash.
    --
    -- If @p@ is @SnapshotHash@, then we have fully loaded this resolver
    -- and know its hash (which is used for file paths to store
    -- generated data). If @p@ is @()@, then we do not have a hash
    -- yet.
    deriving (Generic, Typeable, Show, Data, Eq)
instance Store LoadedResolver
instance NFData LoadedResolver

instance ToJSON Resolver where
    toJSON x = case x of
        ResolverSnapshot{} -> toJSON $ resolverName x
        ResolverCompiler{} -> toJSON $ resolverName x
        ResolverCustom n l _ -> handleCustom n l
      where
        handleCustom n l = object
             [ "name" .= n
             , "location" .= l
             ]
instance FromJSON (WithJSONWarnings Resolver) where
    -- Strange structuring is to give consistent error messages
    parseJSON v@(Object _) = withObjectWarnings "Resolver" (\o -> ResolverCustom
        <$> o ..: "name"
        <*> o ..: "location"
        <*> pure ()) v

    parseJSON (String t) = either (fail . show) return (noJSONWarnings <$> parseResolverText t)

    parseJSON _ = fail "Invalid Resolver, must be Object or String"

-- | Convert a Resolver into its @Text@ representation, as will be used by
-- directory names
resolverDirName :: LoadedResolver -> Text
resolverDirName (ResolverSnapshot name) = renderSnapName name
resolverDirName (ResolverCompiler v) = compilerVersionText v
resolverDirName (ResolverCustom name _ hash) = "custom-" <> name <> "-" <> decodeUtf8 (trimmedSnapshotHash hash)

-- | Convert a Resolver into its @Text@ representation for human
-- presentation.
resolverName :: ResolverWith p -> Text
resolverName (ResolverSnapshot name) = renderSnapName name
resolverName (ResolverCompiler v) = compilerVersionText v
resolverName (ResolverCustom name _ _) = "custom-" <> name

customResolverHash :: LoadedResolver -> Maybe SnapshotHash
customResolverHash (ResolverCustom _ _ hash) = Just hash
customResolverHash _ = Nothing

-- | Try to parse a @Resolver@ from a @Text@. Won't work for complex resolvers (like custom).
parseResolverText :: MonadThrow m => Text -> m Resolver
parseResolverText t
    | Right x <- parseSnapName t = return $ ResolverSnapshot x
    | Just v <- parseCompilerVersion t = return $ ResolverCompiler v
    | otherwise = throwM $ ParseResolverException t

toResolverNotLoaded :: LoadedResolver -> Resolver
toResolverNotLoaded r = case r of
    ResolverSnapshot s -> ResolverSnapshot s
    ResolverCompiler v -> ResolverCompiler v
    ResolverCustom n l _ -> ResolverCustom n l ()

-- | Either an actual resolver value, or an abstract description of one (e.g.,
-- latest nightly).
data AbstractResolver
    = ARLatestNightly
    | ARLatestLTS
    | ARLatestLTSMajor !Int
    | ARResolver !Resolver
    | ARGlobal
    deriving Show

readAbstractResolver :: ReadM AbstractResolver
readAbstractResolver = do
    s <- OA.readerAsk
    case s of
        "global" -> return ARGlobal
        "nightly" -> return ARLatestNightly
        "lts" -> return ARLatestLTS
        'l':'t':'s':'-':x | Right (x', "") <- decimal $ T.pack x ->
            return $ ARLatestLTSMajor x'
        _ ->
            case parseResolverText $ T.pack s of
                Left e -> OA.readerError $ show e
                Right x -> return $ ARResolver x

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Generic, Typeable, Show, Data, Eq)
instance Store SnapName
instance NFData SnapName

data BuildPlanTypesException
    = ParseSnapNameException !Text
    | ParseResolverException !Text
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
        Nightly <$> readMay (T.unpack t1)

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

newtype SnapshotHash = SnapshotHash { unShapshotHash :: ByteString }
    deriving (Generic, Typeable, Show, Data, Eq)
instance Store SnapshotHash
instance NFData SnapshotHash

trimmedSnapshotHash :: SnapshotHash -> ByteString
trimmedSnapshotHash = BS.take 12 . unShapshotHash
