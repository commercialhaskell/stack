{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
  (Resolver
  ,IsLoaded(..)
  ,LoadedResolver
  ,ResolverWith(..)
  ,parseResolverText
  ,AbstractResolver(..)
  ,readAbstractResolver
  ,resolverRawName
  ,SnapName(..)
  ,Snapshots (..)
  ,renderSnapName
  ,parseSnapName
  ,SnapshotHash
  ,trimmedSnapshotHash
  ,snapshotHashToBS
  ,snapshotHashFromBS
  ,snapshotHashFromDigest
  ,parseCustomLocation
  ) where

import           Crypto.Hash as Hash (hash, Digest, SHA256)
import           Data.Aeson.Extended
                 (ToJSON, toJSON, FromJSON, parseJSON,
                  withObject, (.:), withText)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Read (decimal)
import           Data.Time (Day)
import           Network.HTTP.Client (Request, parseUrlThrow)
import           Options.Applicative (ReadM)
import qualified Options.Applicative.Types as OA
import           Path
import           Stack.Prelude
import           Stack.Types.Compiler
import           Stack.Types.PackageIdentifier
import qualified System.FilePath as FP

data IsLoaded = Loaded | NotLoaded

type LoadedResolver = ResolverWith SnapshotHash
type Resolver = ResolverWith (Either Request FilePath)

-- TODO: once GHC 8.0 is the lowest version we support, make these into
-- actual haddock comments...

-- | How we resolve which dependencies to install given a set of packages.
data ResolverWith customContents
    = ResolverStackage !SnapName
    -- ^ Use an official snapshot from the Stackage project, either an
    -- LTS Haskell or Stackage Nightly.

    | ResolverCompiler !(CompilerVersion 'CVWanted)
    -- ^ Require a specific compiler version, but otherwise provide no
    -- build plan. Intended for use cases where end user wishes to
    -- specify all upstream dependencies manually, such as using a
    -- dependency solver.

    | ResolverCustom !Text !customContents
    -- ^ A custom resolver based on the given location (as a raw URL
    -- or filepath). If @customContents@ is a @Either Request
    -- FilePath@, it represents the parsed location value (with
    -- filepaths resolved relative to the directory containing the
    -- file referring to the custom snapshot). Once it has been loaded
    -- from disk, it will be replaced with a @SnapshotHash@ value,
    -- which is used to store cached files.
    deriving (Generic, Typeable, Show, Data, Eq, Functor, Foldable, Traversable)
instance Store LoadedResolver
instance NFData LoadedResolver

instance ToJSON (ResolverWith a) where
    toJSON x = case x of
        ResolverStackage name -> toJSON $ renderSnapName name
        ResolverCompiler version -> toJSON $ compilerVersionText version
        ResolverCustom loc _ -> toJSON loc
instance a ~ () => FromJSON (ResolverWith a) where
    parseJSON = withText "ResolverWith ()" $ return . parseResolverText

-- | Convert a Resolver into its @Text@ representation for human
-- presentation. When possible, you should prefer @sdResolverName@, as
-- it will handle the human-friendly name inside a custom snapshot.
resolverRawName :: ResolverWith a -> Text
resolverRawName (ResolverStackage name) = renderSnapName name
resolverRawName (ResolverCompiler v) = compilerVersionText v
resolverRawName (ResolverCustom loc _ ) = "custom: " <> loc

parseCustomLocation
  :: MonadThrow m
  => Maybe (Path Abs Dir) -- ^ directory config value was read from
  -> ResolverWith () -- could technically be any type parameter, restricting to help with type safety
  -> m Resolver
parseCustomLocation mdir (ResolverCustom t ()) =
  ResolverCustom t <$> case parseUrlThrow $ T.unpack t of
    Nothing -> Right <$> do
      dir <-
        case mdir of
          Nothing -> throwM $ FilepathInDownloadedSnapshot t
          Just x -> return x
      let rel =
              T.unpack
            $ fromMaybe t
            $ T.stripPrefix "file://" t <|> T.stripPrefix "file:" t
      return $ toFilePath dir FP.</> rel
    Just req -> return $ Left req
parseCustomLocation _ (ResolverStackage name) = return $ ResolverStackage name
parseCustomLocation _ (ResolverCompiler cv) = return $ ResolverCompiler cv

-- | Parse a @Resolver@ from a @Text@
parseResolverText :: Text -> ResolverWith ()
parseResolverText t
    | Right x <- parseSnapName t = ResolverStackage x
    | Just v <- parseCompilerVersion t = ResolverCompiler v
    | otherwise = ResolverCustom t ()

-- | Either an actual resolver value, or an abstract description of one (e.g.,
-- latest nightly).
data AbstractResolver
    = ARLatestNightly
    | ARLatestLTS
    | ARLatestLTSMajor !Int
    | ARResolver !(ResolverWith ())
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
        _ -> return $ ARResolver $ parseResolverText $ T.pack s

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Generic, Typeable, Show, Data, Eq)
instance Store SnapName
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

newtype SnapshotHash = SnapshotHash { unSnapshotHash :: StaticSHA256 }
    deriving (Generic, Typeable, Show, Data, Eq)
instance Store SnapshotHash
instance NFData SnapshotHash

-- | Return the first 12 characters of the hash as a B64URL-encoded
-- string.
trimmedSnapshotHash :: SnapshotHash -> Text
trimmedSnapshotHash = decodeUtf8 . B.take 12 . B64URL.encode . staticSHA256ToRaw . unSnapshotHash

-- | Return the raw bytes in the hash
snapshotHashToBS :: SnapshotHash -> ByteString
snapshotHashToBS = staticSHA256ToRaw . unSnapshotHash

-- | Create a new SnapshotHash by SHA256 hashing the given contents
snapshotHashFromBS :: ByteString -> SnapshotHash
snapshotHashFromBS = snapshotHashFromDigest . Hash.hash

-- | Create a new SnapshotHash from the given digest
snapshotHashFromDigest :: Digest SHA256 -> SnapshotHash
snapshotHashFromDigest = SnapshotHash . mkStaticSHA256FromDigest
