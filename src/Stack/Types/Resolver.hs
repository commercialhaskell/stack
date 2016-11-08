{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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

module Stack.Types.Resolver
  (Resolver
  ,IsLoaded(..)
  ,LoadedResolver
  ,ResolverThat's(..)
  ,parseResolverText
  ,resolverDirName
  ,resolverName
  ,customResolverHash
  ,toResolverNotLoaded
  ,AbstractResolver(..)
  ,readAbstractResolver
  ) where

import           Control.Applicative
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Aeson.Extended
                 (ToJSON, toJSON, FromJSON, parseJSON, object,
                  WithJSONWarnings(..), Value(String, Object), (.=),
                  noJSONWarnings, (..:), withObjectWarnings)
import           Data.Monoid.Extra
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Read (decimal)
import           Options.Applicative (ReadM)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Prelude
import           Stack.Types.BuildPlan (parseSnapName, renderSnapName, SnapName, SnapshotHash,
                                        trimmedSnapshotHash)
import           {-# SOURCE #-} Stack.Types.Config (ConfigException(..))
import           Stack.Types.Compiler

data IsLoaded = Loaded | NotLoaded

type LoadedResolver = ResolverThat's 'Loaded
type Resolver = ResolverThat's 'NotLoaded

-- TODO: once GHC 8.0 is the lowest version we support, make these into
-- actual haddock comments...

-- | How we resolve which dependencies to install given a set of packages.
data ResolverThat's (l :: IsLoaded) where
    -- Use an official snapshot from the Stackage project, either an LTS
    -- Haskell or Stackage Nightly.
    ResolverSnapshot :: !SnapName -> ResolverThat's l
    -- Require a specific compiler version, but otherwise provide no
    -- build plan. Intended for use cases where end user wishes to
    -- specify all upstream dependencies manually, such as using a
    -- dependency solver.
    ResolverCompiler :: !CompilerVersion -> ResolverThat's l
    -- A custom resolver based on the given name and URL. When a URL is
    -- provided, it file is to be completely immutable. Filepaths are
    -- always loaded. This constructor is used before the build-plan has
    -- been loaded, as we do not yet know the custom snapshot's hash.
    ResolverCustom :: !Text -> !Text -> ResolverThat's 'NotLoaded
    -- Like 'ResolverCustom', but after loading the build-plan, so we
    -- have a hash. This is necessary in order to identify the location
    -- files are stored for the resolver.
    ResolverCustomLoaded :: !Text -> !Text -> !SnapshotHash -> ResolverThat's 'Loaded

deriving instance Eq (ResolverThat's k)
deriving instance Show (ResolverThat's k)

instance ToJSON (ResolverThat's k) where
    toJSON x = case x of
        ResolverSnapshot{} -> toJSON $ resolverName x
        ResolverCompiler{} -> toJSON $ resolverName x
        ResolverCustom n l -> handleCustom n l
        ResolverCustomLoaded n l _ -> handleCustom n l
      where
        handleCustom n l = object
             [ "name" .= n
             , "location" .= l
             ]
instance FromJSON (WithJSONWarnings (ResolverThat's 'NotLoaded)) where
    -- Strange structuring is to give consistent error messages
    parseJSON v@(Object _) = withObjectWarnings "Resolver" (\o -> ResolverCustom
        <$> o ..: "name"
        <*> o ..: "location") v

    parseJSON (String t) = either (fail . show) return (noJSONWarnings <$> parseResolverText t)

    parseJSON _ = fail "Invalid Resolver, must be Object or String"

-- | Convert a Resolver into its @Text@ representation, as will be used by
-- directory names
resolverDirName :: LoadedResolver -> Text
resolverDirName (ResolverSnapshot name) = renderSnapName name
resolverDirName (ResolverCompiler v) = compilerVersionText v
resolverDirName (ResolverCustomLoaded name _ hash) = "custom-" <> name <> "-" <> decodeUtf8 (trimmedSnapshotHash hash)

-- | Convert a Resolver into its @Text@ representation for human
-- presentation.
resolverName :: ResolverThat's l -> Text
resolverName (ResolverSnapshot name) = renderSnapName name
resolverName (ResolverCompiler v) = compilerVersionText v
resolverName (ResolverCustom name _) = "custom-" <> name
resolverName (ResolverCustomLoaded name _ _) = "custom-" <> name

customResolverHash :: LoadedResolver-> Maybe SnapshotHash
customResolverHash (ResolverCustomLoaded _ _ hash) = Just hash
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
    ResolverCustomLoaded n l _ -> ResolverCustom n l

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
