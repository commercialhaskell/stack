{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stack.Types.Cache
    ( ConfigCacheKey(..)
    , ConfigCacheType(..)
    , PrecompiledCacheKey(..)
    ) where

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Database.Persist.Sql (PersistField(..), PersistFieldSql(..), SqlType(..))
import Path
import Stack.Prelude
import Stack.Types.Compiler
import Stack.Types.GhcPkgId

-- | Key used to retrieve configuration or flag cache
data ConfigCacheKey = ConfigCacheKey
    { cckDirectory :: Path Abs Dir
    , cckType :: ConfigCacheType
    } deriving (Eq, Show)

-- | Type of config cache
data ConfigCacheType
    = ConfigCacheTypeConfig
    | ConfigCacheTypeFlagLibrary GhcPkgId
    | ConfigCacheTypeFlagExecutable PackageIdentifier
    deriving (Eq, Show)

instance PersistField ConfigCacheKey where
    toPersistValue (ConfigCacheKey fp ConfigCacheTypeConfig) =
        toPersistValue ["config", toFilePath fp]
    toPersistValue (ConfigCacheKey fp (ConfigCacheTypeFlagLibrary v)) =
        toPersistValue ["lib", toFilePath fp, T.unpack (unGhcPkgId v)]
    toPersistValue (ConfigCacheKey fp (ConfigCacheTypeFlagExecutable v)) =
        toPersistValue ["exe", toFilePath fp, packageIdentifierString v]
    fromPersistValue x = do
        y <- fromPersistValue x
        case y of
            ["config", fp] -> do
                dir <- mapLeft tshow $ parseAbsDir fp
                Right $ ConfigCacheKey dir ConfigCacheTypeConfig
            ["lib", fp, v] -> do
                dir <- mapLeft tshow $ parseAbsDir fp
                ghcPkgId <- mapLeft tshow $ parseGhcPkgId (T.pack v)
                Right $ ConfigCacheKey dir $ ConfigCacheTypeFlagLibrary ghcPkgId
            ["exe", fp, v] -> do
                dir <- mapLeft tshow $ parseAbsDir fp
                pkgId <-
                    maybe
                        (Left $
                         "Unexpected PackageIdentifier value: " <> T.pack v)
                        Right $
                    parsePackageIdentifier v
                Right $ ConfigCacheKey dir $ ConfigCacheTypeFlagExecutable pkgId
            _ -> Left $ "Unexected ConfigCacheKey value: " <> tshow x

instance PersistFieldSql ConfigCacheKey where
    sqlType _ = SqlString

-- | Key used to retrieve to retrieve the precompiled cache
data PrecompiledCacheKey = PrecompiledCacheKey
    { pckPlatformGhcDir :: Path Rel Dir
    , pckCompiler :: ActualCompiler
    , pckCabalVersion :: Version
    , pckPackageKey :: Text
    , pckOptionsHash :: ByteString
    } deriving (Eq, Show)

instance PersistField PrecompiledCacheKey where
    toPersistValue PrecompiledCacheKey {..} =
        toPersistValue
            [ toFilePath pckPlatformGhcDir
            , compilerVersionString pckCompiler
            , versionString pckCabalVersion
            , T.unpack pckPackageKey
            , S8.unpack $ B64URL.encode pckOptionsHash
            ]
    fromPersistValue x = do
        y <- fromPersistValue x
        case y of
            [platformGhcDir, compiler, cabalVersion, pkg, hash] -> do
                pckPlatformGhcDir <- mapLeft tshow $ parseRelDir platformGhcDir
                pckCompiler <-
                    mapLeft tshow $ parseActualCompiler (T.pack compiler)
                pckCabalVersion <-
                    maybe
                        (Left $
                         "Unexpected version value: " <> tshow cabalVersion)
                        Right $
                    parseVersion cabalVersion
                let pckPackageKey = T.pack pkg
                pckOptionsHash <- mapLeft T.pack $ B64URL.decode (S8.pack hash)
                Right PrecompiledCacheKey {..}
            _ -> Left $ "Unexpected PrecompiledCacheKey value: " <> tshow x

instance PersistFieldSql PrecompiledCacheKey where
    sqlType _ = SqlString
