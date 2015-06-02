{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Stack.PackageDump
    ( Line
    , eachSection
    , eachPair
    , DumpPackage (..)
    , conduitDumpPackage
    , ghcPkgDump
    , ProfilingCache
    , newProfilingCache
    , loadProfilingCache
    , saveProfilingCache
    , addProfiling
    ) where

import Data.Binary (Binary)
import qualified Data.Binary as Binary
import GHC.Generics (Generic)
import Path
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger)
import System.Process.Read
import Control.Exception.Enclosed (tryIO)
import Data.Map (Map)
import Data.IORef
import Control.Monad.Catch (MonadThrow, Exception, throwM)
import Control.Monad (when, liftM)
import Stack.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import Data.Typeable (Typeable)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Map as Map
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import Stack.GhcPkg

-- | Cached information on whether a package has profiling libraries
newtype ProfilingCache = ProfilingCache (IORef (Map GhcPkgId Bool))

-- | Call ghc-pkg dump with appropriate flags and stream to the given @Sink@
ghcPkgDump :: (MonadIO m, MonadLogger m)
           => EnvOverride
           -> [Path Abs Dir] -- ^ package databases
           -> Consumer ByteString IO a
           -> m a
ghcPkgDump menv pkgDbs sink = do
    mapM_ (createDatabase menv) pkgDbs -- FIXME maybe use some retry logic instead?
    sinkProcessStdout menv "ghc-pkg" args sink -- FIXME ensure that GHC_PACKAGE_PATH isn't set?
  where
    args = packageDbFlags pkgDbs ++ ["dump"]

-- | Create a new, empty @ProfilingCache@
newProfilingCache :: MonadIO m => m ProfilingCache
newProfilingCache = liftIO $ ProfilingCache <$> newIORef Map.empty

-- | Load a @ProfilingCache@ from disk, swalloing any errors and returning an empty cache.
loadProfilingCache :: MonadIO m => Path Abs File -> m ProfilingCache
loadProfilingCache path = liftIO $ fmap ProfilingCache $ do
    eres <- tryIO $ Binary.decodeFileOrFail $ toFilePath path
    newIORef $ case eres of
        Right (Right x) -> x
        _ -> Map.empty

-- | Save a @ProfilingCache@ to disk
saveProfilingCache :: MonadIO m => Path Abs File -> ProfilingCache -> m ()
saveProfilingCache path (ProfilingCache ref) = liftIO $ do
    createDirectoryIfMissing True $ toFilePath $ parent path
    readIORef ref >>= Binary.encodeFile (toFilePath path)

-- | Add profiling information to the stream of @DumpPackage@s
addProfiling :: MonadIO m
             => ProfilingCache
             -> Conduit (DumpPackage a) m (DumpPackage Bool)
addProfiling (ProfilingCache ref) =
    CL.mapM go
  where
    go dp = liftIO $ do
        m <- readIORef ref
        let gid = dpGhcPkgId dp
        p <- case Map.lookup gid m of
            Just p -> return p
            Nothing -> do
                let loop [] = return False
                    loop (dir:dirs) = do
                        contents <- getDirectoryContents $ S8.unpack dir
                        let pairs = [(content, lib) | content <- contents, lib <- dpLibraries dp]
                        if or [isProfiling content lib
                              | content <- contents
                              , lib <- dpLibraries dp
                              ]
                            then return True
                            else loop dirs
                loop $ dpLibDirs dp
        return dp { dpProfiling = p }

isProfiling content lib =
    prefix `S.isPrefixOf` S8.pack content
  where
    prefix = S.concat ["lib", lib, "_p"]

-- | Dump information for a single package
data DumpPackage profiling = DumpPackage
    { dpGhcPkgId :: !GhcPkgId
    , dpLibDirs :: ![ByteString]
    , dpLibraries :: ![ByteString]
    , dpDepends :: ![GhcPkgId]
    , dpProfiling :: !profiling
    }
    deriving (Show, Eq, Ord)

data PackageDumpException
    = MissingSingleField ByteString
    | MissingMultiField ByteString
    | MismatchedId PackageName Version GhcPkgId
    deriving (Show, Typeable)
instance Exception PackageDumpException

-- | Convert a stream of bytes into a stream of @DumpPackage@s
conduitDumpPackage :: MonadThrow m
                   => Conduit ByteString m (DumpPackage ())
conduitDumpPackage = (=$= CL.catMaybes) $ eachSection $ do
    pairs <- eachPair (\k -> (k, ) <$> CL.consume) =$= CL.consume
    let m = Map.fromList pairs
    let parseS k =
            case Map.lookup k m of
                Just [v] -> return v
                _ -> throwM $ MissingSingleField k
        parseM k =
            case Map.lookup k m of
                Just vs -> return vs
                Nothing -> throwM $ MissingMultiField k

        parseDepend :: MonadThrow m => ByteString -> m (Maybe GhcPkgId)
        parseDepend "builtin_rts" = return Nothing
        parseDepend bs =
            liftM Just $ parseGhcPkgId bs'
          where
            (bs', _builtinRts) =
                case stripSuffixBS " builtin_rts" bs of
                    Nothing ->
                        case stripPrefixBS "builtin_rts " bs of
                            Nothing -> (bs, False)
                            Just x -> (x, True)
                    Just x -> (x, True)
    case Map.lookup "id" m of
        Just ["builtin_rts"] -> return Nothing
        _ -> do
            name <- parseS "name" >>= parsePackageName
            version <- parseS "version" >>= parseVersion
            ghcPkgId <- parseS "id" >>= parseGhcPkgId
            when (PackageIdentifier name version /= ghcPkgIdPackageIdentifier ghcPkgId)
                $ throwM $ MismatchedId name version ghcPkgId
            libDirs <- parseM "library-dirs"
            libraries <- parseM "hs-libraries"
            depends <- parseM "depends" >>= mapM parseDepend
            return $ Just DumpPackage
                { dpGhcPkgId = ghcPkgId
                , dpLibDirs = libDirs
                , dpLibraries = S8.words $ S8.unwords libraries
                , dpDepends = catMaybes (depends :: [Maybe GhcPkgId])
                , dpProfiling = ()
                }

stripPrefixBS x y
    | x `S.isPrefixOf` y = Just $ S.drop (S.length x) y
    | otherwise = Nothing

stripSuffixBS x y
    | x `S.isSuffixOf` y = Just $ S.take (S.length y - S.length x) y
    | otherwise = Nothing

-- | A single line of input, not including line endings
type Line = ByteString

-- | Apply the given Sink to each section of output, broken by a single line containing ---
eachSection :: Monad m
            => Sink Line m a
            -> Conduit ByteString m a
eachSection inner =
    CL.map (S.filter (/= _cr)) =$= CB.lines =$= start
  where
    _cr = 13

    start = CL.peek >>= maybe (return ()) (const go)

    go = do
        x <- toConsumer $ takeWhileC (/= "---") =$= inner
        yield x
        CL.drop 1
        start

-- | Grab each key/value pair
eachPair :: Monad m
         => (ByteString -> Sink Line m a)
         -> Conduit Line m a
eachPair inner =
    start
  where
    start = await >>= maybe (return ()) start'

    _colon = 58
    _space = 32

    start' bs1 =
        toConsumer (valSrc =$= inner key) >>= yield >> start
      where
        (key, bs2) = S.breakByte _colon bs1
        (spaces, bs3) = S.span (== _space) $ S.drop 1 bs2
        indent = S.length key + 1 + S.length spaces

        valSrc
            | S.null bs3 = noIndent
            | otherwise = yield bs3 >> loopIndent indent

    noIndent = do
        mx <- await
        case mx of
            Nothing -> return ()
            Just bs -> do
                let (spaces, val) = S.span (== _space) bs
                if S.length spaces == 0
                    then leftover val
                    else do
                        yield val
                        loopIndent (S.length spaces)

    loopIndent i =
        loop
      where
        loop = await >>= maybe (return ()) go

        go bs
            | S.length spaces == i && S.all (== _space) spaces =
                yield val >> loop
            | otherwise = leftover bs
          where
            (spaces, val) = S.splitAt i bs

-- | General purpose utility
takeWhileC :: Monad m => (a -> Bool) -> Conduit a m a
takeWhileC f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go x
        | f x = yield x >> loop
        | otherwise = leftover x
