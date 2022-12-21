{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.List
  ( listPackages
  ) where

import           RIO.List ( intercalate )
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import           Stack.Prelude

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.List" module.
newtype ListException
  = CouldNotParsePackageSelectors [String]
    deriving (Show, Typeable)

instance Exception ListException where
    displayException (CouldNotParsePackageSelectors strs) = unlines $
        "Error: [S-4926]"
        : map ("- " ++) strs

-- | Intended to work for the command line command.
listPackages ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Maybe RawSnapshot -- ^ when looking up by name, take from this build plan
  -> [String] -- ^ names or identifiers
  -> RIO env ()
listPackages mSnapshot input = do
    let (errs1, names) = case mSnapshot of
                   Just snapshot | null input ->
                                     ([], Map.keys (rsPackages snapshot))
                   _ -> partitionEithers $ map parse input
    (errs2, locs) <- partitionEithers <$> traverse toLoc names
    case errs1 ++ errs2 of
      [] -> pure ()
      errs -> throwM $ CouldNotParsePackageSelectors errs
    mapM_ (logInfo . fromString . packageIdentifierString) locs
  where
    toLoc | Just snapshot <- mSnapshot = toLocSnapshot snapshot
          | otherwise = toLocNoSnapshot

    toLocNoSnapshot :: PackageName -> RIO env (Either String PackageIdentifier)
    toLocNoSnapshot name = do
      mloc1 <- getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
      mloc <-
        case mloc1 of
          Just _ -> pure mloc1
          Nothing -> do
            updated <- updateHackageIndex $ Just $ "Could not find package " <> fromString (packageNameString name) <> ", updating"
            case updated of
              UpdateOccurred -> getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
              NoUpdateOccurred -> pure Nothing
      case mloc of
        Nothing -> do
          candidates <- getHackageTypoCorrections name
          pure $ Left $ concat
            [ "Could not find package "
            , packageNameString name
            , " on Hackage"
            , if null candidates
                then ""
                else ". Perhaps you meant: " ++ intercalate ", " (map packageNameString candidates)
            ]
        Just loc -> pure $ Right (packageLocationIdent loc)

    toLocSnapshot :: RawSnapshot -> PackageName -> RIO env (Either String PackageIdentifier)
    toLocSnapshot snapshot name =
        case Map.lookup name (rsPackages snapshot) of
          Nothing ->
            pure $ Left $ "Package does not appear in snapshot: " ++ packageNameString name
          Just sp -> do
            loc <- cplComplete <$> completePackageLocation (rspLocation sp)
            pure $ Right (packageLocationIdent loc)

    parse s =
        case parsePackageName s of
            Just x -> Right x
            Nothing -> Left $ "Could not parse as package name or identifier: " ++ s
