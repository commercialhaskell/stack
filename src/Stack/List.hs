{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and functions related to Stack's @list@ command.
module Stack.List
  ( listCmd
  , listPackages
  ) where

import           Pantry ( loadSnapshot )
import           RIO.List ( intercalate )
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import           Stack.Config ( makeConcreteResolver )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.List" module.
newtype ListException
  = CouldNotParsePackageSelectors [String]
  deriving (Show, Typeable)

instance Exception ListException where
  displayException (CouldNotParsePackageSelectors strs) = unlines $
    "Error: [S-4926]"
    : map ("- " ++) strs

-- | Function underlying the @stack list@ command. List packages.
listCmd :: [String] -> RIO Runner ()
listCmd names = withConfig NoReexec $ do
  mresolver <- view $ globalOptsL.to globalResolver
  mSnapshot <- forM mresolver $ \resolver -> do
    concrete <- makeConcreteResolver resolver
    loc <- completeSnapshotLocation concrete
    loadSnapshot loc
  listPackages mSnapshot names

-- | Intended to work for the command line command.
listPackages ::
     forall env. (HasPantryConfig env, HasProcessContext env, HasTerm env)
  => Maybe RawSnapshot
     -- ^ When looking up by name, take from this build plan.
  -> [String]
     -- ^ Names or identifiers.
  -> RIO env ()
listPackages mSnapshot input = do
  let (errs1, names) = case mSnapshot of
        Just snapshot | null input -> ([], Map.keys (rsPackages snapshot))
        _ -> partitionEithers $ map parse input
  (errs2, locs) <- partitionEithers <$> traverse toLoc names
  case errs1 ++ errs2 of
    [] -> pure ()
    errs -> throwM $ CouldNotParsePackageSelectors errs
  mapM_ (prettyInfo . fromString . packageIdentifierString) locs
 where
  toLoc | Just snapshot <- mSnapshot = toLocSnapshot snapshot
        | otherwise = toLocNoSnapshot

  toLocNoSnapshot :: PackageName -> RIO env (Either String PackageIdentifier)
  toLocNoSnapshot name = do
    mloc1 <-
      getLatestHackageLocation YesRequireHackageIndex name UsePreferredVersions
    mloc <-
      case mloc1 of
        Just _ -> pure mloc1
        Nothing -> do
          updated <-
            updateHackageIndex $ Just $
                 "Could not find package "
              <> fromString (packageNameString name)
              <> ", updating"
          case updated of
            UpdateOccurred ->
              getLatestHackageLocation
                YesRequireHackageIndex
                name
                UsePreferredVersions
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
              else ". Perhaps you meant: " ++
                     intercalate ", " (map packageNameString candidates)
          ]
      Just loc -> pure $ Right (packageLocationIdent loc)

  toLocSnapshot ::
       RawSnapshot
    -> PackageName
    -> RIO env (Either String PackageIdentifier)
  toLocSnapshot snapshot name =
    case Map.lookup name (rsPackages snapshot) of
      Nothing ->
        pure $ Left $
          "Package does not appear in snapshot: " ++ packageNameString name
      Just sp -> do
        loc <- cplComplete <$> completePackageLocation (rspLocation sp)
        pure $ Right (packageLocationIdent loc)

  parse s =
    case parsePackageName s of
      Just x -> Right x
      Nothing -> Left $ "Could not parse as package name or identifier: " ++ s
