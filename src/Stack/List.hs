{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and functions related to Stack's @list@ command.
module Stack.List
  ( listCmd
  , listPackages
  ) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import           Stack.Config ( getRawSnapshot )
import           Stack.Prelude
import           Stack.Runners ( ShouldReexec (..), withConfig )
import           Stack.Types.Runner ( Runner )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.List" module.
newtype ListPrettyException
  = CouldNotParsePackageSelectors [StyleDoc]
  deriving (Show, Typeable)

instance Pretty ListPrettyException where
  pretty (CouldNotParsePackageSelectors errs) =
    "[S-4926]"
    <> line
    <> bulletedList errs

instance Exception ListPrettyException

-- | Function underlying the @stack list@ command. List packages.
listCmd :: [String] -> RIO Runner ()
listCmd names = withConfig NoReexec $ do
  mSnapshot <- getRawSnapshot
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
    errs -> prettyThrowM $ CouldNotParsePackageSelectors errs
  mapM_ (Lazy.putStrLn . fromPackageId) locs
 where
  toLoc | Just snapshot <- mSnapshot = toLocSnapshot snapshot
        | otherwise = toLocNoSnapshot

  toLocNoSnapshot :: PackageName -> RIO env (Either StyleDoc PackageIdentifier)
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
              <> fromPackageName name
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
        pure $ Left $ fillSep
          [ flow "Could not find package"
          , style Current (fromPackageName name)
          , flow "on Hackage."
          , if null candidates
              then mempty
              else fillSep $
                  flow "Perhaps you meant one of:"
                : mkNarrativeList (Just Good) False
                    (map fromPackageName candidates :: [StyleDoc])
          ]
      Just loc -> pure $ Right (packageLocationIdent loc)

  toLocSnapshot ::
       RawSnapshot
    -> PackageName
    -> RIO env (Either StyleDoc PackageIdentifier)
  toLocSnapshot snapshot name =
    case Map.lookup name (rsPackages snapshot) of
      Nothing ->
        pure $ Left $ fillSep
          [ flow "Package does not appear in snapshot:"
          , style Current (fromPackageName name) <> "."
          ]
      Just sp -> do
        loc <- cplComplete <$> completePackageLocation (rspLocation sp)
        pure $ Right (packageLocationIdent loc)

  parse s =
    case parsePackageName s of
      Just x -> Right x
      Nothing -> Left $ fillSep
        [ flow "Could not parse as package name or identifier:"
        , style Current (fromString s) <> "."
        ]
