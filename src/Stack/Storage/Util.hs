{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Utils for the other Stack.Storage modules
module Stack.Storage.Util
  ( handleMigrationException
  , updateCollection
  , setUpdateDiff
  , listUpdateDiff
  ) where

import qualified Data.Set as Set
import           Database.Persist
                   ( BaseBackend, EntityField, PersistEntity
                   , PersistEntityBackend, PersistField, PersistQueryWrite
                   , SafeToInsert, (<-.), deleteWhere, insertMany_, Filter
                   )
import           Stack.Prelude
import           Stack.Types.Storage ( StoragePrettyException (..) )

-- | Efficiently update a collection of values with a given diff function.
updateCollection ::
     ( PersistEntityBackend record ~ BaseBackend backend
     , Eq (collection rawValue)
     , PersistEntity record
     , PersistField value
     , MonadIO m
     , PersistQueryWrite backend
     , SafeToInsert record
     , Foldable collection
     )
  => (collection rawValue -> collection rawValue -> ([Filter record], [value]))
  -> (value -> record)
  -> [Filter record]
  -> collection rawValue
  -> collection rawValue
  -> ReaderT backend m ()
updateCollection fnDiffer recordCons extra old new =
  when (old /= new) $ do
    let (oldMinusNewFilter, newMinusOld) = fnDiffer old new
    unless (null oldMinusNewFilter) $ deleteWhere
        (extra ++ oldMinusNewFilter)
    unless (null newMinusOld) $ insertMany_ $
      map recordCons $ toList newMinusOld

setUpdateDiff :: 
  (Ord value, PersistField value)
  => EntityField record value
  -> Set value
  -> Set value
  -> ([Filter record], [value])
setUpdateDiff indexFieldCons old new = 
    let oldMinusNew = Set.difference old new
    in ([indexFieldCons <-. toList oldMinusNew], toList $ Set.difference new old)

listUpdateDiff ::
  (Ord value)
  => EntityField record Int
  -> [value]
  -> [value]
  -> ([Filter record], [(Int, value)])
listUpdateDiff indexFieldCons old new =
    let oldSet = Set.fromList (zip [0 ..] old)
        newSet = Set.fromList (zip [0 ..] new)
        oldMinusNew = Set.difference oldSet newSet
        indexList = map fst (Set.toList oldMinusNew)
    in ([indexFieldCons <-. indexList], toList $ Set.difference newSet oldSet)

handleMigrationException :: HasLogFunc env => RIO env a -> RIO env a
handleMigrationException inner = do
  eres <- try inner
  either
    ( \e -> case e :: PantryException of
              MigrationFailure desc fp ex ->
                prettyThrowIO $ StorageMigrationFailure desc fp ex
              _ -> throwIO e
    )
    pure
    eres
