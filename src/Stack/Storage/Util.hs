{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Utils for the other Stack.Storage modules
module Stack.Storage.Util
  ( handleMigrationException
  , updateList
  , updateSet
  ) where

import qualified Data.Set as Set
import           Database.Persist
                   ( BaseBackend, EntityField, PersistEntity
                   , PersistEntityBackend, PersistField, PersistQueryWrite
                   , SafeToInsert, (<-.), (==.), deleteWhere, insertMany_
                   )
import           Stack.Prelude
import           Stack.Types.Storage ( StoragePrettyException (..) )

-- | Efficiently update a set of values stored in a database table
updateSet ::
     ( PersistEntityBackend record ~ BaseBackend backend
     , PersistField parentid
     , PersistField value
     , Ord value
     , PersistEntity record
     , MonadIO m
     , PersistQueryWrite backend
     , SafeToInsert record
     )
  => (parentid -> value -> record)
  -> EntityField record parentid
  -> parentid
  -> EntityField record value
  -> Set value
  -> Set value
  -> ReaderT backend m ()
updateSet recordCons parentFieldCons parentId valueFieldCons old new =
  when (old /= new) $ do
    deleteWhere
      [ parentFieldCons ==. parentId
      , valueFieldCons <-. Set.toList (Set.difference old new)
      ]
    insertMany_ $
      map (recordCons parentId) $ Set.toList (Set.difference new old)

-- | Efficiently update a list of values stored in a database table.
updateList ::
     ( PersistEntityBackend record ~ BaseBackend backend
     , PersistField parentid
     , Ord value
     , PersistEntity record
     , MonadIO m
     , PersistQueryWrite backend
     , SafeToInsert record
     )
  => (parentid -> Int -> value -> record)
  -> EntityField record parentid
  -> parentid
  -> EntityField record Int
  -> [value]
  -> [value]
  -> ReaderT backend m ()
updateList recordCons parentFieldCons parentId indexFieldCons old new =
  when (old /= new) $ do
    let oldSet = Set.fromList (zip [0 ..] old)
        newSet = Set.fromList (zip [0 ..] new)
    deleteWhere
      [ parentFieldCons ==. parentId
      , indexFieldCons <-.
        map fst (Set.toList $ Set.difference oldSet newSet)
      ]
    insertMany_ $
      map (uncurry $ recordCons parentId) $
      Set.toList (Set.difference newSet oldSet)

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
