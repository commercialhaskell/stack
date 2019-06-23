{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- | Utils for the other Stack.Storage modules
module Stack.Storage.Util
    ( updateList
    , updateSet
    ) where

import qualified Data.Set as Set
import Database.Persist
import Stack.Prelude hiding (MigrationFailure)

-- | Efficiently update a set of values stored in a database table
updateSet ::
       ( PersistEntityBackend record ~ BaseBackend backend
       , PersistField parentid
       , PersistField value
       , Ord value
       , PersistEntity record
       , MonadIO m
       , PersistQueryWrite backend
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
