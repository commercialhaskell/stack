{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
-- Concurrent execution with dependencies. Types currently hard-coded for needs
-- of stack, but could be generalized easily.
module Control.Concurrent.Execute
    ( ActionType (..)
    , ActionId (..)
    , ActionContext (..)
    , Action (..)
    , runActions
    ) where

import           Control.Applicative
import           Control.Concurrent.Async (Concurrently (..), async)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad            (join, unless)
import           Data.Foldable            (sequenceA_)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Typeable            (Typeable)
import           Prelude -- Fix AMP warning
import           Stack.Types

data ActionType
    = ATBuild
    | ATBuildFinal
    | ATFinal
    deriving (Show, Eq, Ord)
data ActionId = ActionId !PackageIdentifier !ActionType
    deriving (Show, Eq, Ord)
data Action = Action
    { actionId   :: !ActionId
    , actionDeps :: !(Set ActionId)
    , actionDo   :: !(ActionContext -> IO ())
    }

data ActionContext = ActionContext
    { acRemaining :: !(Set ActionId)
    -- ^ Does not include the current action
    }
    deriving Show

data ExecuteState = ExecuteState
    { esActions    :: TVar [Action]
    , esExceptions :: TVar [SomeException]
    , esInAction   :: TVar (Set ActionId)
    , esCompleted  :: TVar Int
    , esFinalLock  :: Maybe (TMVar ())
    , esKeepGoing  :: Bool
    }

data ExecuteException
    = InconsistentDependencies
    deriving Typeable
instance Exception ExecuteException

instance Show ExecuteException where
    show InconsistentDependencies =
        "Inconsistent dependencies were discovered while executing your build plan. This should never happen, please report it as a bug to the stack team."

runActions :: Int -- ^ threads
           -> Bool -- ^ keep going after one task has failed
           -> Bool -- ^ run final actions concurrently?
           -> [Action]
           -> (TVar Int -> IO ()) -- ^ progress updated
           -> IO [SomeException]
runActions threads keepGoing concurrentFinal actions0 withProgress = do
    es <- ExecuteState
        <$> newTVarIO actions0
        <*> newTVarIO []
        <*> newTVarIO Set.empty
        <*> newTVarIO 0
        <*> (if concurrentFinal
                then pure Nothing
                else Just <$> atomically (newTMVar ()))
        <*> pure keepGoing
    _ <- async $ withProgress $ esCompleted es
    if threads <= 1
        then runActions' es
        else runConcurrently $ sequenceA_ $ replicate threads $ Concurrently $ runActions' es
    readTVarIO $ esExceptions es

runActions' :: ExecuteState -> IO ()
runActions' ExecuteState {..} =
    loop
  where
    breakOnErrs inner = do
        errs <- readTVar esExceptions
        if null errs || esKeepGoing
            then inner
            else return $ return ()
    withActions inner = do
        as <- readTVar esActions
        if null as
            then return $ return ()
            else inner as
    loop = join $ atomically $ breakOnErrs $ withActions $ \as ->
        case break (Set.null . actionDeps) as of
            (_, []) -> do
                inAction <- readTVar esInAction
                if Set.null inAction
                    then do
                        unless esKeepGoing $
                            modifyTVar esExceptions (toException InconsistentDependencies:)
                        return $ return ()
                    else retry
            (xs, action:ys) -> do
                unlock <-
                    case (actionId action, esFinalLock) of
                        (ActionId _ ATFinal, Just lock) -> do
                            takeTMVar lock
                            return $ putTMVar lock ()
                        _ -> return $ return ()

                let as' = xs ++ ys
                inAction <- readTVar esInAction
                let remaining = Set.union
                        (Set.fromList $ map actionId as')
                        inAction
                writeTVar esActions as'
                modifyTVar esInAction (Set.insert $ actionId action)
                return $ mask $ \restore -> do
                    eres <- try $ restore $ actionDo action ActionContext
                        { acRemaining = remaining
                        }
                    atomically $ do
                        unlock
                        modifyTVar esInAction (Set.delete $ actionId action)
                        modifyTVar esCompleted (+1)
                        case eres of
                            Left err -> modifyTVar esExceptions (err:)
                            Right () ->
                                let dropDep a = a { actionDeps = Set.delete (actionId action) $ actionDeps a }
                                 in modifyTVar esActions $ map dropDep
                    restore loop
