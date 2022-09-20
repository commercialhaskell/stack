{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

-- Concurrent execution with dependencies. Types currently hard-coded for needs
-- of stack, but could be generalized easily.
module Control.Concurrent.Execute
    ( ActionType (..)
    , ActionId (..)
    , ActionContext (..)
    , Action (..)
    , Concurrency(..)
    , runActions
    ) where

import           Control.Concurrent.STM   (retry)
import           Stack.Prelude
import           Data.List (sortBy)
import qualified Data.Set                 as Set

data ActionType
    = ATBuild
      -- ^ Action for building a package's library and executables. If
      -- 'taskAllInOne' is 'True', then this will also build benchmarks
      -- and tests. It is 'False' when then library's benchmarks or
      -- test-suites have cyclic dependencies.
    | ATBuildFinal
      -- ^ Task for building the package's benchmarks and test-suites.
      -- Requires that the library was already built.
    | ATRunTests
      -- ^ Task for running the package's test-suites.
    | ATRunBenchmarks
      -- ^ Task for running the package's benchmarks.
    deriving (Show, Eq, Ord)
data ActionId = ActionId !PackageIdentifier !ActionType
    deriving (Show, Eq, Ord)
data Action = Action
    { actionId :: !ActionId
    , actionDeps :: !(Set ActionId)
    , actionDo :: !(ActionContext -> IO ())
    , actionConcurrency :: !Concurrency
    }

data Concurrency = ConcurrencyAllowed | ConcurrencyDisallowed
    deriving (Eq)

data ActionContext = ActionContext
    { acRemaining :: !(Set ActionId)
    -- ^ Does not include the current action
    , acDownstream :: [Action]
    -- ^ Actions which depend on the current action
    , acConcurrency :: !Concurrency
    -- ^ Whether this action may be run concurrently with others
    }

data ExecuteState = ExecuteState
    { esActions    :: TVar [Action]
    , esExceptions :: TVar [SomeException]
    , esInAction   :: TVar (Set ActionId)
    , esCompleted  :: TVar Int
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
           -> [Action]
           -> (TVar Int -> TVar (Set ActionId) -> IO ()) -- ^ progress updated
           -> IO [SomeException]
runActions threads keepGoing actions0 withProgress = do
    es <- ExecuteState
        <$> newTVarIO (sortActions actions0)
        <*> newTVarIO []
        <*> newTVarIO Set.empty
        <*> newTVarIO 0
        <*> pure keepGoing
    _ <- async $ withProgress (esCompleted es) (esInAction es)
    if threads <= 1
        then runActions' es
        else replicateConcurrently_ threads $ runActions' es
    readTVarIO $ esExceptions es

-- | Sort actions such that those that can't be run concurrently are at
-- the end.
sortActions :: [Action] -> [Action]
sortActions = sortBy (compareConcurrency `on` actionConcurrency)
  where
    -- NOTE: Could derive Ord. However, I like to make this explicit so
    -- that changes to the datatype must consider how it's affecting
    -- this.
    compareConcurrency ConcurrencyAllowed ConcurrencyDisallowed = LT
    compareConcurrency ConcurrencyDisallowed ConcurrencyAllowed = GT
    compareConcurrency _ _ = EQ

runActions' :: ExecuteState -> IO ()
runActions' ExecuteState {..} =
    loop
  where
    breakOnErrs inner = do
        errs <- readTVar esExceptions
        if null errs || esKeepGoing
            then inner
            else pure $ pure ()
    withActions inner = do
        as <- readTVar esActions
        if null as
            then pure $ pure ()
            else inner as
    loop = join $ atomically $ breakOnErrs $ withActions $ \as ->
        case break (Set.null . actionDeps) as of
            (_, []) -> do
                inAction <- readTVar esInAction
                if Set.null inAction
                    then do
                        unless esKeepGoing $
                            modifyTVar esExceptions (toException InconsistentDependencies:)
                        pure $ pure ()
                    else retry
            (xs, action:ys) -> do
                inAction <- readTVar esInAction
                case actionConcurrency action of
                  ConcurrencyAllowed -> pure ()
                  ConcurrencyDisallowed -> unless (Set.null inAction) retry
                let as' = xs ++ ys
                    remaining = Set.union
                        (Set.fromList $ map actionId as')
                        inAction
                writeTVar esActions as'
                modifyTVar esInAction (Set.insert $ actionId action)
                pure $ mask $ \restore -> do
                    eres <- try $ restore $ actionDo action ActionContext
                        { acRemaining = remaining
                        , acDownstream = downstreamActions (actionId action) as'
                        , acConcurrency = actionConcurrency action
                        }
                    atomically $ do
                        modifyTVar esInAction (Set.delete $ actionId action)
                        modifyTVar esCompleted (+1)
                        case eres of
                            Left err -> modifyTVar esExceptions (err:)
                            Right () ->
                                let dropDep a = a { actionDeps = Set.delete (actionId action) $ actionDeps a }
                                 in modifyTVar esActions $ map dropDep
                    restore loop

downstreamActions :: ActionId -> [Action] -> [Action]
downstreamActions aid = filter (\a -> aid `Set.member` actionDeps a)
