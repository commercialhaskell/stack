{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards    #-}

-- Concurrent execution with dependencies. Types currently hard-coded for needs
-- of stack, but could be generalized easily.
module Control.Concurrent.Execute
    ( ActionType (..)
    , ActionId (..)
    , ActionContext (..)
    , Action (..)
    , Concurrency (..)
    , runActions
    ) where

import           Control.Concurrent.STM ( check )
import           Stack.Prelude
import           Data.List ( sortBy )
import qualified Data.Set as Set

-- | Type representing exceptions thrown by functions exported by the
-- "Control.Concurrent.Execute" module.
data ExecuteException
  = InconsistentDependenciesBug
  deriving (Show, Typeable)

instance Exception ExecuteException where
  displayException InconsistentDependenciesBug = bugReport "[S-2816]"
    "Inconsistent dependencies were discovered while executing your build \
    \plan."

-- | Type representing types of Stack build actions.
data ActionType
  = ATBuild
    -- ^ Action for building a package's library and executables. If
    -- 'taskAllInOne' is 'True', then this will also build benchmarks and tests.
    -- It is 'False' when the library's benchmarks or test-suites have cyclic
    -- dependencies.
  | ATBuildFinal
    -- ^ Task for building the package's benchmarks and test-suites. Requires
    -- that the library was already built.
  | ATRunTests
    -- ^ Task for running the package's test-suites.
  | ATRunBenchmarks
    -- ^ Task for running the package's benchmarks.
  deriving (Show, Eq, Ord)

-- | Types representing the unique ids of Stack build actions.
data ActionId
  = ActionId !PackageIdentifier !ActionType
  deriving (Eq, Ord, Show)

-- | Type representing Stack build actions.
data Action = Action
  { actionId :: !ActionId
    -- ^ The action's unique id.
  , actionDeps :: !(Set ActionId)
    -- ^ Actions on which this action depends.
  , actionDo :: !(ActionContext -> IO ())
    -- ^ The action's 'IO' action, given a context.
  , actionConcurrency :: !Concurrency
    -- ^ Whether this action may be run concurrently with others.
  }

-- | Type representing permissions for actions to be run concurrently with
-- others.
data Concurrency
  = ConcurrencyAllowed
  | ConcurrencyDisallowed
  deriving Eq

data ActionContext = ActionContext
  { acRemaining :: !(Set ActionId)
    -- ^ Does not include the current action.
  , acDownstream :: [Action]
    -- ^ Actions which depend on the current action.
  , acConcurrency :: !Concurrency
    -- ^ Whether this action may be run concurrently with others.
  }

data ExecuteState = ExecuteState
  { esActions    :: TVar [Action]
  , esExceptions :: TVar [SomeException]
  , esInAction   :: TVar (Set ActionId)
  , esCompleted  :: TVar Int
  , esKeepGoing  :: Bool
  }

runActions :: 
     Int -- ^ threads
  -> Bool -- ^ keep going after one task has failed
  -> [Action]
  -> (TVar Int -> TVar (Set ActionId) -> IO ()) -- ^ progress updated
  -> IO [SomeException]
runActions threads keepGoing actions withProgress = do
  es <- ExecuteState
    <$> newTVarIO (sortActions actions) -- esActions
    <*> newTVarIO [] -- esExceptions
    <*> newTVarIO Set.empty -- esInAction
    <*> newTVarIO 0 -- esCompleted
    <*> pure keepGoing -- esKeepGoing
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
runActions' ExecuteState {..} = loop
 where
  loop :: IO ()
  loop = join $ atomically $ breakOnErrs $ withActions processActions

  breakOnErrs :: STM (IO ()) -> STM (IO ())
  breakOnErrs inner = do
    errs <- readTVar esExceptions
    if null errs || esKeepGoing
      then inner
      else doNothing

  withActions :: ([Action] -> STM (IO ())) -> STM (IO ())
  withActions inner = do
    actions <- readTVar esActions
    if null actions
      then doNothing
      else inner actions

  processActions :: [Action] -> STM (IO ())
  processActions actions = do
    inAction <- readTVar esInAction
    case break (Set.null . actionDeps) actions of
      (_, []) -> do
        check (Set.null inAction)
        unless esKeepGoing $
          modifyTVar esExceptions (toException InconsistentDependenciesBug:)
        doNothing
      (xs, action:ys) -> processAction inAction (xs ++ ys) action

  processAction :: Set ActionId -> [Action] -> Action -> STM (IO ())
  processAction inAction otherActions action = do
    let concurrency = actionConcurrency action
    unless (concurrency == ConcurrencyAllowed) $
      check (Set.null inAction)
    let action' = actionId action
        otherActions' = Set.fromList $ map actionId otherActions
        remaining = Set.union otherActions' inAction
        actionContext = ActionContext
          { acRemaining = remaining
          , acDownstream = downstreamActions action' otherActions
          , acConcurrency = concurrency
          }
    writeTVar esActions otherActions
    modifyTVar esInAction (Set.insert action')
    pure $ do
      mask $ \restore -> do
        eres <- try $ restore $ actionDo action actionContext
        atomically $ do
          modifyTVar esInAction (Set.delete action')
          modifyTVar esCompleted (+1)
          case eres of
            Left err -> modifyTVar esExceptions (err:)
            Right () -> modifyTVar esActions $ map (dropDep action')
      loop

  -- | Filter a list of actions to include only those that depend on the given
  -- action.
  downstreamActions :: ActionId -> [Action] -> [Action]
  downstreamActions aid = filter (\a -> aid `Set.member` actionDeps a)
  
  -- | Given two actions (the first specified by its id) yield an action
  -- equivalent to the second but excluding any dependency on the first action.
  dropDep :: ActionId -> Action -> Action
  dropDep action' action =
    action { actionDeps = Set.delete action' $ actionDeps action }
  
  -- | @IO ()@ lifted into 'STM'.
  doNothing :: STM (IO ())
  doNothing = pure $ pure ()
