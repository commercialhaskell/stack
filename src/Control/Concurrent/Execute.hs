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
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad            (join)
import           Data.Foldable            (sequenceA_)
import           Data.List                (intercalate)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Typeable            (Typeable)
import           Prelude -- Fix AMP warning
import           Stack.Types

data ActionType
    = ATBuild
    | ATInstall
    | ATWanted
    deriving (Show, Eq, Ord)
data ActionId = ActionId !PackageIdentifier !ActionType
    deriving (Show, Eq, Ord)
data Action = Action
    { actionId   :: !ActionId
    , actionDeps :: !(Set ActionId)
    , actionDo   :: !(ActionContext -> IO ())
    }

data ActionContext = ActionContext
    { acRemaining :: !Int
    -- ^ Does not include the current action
    }
    deriving Show

data ExecuteState = ExecuteState
    { esActions    :: TVar [Action]
    , esExceptions :: TVar [SomeException]
    , esInAction   :: TVar Int
    }

data ExecuteException
    = InconsistentDependencies
    | ExecutionFailure [SomeException]
    deriving Typeable
instance Exception ExecuteException

instance Show ExecuteException where
    show InconsistentDependencies =
        "Inconsistent dependencies were discovered while executing your build plan. This should never happen, please report it as a bug to the stack team."
    show (ExecutionFailure es) = intercalate "\n\n" $ map show es

runActions :: Int -- ^ threads
           -> [Action]
           -> IO ()
runActions threads actions0 = do
    es <- ExecuteState
        <$> newTVarIO actions0
        <*> newTVarIO []
        <*> newTVarIO 0
    if threads <= 1
        then runActions' es
        else runConcurrently $ sequenceA_ $ replicate threads $ Concurrently $ runActions' es
    errs <- readTVarIO $ esExceptions es
    if null errs
        then return ()
        else throwIO $ ExecutionFailure errs

runActions' :: ExecuteState -> IO ()
runActions' ExecuteState {..} =
    loop
  where
    breakOnErrs inner = do
        errs <- readTVar esExceptions
        if null errs
            then inner
            else return $ return ()
    withActions inner = do
        as <- readTVar esActions
        if null as
            then return $ return ()
            else inner as
    loop = join $ atomically $ breakOnErrs $ withActions $ \as -> do
        case break (Set.null . actionDeps) as of
            (_, []) -> do
                inAction <- readTVar esInAction
                if inAction == 0
                    then do
                        modifyTVar esExceptions (toException InconsistentDependencies:)
                        return $ return ()
                    else retry
            (xs, action:ys) -> do
                let as' = xs ++ ys
                inAction <- readTVar esInAction
                let remaining = length as' + inAction
                writeTVar esActions as'
                modifyTVar esInAction (+ 1)
                return $ mask $ \restore -> do
                    eres <- try $ restore $ actionDo action ActionContext
                        { acRemaining = remaining
                        }
                    case eres of
                        Left err -> atomically $ do
                            modifyTVar esExceptions (err:)
                            modifyTVar esInAction (subtract 1)
                        Right () -> do
                            atomically $ do
                                modifyTVar esInAction (subtract 1)
                                let dropDep a = a { actionDeps = Set.delete (actionId action) $ actionDeps a }
                                modifyTVar esActions $ map dropDep
                            restore loop
