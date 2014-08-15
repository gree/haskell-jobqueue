-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.JobQueue.Types
  ( JobActionState(..)
  , JobM
  , ActionM
  , ActionFn
  , ActionError(..)
  , ActionEnv(..)
  , Unit(..)
  , RuntimeState(..)
  , Failure(..)
  , JobResult
  , LogLevel(..)
  , setNextJob
  , setNextJobIfEmpty
  , addForkJob
  , incrementCommits
  , getCommits
  , runS
  , runAM
  , addAction
  , setResult
  ) where

import Data.Time.Clock

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger
import Data.Default (Default, def)

import Network.JobQueue.Class

-------------------------------- Types

data RuntimeState a = RS
  { rsNextJob :: (Maybe a)
  , rsNextForks :: [(a, Maybe UTCTime)]
  , rsCommits :: Int
  }

data Failure = Failure | Retriable

-------------------------------- JobResult

type JobResult a = Either Failure (RuntimeState a)

instance (Unit a) => Default (JobResult a) where
  def = Right $ RS Nothing [] 0

setNextJob :: (Unit a) => a -> (JobResult a) -> (JobResult a)
setNextJob x (Right next@(RS _ _ _)) = Right next { rsNextJob = Just x }
setNextJob _ jr@(Left _) = jr

setNextJobIfEmpty :: (Unit a) => a -> (JobResult a) -> (JobResult a)
setNextJobIfEmpty x jr@(Right next@(RS mju _ _)) = maybe (Right next { rsNextJob = Just x }) (const jr) mju
setNextJobIfEmpty _ jr@(Left _) = jr

addForkJob :: (Unit a) => (a, Maybe UTCTime) -> (JobResult a) -> (JobResult a)
addForkJob (x, mt) (Right next@(RS _ xs _)) = Right next { rsNextForks = ((x, mt):xs) }
addForkJob (_, _) jr@(Left _) = jr

incrementCommits :: (Unit a) => (JobResult a) -> (JobResult a)
incrementCommits (Right next@(RS _ _ cnt)) = Right next { rsCommits = cnt + 1 }
incrementCommits jr@(Left _) = jr

getCommits :: (Unit a) => (JobResult a) -> Int
getCommits (Right (RS _ _ cnt)) = cnt
getCommits (Left _) = 0

-------------------------------- JobActionState

type ActionFn e a = e -> a -> IO (Maybe (JobResult a))

data JobActionState e a = JobActionState { jobActions :: [ActionFn e a] }

addAction :: (Env e, Unit a) => ActionFn e a -> JobActionState e a -> JobActionState e a
addAction action s@(JobActionState { jobActions = actions }) = s { jobActions = action:actions }

instance Default (JobActionState e a) where
  def = JobActionState []

-------------------------------- ActionM

newtype (Env e, Unit a) => JobM e a b = JobM { runS :: StateT (JobActionState e a) IO b }
  deriving (Monad, MonadIO, Functor, MonadState (JobActionState e a))

data ActionError = AbortError String
  deriving (Show)

data ActionEnv e a = ActionEnv
  { getJobEnv :: e
  , getJobUnit :: a
  }

type JobResultState a = Maybe (JobResult a)

newtype ActionM e a b = ActionM { runAM :: LoggingT (ExceptT ActionError (ReaderT (ActionEnv e a) (StateT (JobResultState a) IO))) b }
  deriving ( Monad, MonadIO, MonadLogger, Functor
           , MonadReader (ActionEnv e a), MonadState (JobResultState a), MonadError ActionError)

setResult :: (Unit a) => Maybe (JobResult a) -> JobResultState a -> JobResultState a
setResult result _ = result

--------------------------------
