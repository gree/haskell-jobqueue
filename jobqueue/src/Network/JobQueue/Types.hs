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
  , ActionEnv(..)
  , Unit(..)
  , RuntimeState(..)
  , Break(..)
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

data Break = Failure | Retriable

-------------------------------- State in Action

instance (Unit a) => Default (RuntimeState a) where
  def = RS Nothing [] 0

setNextJob :: (Unit a) => a -> (RuntimeState a) -> (RuntimeState a)
setNextJob x next@(RS _ _ _) = next { rsNextJob = Just x }

setNextJobIfEmpty :: (Unit a) => a -> (RuntimeState a) -> (RuntimeState a)
setNextJobIfEmpty x next@(RS mju _ _) = maybe (next { rsNextJob = Just x }) (const next) mju

addForkJob :: (Unit a) => (a, Maybe UTCTime) -> (RuntimeState a) -> (RuntimeState a)
addForkJob (x, mt) next@(RS _ xs _) = next { rsNextForks = ((x, mt):xs) }

incrementCommits :: (Unit a) => (RuntimeState a) -> (RuntimeState a)
incrementCommits next@(RS _ _ cnt) = next { rsCommits = cnt + 1 }

getCommits :: (Unit a) => (RuntimeState a) -> Int
getCommits (RS _ _ cnt) = cnt

-------------------------------- JobActionState

type ActionFn e a = e -> a -> IO (Maybe (Either Break (RuntimeState a)))

data JobActionState e a = JobActionState { jobActions :: [ActionFn e a] }

addAction :: (Env e, Unit a) => ActionFn e a -> JobActionState e a -> JobActionState e a
addAction action s@(JobActionState { jobActions = actions }) = s { jobActions = action:actions }

instance Default (JobActionState e a) where
  def = JobActionState []

-------------------------------- ActionM

newtype (Env e, Unit a) => JobM e a b = JobM { runS :: StateT (JobActionState e a) IO b }
  deriving (Monad, MonadIO, Functor, MonadState (JobActionState e a))

data ActionEnv e a = ActionEnv
  { getJobEnv :: e
  , getJobUnit :: a
  }

newtype ActionM e a b = ActionM
  { runAM :: LoggingT (ExceptT Break (ReaderT (ActionEnv e a) (StateT (Maybe (RuntimeState a)) IO))) b
  } deriving ( Monad, MonadIO, MonadLogger, Functor, MonadError Break
           , MonadReader (ActionEnv e a), MonadState (Maybe (RuntimeState a)))

setResult :: (Unit a) => Maybe (RuntimeState a) -> Maybe (RuntimeState a) -> Maybe (RuntimeState a)
setResult result _ = result

--------------------------------
