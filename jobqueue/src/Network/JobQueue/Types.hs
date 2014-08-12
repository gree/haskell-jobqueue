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
  , Next(..)  
  , Failure(..)
  , JobResult
  , Alert(..)
  , setNextJob
  , setNextJobIfEmpty
  , addForkJob
  , runS
  , runAM
  , addAction
  , setResult
  ) where

import Data.Time.Clock

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger
import Data.Default (Default, def)

import Network.JobQueue.Class

-------------------------------- Types

data Alert = Critical | Error | Warning | Notice | Info deriving (Show)

data Next a = Next
  { nextJob :: (Maybe a)
  , nextForks :: [(a, Maybe UTCTime)]
  }

data Failure = Failure Alert String

-------------------------------- JobResult

type JobResult a = Either Failure (Next a)

instance (Unit a) => Default (JobResult a) where
  def = Right $ Next Nothing []

setNextJob :: (Unit a) => a -> (JobResult a) -> (JobResult a)
setNextJob x (Right next@(Next _ _xs)) = Right next { nextJob = Just x }
setNextJob _ jr@(Left _) = jr

setNextJobIfEmpty :: (Unit a) => a -> (JobResult a) -> (JobResult a)
setNextJobIfEmpty x jr@(Right next@(Next mju _xs)) = maybe (Right next { nextJob = Just x }) (const jr) mju
setNextJobIfEmpty _ jr@(Left _) = jr

addForkJob :: (Unit a) => (a, Maybe UTCTime) -> (JobResult a) -> (JobResult a)
addForkJob (x, mt) (Right next@(Next _ju xs)) = Right next { nextForks = ((x, mt):xs) }
addForkJob (_, _) jr@(Left _) = jr

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

data ActionError = ActionError Alert String
  deriving (Show)

instance Error ActionError where
  strMsg = ActionError Warning

data ActionEnv e a = ActionEnv {
    getJobEnv :: e
  , getJobUnit :: a
  }

type JobResultState a = Maybe (JobResult a)

newtype ActionM e a b = ActionM { runAM :: LoggingT (ErrorT ActionError (ReaderT (ActionEnv e a) (StateT (JobResultState a) IO))) b }
  deriving ( Monad, MonadIO, MonadLogger, Functor
           , MonadReader (ActionEnv e a), MonadState (JobResultState a), MonadError ActionError)

setResult :: (Unit a) => Maybe (JobResult a) -> JobResultState a -> JobResultState a
setResult result _ = result

--------------------------------
