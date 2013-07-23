{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.JobQueue.Types (
    JobActionState(..)
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
import Data.Default (Default, def)

import Network.JobQueue.JobEnv
import Network.JobQueue.Util.Desc

--------------------------------

class (Read a, Show a, Desc a) => Unit a where
  getPriority :: a -> Int
  getRecovery :: a -> a

--------------------------------

data Alert = Critical | Error | Warning | Notice | Info deriving (Show)

data Next a = Next {
    nextJob :: (Maybe a)
  , nextForks :: [(a, Maybe UTCTime)]
  }

data Failure = Failure Alert String

type JobResult a = Either Failure (Next a)

instance (Unit a) => Default (JobResult a) where
  def = Right $ Next Nothing []

setNextJob :: (Unit a) => a -> (JobResult a) -> (JobResult a)
setNextJob x (Right next@(Next _ju _xs)) = Right next { nextJob = Just x }
setNextJob _ jr@(Left _) = jr

addForkJob :: (Unit a) => (a, Maybe UTCTime) -> (JobResult a) -> (JobResult a)
addForkJob (x, mt) (Right next@(Next _ju xs)) = Right next { nextForks = ((x, mt):xs) }
addForkJob (_, _) jr@(Left _) = jr

--------------------------------

type ActionFn a = JobEnv -> a -> IO (Maybe (JobResult a))

data JobActionState a = JobActionState { jobActions :: [ActionFn a] }

addAction :: (Unit a) => ActionFn a -> JobActionState a -> JobActionState a
addAction action s@(JobActionState { jobActions = actions }) = s { jobActions = action:actions }

instance Default (JobActionState a) where
  def = JobActionState []

--------------------------------

newtype (Unit a) => JobM a b = JobM { runS :: StateT (JobActionState a) IO b }
  deriving (Monad, MonadIO, Functor, MonadState (JobActionState a))

data ActionError = ActionError Alert String
  deriving (Show)

instance Error ActionError where
  strMsg = ActionError Warning

data ActionEnv a = ActionEnv {
    getJobEnv :: JobEnv
  , getJobUnit :: a
  }

type JobResultState a = Maybe (JobResult a)

newtype ActionM a b = ActionM { runAM :: ErrorT ActionError (ReaderT (ActionEnv a) (StateT (JobResultState a) IO)) b }
  deriving ( Monad, MonadIO, Functor
           , MonadReader (ActionEnv a), MonadState (JobResultState a), MonadError ActionError)

setResult :: (Unit a) => Maybe (JobResult a) -> JobResultState a -> JobResultState a
setResult result _ = result

