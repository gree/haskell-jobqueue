-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, UndecidableInstances #-}


module Network.JobQueue.Types
  ( JobActionState(..)
  , JobM
  , ActionM
  , ActionT
  , ActionFn
  , ActionEnv(..)
  , Unit(..)
  , RuntimeState(..)
  , Break(..)
  , LogLevel(..)
  , setNextJob
  , setNextJobIfEmpty
  , emptyNextJob
  , addForkJob
  , incrementCommits
  , getCommits
  , runS
  , runAM
  , addAction
  , setResult
  ) where

import Data.Time.Clock

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger
import Control.Exception
import Data.Default (Default, def)

import Network.JobQueue.Class

-------------------------------- Types

data Break = Unhandled SomeException | Failure String | Retriable deriving (Show)

-------------------------------- State in Action

data RuntimeState a = RS
  { rsNextJob :: (Maybe a)
  , rsNextForks :: [(a, Maybe UTCTime)]
  , rsCommits :: Int
  } deriving (Show)

instance (Unit a) => Default (RuntimeState a) where
  def = RS Nothing [] 0

setNextJob :: (Unit a) => a -> (RuntimeState a) -> (RuntimeState a)
setNextJob x next@(RS _ _ _) = next { rsNextJob = Just x }

setNextJobIfEmpty :: (Unit a) => a -> (RuntimeState a) -> (RuntimeState a)
setNextJobIfEmpty x next@(RS mju _ _) = maybe (next { rsNextJob = Just x }) (const next) mju

emptyNextJob :: (Unit a) => (RuntimeState a) -> (RuntimeState a)
emptyNextJob next@(RS _ _ _) = next { rsNextJob = Nothing }

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

newtype ActionT e a m b = ActionT
  { runAM :: ExceptT Break (ReaderT (ActionEnv e a) (StateT (Maybe (RuntimeState a)) (LoggingT m))) b
  } deriving ( Applicative, Functor, Monad, MonadIO, MonadLogger, MonadError Break
             , MonadReader (ActionEnv e a), MonadState (Maybe (RuntimeState a)), MonadBase base)

type ActionM e a b = ActionT e a IO b

instance MonadTrans (ActionT e a) where
  lift = ActionT . lift . lift . lift . lift

instance MonadTransControl (ActionT e a) where
  newtype StT (ActionT e a) b = StAction { unStAction :: (Either Break b, Maybe (RuntimeState a)) }
  restoreT = ActionT . ExceptT . ReaderT . const . StateT . const
           . LoggingT . const . liftM unStAction
  liftWith f = ActionT . ExceptT . ReaderT $ \r -> StateT $ \s -> LoggingT $ \l ->
    liftM (\x -> (Right x, s))
          (f $ \t -> liftM StAction (runLoggingT (runStateT (runReaderT (runExceptT (runAM t)) r) s) l))

instance MonadBaseControl base m => MonadBaseControl base (ActionT e a m) where
  newtype StM (ActionT e a m) b = StMActionT { unStMActionT :: ComposeSt (ActionT e a) m b }
  liftBaseWith = defaultLiftBaseWith StMActionT
  restoreM = defaultRestoreM unStMActionT

setResult :: (Unit a) => Maybe (RuntimeState a) -> Maybe (RuntimeState a) -> Maybe (RuntimeState a)
setResult result _ = result


--------------------------------
