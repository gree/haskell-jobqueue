
{-# LANGUAGE ScopedTypeVariables #-}

module Network.JobQueue (
    JobQueue
  , Job(..)
  , Unit(..)
  , ActionM
  , JobM
  , FailureHandleFn
  , AfterExecuteHandleFn
  , Desc
  , openSession
  , closeSession
  , openJobQueue
  , closeJobQueue
  , process
  , createJob
  , executeJob
  , scheduleJob
  , deleteJob
  , fin
  , none
  , next
  , fork
  , forkInTime
  , forkOnTime
  , getEnv
  , param
  , abort
  , logMsg
  , result
  , commitIO
  , module Network.JobQueue.JobEnv
  , module Network.JobQueue.JobResult
  , runJobQueue
  , onJobQueue
  ) where

import Prelude hiding (log)
import Control.Exception
import Control.Monad
import Data.Default

import Network.JobQueue.Types
import Network.JobQueue.Class
import Network.JobQueue.Action
import Network.JobQueue.JobQueue
import Network.JobQueue.JobEnv
import Network.JobQueue.Job
import Network.JobQueue.JobResult

runJobQueue :: (Unit a) => String -> String -> JobM a () -> IO ()
runJobQueue loc name jobm = do
  bracket (openSession loc) (closeSession) $ \session -> do
    jq <- openJobQueue session name def jobm
    loop' jq `catch` (\(e :: SomeException) -> print e)
    closeJobQueue jq
  where
    loop' jq = do
      executeJob jq (initJobEnv loc name [])
      count <- countJobQueue jq
      when (count > 0) $ loop' jq

onJobQueue :: (Unit a) => String -> String -> (JobQueue a -> IO ()) -> IO ()
onJobQueue loc name act = do
  bracket (openSession loc) (closeSession) $ \session -> do
    jq <- openJobQueue session name def (return ())
    act jq
    closeJobQueue jq
