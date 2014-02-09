-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.JobQueue (
    JobQueue
  , FailureHandleFn
  , AfterExecuteHandleFn
  , Session
  , Settings (..)
  , openSession
  , newSession
  , closeSession
  , openJobQueue
  , closeJobQueue
  , countJobQueue
  , executeJob
  , scheduleJob
  , deleteJob
  ) where

import Control.Applicative
import Control.Monad

import Network.JobQueue.Class
import Network.JobQueue.Types
import Network.JobQueue.Action
import Network.JobQueue.Job
import Network.JobQueue.Backend
import Network.JobQueue.Backend.Class
import Network.JobQueue.Backend.Types

import Network.JobQueue.JobQueue.Internal


{- | A session handler

     A session usually represents a database session to access job queues stored in the backend
     database.
-}
data Session = Session Bool String Backend

{- | Open a queue session with a resource locator
-}
openSession :: String          -- ^ a resource locator
               -> IO (Session) -- ^ a session handler
openSession locator = Session True locator <$> openBackend locator

{- | Create a queue session with a backend handler
-}
newSession :: String                -- ^ a resource locator (dummy)
              -> Backend -> Session -- ^ a session handler
newSession dummyLocator backend = Session False dummyLocator backend

{- | Close a queue session if needed
-}
closeSession :: Session -> IO ()
closeSession (Session isOwner _locator backend@(Backend { bClose = c })) = when isOwner $ c backend

{- | Open a job queue with a session.
-}
openJobQueue :: (Env e, Unit a)
                => Session     -- ^ a session handler
                -> String      -- ^ a queue name
                -> Settings a  -- ^ queue settings
                -> JobM e a () -- ^ a state machine definition
                -> IO (JobQueue e a)
openJobQueue (Session _isOwner _locator _backend@(Backend { bOpenQueue = oq })) name (Settings fhFn aeFn) jobm = do
  JobQueue <$> oq name <*> buildActionState jobm <*> pure fhFn <*> pure aeFn

{- | Close a job queue.
-}
closeJobQueue :: (Env e, Unit a) => JobQueue e a -> IO ()
closeJobQueue JobQueue { jqBackendQueue = bq } = closeQueue bq

{- | Count the number of jobs queued in a job queue.
-}
countJobQueue :: (Env e, Unit a) => JobQueue e a -> IO (Int)
countJobQueue JobQueue { jqBackendQueue = bq } = countQueue bq

{- | Execute an action of the head job in a job queue.
-}
executeJob :: (Env e, Unit a) => JobQueue e a -> e -> IO ()
executeJob jobqueue env = do
  r <- peekJob jobqueue
  case r of
    Just (job, nodeName, idName, version) -> case actionForJob job idName of
      Execute job' -> do
        isUpdated <- updateJob jobqueue nodeName job' version
        when (isUpdated && jobState job == Runnable && jobState job' == Running) $ do
          executeJob' jobqueue env nodeName job' version >>= afterExecuteJob jobqueue nodeName job' version
          (jqAfterExecuteFn jobqueue) job'
      Delete -> do
        _r <- deleteJob jobqueue nodeName
        executeJob jobqueue env
      Skip -> return ()
    Nothing -> return ()

{- | Schedule a job.
-}
scheduleJob :: (Unit a)
               => JobQueue e a -- ^ a job queue
               -> a            -- ^ a unit
               -> IO ()
scheduleJob JobQueue { jqBackendQueue = bq } ju = do
  job <- createJob Initialized ju
  void $ writeQueue bq (pack job) (getPriority ju)

{- | Delete a job from a job queue.
-}
deleteJob :: (Unit a)
             => JobQueue e a -- ^ a job queue
             -> String       -- ^ a job identifier
             -> IO (Bool)
deleteJob JobQueue { jqBackendQueue = bq } nodeName = deleteQueue bq nodeName

---------------------------------------------------------------- PRIVATE

  
  
