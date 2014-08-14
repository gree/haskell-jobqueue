-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE ScopedTypeVariables #-}

module Network.JobQueue.JobQueue (
    JobQueue
  , Session
  , openSession
  , newSession
  , closeSession
  , openJobQueue
  , closeJobQueue
  , countJobQueue
  , resumeJobQueue
  , suspendJobQueue
  , executeJob
  , scheduleJob
  , deleteJob
  , clearJobs
  , peekJob
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

import Network.JobQueue.Class
import Network.JobQueue.AuxClass
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
closeSession (Session isOwner _locator backend) = when isOwner $ bClose backend

{- | Open a job queue with a session.
-}
openJobQueue :: (Env e, Unit a)
                => Session     -- ^ a session handler
                -> String      -- ^ a queue name
                -> JobM e a () -- ^ a state machine definition
                -> IO (JobQueue e a)
openJobQueue (Session _isOwner _locator _backend@(Backend { bOpenQueue = oq }))
             name
             jobm = do
  JobQueue <$> oq name <*> buildActionState jobm

{- | Close a job queue.
-}
closeJobQueue :: (Env e, Unit a) => JobQueue e a -> IO ()
closeJobQueue JobQueue { jqBackendQueue = bq } = closeQueue bq

{- | Count the number of jobs queued in a job queue.
-}
countJobQueue :: (Env e, Unit a) => JobQueue e a -> IO (Int)
countJobQueue JobQueue { jqBackendQueue = bq } = countQueue bq

{- | Resume a job queue
-}
resumeJobQueue :: (Env e, Unit a) => JobQueue e a -> IO (Bool)
resumeJobQueue jobqueue = do
  r <- peekJob' jobqueue
  case r of
    Just (job, nodeName, idName, _version) -> case actionForJob job idName of
      Execute StopTheWorld -> resume jobqueue nodeName
      _ -> return True
    _ -> return True
  where
    resume JobQueue { jqBackendQueue = bq } key = deleteQueue bq key

{- | Suspend a job queue
-}
suspendJobQueue :: forall e. forall a. (Env e, Unit a) => JobQueue e a -> IO (Bool)
suspendJobQueue jobqueue = do
  r <- peekJob' jobqueue
  case r of
    Just (job, _nodeName, idName, _version) -> case actionForJob job idName of
      Execute StopTheWorld -> return False
      _ -> suspend jobqueue >> return True
    _ -> suspend jobqueue >> return True
  where
    suspend JobQueue { jqBackendQueue = bq } = writeQueue bq (pack (StopTheWorld :: Job a)) (-1)

{- | Execute an action of the head job in a job queue.
-}
executeJob :: (Aux e, Env e, Unit a) => JobQueue e a -> e -> IO ()
executeJob jobqueue env = do
  r <- peekJob' jobqueue
  case r of
    Just (job, nodeName, idName, version) -> case actionForJob job idName of
      Execute StopTheWorld -> do
        threadDelay 1000000
        return ()
      Execute job' -> do
        isUpdated <- updateJob jobqueue nodeName job' version
        when (isUpdated && jobState job == Runnable && jobState job' == Running) $ do
          executeJob' jobqueue env nodeName job' version >>= afterExecuteJob jobqueue env nodeName job' version
          auxHandleAfterExecute env job'
      Delete -> do
        void $ deleteJob jobqueue nodeName
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
             -> IO Bool
deleteJob JobQueue { jqBackendQueue = bq } nodeName = do
  deleteQueue bq nodeName `catch` \e -> case e of
    NotFound _ -> return True
    _ -> throwIO e

{- | Clear all jobs from a job queue.
-}
clearJobs :: (Unit a)
             => JobQueue e a -- ^ a job queue
             -> IO [(String, Job a)]
clearJobs JobQueue { jqBackendQueue = bq } = loop []
  where
    loop dequeued = do
      obj <- readQueue bq
      case obj of
        Nothing -> return dequeued
        Just (bs, nodeName) -> case (fmap fst . listToMaybe . reads) $ BS.unpack bs of
          Nothing -> return dequeued
          Just job -> loop ((nodeName, job):dequeued)

{- | Peek a job form a job queue.
-}
peekJob :: (Unit a)
           => JobQueue e a -- ^ a job queue
           -> IO (Maybe (Job a))
peekJob jobqueue = do
  mjob <- peekJob' jobqueue
  return $ case mjob of
    Just (job, _nodeName, _idName, _version) -> Just job
    Nothing -> Nothing

---------------------------------------------------------------- PRIVATE

  
  
