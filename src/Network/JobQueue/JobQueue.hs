
{-# LANGUAGE GADTs #-}

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

import Prelude hiding (catch)
import Control.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Zookeeper as Z
import Control.Exception
import Data.Time.Clock
import System.Log.Logger
import Control.Monad
import Data.Maybe
import Data.Default
import System.IO

import Network.JobQueue.Class
import Network.JobQueue.Types
import Network.JobQueue.Action
import Network.JobQueue.Job
import Network.JobQueue.Backend
import Network.JobQueue.Backend.Class
import Network.JobQueue.Backend.Types

data ErrorAction = Delete | Skip

type FailureHandleFn a = Alert -> String -> String -> Maybe (Job a) -> IO (Maybe (Job a))
type AfterExecuteHandleFn a = Job a -> IO ()

{- | Job queue settings
-}
data (Unit a) => Settings a = Settings {
    failureHandleFn :: FailureHandleFn a     -- ^ a function called when an action fails
  , afterExecuteFn :: AfterExecuteHandleFn a -- ^ a function called after an action is executed (for debugging)
  }
                              
instance (Unit a) => Default (Settings a) where
  def = Settings handleFailure handleAfterExecute
    where
      handleFailure :: (Unit a) => FailureHandleFn a
      handleFailure _al _subject msg mjob = do
        hPutStrLn stderr msg
        hFlush stderr
        case mjob of
          Just job -> Just <$> createJob Runnable (getRecovery (jobUnit job))
          Nothing -> return (Nothing)

      handleAfterExecute :: (Unit a) => Job a -> IO ()
      handleAfterExecute _job = return ()

data JobQueue e a where
  JobQueue :: (BackendQueue q) => {
    jqBackendQueue :: q
  , jqActionState :: JobActionState e a
  , jqFailureHandleFn :: FailureHandleFn a
  , jqAfterExecuteFn :: AfterExecuteHandleFn a
  } -> JobQueue e a

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
  obj <- peekJob jobqueue
  case obj of
    Nothing -> return ()
    Just (job, nodeName, version) -> do
      let eitherJob = case jobState job of
            Initialized -> case maybeRead $ drop (length nodeName - idSuffixLen) nodeName of
              Just ident -> Right $ job { jobState = Runnable, jobId = ident }
              Nothing -> Right $ job { jobState = Runnable, jobId = (-1) }
            Runnable -> Right $ job { jobState = Running }
            Running -> Left Skip
            Aborted -> Left Skip
            Finished -> Left Delete
      case eitherJob of
        Right myJob -> do
          isAcquired <- updateJob jobqueue nodeName myJob version
          when (isAcquired && jobState job == Runnable && jobState myJob == Running) $ do
            jr <- executeJob' jobqueue env nodeName myJob version
            afterExecuteJob jobqueue nodeName myJob version jr
            (jqAfterExecuteFn jobqueue) myJob
        Left Delete -> do
          _r <- deleteJob jobqueue nodeName
          executeJob jobqueue env
        Left Skip -> return ()
  where
    maybeRead = fmap fst . listToMaybe . reads
    idSuffixLen = 10

{- | Schedule a job.
-}
scheduleJob :: (Unit a)
               => JobQueue e a -- ^ a job queue
               -> a            -- ^ a unit
               -> IO ()
scheduleJob JobQueue { jqBackendQueue = bq } ju = do
  job <- createJob Initialized ju
  void $ writeQueue' bq (pack job) (getPriority ju)

{- | Delete a job from a job queue.
-}
deleteJob :: (Unit a)
             => JobQueue e a -- ^ a job queue
             -> String       -- ^ a job identifier
             -> IO (Bool)
deleteJob JobQueue { jqBackendQueue = bq } nodeName = deleteQueue bq nodeName

---------------------------------------------------------------- PRIVATE

peekJob :: (Unit a) => JobQueue e a -> IO (Maybe (Job a, String, Int))
peekJob JobQueue { jqBackendQueue = bq } = do
  obj <- peekQueue bq
  case obj of
    Nothing -> return (Nothing)
    Just (value, nodeName, version) -> do
      case maybeRead $ C.unpack value of
        Nothing -> return (Nothing)
        Just job -> return (Just (job, nodeName, version))
  where
    maybeRead = fmap fst . listToMaybe . reads

executeJob' :: (Env e, Unit a) => JobQueue e a -> e -> String -> Job a -> Int -> IO (Maybe (JobResult a))
executeJob' jqueue@JobQueue { jqBackendQueue = bq, jqActionState = actionState } env nodeName currentJob version = do
  currentTime <- getCurrentTime
  if jobOnTime currentJob < currentTime
    then do
      noticeM "jobqueue" (show currentJob)
      runActionState actionState env (jobUnit currentJob) `catch` handleSome
    else do
      r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
      when r $ void $ writeQueue' bq (pack $ currentJob { jobState = Runnable } ) (jobPriority currentJob)
      return (Nothing)
  where
    handleSome :: SomeException -> IO (Maybe (JobResult a))
    handleSome e = do
      _r <- (jqFailureHandleFn jqueue) Error (show e) (show e) (Just currentJob)
      return (Nothing)

afterExecuteJob :: (Unit a) => JobQueue e a -> String -> Job a -> Int -> Maybe (JobResult a) -> IO ()
afterExecuteJob jqueue nodeName currentJob version mResult = case mResult of
  Just res -> case res of
    Right (Next mNextJu forks) -> do
      case mNextJu of
        Just nextJu -> do
          _r <- updateJob jqueue nodeName currentJob { jobState = Runnable, jobUnit = nextJu } (version+1)
          return ()
        Nothing -> do
          _r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
          return ()
      forM_ (reverse forks) $ \f -> case f of
        (forked, ontime) -> rescheduleJob jqueue ontime forked
    Left (Failure alert msg) -> do
      let subject = "[" ++ shortDesc (jobUnit currentJob) ++ "] " ++ msg
      n <- (jqFailureHandleFn jqueue) alert subject msg (Just currentJob)
      recover n
  Nothing -> do
    -- let subject = "[" ++ shortDesc (jobUnit currentJob) ++ "] aborted"
    -- n <- (failureHandleFn jqueue) Critical subject (desc (jobUnit currentJob))
    -- recover n
    recover Nothing
  where
    recover n = case n of
      Just nextJu -> do
        _r <- updateJob jqueue nodeName nextJu (version+1)
        return ()
      Nothing -> do
        _r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
        return ()

rescheduleJob :: (Unit a) => JobQueue e a -> Maybe UTCTime -> a -> IO ()
rescheduleJob JobQueue { jqBackendQueue = bq } mOntime ju = do
  job <- case mOntime of
    Just ontime -> createOnTimeJob Initialized ontime ju
    Nothing -> createJob Initialized ju
  void $ writeQueue' bq (pack $ job) (getPriority ju)

updateJob :: (Unit a) => JobQueue e a -> String -> Job a -> Int -> IO (Bool)
updateJob JobQueue { jqBackendQueue = bq } nodeName job version = do
  updateQueue bq nodeName (pack job) version `catch` handleError
  where
    handleError :: Z.ZooError -> IO (Bool)
    handleError _e = do
      return (False)

pack :: (Unit a) => Job a -> C.ByteString
pack = C.pack . show

  
  
