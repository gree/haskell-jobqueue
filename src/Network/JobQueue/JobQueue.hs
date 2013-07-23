
module Network.JobQueue.JobQueue (
    JobQueue
  , FailureHandleFn
  , AfterExecuteHandleFn
  , initJobQueue
  , executeJob
  , scheduleJob
  , deleteJob
  ) where

import Prelude hiding (catch)
import qualified Data.ByteString.Char8 as C
import qualified Zookeeper as Z
import Control.Exception
import Data.Time.Clock
import System.Log.Logger
import Control.Monad
import Data.Maybe

import Network.JobQueue.Util.Desc
import Network.JobQueue.Types
import Network.JobQueue.Action
import Network.JobQueue.Job
import Network.JobQueue.JobEnv
import Network.JobQueue.Backend
import Network.JobQueue.Backend.Zookeeper.ZookeeperQueue

data ErrorAction = Delete | Skip

type FailureHandleFn a = Alert -> String -> String -> Maybe (Job a) -> IO (Maybe (Job a))
type AfterExecuteHandleFn a = Job a -> IO ()

data JobQueue a = JobQueue {
    jqZq :: ZookeeperQueue
  , jqActionState :: JobActionState a
  , jqFailureHandleFn :: FailureHandleFn a
  , jqAfterExecuteFn :: AfterExecuteHandleFn a
  }

initJobQueue :: (Unit a) => Z.ZHandle -> String -> JobActionState a -> FailureHandleFn a -> AfterExecuteHandleFn a -> JobQueue a
initJobQueue zh path a fhFn aeFn = JobQueue {
    jqZq = initZQueue zh path Z.OpenAclUnsafe
  , jqActionState = a
  , jqFailureHandleFn = fhFn
  , jqAfterExecuteFn = aeFn
  }

executeJob :: (Unit a) => JobQueue a -> JobEnv -> IO ()
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

scheduleJob :: (Unit a) => JobQueue a -> a -> IO ()
scheduleJob jqueue ju = do
  job <- createJob Initialized ju
  void $ writeQueue' (jqZq jqueue) (pack job) (getPriority ju)

deleteJob :: (Unit a) => JobQueue a -> String -> IO (Bool)
deleteJob jqueue nodeName = deleteQueue (jqZq jqueue) nodeName

---------------------------------------------------------------- PRIVATE

peekJob :: (Unit a) => JobQueue a -> IO (Maybe (Job a, String, Int))
peekJob jqueue = do
  obj <- peekQueue $ jqZq jqueue
  case obj of
    Nothing -> return (Nothing)
    Just (value, nodeName, version) -> do
      case maybeRead $ C.unpack value of
        Nothing -> return (Nothing)
        Just job -> return (Just (job, nodeName, version))
  where
    maybeRead = fmap fst . listToMaybe . reads

executeJob' :: (Unit a) => JobQueue a -> JobEnv -> String -> Job a -> Int -> IO (Maybe (JobResult a))
executeJob' jqueue jobEnv nodeName currentJob version = do
  currentTime <- getCurrentTime
  if jobOnTime currentJob < currentTime
    then do
      noticeM "jobqueue" (show currentJob)
      runActionState (jqActionState jqueue) jobEnv (jobUnit currentJob) `catch` handleSome
    else do
      r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
      when r $ void $ writeQueue' (jqZq jqueue) (pack $ currentJob { jobState = Runnable } ) (jobPriority currentJob)
      return (Nothing)
  where
    handleSome :: SomeException -> IO (Maybe (JobResult a))
    handleSome e = do
      _r <- (jqFailureHandleFn jqueue) Error (show e) (show e) (Just currentJob)
      return (Nothing)

afterExecuteJob :: (Unit a) => JobQueue a -> String -> Job a -> Int -> Maybe (JobResult a) -> IO ()
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

rescheduleJob :: (Unit a) => JobQueue a -> Maybe UTCTime -> a -> IO ()
rescheduleJob jqueue mOntime ju = do
  job <- case mOntime of
    Just ontime -> createOnTimeJob Initialized ontime ju
    Nothing -> createJob Initialized ju
  void $ writeQueue' (jqZq jqueue) (pack $ job) (getPriority ju)

updateJob :: (Unit a) => JobQueue a -> String -> Job a -> Int -> IO (Bool)
updateJob jqueue nodeName job version = do
  updateQueue (jqZq jqueue) nodeName (pack job) version `catch` handleError
  where
    handleError :: Z.ZooError -> IO (Bool)
    handleError _e = do
      return (False)

pack :: (Unit a) => Job a -> C.ByteString
pack = C.pack . show

  
  
