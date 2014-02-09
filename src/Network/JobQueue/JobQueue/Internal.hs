-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GADTs #-}

module Network.JobQueue.JobQueue.Internal where

import Control.Applicative
import qualified Data.ByteString.Char8 as C
-- import qualified Zookeeper as Z
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
import Network.JobQueue.Backend.Class
import Network.JobQueue.Backend.Types

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

data ActionForJob a = (Unit a) => Execute (Job a) | Delete | Skip


actionForJob :: Unit a => Job a -> String -> ActionForJob a
actionForJob job idName  = case jobState job of
  Initialized -> case (fmap fst . listToMaybe . reads) idName of
    Just ident -> Execute $ job { jobState = Runnable, jobId = ident }
    Nothing -> Execute $ job { jobState = Runnable, jobId = (-1) }
  Runnable -> Execute $ job { jobState = Running }
  Running -> Skip
  Aborted -> Skip
  Finished -> Delete

peekJob :: (Unit a) => JobQueue e a -> IO (Maybe (Job a, String, String, Int))
peekJob JobQueue { jqBackendQueue = bq } = do
  obj <- peekQueue bq
  case obj of
    Nothing -> return (Nothing)
    Just (value, nodeName, idName, version) -> do
      case (fmap fst . listToMaybe . reads) $ C.unpack value of
        Nothing -> return (Nothing)
        Just job -> return (Just (job, nodeName, idName, version))

executeJob' :: (Env e, Unit a) => JobQueue e a -> e -> String -> Job a -> Int -> IO (Maybe (JobResult a))
executeJob' jqueue@JobQueue { jqBackendQueue = bq, jqActionState = actionState } env nodeName currentJob version = do
  currentTime <- getCurrentTime
  if jobOnTime currentJob < currentTime
    then do
      noticeM "jobqueue" (show currentJob)
      runActionState actionState env (jobUnit currentJob) `catch` handleSome
    else do
      r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
      when r $ void $ writeQueue bq (pack $ currentJob { jobState = Runnable } ) (jobPriority currentJob)
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
  void $ writeQueue bq (pack $ job) (getPriority ju)

updateJob :: (Unit a) => JobQueue e a -> String -> Job a -> Int -> IO (Bool)
updateJob JobQueue { jqBackendQueue = bq } nodeName job version = do
  updateQueue bq nodeName (pack job) version `catch` handleError
  where
    handleError :: BackendError -> IO (Bool)
    handleError _ = return (False)

pack :: (Unit a) => Job a -> C.ByteString
pack = C.pack . show

