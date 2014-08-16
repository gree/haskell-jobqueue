-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GADTs #-}

module Network.JobQueue.JobQueue.Internal where

import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Data.Time.Clock
import Control.Monad
import Data.Maybe

import Network.JobQueue.Class
import Network.JobQueue.AuxClass
import Network.JobQueue.Types
import Network.JobQueue.Action
import Network.JobQueue.Job
import Network.JobQueue.Backend.Class
import Network.JobQueue.Backend.Types


data JobQueue e a where
  JobQueue :: (BackendQueue q) => {
    jqBackendQueue :: q
  , jqActionState :: JobActionState e a
  } -> JobQueue e a

data ActionForJob a = (Unit a) => Execute (Job a) | Delete | Skip


actionForJob :: Unit a => Job a -> String -> ActionForJob a
actionForJob job idName = do
  case job of
    StopTheWorld -> Execute job
    _ -> case jobState job of
           Initialized -> case (fmap fst . listToMaybe . reads) idName of
             Just ident -> Execute $ job { jobState = Runnable, jobId = ident }
             Nothing -> Execute $ job { jobState = Runnable, jobId = (-1) }
           Runnable -> Execute $ job { jobState = Running }
           Running -> Skip
           Aborted -> Skip
           Finished -> Delete

peekJob' :: (Unit a) => JobQueue e a -> IO (Maybe (Job a, String, String, Int))
peekJob' JobQueue { jqBackendQueue = bq } = do
  obj <- peekQueue bq
  case obj of
    Nothing -> return (Nothing)
    Just (value, nodeName, idName, version) -> do
      case (fmap fst . listToMaybe . reads) $ BS.unpack value of
        Nothing -> return (Nothing)
        Just job -> return (Just (job, nodeName, idName, version))

executeJob' :: (Aux e, Env e, Unit a) => JobQueue e a -> e -> String -> Job a -> Int -> IO (Either Break (Maybe (RuntimeState a)))
executeJob' jqueue@JobQueue { jqBackendQueue = bq, jqActionState = actionState } env nodeName currentJob version = do
  currentTime <- getCurrentTime
  if jobOnTime currentJob < currentTime
    then do
      runActionState actionState env (jobUnit currentJob)
    else do
      r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
      when r $ void $ writeQueue bq (pack $ currentJob { jobState = Runnable } ) (jobPriority currentJob)
      return $ Right Nothing

afterExecuteJob :: (Aux e, Env e, Unit a) => JobQueue e a -> e -> String -> Job a -> Int -> Either Break (Maybe (RuntimeState a)) -> IO ()
afterExecuteJob jqueue env nodeName currentJob version mResult = case mResult of
  Right (Just (RS mNextJu forks _)) -> do
    case mNextJu of
      Just nextJu -> do
        _r <- updateJob jqueue nodeName currentJob { jobState = Runnable, jobUnit = nextJu } (version+1)
        return ()
      Nothing -> do
        _r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
        return ()
    forM_ (reverse forks) $ \(forked, ontime) -> rescheduleJob jqueue ontime forked
  Right (Nothing) -> recover Nothing -- nothing to do anymore
  Left (Failure _msg) -> do
    n <- auxHandleFailure env (Just currentJob)
    recover n
  Left Retriable -> do
    _r <- updateJob jqueue nodeName currentJob { jobState = Runnable } (version+1)
    return ()
  Left (Unhandled _someException) -> do
    _r <- updateJob jqueue nodeName currentJob { jobState = Finished } (version+1)
    return ()
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

pack :: (Unit a) => Job a -> BS.ByteString
pack = BS.pack . show

