-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Util where

import Control.Concurrent
import Network.JobQueue.Types
import Network.JobQueue.Class
import Network.JobQueue.Job
import Network.JobQueue.JobQueue
import Network.JobQueue.JobQueue.Internal

waitForNextJob :: (Env e, Unit a) => JobQueue e a -> Job a -> Int -> IO (Maybe (Job a))
waitForNextJob jq job0 timeoutCount = loop 0
  where
    loop tickCount = do
      threadDelay 250000
      mjob <- liftIO $ peekJob jq
      case mjob of
        Nothing -> return Nothing
        Just job | job0 == job && tickCount < timeoutCount -> loop (tickCount + 1)
                 | otherwise                               -> return mjob

waitForAllJobs :: (Env e, Unit a) => JobQueue e a -> Int -> ((Maybe (Job a)) -> Int -> IO ()) -> IO (Maybe (Job a))
waitForAllJobs jq timeoutCount reportAct = loop 0
  where
    loop count = do
      mjob <- liftIO $ peekJob jq
      case mjob of
        Nothing -> reportAct Nothing count >> return Nothing
        Just job | count < timeoutCount -> do
                     reportAct mjob count
                     mjob' <- waitForNextJob jq job timeoutCount
                     if mjob' == Just job then return mjob else loop (count + 1)
                 | otherwise -> reportAct mjob count >> return mjob

