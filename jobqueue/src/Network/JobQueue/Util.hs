-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Util
  ( waitForAllJobs
  , waitUntilMatch
  ) where

import Data.Maybe
import Control.Concurrent
import Network.JobQueue.Types
import Network.JobQueue.Class
import Network.JobQueue.Job
import Network.JobQueue.JobQueue
import Network.JobQueue.JobQueue.Internal
import Text.Regex.Posix

waitForAllJobs :: (Env e, Unit a) => JobQueue e a -> Int -> ((Maybe (Job a)) -> Int -> IO ()) -> IO (Maybe (Job a))
waitForAllJobs jq timeoutCount = waitWhile jq (\mjob count -> isJust mjob && count < timeoutCount)

waitUntilMatch :: (Env e, Unit a) => JobQueue e a -> String -> Int -> ((Maybe (Job a)) -> Int -> IO ()) -> IO (Maybe (Job a))
waitUntilMatch jq pattern timeoutCount = waitWhile jq (\mjob count -> not (show mjob =~ pattern) && count < timeoutCount)

waitWhile :: (Env e, Unit a)
             => JobQueue e a
             -> (Maybe (Job a) -> Int -> Bool)
             -> (Maybe (Job a) -> Int -> IO ())
             -> IO (Maybe (Job a))
waitWhile jq cond reportAct = loop 0
  where
    loop count = do
      mjob <- liftIO $ peekJob jq
      reportAct mjob count
      if cond mjob count
        then do
          mjob' <- innerloop jq mjob cond
          if mjob' == mjob then return mjob else loop (count + 1)
        else do
          return mjob
    
    innerloop :: (Env e, Unit a) => JobQueue e a -> Maybe (Job a) -> (Maybe (Job a) -> Int -> Bool) -> IO (Maybe (Job a))
    innerloop jq mjob0 cond = loop 0
      where
        loop tickCount = do
          threadDelay 250000
          mjob <- liftIO $ peekJob jq
          if mjob0 == mjob && cond mjob tickCount then loop (tickCount + 1) else return mjob
