
{- |
Module: Network.JobQueue
Maintainer: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

Haskell JobQueue is a library used for building a job scheduler with priority queues.
The state of jobs is stored in a backend database such as Apache Zookeeper or other 
highly reliable mesage queue systems.

* Job
Jobs are described as state machines and each state only do one thing especially for
modifying operations. This prevents the job from resulting in a failure state which
is not able to be handled by the state machine.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Network.JobQueue (
    buildJobQueue
  , runJobQueue
  , JobQueue
  , Job(..)
  , Unit(..)
  , ActionM
  , JobM
  , FailureHandleFn
  , AfterExecuteHandleFn
  , openSession
  , closeSession
  , openJobQueue
  , closeJobQueue
  , countJobQueue
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
  , abort
  , getEnv
  , param
  , logMsg
  , commitIO
  , module Network.JobQueue.Class
  ) where

import Prelude hiding (log)
import Control.Exception
import Control.Monad
import Data.Default

import Network.JobQueue.Types
import Network.JobQueue.Class
import Network.JobQueue.Action
import Network.JobQueue.JobQueue
import Network.JobQueue.Job

{- | Build a function that takes an action function (('JobQueue' a -> 'IO' ()) -> IO ()) as its first parameter.

The following code executes jobs as long as the queue is not empty.

>  main' loc name = do
>    let withJobQueue = buildJobQueue loc name $ do
>          process $ \WorldStep -> commitIO (putStrLn "world") >> fin
>          process $ \HelloStep -> commitIO (putStr "hello, ") >> next WorldStep
>    withJobQueue $ loop (initJobEnv loc name [])
>    where
>      loop env jq = do
>        executeJob jq env
>        count <- countJobQueue jq
>        when (count > 0) $ loop env jq

The following code registers a job with initial state.

>  main' loc name = do
>    let withJobQueue = buildJobQueue loc name $ do
>          process $ \WorldStep -> commitIO (putStrLn "world") >> fin
>          process $ \HelloStep -> commitIO (putStr "hello, ") >> next WorldStep
>    withJobQueue $ \jq -> scheduleJob jq HelloStep

-}
buildJobQueue :: (Env e, Unit a) => String -- ^ locator (ex.\"zookeeper:\/\/192.168.0.1\/myapp\")
                 -> String          -- ^ queue name (ex. \"/jobqueue\")
                 -> JobM e a ()     -- ^ job construction function
                 -> ((JobQueue e a -> IO ()) -> IO ())
buildJobQueue loc name jobm = \action -> do
  bracket (openSession loc) (closeSession) $ \session -> do
    jq <- openJobQueue session name def jobm
    action jq
    closeJobQueue jq

{- | Run a job queue while there is at least one job in the queue.
-}
runJobQueue :: (Env e, Unit a)
               => e
               -> String          -- ^ locator (ex.\"zookeeper:\/\/192.168.0.1\/myapp\")
               -> String          -- ^ queue name (ex. \"/jobqueue\")
               -> JobM e a ()     -- ^ job construction function
               -> IO ()
runJobQueue env loc name jobm = buildJobQueue loc name jobm loop
  where
    loop jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop jq

----------------------------------------------------------------------
-- Docs
----------------------------------------------------------------------

