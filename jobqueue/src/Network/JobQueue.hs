{- |
  Module      : Network.JobQueue
  Copyright   : (c) Gree, Inc. 2013
  License     : MIT-style
  
  Maintainer  : Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>
  Stability   : experimental
  Portability : portable

Haskell JobQueue is a library used for building a job scheduler with a priority queue.
The state of a job is stored in a backend database such as Apache Zookeeper or other 
highly reliable mesage queue systems.

[@Unit@]

Unit represents each state in an entire state machine. Units are described as value
constructors in Haskell code.
Unit itself is not executable. To execute using job queue system, extra information such
as job identifier, scheduled time is needed. An instance of a unit is wrapped by a 'job'
and stored into the job queue with those information.

The code shown below describes how to define a Unit.

>  data JobUnit = HelloStep | WorldStep deriving (Show, Read)
>  
>  instance Unit JobUnit where

In this case, you define JobUnit type with 2 states, HelloStep and WorldStep.
This is the entire state machine of your job queue system.
You can define nested or child state machines by defining more complex data types as 
long as they are serializable with read and show functions.

For more information, see "Network.JobQueue.Class".

[@Job@]

Each task executed by state machines (such as checking server state or repairing a
cluster) is called a 'job'.

A job is described as a particular state of a state machine. Each state only does one
thing (especially for modifying operations).
This prevents jobs ending in a failure state, which the state machine is unable to handle.

You don't have to know the internal data structure of a job, but need to understand
its when you write action code.

For more information, see "Network.JobQueue.Job".

[@Environment@]

Each unit can contain information used in the action of the state. But in many cases,
there is some information used by almost all states and it is convenient if there is 
some kind of global data set that is accessible from all the state's actions.

For this reason, you can define global data structures called environment.
The enviroment can be retrieved using getEnv function in action monad.

>  env <- getEnv

For more information, see "Network.JobQueue.Class".

[@Action@]

An action is a function that is called with a unit. You can define actions with the 
"process" function.

>    let withJobQueue = buildJobQueue loc name $ do
>          process $ \WorldStep -> commitIO (putStrLn "world") >> fin
>          process $ \HelloStep -> commitIO (putStr "hello, ") >> next WorldStep

In general, an action does the following things:

  * check if the precondition of the state is satisfied or not

  * do the action associated with the state

  * check the postcondition and return the next state.

For more information, see "Network.JobQueue.Action".

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Network.JobQueue (
    buildJobQueue
  , runJobQueue
  , Job(..)
  , JobState(..)
  , Unit(..)
  , ActionM
  , JobM
  , JobActionState
  , Alert(..)
  , process
  , createJob
  , fin
  , none
  , next
  , orNext
  , fork
  , forkInTime
  , forkOnTime
  , abort
  , getEnv
  , param
  , logMsg
  , commitIO
  , module Network.JobQueue.Class
  , module Network.JobQueue.Aux
  , module Network.JobQueue.JobQueue
  ) where

import Prelude hiding (log)
import Control.Exception
import Control.Monad
import Data.Default

import Network.JobQueue.Types
import Network.JobQueue.Class
import Network.JobQueue.Aux
import Network.JobQueue.Action
import Network.JobQueue.JobQueue
import Network.JobQueue.Job

{- | Build a function that takes a function (('JobQueue' a -> 'IO' ()) -> IO ()) as its first parameter.

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
                 -> ((JobQueue e a -> IO ()) -> IO ()) -- ^ job queue executor
buildJobQueue loc name jobm = \action -> do
  bracket (openSession loc) (closeSession) $ \session -> do
    jq <- openJobQueue session name def jobm
    action jq
    closeJobQueue jq

{- | Run a job queue while there is at least one job in the queue.
-}
runJobQueue :: (Aux e, Env e, Unit a)
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

