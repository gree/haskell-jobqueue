-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Job (
    Job(jobState, jobUnit, jobCTime, jobOnTime, jobId, jobGroup, jobPriority, StopTheWorld)
  , JobState(..)
  , buildActionState
  , process
  , createJob
  , createOnTimeJob
  , printJob
  , module Network.JobQueue.Types
  , module Network.JobQueue.Action
  ) where

import Control.Monad.State hiding (state)

import Network.JobQueue.Class
import Network.JobQueue.AuxClass
import Network.JobQueue.Types
import Network.JobQueue.Action
import Network.JobQueue.Job.Internal

--------------------------------

buildActionState :: (Env e, Unit a) => JobM e a () -> IO (JobActionState e a)
buildActionState jobs = execStateT (runS jobs) (JobActionState [])

{- | Declare a function which accepts a unit and execute the action of it if possible.
-}
process :: (Aux e, Env e, Unit a) => (a -> ActionM e a ()) -> JobM e a ()
process action = modify $ addAction $ eval action

eval :: (Aux e, Env e, Unit a) => (a -> ActionM e a ()) -> ActionFn e a
eval action env ju = runAction env ju (action ju)

