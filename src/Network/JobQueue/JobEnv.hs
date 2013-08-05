
module Network.JobQueue.JobEnv (
    JobEnv
  , initJobEnv
  ) where

import Network.JobQueue.Class

data JobEnv = JobEnvV1 {
    jeLocator    :: String
  , jeName       :: String
  , jeParameters :: [(String, String)]
  } deriving (Eq, Show)

instance Env JobEnv where
  envParameters = jeParameters
  envName = jeName

initJobEnv :: String -> String -> [(String, String)] -> JobEnv
initJobEnv = JobEnvV1

