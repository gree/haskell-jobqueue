
module Network.JobQueue.JobEnv (
    JobEnv
  , initJobEnv
  , envLocator
  , envName
  , envParameters
  ) where

data JobEnv = JobEnvV1 {
    envLocator    :: String
  , envName       :: String
  , envParameters :: [(String, String)]
  } deriving (Eq, Show)

initJobEnv :: String -> String -> [(String, String)] -> JobEnv
initJobEnv = JobEnvV1

