
module Network.JobQueue.JobEnv (
    JobEnv
  , initJobEnv
  , envClusterName
  , envCoord
  , envConfigParameters
  ) where

type ConfigParameters = [(String, String)]

data JobEnv = JobEnvV1 {
    envClusterName :: String
  , envCoord :: String
  , envConfigParameters :: ConfigParameters
  } deriving (Eq, Show)

initJobEnv :: String -> String -> ConfigParameters -> JobEnv
initJobEnv = JobEnvV1

