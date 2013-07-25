
import Network.JobQueue
import Data.Default
import Control.Exception

data JobUnit = HelloStep | WorldStep deriving (Show, Read)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where

main = do
  bracket (openSession "zookeeper://10.0.62.86:2181/jobqueue") (closeSession) $ \session -> do
    jq <- openJobQueue session "/test" def $ do
      process $ \WorldStep -> do
        commitIO $ putStrLn "world"
        fin
      process $ \HelloStep -> do
        commitIO $ putStr "hello, "
        next WorldStep
    scheduleJob jq HelloStep
    let exec = executeJob jq (initJobEnv "hoge" "hoge" [])
    exec
    exec
    exec
    exec
    closeJobQueue jq
    return ()
  
