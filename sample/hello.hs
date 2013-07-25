
import Network.JobQueue
import Data.Default

data JobUnit = HelloState | WorldState deriving (Show, Read)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloState

instance Desc JobUnit where

main = do
  session <- openSession "zookeeper://10.0.62.86:2181/jobqueue"
  jq <- openJobQueue session "/test" def $ do
    process $ \ju -> case ju of
      HelloState -> do
        commitIO $ putStr "hello, "
        next WorldState
      _ -> none
    process $ \ju -> case ju of
      WorldState -> do
        commitIO $ putStrLn "world"
        fin
      _ -> none
  scheduleJob jq HelloState
  let exec = executeJob jq (initJobEnv "hoge" "hoge" [])
  exec
  exec
  exec
  exec
  closeSession session
  return ()
  
