
import Control.Concurrent
import Control.Monad
import System.Environment hiding (getEnv)
import Network.JobQueue

data JobEnv = JobEnv {
    jeLimit :: Integer
  } deriving (Eq, Show)

instance Env JobEnv where

data JobUnit = ExecuteStep Integer deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = ExecuteStep 0

instance Desc JobUnit where

main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ do
            process $ \(ExecuteStep r) -> do
              commitIO (putStrLn "executing")
              commitIO (threadDelay 1000000)
              env <- getEnv
              if r < jeLimit env
                then next $ ExecuteStep (r+1)
                else fin
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv 30)
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq $ ExecuteStep 0
        ("suspend":[]) -> withJobQueue $ \jq -> void $ suspendJobQueue jq
        ("resume":[]) -> withJobQueue $ \jq -> void $ resumeJobQueue jq
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
        _ -> putStrLn $ "invalid operation"
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
