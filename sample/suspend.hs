
import Control.Concurrent
import Control.Monad
import System.Environment hiding (getEnv)
import Network.JobQueue

data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where

data JobUnit = ExecuteStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = ExecuteStep

instance Desc JobUnit where

main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ do
            process $ \ExecuteStep -> do
              env <- getEnv
              commitIO (putStrLn $ (jeHello env))
              commitIO (threadDelay 2000000)
              next ExecuteStep
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv "executing")
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq ExecuteStep
        ("suspend":[]) -> withJobQueue $ \jq -> do key <- suspendJobQueue jq; putStrLn $ "suspended (key = " ++ key ++ ")"
        ("resume":key:[]) -> withJobQueue $ \jq -> resumeJobQueue jq key
        ("close":[]) -> withJobQueue $ \jq -> closeJobQueue jq
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
