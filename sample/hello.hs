
import Control.Monad
import Data.Default
import Control.Exception
import System.Environment
import Network.JobQueue

data JobUnit = HelloStep | WorldStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where

main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ do
            process $ \WorldStep -> commitIO (putStrLn "world") >> fin
            process $ \HelloStep -> commitIO (putStr "hello, ") >> next WorldStep
      case args' of
        ("run":[]) -> withJobQueue $ loop (initJobEnv loc name [])
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq HelloStep
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
