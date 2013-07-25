
import Network.JobQueue
import Data.Default
import Control.Exception
import System.Environment

data JobUnit = HelloStep | WorldStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where

main = do
  args <- getArgs
  case args of
    (loc:_) -> bracket (openSession loc) (closeSession) $ \session -> do
      jq <- openJobQueue session "/test" def $ do
        process $ \WorldStep -> commitIO (putStrLn "world") >> fin
        process $ \HelloStep -> commitIO (putStr "hello, ") >> next WorldStep
      scheduleJob jq HelloStep
      let exec = executeJob jq (initJobEnv "hoge" "hoge" [])
      exec
      exec
      exec
      exec
      closeJobQueue jq
      return ()
    _ -> return ()
