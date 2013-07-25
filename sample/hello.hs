
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
    (loc:"run":[]) -> runJobQueue loc "/test" $ do
      process $ \WorldStep -> commitIO (putStrLn "world") >> fin
      process $ \HelloStep -> commitIO (putStr "hello, ") >> next WorldStep
    (loc:"init":[]) -> onJobQueue loc "/test" $ \jq -> do
      scheduleJob jq HelloStep
    _ -> return ()
