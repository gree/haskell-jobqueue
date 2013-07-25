
import Network.JobQueue
import System.IO
import Data.Default

data JobUnit = InitialState | EndState deriving (Show, Read)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = InitialState

instance Desc JobUnit where
  

action ju@InitialState = do
  return ()

action ju@EndState = do
  return ()

action _ = fin

main = do
  session <- openSession "zookeeper://10.0.62.86:2181/hello"
  jq <- openJobQueue session "test" def $ do
    process action
  return ()
  
