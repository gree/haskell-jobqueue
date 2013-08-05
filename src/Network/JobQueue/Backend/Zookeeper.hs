
module Network.JobQueue.Backend.Zookeeper where

import qualified Zookeeper as Z
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Zookeeper.ZookeeperQueue

openZookeeperBackend :: String -> IO Backend
openZookeeperBackend coord = do
  Z.setDebugLevel Z.LogDisabled -- XXX
  zh <- Z.init coord Nothing 100000
  return $ Backend {
      bOpenQueue = \queueName -> return $ initZQueue zh queueName Z.OpenAclUnsafe
    , bClose = \_ -> Z.close zh
    }

newZookeeperBackend :: Z.ZHandle -> Backend
newZookeeperBackend zh = Backend {
      bOpenQueue = \queueName -> return $ initZQueue zh queueName Z.OpenAclUnsafe
    , bClose = \_ -> return ()
    }

