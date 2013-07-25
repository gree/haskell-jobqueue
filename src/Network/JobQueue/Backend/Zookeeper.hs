
module Network.JobQueue.Backend.Zookeeper where

import qualified Zookeeper as Z
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Zookeeper.ZookeeperQueue

openZookeeperBackend :: String -> IO Backend
openZookeeperBackend coord = do
  zh <- Z.init coord Nothing 100000
  return $ Backend {
      openQueue = \queueName -> return $ initZQueue zh queueName Z.OpenAclUnsafe
    , closeQueue = \queue -> return ()
    , close = \_ -> Z.close zh
    }

