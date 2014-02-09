-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Backend.Zookeeper where

import qualified Database.Zookeeper as Z
import Control.Concurrent
import Control.Concurrent.STM
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Zookeeper.ZookeeperQueue

openZookeeperBackend :: String -> IO Backend
openZookeeperBackend endpoint = do
  Z.setDebugLevel Z.ZLogError
  zvar <- newTVarIO Nothing
  _ <- forkIO $ Z.withZookeeper endpoint 100000 Nothing Nothing $ \z -> do
    atomically $ writeTVar zvar (Just z)
    atomically $ do
      mz <- readTVar zvar
      case mz of
        Just _ -> retry
        Nothing -> return ()
  return $ Backend {
      bOpenQueue = \queueName -> do
        z <- atomically $ readTVar zvar >>= maybe retry return
        return $ initZQueue z queueName Z.OpenAclUnsafe
    , bClose = \_ -> atomically $ writeTVar zvar Nothing
    }

newZookeeperBackend :: Z.Zookeeper -> Backend
newZookeeperBackend zh = Backend {
      bOpenQueue = \queueName -> return $ initZQueue zh queueName Z.OpenAclUnsafe
    , bClose = \_ -> return ()
    }

