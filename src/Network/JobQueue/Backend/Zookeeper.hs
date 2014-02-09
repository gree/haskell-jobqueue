-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Backend.Zookeeper (
    openZookeeperBackend
  , newZookeeperBackend
  ) where

import Data.List
import qualified Database.Zookeeper as Z
import Control.Concurrent
import Control.Concurrent.STM
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Zookeeper.ZookeeperQueue

openZookeeperBackend :: String -> IO Backend
openZookeeperBackend endpoint = do
  Z.setDebugLevel Z.ZLogError
  zvar <- newTVarIO Nothing
  stateVar <- newTVarIO Z.ConnectingState
  _ <- forkIO $ Z.withZookeeper endpoint 100000 (Just $ watcher stateVar) Nothing $ \z -> do
    atomically $ do
      state <- readTVar stateVar
      case state of
        Z.ConnectingState -> retry
        _ -> return ()
    atomically $ writeTVar zvar (Just z)
    atomically $ do
      mz <- readTVar zvar
      case mz of
        Just _ -> retry
        Nothing -> return ()
  return $ Backend {
      bOpenQueue = openQueue zvar
    , bClose = \_ -> atomically $ writeTVar zvar Nothing
    }
  where
    openQueue :: TVar (Maybe Z.Zookeeper) -> String -> IO (ZookeeperQueue)
    openQueue zvar queueName = do
      z <- atomically $ readTVar zvar >>= maybe retry return
      zq <- initZQueue z (basePath queueName) Z.OpenAclUnsafe
      return zq

    watcher :: TVar Z.State -> Z.Watcher
    watcher stateVar _z event state _mZnode = do
      case event of
        Z.SessionEvent -> atomically $ writeTVar stateVar state
        _ -> return ()

newZookeeperBackend :: Z.Zookeeper -> Backend
newZookeeperBackend zh = Backend {
      bOpenQueue = \queueName -> initZQueue zh (basePath queueName) Z.OpenAclUnsafe
    , bClose = \_ -> return ()
    }

basePath queueName = if "/" `isPrefixOf` queueName then queueName else "/" ++ queueName
