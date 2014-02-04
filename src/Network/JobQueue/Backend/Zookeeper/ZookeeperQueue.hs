-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Backend.Zookeeper.ZookeeperQueue (
    ZookeeperQueue
  , initZQueue
  , readZQueue
  , peekZQueue
  , updateZQueue
  , deleteZQueue
  , writeZQueue
  , destroyZQueue
  , listZQueue
  , itemsZQueue
  , countZQueue
  ) where

import Prelude hiding (catch)
import qualified Zookeeper as Z
import qualified Data.ByteString.Char8 as C
import Control.Exception hiding (handle)
import Data.List
import Control.Monad
import Data.Maybe

import Network.JobQueue.Backend.Class

data ZookeeperQueue = ZookeeperQueue {
    zqHandle         :: Z.ZHandle
  , zqBasePath       :: String
  , zqNodeName       :: String
  , zqAcls           :: Z.Acls
  }

instance BackendQueue ZookeeperQueue where
  readQueue    = readZQueue
  peekQueue    = peekZQueue
  updateQueue  = updateZQueue
  deleteQueue  = deleteZQueue
  writeQueue   = writeZQueue
  listQueue    = listZQueue
  itemsQueue   = itemsZQueue
  countQueue   = countZQueue
  

maxPrio :: Int
maxPrio = 999

minPrio :: Int
minPrio = -999

qnPrefix = "qn-"

----

initZQueue :: Z.ZHandle -> String -> Z.Acls -> ZookeeperQueue
initZQueue zh path acls = ZookeeperQueue zh path qnPrefix acls

-- take
readZQueue :: ZookeeperQueue -> IO (Maybe (C.ByteString, String))
readZQueue zkQueue = do
  children <- getChildren zkQueue
  case children of
    [] -> return (Nothing)
    _  -> takeHead (sortChildren children) `catch` handleZooError
  where
    takeHead [] = return (Nothing)
    takeHead (nodeName:xs) = do
      let path = (zqBasePath zkQueue ++ "/" ++ nodeName)
      (value, _stat) <- Z.get (zqHandle zkQueue) path Z.NoWatch
      case value of
        Just value' -> do
          isDeleted <- deleteChild zkQueue nodeName
          if isDeleted
            then return (Just (value', nodeName))
            else do
              r <- Z.exists (zqHandle zkQueue) path Z.NoWatch
              case r of
                Just _stat -> takeHead (nodeName:xs)
                Nothing -> takeHead xs
        Nothing -> takeHead xs

    handleZooError :: Z.ZooError -> IO (Maybe (C.ByteString, String))
    handleZooError (Z.ErrNoNode _) = return (Nothing)
    handleZooError e = throw e

-- peek
peekZQueue :: ZookeeperQueue -> IO (Maybe (C.ByteString, String, String, Int))
peekZQueue zkQueue = do
  children <- getChildren zkQueue
  case children of
    [] -> return (Nothing)
    _  -> getHead (sortChildren children) `catch` handleZooError
  where
    idSuffixLen = 10
    
    getHead :: [String] -> IO (Maybe (C.ByteString, String, String, Int))
    getHead [] = return Nothing
    getHead (x:xs) = do
      (value, stat) <- Z.get (zqHandle zkQueue) (fullPath zkQueue x) Z.NoWatch
      case value of
        Just v -> return $ Just (v, x, drop (length x - idSuffixLen) x, fromIntegral $ Z.stat_version stat)
        Nothing -> getHead xs
    
    handleZooError :: Z.ZooError -> IO (Maybe (C.ByteString, String, String, Int))
    handleZooError (Z.ErrNoNode _) = peekZQueue zkQueue
    handleZooError e = throw e
    
-- update
updateZQueue :: ZookeeperQueue -> String -> C.ByteString -> Int -> IO (Bool)
updateZQueue zkQueue znodeName value version = update `catch` handleZooError
  where
    update = do
      Z.set (zqHandle zkQueue) (fullPath zkQueue znodeName) (Just value) version
      return (True)
    
    handleZooError :: Z.ZooError -> IO (Bool)
    handleZooError (Z.ErrBadVersion _) = return (False)
    handleZooError (Z.ErrNoNode _) = return (False)
    handleZooError e = throw e

-- delete
deleteZQueue :: ZookeeperQueue -> String -> IO (Bool)
deleteZQueue = deleteChild

-- offer
writeZQueue :: ZookeeperQueue -> C.ByteString -> Int -> IO (String)
writeZQueue zkQueue value prio = do
  r <- Z.create (zqHandle zkQueue)
                (zqBasePath zkQueue ++ "/" ++ (nodePrefix (zqNodeName zkQueue) prio))
                (Just value)
                (zqAcls zkQueue)
                (Z.CreateMode False True)
  return (r)

-- destroy
destroyZQueue :: ZookeeperQueue -> IO ()
destroyZQueue _zkQueue = return ()

-- elems
listZQueue :: ZookeeperQueue -> IO ([C.ByteString])
listZQueue zkQueue = do
  results <- getChildren zkQueue
  values <- forM (sortChildren results) (\x -> getItem x `catch` handleZooError)
  return (catMaybes values)
  where
    getItem x = do
      (value, _) <- Z.get (zqHandle zkQueue) (zqBasePath zkQueue ++ "/" ++ x) Z.NoWatch
      return (value)

    handleZooError :: Z.ZooError -> IO (Maybe a)
    handleZooError zerr = case zerr of
      Z.ErrNoNode _ -> return (Nothing)
      e -> throwIO e

-- items
itemsZQueue :: ZookeeperQueue -> IO ([String])
itemsZQueue zkQueue = do
  items <- getChildren zkQueue
  return (sortChildren items)

-- count
countZQueue :: ZookeeperQueue -> IO (Int)
countZQueue zkQueue = do
  items <- itemsZQueue zkQueue
  return (length items)

----

deleteChild :: ZookeeperQueue -> String -> IO (Bool)
deleteChild zkQueue child = do { Z.delete (zqHandle zkQueue) (fullPath zkQueue child) (-1); return (True); }
  `catch` (\(SomeException _e) -> return (False))

getChildren :: ZookeeperQueue -> IO ([String])
getChildren zkQueue = do
  results <- Z.getChildren (zqHandle zkQueue) (zqBasePath zkQueue) Z.NoWatch
  return (results)

sortChildren :: [String] -> [String]
sortChildren = sort . filter (isPrefixOf qnPrefix)

fullPath :: ZookeeperQueue -> String -> String
fullPath zkQueue x = (zqBasePath zkQueue ++ "/" ++ x)

nodePrefix :: String -> Int -> String
nodePrefix base prio = base ++ priorityPart' ++ "-"
  where
    priority = if prio > maxPrio then maxPrio else (if prio < minPrio then minPrio else prio)
    plus = priority >= 0
    priorityPart = show $ if plus then abs priority else maxPrio + 1 + priority
    priorityPart' = (if plus then "0" else "-")
                    ++ (take (3 - length priorityPart) $ repeat '0')
                    ++ priorityPart

