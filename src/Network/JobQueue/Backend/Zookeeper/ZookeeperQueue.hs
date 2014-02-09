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

import qualified Database.Zookeeper as Z
import qualified Data.ByteString.Char8 as C
import Control.Exception hiding (handle)
import Data.List
import Control.Monad
import Data.Maybe

import Network.JobQueue.Backend.Class
import Network.JobQueue.Backend.Types

data ZookeeperQueue = ZookeeperQueue {
    zqHandle         :: Z.Zookeeper
  , zqBasePath       :: String
  , zqNodeName       :: String
  , zqAcls           :: Z.AclList
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

qnPrefix :: String
qnPrefix = "qn-"

----

initZQueue :: Z.Zookeeper -> String -> Z.AclList -> IO (ZookeeperQueue)
initZQueue z path acls = do
  e <- Z.exists z path Nothing
  case e of
    Right _stat -> return ()
    Left Z.NoNodeError -> do
      e' <- Z.create z path Nothing acls []
      case e' of
        Right _ -> return ()
        Left zkerr -> throwZKError "initZQueue" zkerr
    Left zkerr -> throwZKError "initZQueue" zkerr
  return (ZookeeperQueue z path qnPrefix acls)

-- take
readZQueue :: ZookeeperQueue -> IO (Maybe (C.ByteString, String))
readZQueue zkQueue = do
  children <- getChildren zkQueue
  case children of
    [] -> return (Nothing)
    _  -> takeHead (sortChildren children)
  where
    takeHead [] = return (Nothing)
    takeHead (nodeName:xs) = do
      let path = zqBasePath zkQueue ++ "/" ++ nodeName
      e <- Z.get (zqHandle zkQueue) path Nothing
      case e of
        Right (Just value, _stat) -> do
          e' <- Z.delete (zqHandle zkQueue) path Nothing
          case e' of
            Right () -> return (Just (value, nodeName))
            Left _zkerr -> do
              r <- Z.exists (zqHandle zkQueue) path Nothing
              case r of
                Right _stat -> takeHead (nodeName:xs)
                Left Z.NoNodeError -> takeHead xs
                Left zkerr -> throwZKError "readZQueue" zkerr
        Right (Nothing, _stat) -> takeHead xs -- ignore if the content is empty
        Left Z.NoNodeError -> return (Nothing)
        Left zkerr -> throwZKError "readZQueue" zkerr

-- peek
peekZQueue :: ZookeeperQueue -> IO (Maybe (C.ByteString, String, String, Int))
peekZQueue zkQueue = do
  children <- getChildren zkQueue
  case children of
    [] -> return Nothing
    _  -> getHead (sortChildren children)
  where
    idSuffixLen :: Int
    idSuffixLen = 10
    
    getHead :: [String] -> IO (Maybe (C.ByteString, String, String, Int))
    getHead [] = return Nothing
    getHead (x:xs) = do
      e <- Z.get (zqHandle zkQueue) (fullPath zkQueue x) Nothing
      case e of
        Right (mValue, stat) -> do
          case mValue of
            Just v -> return $ Just (v, x, drop (length x - idSuffixLen) x, fromIntegral $ Z.statVersion stat)
            Nothing -> getHead xs
        Left Z.NoNodeError -> peekZQueue zkQueue
        Left zkerr -> throwZKError "peekZQueue" zkerr
    
-- update
updateZQueue :: ZookeeperQueue -> String -> C.ByteString -> Int -> IO (Bool)
updateZQueue zkQueue znodeName value version = do
  e <- Z.set (zqHandle zkQueue) (fullPath zkQueue znodeName) (Just value) (Just (fromIntegral version))
  case e of
    Right _stat -> return (True)
    Left Z.BadVersionError -> return (False)
    Left Z.NoNodeError -> return (False)
    Left zkerr -> throwZKError "updateZQueue" zkerr

-- delete
deleteZQueue :: ZookeeperQueue -> String -> IO (Bool)
deleteZQueue zkQueue nodeName = do
  let nodeName' = zqBasePath zkQueue ++ "/" ++ nodeName
  e <- Z.delete (zqHandle zkQueue) nodeName' Nothing
  case e of
    Right () -> return (True)
    Left zkerr -> throwZKError ("deleteZQueue(nodeName=" ++ nodeName' ++ ")") zkerr

-- offer
writeZQueue :: ZookeeperQueue -> C.ByteString -> Int -> IO (String)
writeZQueue zkQueue value prio = do
  r <- Z.create (zqHandle zkQueue)
                (zqBasePath zkQueue ++ "/" ++ (nodePrefix (zqNodeName zkQueue) prio))
                (Just value)
                (zqAcls zkQueue)
                [Z.Sequence]
  case r of
    Right znode -> return znode
    Left zkerr -> throwZKError "writeZQueue" zkerr

-- destroy
destroyZQueue :: ZookeeperQueue -> IO ()
destroyZQueue _zkQueue = return ()

-- elems
listZQueue :: ZookeeperQueue -> IO ([C.ByteString])
listZQueue zkQueue = do
  results <- getChildren zkQueue
  values <- forM (sortChildren results) getItem
  return (catMaybes values)
  where
    getItem x = do
      e <- Z.get (zqHandle zkQueue) (zqBasePath zkQueue ++ "/" ++ x) Nothing   
      case e of
        Right (mValue, stat) -> return (mValue)
        Left Z.NoNodeError -> return (Nothing)
        Left zkerr -> throwZKError "listZQueue" zkerr

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

getChildren :: ZookeeperQueue -> IO ([String])
getChildren zkQueue = do
  e <- Z.getChildren (zqHandle zkQueue) (zqBasePath zkQueue) Nothing
  case e of
    Right results -> return (results)
    Left zkerr -> throwZKError "getChildren" zkerr

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

throwZKError :: String -> Z.ZKError -> IO a
throwZKError func zkerr = throwIO $ SessionError (func ++ ": " ++ show zkerr)
