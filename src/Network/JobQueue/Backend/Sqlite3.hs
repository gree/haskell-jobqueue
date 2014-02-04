-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Backend.Sqlite3 (openSqlite3Backend, newSqlite3Backend) where

import qualified Data.ByteString.Char8 as BS
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class

data Sqlite3Queue = Sqlite3Queue Connection String

instance BackendQueue Sqlite3Queue where
  readQueue    = readDBQueue
  peekQueue    = peekDBQueue
  updateQueue  = updateDBQueue
  deleteQueue  = deleteDBQueue
  writeQueue   = writeDBQueue
  writeQueue'  = writeDBQueue'
  listQueue    = listDBQueue
  itemsQueue   = itemsDBQueue
  countQueue   = countDBQueue

openSqlite3Backend :: String -> IO Backend
openSqlite3Backend filePath = do
  conn <- connectSqlite3 filePath
  return $ Backend {
      bOpenQueue = \queueName -> do
         run conn "CREATE TABLE IF NOT EXISTS ? (key INTEGER PRIMARY KEY AUTOINCREMENT, prio INTEGER, value TEXT, version INTEGER)" [toSql queueName]
         return (Sqlite3Queue conn queueName)
    , bClose = \_ -> disconnect conn
    }

newSqlite3Backend :: Connection -> Backend
newSqlite3Backend conn = Backend {
      bOpenQueue = \queueName -> return (Sqlite3Queue conn queueName)
    , bClose = \_ -> return ()
    }

readDBQueue :: Sqlite3Queue -> IO (Maybe (BS.ByteString, String))
readDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' "SELECT key, value FROM ? ORDER BY prio, key desc LIMIT 1" [toSql queueName]
  case sqlvalues of
    [] -> return (Nothing)
    ((key:value:_):_) -> do
      run conn' "DELETE FROM ? WHERE key = ?" [toSql queueName, key]
      return (Just (fromSql value, fromSql key))

peekDBQueue :: Sqlite3Queue -> IO (Maybe (BS.ByteString, String, String, Int))
peekDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' "SELECT key, value, version FROM ? ORDER BY prio, key DESC LIMIT 1" [toSql queueName]
  case sqlvalues of
    [] -> return (Nothing)
    ((key:value:version:_):_) -> return (Just (fromSql value, fromSql key, fromSql key, fromSql version))

writeDBQueue :: Sqlite3Queue -> BS.ByteString -> IO (String)
writeDBQueue q value = writeDBQueue' q value 0

writeDBQueue' :: Sqlite3Queue -> BS.ByteString -> Int -> IO (String)
writeDBQueue' (Sqlite3Queue conn queueName) value prio = withTransaction conn $ \conn' -> do
  run conn' "INSERT INTO ?(prio, value, version) VALUES (?,?,0)" [toSql queueName, toSql prio, toSql value]
  sqlvalues <- quickQuery conn' "SELECT key FROM ? ORDER BY prio, key DESC LIMIT 1" [toSql queueName]
  case sqlvalues of
    [] -> return ("")
    ((key:_):_) -> do
      return (fromSql key)

deleteDBQueue :: Sqlite3Queue -> String -> IO (Bool)
deleteDBQueue (Sqlite3Queue conn queueName) key = withTransaction conn $ \conn' -> do
  run conn' ("DELETE FROM ? WHERE key = ?") [toSql queueName, toSql key]
  return (True)

updateDBQueue :: Sqlite3Queue -> String -> BS.ByteString -> Int -> IO (Bool)
updateDBQueue (Sqlite3Queue conn queueName) key value version = withTransaction conn $ \conn' -> do
  run conn' "UPDATE ? SET value = ?, version = ? WHERE key = ? AND version = ?" [toSql queueName, toSql value, toSql version, toSql key, toSql version]
  sqlvalues <- quickQuery conn' "SELECT key FROM ? WHERE key = ? AND version = ? ORDER BY prio, key DESC LIMIT 1" [toSql queueName, toSql key, toSql (version+1)]
  case sqlvalues of
    [] -> return (False)
    _ -> return (True)

countDBQueue :: Sqlite3Queue -> IO (Int)
countDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' "SELECT COUNT (*) FROM ? ORDER BY prio, key DESC LIMIT 1" [toSql queueName]
  case sqlvalues of
    [] -> return (0)
    ((count:_):_) -> return (fromSql count)

itemsDBQueue :: Sqlite3Queue -> IO ([String])
itemsDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' "SELECT COUNT (*) FROM ? ORDER BY prio, key DESC LIMIT 1" [toSql queueName]
  case sqlvalues of
    [] -> return ([])
    (keys:_) -> return (map fromSql keys)

listDBQueue :: Sqlite3Queue -> IO ([BS.ByteString])
listDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' "SELECT COUNT (*) FROM ? ORDER BY prio, key DESC LIMIT 1" [toSql queueName]
  case sqlvalues of
    [] -> return ([])
    (keys:_) -> return (map fromSql keys)

