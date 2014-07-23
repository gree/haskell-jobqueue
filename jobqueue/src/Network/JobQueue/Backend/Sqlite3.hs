-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.JobQueue.Backend.Sqlite3 (openSqlite3Backend, newSqlite3Backend) where

import qualified Data.ByteString.Char8 as BS
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class
import Control.Concurrent.MVar
import Control.Exception

data Sqlite3Queue = Sqlite3Queue
  { conn      :: Connection
  , queueName :: String
  , mlock     :: MVar ()
  }

instance BackendQueue Sqlite3Queue where
  readQueue    = readDBQueue
  peekQueue    = peekDBQueue
  updateQueue  = updateDBQueue
  deleteQueue  = deleteDBQueue
  writeQueue   = writeDBQueue
  listQueue    = listDBQueue
  itemsQueue   = itemsDBQueue
  countQueue   = countDBQueue

openSqlite3Backend :: String -> IO Backend
openSqlite3Backend filePath = do
  c <- connectSqlite3 filePath
  m <- newMVar ()
  return $ Backend {
      bOpenQueue = \qn -> do
        _ <- withLock m $ withTransaction c $ \c' -> do
          run c' ("CREATE TABLE IF NOT EXISTS '" ++ qn ++ "' (key INTEGER PRIMARY KEY AUTOINCREMENT, prio INTEGER, value TEXT, version INTEGER)") []
        return (Sqlite3Queue c qn m)
    , bClose = disconnect c
    }

newSqlite3Backend :: Connection -> Backend
newSqlite3Backend c = do
  m <- newMVar ()
  return $ Backend {
      bOpenQueue = \qn -> do
        return (Sqlite3Queue c qn m)
    , bClose = return ()
    }

readDBQueue :: Sqlite3Queue -> IO (Maybe (BS.ByteString, String))
readDBQueue Sqlite3Queue {..} = withLock mlock $ withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' ("SELECT key, value FROM '" ++ queueName ++ "' ORDER BY prio, key LIMIT 1") []
  case sqlvalues of
    ((key:value:_):_) -> do
      _ <- run conn' ("DELETE FROM '" ++ queueName ++ "' WHERE key = ?") [toSql key]
      return (Just (fromSql value, fromSql key))
    _ -> return (Nothing)

peekDBQueue :: Sqlite3Queue -> IO (Maybe (BS.ByteString, String, String, Int))
peekDBQueue Sqlite3Queue {..} = withLock mlock $ withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' ("SELECT key, value, version FROM '" ++ queueName ++ "' ORDER BY prio, key LIMIT 1") []
  case sqlvalues of
    ((key:value:version:_):_) -> return (Just (fromSql value, fromSql key, fromSql key, fromSql version))
    _ -> return (Nothing)

writeDBQueue :: Sqlite3Queue -> BS.ByteString -> Int -> IO (String)
writeDBQueue Sqlite3Queue {..} value prio = do
  withLock mlock $ withTransaction conn $ \conn' -> do
    _ <- run conn' ("INSERT INTO '" ++ queueName ++ "'(prio, value, version) VALUES (?,?,0)") [toSql prio, toSql value]
    sqlvalues <- quickQuery conn' ("SELECT seq FROM sqlite_sequence where name = '" ++ queueName ++ "'") []
    case sqlvalues of
      ((key:_):_) -> do
        return (fromSql key)
      _ -> return ("")

deleteDBQueue :: Sqlite3Queue -> String -> IO (Bool)
deleteDBQueue Sqlite3Queue {..} key = withLock mlock $ withTransaction conn $ \conn' -> do
  _ <- run conn' ("DELETE FROM '" ++ queueName ++ "' WHERE key = ?") [toSql key]
  return (True)

updateDBQueue :: Sqlite3Queue -> String -> BS.ByteString -> Int -> IO (Bool)
updateDBQueue Sqlite3Queue {..} key value version = do
  withLock mlock $ withTransaction conn $ \conn' -> do
    _ <- run conn' ("UPDATE '" ++ queueName ++ "' SET value = ?, version = ? WHERE key = ? AND version = ?") [toSql value, toSql (version+1), toSql key, toSql version]
    sqlvalues <- quickQuery conn' ("SELECT key FROM '" ++ queueName ++ "' WHERE key = ? AND version = ? ORDER BY prio, key LIMIT 1") [toSql key, toSql (version+1)]
    case sqlvalues of
      [] -> return (False)
      _ -> return (True)

countDBQueue :: Sqlite3Queue -> IO (Int)
countDBQueue Sqlite3Queue {..} = withLock mlock $ withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' ("SELECT COUNT (*) FROM '" ++ queueName ++ "' ORDER BY prio, key LIMIT 1") []
  case sqlvalues of
    ((count:_):_) -> return (fromSql count)
    _ -> return (0)

itemsDBQueue :: Sqlite3Queue -> IO ([String])
itemsDBQueue Sqlite3Queue {..} = withLock mlock $ withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' ("SELECT key FROM '" ++ queueName ++ "' ORDER BY prio, key") []
  case sqlvalues of
    keys -> return (map (fromSql . head) keys)

listDBQueue :: Sqlite3Queue -> IO ([BS.ByteString])
listDBQueue Sqlite3Queue {..} = withLock mlock $ withTransaction conn $ \conn' -> do
  sqlvalues <- quickQuery conn' ("SELECT value FROM '" ++ queueName ++ "' ORDER BY prio, key") []
  case sqlvalues of
    keys -> return (map (fromSql . head) keys)

handleError :: forall a . IO a -> IO a
handleError act = do
  e <- try act :: IO (Either SqlError a)
  case e of
    Right r -> return r
    Left err -> throwIO $ SessionError (show err)

withLock :: MVar () -> IO a -> IO a
withLock m act = withMVar m $ const (handleError act)
