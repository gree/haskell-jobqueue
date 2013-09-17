
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
         -- run conn "CREATE TABLE IF NOT EXISTS ? (key INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT, version INTEGER)" [toSql queueName]
         run conn ("CREATE TABLE IF NOT EXISTS " ++ queueName ++ " (key INTEGER PRIMARY KEY AUTOINCREMENT, value TEXT, version INTEGER)") []
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
  -- sqlvalues <- quickQuery conn' "SELECT key, value FROM ? ORDER BY key desc LIMIT 1" [toSQL queueName]
  sqlvalues <- quickQuery conn' ("SELECT key, value FROM " ++ queueName ++ " ORDER BY key desc LIMIT 1") []
  case sqlvalues of
    [] -> return (Nothing)
    ((key:value:_):_) -> do
      -- run conn' "DELETE FROM ? WHERE key = ?" [toSql queueName, key]
      run conn' ("DELETE FROM " ++ queueName ++ " WHERE key = " ++ fromSql key) []
      return (Just (fromSql value, fromSql key))

peekDBQueue :: Sqlite3Queue -> IO (Maybe (BS.ByteString, String, Int))
peekDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  -- sqlvalues <- quickQuery conn' "SELECT key, value, version FROM ? ORDER BY key DESC LIMIT 1" [toSql queueName]
  sqlvalues <- quickQuery conn' ("SELECT key, value, version FROM " ++ queueName ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return (Nothing)
    ((key:value:version:_):_) -> return (Just (fromSql value, fromSql key, fromSql version))

writeDBQueue :: Sqlite3Queue -> BS.ByteString -> IO (String)
writeDBQueue (Sqlite3Queue conn queueName) value = withTransaction conn $ \conn' -> do
  -- run conn' "INSERT INTO ?(value, version) VALUES (?,0)" [toSql queueName, toSql value]
  run conn' ("INSERT INTO " ++ queueName ++ "(value, version) VALUES ('" ++ BS.unpack value ++ "',0)") []
  -- sqlvalues <- quickQuery conn' "SELECT key FROM ? ORDER BY key DESC LIMIT 1" [toSql queueName]
  sqlvalues <- quickQuery conn' ("SELECT key FROM " ++ queueName ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return ("")
    ((key:_):_) -> do
      return (fromSql key)

writeDBQueue' :: Sqlite3Queue -> BS.ByteString -> Int -> IO (String)
writeDBQueue' (Sqlite3Queue conn queueName) value version = withTransaction conn $ \conn' -> do
  -- run conn' "INSERT INTO ?(value, version) VALUES (?,0)" [toSql queueName, toSql value]
  run conn' ("INSERT INTO " ++ queueName ++ "(value, version) VALUES ('" ++ BS.unpack value ++ "',0)") []
  -- sqlvalues <- quickQuery conn' "SELECT key FROM ? ORDER BY key DESC LIMIT 1" [toSql queueName]
  sqlvalues <- quickQuery conn' ("SELECT key FROM " ++ queueName ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return ("")
    ((key:_):_) -> do
      return (fromSql key)

deleteDBQueue :: Sqlite3Queue -> String -> IO (Bool)
deleteDBQueue (Sqlite3Queue conn queueName) key = withTransaction conn $ \conn' -> do
  -- run conn' ("DELETE FROM ? WHERE key = ?") [toSql queueName, key]
  run conn' ("DELETE FROM " ++ queueName ++ " WHERE key = " ++ key) []
  return (True)

updateDBQueue :: Sqlite3Queue -> String -> BS.ByteString -> Int -> IO (Bool)
updateDBQueue (Sqlite3Queue conn queueName) key value version = withTransaction conn $ \conn' -> do
  -- run conn' "UPDATE ? SET value = ?, version = ? WHERE key = ? AND version = ?" [toSql value, toSql version, toSql key, toSql version]
  run conn' ("UPDATE " ++ queueName ++ " SET value = '" ++ BS.unpack value ++ "', version = version+1 WHERE key = " ++ key ++ " AND version = " ++ show version) []
  -- sqlvalues <- quickQuery conn' "SELECT key FROM ? WHERE key = ? AND version = ? ORDER BY key DESC LIMIT 1" [toSql queueName, toSql key, toSql (version+1)]
  sqlvalues <- quickQuery conn' ("SELECT key FROM " ++ queueName ++ " WHERE key = " ++ key ++ " AND version = " ++ show (version+1) ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return (False)
    _ -> return (True)

countDBQueue :: Sqlite3Queue -> IO (Int)
countDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  -- sqlvalues <- quickQuery conn' "SELECT COUNT (*) FROM ? ORDER BY key DESC LIMIT 1" [toSql queueName]
  sqlvalues <- quickQuery conn' ("SELECT COUNT (*) FROM " ++ queueName ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return (0)
    ((count:_):_) -> return (fromSql count)

itemsDBQueue :: Sqlite3Queue -> IO ([String])
itemsDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  -- sqlvalues <- quickQuery conn' "SELECT COUNT (*) FROM ? ORDER BY key DESC LIMIT 1" [toSql queueName]
  sqlvalues <- quickQuery conn' ("SELECT key FROM " ++ queueName ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return ([])
    (keys:_) -> return (map fromSql keys)

listDBQueue :: Sqlite3Queue -> IO ([BS.ByteString])
listDBQueue (Sqlite3Queue conn queueName) = withTransaction conn $ \conn' -> do
  -- sqlvalues <- quickQuery conn' "SELECT COUNT (*) FROM ? ORDER BY key DESC LIMIT 1" [toSql queueName]
  sqlvalues <- quickQuery conn' ("SELECT key FROM " ++ queueName ++ " ORDER BY key DESC LIMIT 1") []
  case sqlvalues of
    [] -> return ([])
    (keys:_) -> return (map fromSql keys)

