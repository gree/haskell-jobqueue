-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.Backend.Class (BackendQueue(..)) where

import qualified Data.ByteString.Char8 as BS

class BackendQueue q where
  readQueue    :: q -> IO (Maybe (BS.ByteString, String))
  peekQueue    :: q -> IO (Maybe (BS.ByteString, String, String, Int))
  updateQueue  :: q -> String -> BS.ByteString -> Int -> IO (Bool)
  deleteQueue  :: q -> String -> IO (Bool)
  writeQueue   :: q -> BS.ByteString -> IO (String)
  writeQueue'  :: q -> BS.ByteString -> Int -> IO (String)
  listQueue    :: q -> IO ([BS.ByteString])
  itemsQueue   :: q -> IO ([String])
  countQueue   :: q -> IO (Int)
  closeQueue   :: q -> IO ()
  closeQueue _ = return ()

