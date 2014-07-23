-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE TemplateHaskell #-}

module BackendQueue (testJobQueueBackend) where

import Control.Exception
import Test.Hspec
import System.Directory
import System.IO.Error (isDoesNotExistError)
import System.Environment (lookupEnv)

import qualified Data.ByteString.Char8 as BS
import Network.JobQueue.Backend.Sqlite3
import Network.JobQueue.Backend.Zookeeper
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class

testJobQueueBackend :: String -> Spec
testJobQueueBackend backend = do
  describe "backend queue" $ do
    it "peeks" $ do
      withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/peek_1"
        k <- writeQueue q (BS.pack "hoge") 0
        Just (bs, name, idName, version) <- peekQueue q
        _ <- deleteQueue q name
        countQueue q `shouldReturn` 0

    it "writes and reads" $ do
      withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/read_and_write_1"
        k <- writeQueue q (BS.pack "hoge") 0
        readQueue q `shouldReturn` Just (BS.pack "hoge", k)

    it "counts" $ do
      withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/count_1"
        _ <- writeQueue q (BS.pack "hoge1") 0
        countQueue q `shouldReturn` 1
        _ <- writeQueue q (BS.pack "hoge2") 0
        countQueue q `shouldReturn` 2
        _ <- readQueue q
        _ <- readQueue q
        return ()

    it "has items" $ do
      withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/items_1"
        k1 <- writeQueue q (BS.pack "hoge1") 0
        itemsQueue q `shouldReturn` [k1]
        k2 <- writeQueue q (BS.pack "hoge2") 0
        itemsQueue q `shouldReturn` [k1, k2]
        _ <- readQueue q
        _ <- readQueue q
        return ()

---------------------------------------------------------------- Utils

withBackend :: (Backend -> IO ()) -> IO ()
withBackend act = do
  backend <- lookupEnv "JOBQUEUE_TEST_BACKEND"
  case backend of
    Just "zookeeper" -> do
      bracket (openZookeeperBackend "localhost:2181") bClose act
    _ -> do
      let testFile = "test.sqlite3"
      r <- bracket (openSqlite3Backend testFile) bClose act
      removeIfExists testFile
      return r

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
