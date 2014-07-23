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
import Network.JobQueue.Backend
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class

testJobQueueBackend :: String -> Spec
testJobQueueBackend backend = do
  describe "backend queue" $ do
    it "peeks" $ do
      withBackend backend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/peek_1"
        k <- writeQueue q (BS.pack "hoge") 0
        Just (bs, name, idName, version) <- peekQueue q
        _ <- deleteQueue q name
        countQueue q `shouldReturn` 0

    it "writes and reads" $ do
      withBackend backend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/read_and_write_1"
        k <- writeQueue q (BS.pack "hoge") 0
        readQueue q `shouldReturn` Just (BS.pack "hoge", k)

    it "counts" $ do
      withBackend backend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/count_1"
        _ <- writeQueue q (BS.pack "hoge1") 0
        countQueue q `shouldReturn` 1
        _ <- writeQueue q (BS.pack "hoge2") 0
        countQueue q `shouldReturn` 2
        _ <- readQueue q
        _ <- readQueue q
        return ()

    it "has items" $ do
      withBackend backend $ \(Backend { bOpenQueue = openQueue }) -> do
        q <- openQueue "/case/items_1"
        k1 <- writeQueue q (BS.pack "hoge1") 0
        itemsQueue q `shouldReturn` [k1]
        k2 <- writeQueue q (BS.pack "hoge2") 0
        itemsQueue q `shouldReturn` [k1, k2]
        _ <- readQueue q
        _ <- readQueue q
        return ()

---------------------------------------------------------------- Utils

withBackend :: String -> (Backend -> IO ()) -> IO ()
withBackend backend act = do
  bracket (openBackend backend) bClose act

