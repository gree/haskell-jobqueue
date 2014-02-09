
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import System.Directory
import System.IO.Error (isDoesNotExistError)

import qualified Data.ByteString.Char8 as BS
import Network.JobQueue.Backend.Sqlite3
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class

main :: IO ()
main = $(defaultMainGenerator)

case_read_and_write1 :: Assertion
case_read_and_write1 = withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
  q <- openQueue "read_and_write1"
  k <- writeQueue q (BS.pack "hoge") 0
  v <- readQueue q
  v @?= Just (BS.pack "hoge", k)

case_count1 :: Assertion
case_count1 = withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
  q <- openQueue "count1"
  _ <- writeQueue q (BS.pack "hoge") 0
  c1 <- countQueue q
  c1 @?= 1
  _ <- writeQueue q (BS.pack "hoge") 0
  c2 <- countQueue q
  c2 @?= 2

case_items1 :: Assertion
case_items1 = withBackend $ \(Backend { bOpenQueue = openQueue }) -> do
  q <- openQueue "items1"
  k1 <- writeQueue q (BS.pack "hoge1") 0
  e1 <- itemsQueue q
  e1 @?= [k1]
  k2 <- writeQueue q (BS.pack "hoge2") 0
  e2 <- itemsQueue q
  e2 @?= [k1, k2]

---------------------------------------------------------------- Utils

withBackend :: (Backend -> Assertion) -> Assertion
withBackend act = do
  let testFile = "test.sqlite3"
  r <- bracket (openSqlite3Backend testFile) (\b -> bClose b b) act
  removeIfExists testFile
  return r

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
