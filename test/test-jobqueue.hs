
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
  q <- openQueue "test"
  k <- writeQueue q (BS.pack "hoge") 0
  v <- readQueue q
  v @?= Just (BS.pack "hoge", k)
  
---------------------------------------------------------------- Utils

withBackend :: (Backend -> IO a) -> IO a
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