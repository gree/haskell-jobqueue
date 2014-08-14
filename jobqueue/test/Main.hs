-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Main where

import Test.Hspec
import System.Environment (lookupEnv)
import Data.Maybe
import System.IO

import Action
import BackendQueue
import JobQueue

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  backend <- fmap (fromMaybe "sqlite3://test.sqlite3") $ lookupEnv "JOBQUEUE_TEST_BACKEND"
  hspec $ do
    testAction backend
    testJobQueueBackend backend
    testJobQueue backend
