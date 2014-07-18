-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Main where

import Test.Hspec
import JobQueue
import BackendQueue

main :: IO ()
main = hspec $ do
  testJobQueue
  testJobQueueBackend
