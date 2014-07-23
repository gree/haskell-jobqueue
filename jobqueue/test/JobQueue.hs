-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE TemplateHaskell #-}

module JobQueue (testJobQueue) where

import Control.Exception
import Test.Hspec
import Control.Monad
import System.Directory
import System.IO.Error (isDoesNotExistError)
import Control.Concurrent.Async

import Network.JobQueue

data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where

data JobUnit = HelloStep | WorldStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where

data Looping = Looping Int deriving (Show, Read, Eq, Ord)

instance Unit Looping where
  getPriority _ju = 1
  getRecovery _ju = (Looping 0)

instance Desc Looping where

testJobQueue :: Spec
testJobQueue = do
  describe "job queue" $ do
    it "says hello" $ do
      removeIfExists "case.sqlite3"
      let withJobQueue = buildJobQueue "sqlite3://case.sqlite3" "/hello_1" $ do
            process $ \WorldStep -> commitIO (putStrLn "world") >> fin
            process $ \HelloStep -> do
              env <- getEnv
              commitIO (putStr $ (jeHello env) ++ ", ")
              next WorldStep
      withJobQueue $ \jq -> do
        scheduleJob jq HelloStep
        countJobQueue jq `shouldReturn` 1
      withJobQueue $ \jq -> do
        let loop = \env jq' -> do
              executeJob jq' env
              count <- countJobQueue jq'
              when (count > 0) $ loop env jq'
        loop (JobEnv "hello") jq
        countJobQueue jq `shouldReturn` 0
      removeIfExists "case.sqlite3"
    
    it "suspends" $ do
      removeIfExists "case.sqlite3"
      let withJobQueue = buildJobQueue "sqlite3://case.sqlite3" "/hello_1" $ do
            process $ \WorldStep -> commitIO (putStrLn "world") >> fin
            process $ \HelloStep -> do
              env <- getEnv
              commitIO (putStr $ (jeHello env) ++ ", ")
              next WorldStep
      withJobQueue $ \jq -> do
        scheduleJob jq HelloStep
        suspendJobQueue jq `shouldReturn` True
        suspendJobQueue jq `shouldReturn` False
        step (JobEnv "hello") jq 5
        countJobQueue jq `shouldReturn` 2
      withJobQueue $ \jq -> do
        resumeJobQueue jq `shouldReturn` True
        step (JobEnv "hello") jq 5
        countJobQueue jq `shouldReturn` 0
      removeIfExists "case.sqlite3"

    it "can be used concurrently" $ do
      removeIfExists "case.sqlite3"
      let p = process $ \(Looping count) -> if count > 0 then fork (Looping (count - 1)) else fin
      buildJobQueue "sqlite3://case.sqlite3" "/conc_1" p $ \jq -> do
        scheduleJob jq (Looping 10000)
        countJobQueue jq `shouldReturn` 1
      let act = buildJobQueue "sqlite3://case.sqlite3" "/conc_1" p $ \jq -> do
                  let loop = \env jq' -> do
                        executeJob jq' env
                        count <- countJobQueue jq'
                        when (count > 0) $ loop env jq'
                  loop (JobEnv "hello") jq
                  countJobQueue jq `shouldReturn` 0
      as <- forM [1..50] $ \_ -> async act
      _ <- waitAny as
      removeIfExists "case.sqlite3"

---------------------------------------------------------------- Utils

step :: (Unit a1, Env e) => e -> JobQueue e a1 -> Int -> IO ()
step env jq c
  | c > 0 = do
    executeJob jq env
    step env jq (pred c)
  | otherwise = return ()

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
