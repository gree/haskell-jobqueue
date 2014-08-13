-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE TemplateHaskell #-}

module JobQueue (testJobQueue) where

import Control.Exception
import Test.Hspec
import Control.Monad
import System.Directory
import System.IO.Error (isDoesNotExistError)
import Control.Concurrent
import Control.Concurrent.Async
import System.IO

import Network.JobQueue

data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where
instance Aux JobEnv where

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

testJobQueue :: String -> Spec
testJobQueue backend = do
  describe "job queue" $ do
    it "says hello" $ do
      let withJobQueue = buildJobQueue backend "/says_hello_1" $ do
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
    
    it "suspends" $ do
      let withJobQueue = buildJobQueue backend "/suspends_1" $ do
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

    it "can be used concurrently" $ do
      let p = process $ \(Looping count) -> if count > 0 then commitIO (hPutStrLn stderr (show count)) >> fork (Looping (count - 1)) else fin
          env0 = (JobEnv "hello")
      buildJobQueue backend "/concurrently_1" p $ \jq -> do
        scheduleJob jq (Looping 1000)
        countJobQueue jq `shouldReturn` 1
      bracket (openSession backend) (closeSession) $ \session -> do
        let loop = \env jq' -> do
                        executeJob jq' env
                        count <- countJobQueue jq'
                        when (count > 0) $ loop env jq'
        _ <- flip mapConcurrently [1..50] $ \_ -> do
          jq <- openJobQueue session "/concurrently_1" p
          loop env0 jq
          closeJobQueue jq
        return ()
      buildJobQueue backend "/concurrently_1" p $ \jq -> do
        executeJob jq env0
        countJobQueue jq `shouldReturn` 0
      return ()

---------------------------------------------------------------- Utils

step :: (Aux e, Env e, Unit a) => e -> JobQueue e a -> Int -> IO ()
step env jq c
  | c > 0 = do
    executeJob jq env
    step env jq (pred c)
  | otherwise = return ()

