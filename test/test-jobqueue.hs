-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Monad
import System.Directory
import System.IO.Error (isDoesNotExistError)

import Network.JobQueue

main :: IO ()
main = $(defaultMainGenerator)


data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where

data JobUnit = HelloStep | WorldStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where

case_hello_1 :: IO ()
case_hello_1 = do
  removeIfExists "case.sqlite3"
  let withJobQueue = buildJobQueue "sqlite3://case.sqlite3" "/hello_1" $ do
        process $ \WorldStep -> commitIO (putStrLn "world") >> fin
        process $ \HelloStep -> do
          env <- getEnv
          commitIO (putStr $ (jeHello env) ++ ", ")
          next WorldStep
  withJobQueue $ \jq -> do
    scheduleJob jq HelloStep
    count <- countJobQueue jq
    count @?= 1
  withJobQueue $ \jq -> do
    loop (JobEnv "hello") jq
    count <- countJobQueue jq
    count @?= 0
  removeIfExists "case.sqlite3"
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq

case_suspend_1 :: IO ()
case_suspend_1 = do
  removeIfExists "case.sqlite3"
  let withJobQueue = buildJobQueue "sqlite3://case.sqlite3" "/hello_1" $ do
        process $ \WorldStep -> commitIO (putStrLn "world") >> fin
        process $ \HelloStep -> do
          env <- getEnv
          commitIO (putStr $ (jeHello env) ++ ", ")
          next WorldStep
  withJobQueue $ \jq -> do
    scheduleJob jq HelloStep
    r <- suspendJobQueue jq
    r @?= True
    r' <- suspendJobQueue jq -- should be ignored
    r' @?= False
    step (JobEnv "hello") jq 5
    count <- countJobQueue jq
    count @?= 2
  withJobQueue $ \jq -> do
    r <- resumeJobQueue jq
    r @?= True
    step (JobEnv "hello") jq 5
    count <- countJobQueue jq
    count @?= 0
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
