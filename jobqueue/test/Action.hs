-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Action (testAction) where

import Control.Exception
import Test.Hspec
import Control.Monad
import System.Directory
import System.IO.Error (isDoesNotExistError)
import Control.Concurrent
import Control.Concurrent.Async
import System.IO
import Control.Concurrent.STM

import Network.JobQueue

data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where

instance Aux JobEnv where
  auxHandleFailure _ mjob = do
    case mjob of
      Just job -> do
        nextJob <- createJob Runnable (getRecovery (jobUnit job))
        return (Just nextJob)
      Nothing -> return (Nothing)

data JobUnit = Initial | Recovery deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = Recovery
  toBeLogged _ = False

instance Desc JobUnit where

newMarker val = do
  var <- liftIO $ newTVarIO val
  return (liftIO $ readTVarIO var, liftIO . atomically . (writeTVar var))

testAction :: String -> Spec
testAction backend = do
  describe "action" $ do
    it "unhandled error" $ do
      (get, set) <- newMarker 0
      go $ buildJobQueue backend "/unhandled_error_1" $ do
            process $ \Initial -> do
              set 1
              $(logWarn) "Throw an IOError." ()
              liftIO $ throwIO $ userError "an IOError"
              $(logError) "Never reach here." ()
              set 2
              fin
            process $ \Recovery -> do
              set 3
              fin
      get `shouldReturn` 1

    it "abort and recover" $ do
      (get, set) <- newMarker 0
      go $ buildJobQueue backend "/abort_and_recover_1" $ do
            process $ \Initial -> do
              set 1
              $(logWarn) "Abort" ()
              abort
              $(logError) "Never reach here." ()
              set 2
              fin
            process $ \Recovery -> do
              set 3
              fin
      get `shouldReturn` 3

  where
    go withJobQueue = withJobQueue $ \jq -> do
      scheduleJob jq Initial
      countJobQueue jq `shouldReturn` 1
      let loop = \env jq' -> do
            executeJob jq' env
            count <- countJobQueue jq'
            when (count > 0) $ loop env jq'
      loop (JobEnv "hello") jq
      countJobQueue jq `shouldReturn` 0

