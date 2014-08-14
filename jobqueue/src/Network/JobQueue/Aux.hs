-- Copyright (c) Gree, Inc. 2014
-- License: MIT-style

{-# LANGUAGE OverloadedStrings #-}

module Network.JobQueue.Aux where

import Control.Monad.Logger
import Control.Applicative
import System.IO
import System.Log.FastLogger
import System.Log.Logger
import System.Environment (getProgName)

import Network.JobQueue.Types
import Network.JobQueue.Job.Internal

import qualified Data.ByteString.Char8 as S8

class Aux a where
  auxLogger :: a -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  auxLogger _ loc logsrc loglevel msg = do
    progName <- getProgName
    logFunc loglevel progName $ S8.unpack $ fromLogStr $ defaultLogStr loc logsrc loglevel msg
    where
      logFunc level = case level of
        LevelDebug -> debugM
        LevelInfo -> infoM
        LevelWarn -> warningM
        LevelError -> errorM
        LevelOther "notice" -> noticeM
        LevelOther "critical" -> criticalM
        LevelOther _ -> warningM

  auxHandleFailure :: (Unit b) => a -> Maybe (Job b) -> IO (Maybe (Job b))
  auxHandleFailure _ mjob = do
    case mjob of
      Just job -> Just <$> createJob Runnable (getRecovery (jobUnit job))
      Nothing -> return (Nothing)

  auxHandleAfterExecute :: (Unit b) => a -> Job b -> IO ()
  auxHandleAfterExecute _ _job = return ()

  auxHandleLogging :: (Unit b) => a -> Job b -> IO ()
  auxHandleLogging _ job = do
    hPutStrLn stderr $ show job
    hFlush stderr
    return ()
