-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

module Network.JobQueue.JobResult (
    returnFailure
  , returnFinished
  , returnContinue
  , returnContinueAndFork
  , returnOnTime
  , returnInTime
  , returnFork
  , returnReschedule
  , Alert(..)
  ) where

import Data.Time.Clock

import Network.JobQueue.Types


returnFailure :: Alert -> String -> IO (Maybe (JobResult a))
returnFailure alertLevel msg = return $ Just $ Left $ Failure alertLevel msg

returnFinished :: IO (Maybe (JobResult a))
returnFinished = return $ Just $ Right $ Next Nothing []

returnContinue :: (Unit a) => a -> IO (Maybe (JobResult a))
returnContinue junit = return $ Just $ Right $ Next (Just junit) []

returnContinueAndFork :: (Unit a) => a -> [a] -> IO (Maybe (JobResult a))
returnContinueAndFork junit forkunits = return $ Just $ Right $ Next (Just junit) $ map (\x -> (x, Nothing)) forkunits

returnOnTime :: (Unit a) => UTCTime -> a -> IO (Maybe (JobResult a))
returnOnTime time junit = return $ Just $ Right $ Next Nothing [(junit, Just time)]

returnInTime :: (Unit a) => NominalDiffTime -> a -> IO (Maybe (JobResult a))
returnInTime timeDiff junit = do
  currentTime <- getCurrentTime
  let onTime = addUTCTime timeDiff currentTime
  returnOnTime onTime junit

returnFork :: (Unit a) => [a] -> IO (Maybe (JobResult a))
returnFork junits = return $ Just $ Right $ Next Nothing $ map (\x -> (x, Nothing)) junits

returnReschedule :: (Unit a) => a -> IO (Maybe (JobResult a))
returnReschedule junits = returnFork [junits]
