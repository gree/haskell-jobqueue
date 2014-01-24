-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GADTs #-}

module Network.JobQueue.Backend.Types where

import Network.JobQueue.Backend.Class

data Backend where
  Backend :: (BackendQueue q) => {
      bOpenQueue :: String -> IO q
    , bClose :: Backend -> IO ()
    } -> Backend
