-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.JobQueue.Backend.Types (Backend(..), BackendError(..)) where

import Control.Exception
import Network.JobQueue.Backend.Class
import Data.Typeable

data Backend where
  Backend :: (BackendQueue q) => {
      bOpenQueue :: String -> IO q
    , bClose :: IO ()
    } -> Backend

data BackendError = 
    NotFound String
  | SessionError String
  deriving (Show, Typeable)

instance Exception BackendError
