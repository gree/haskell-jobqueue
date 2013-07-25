
{-# LANGUAGE ExistentialQuantification #-}

module Network.JobQueue.Backend.Types where

import Network.JobQueue.Backend.Class

data Backend = forall q . (BackendQueue q) => Backend {
    bOpenQueue :: (String -> IO q)
  , bClose :: Backend -> IO ()
  }

