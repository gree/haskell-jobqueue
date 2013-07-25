
{-# LANGUAGE ExistentialQuantification #-}

module Network.JobQueue.Backend.Types where

import Network.JobQueue.Backend.Class

data Backend = forall q . (BackendQueue q) => Backend {
    openQueue :: (String -> IO q)
  , closeQueue :: (q -> IO ())
  , close :: Backend -> IO ()
  }

