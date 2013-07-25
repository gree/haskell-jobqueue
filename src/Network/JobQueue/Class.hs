
module Network.JobQueue.Class where

class (Show a) => Desc a where
  desc :: a -> String
  desc x = show x
  shortDesc :: a -> String
  shortDesc x = takeWhile (/= ' ') $ show x

class (Read a, Show a, Desc a) => Unit a where
  getPriority :: a -> Int
  getRecovery :: a -> a
