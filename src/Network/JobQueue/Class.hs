
module Network.JobQueue.Class where

class (Read a, Show a, Desc a) => Unit a where
  getPriority :: a -> Int
  getRecovery :: a -> a

class (Show a) => Desc a where
  desc :: a -> String
  desc x = show x
  shortDesc :: a -> String
  shortDesc x = takeWhile (/= ' ') $ show x
