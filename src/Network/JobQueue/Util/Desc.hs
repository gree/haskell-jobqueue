
module Network.JobQueue.Util.Desc where

class (Show a) => Desc a where
  desc :: a -> String
  desc x = show x
  shortDesc :: a -> String
  shortDesc x = takeWhile (/= ' ') $ show x
