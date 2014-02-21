
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Monad.IO.Class
import System.Environment hiding (getEnv)
import Network.JobQueue

data JobEnv = JobEnv {
    jeLimit :: Int
  } deriving (Eq, Show)

instance Env JobEnv where

data JobUnit =
    InitialStep
  | ComputationStep Integer Integer [Integer]
  deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = InitialStep

instance Desc JobUnit where

main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ process $ \case
            InitialStep -> next $ ComputationStep 0 1 []
            (ComputationStep a b r) -> do
              env <- getEnv
              if length r > jeLimit env
                then liftIO (print (reverse r)) >> fin
                else next $ ComputationStep b (a+b) (a:r)
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv 100)
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq InitialStep
        [] -> putStrLn $ "command not specified."
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
