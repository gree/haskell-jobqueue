
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import System.Environment hiding (getEnv)
import Network.JobQueue

data JobEnv = JobEnv {
  } deriving (Eq, Show)

instance Env JobEnv where
instance Aux JobEnv where

data JobUnit = Priority0 | Priority1 Int | Priority1' Int | Priority2 | Failed deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority Priority0 = -1
  getPriority Priority1 {} = 0
  getPriority Priority1' {} = 0
  getPriority Priority2 = 1
  getPriority Failed    = -2
  getRecovery _ju = Failed

instance Desc JobUnit where

main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ do
            process $ \case
              Priority0 -> commitIO (putStrLn "0") >> fin
              (Priority1 ttl) -> commitIO (putStrLn "1") >> if ttl > 0 then fork $ Priority1 (ttl-1) else fin
              (Priority1' ttl) -> commitIO (putStrLn "1'") >> if ttl > 0 then fork $ Priority1' (ttl-1) else fin
              _ -> none
            process $ \case
              Priority2 -> commitIO (putStrLn "2") >> fin
              Failed    -> commitIO (putStrLn "failed.") >> fin
              _ -> none
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv)
        ("init":[]) -> withJobQueue $ \jq -> mapM_ (scheduleJob jq) [Priority1 10, Priority1' 10, Priority2, Priority0]
        ("show":[]) -> withJobQueue $ loop (JobEnv)
        [] -> putStrLn $ "command not specified."
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
