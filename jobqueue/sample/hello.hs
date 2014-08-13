
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Logger
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Environment hiding (getEnv)
import Network.JobQueue

data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where
instance Aux JobEnv where

data JobUnit = HelloStep | WorldStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where

main :: IO ()
main = do
  h <- openlog "hello" [] SYSLOG INFO
  updateGlobalLogger rootLoggerName (setHandlers [h])

  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ process $ \case
            WorldStep -> do
              $(logWarn) "WorldStep"
              commitIO (putStrLn "world") >> fin
            HelloStep -> do
              $(logWarn) "HelloStep"
              env <- getEnv
              commitIO (putStr $ (jeHello env) ++ ", ")
              next WorldStep
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv "hello")
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq HelloStep
        [] -> putStrLn $ "command not specified."
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
