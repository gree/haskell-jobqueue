
module Network.JobQueue.Settings
       ( Settings(..)
       , FailureHandleFn
       , AfterExecuteHandleFn
       , LoggingHandleFn
       ) where

import Control.Applicative
import Data.Default
import System.IO

import Network.JobQueue.Class
import Network.JobQueue.Types
import Network.JobQueue.Job

type FailureHandleFn a = Alert -> String -> String -> Maybe (Job a) -> IO (Maybe (Job a))
type AfterExecuteHandleFn a = Job a -> IO ()
type LoggingHandleFn a = Job a -> IO ()

{- | Job queue settings
-}
data (Unit a) => Settings a = Settings
  { failureHandleFn :: FailureHandleFn a      -- ^ a function called when an action fails
  , afterExecuteFn  :: AfterExecuteHandleFn a -- ^ a function called after an action is executed (for debugging)
  , loggingHandleFn :: LoggingHandleFn a      -- ^ a function called when an action should be logged
  }

instance (Unit a) => Default (Settings a) where
  def = Settings handleFailure handleAfterExecute handleLogging
    where
      handleFailure :: (Unit a) => FailureHandleFn a
      handleFailure _al _subject msg mjob = do
        hPutStrLn stderr msg
        hFlush stderr
        case mjob of
          Just job -> Just <$> createJob Runnable (getRecovery (jobUnit job))
          Nothing -> return (Nothing)

      handleAfterExecute :: (Unit a) => Job a -> IO ()
      handleAfterExecute _job = return ()

      handleLogging :: (Unit a) => Job a -> IO ()
      handleLogging job = do
        hPutStrLn stderr $ show job
        hFlush stderr
        return ()
