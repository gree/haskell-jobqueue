
module Network.JobQueue.Aux where

import Control.Monad.Logger
import Control.Applicative
import System.Log.FastLogger
import System.IO

import Network.JobQueue.Types
import Network.JobQueue.Job.Internal

import qualified Data.ByteString.Char8 as S8

class Aux a where
  auxLogger :: a -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  auxLogger _ = defaultOutput stderr
    where
      defaultOutput h loc src level msg = S8.hPutStrLn h $ fromLogStr $ defaultLogStr loc src level msg

  auxHandleFailure :: (Unit b) => a -> Alert -> String -> String -> Maybe (Job b) -> IO (Maybe (Job b))
  auxHandleFailure _ _al _subject msg mjob = do
    hPutStrLn stderr msg
    hFlush stderr
    case mjob of
      Just job -> Just <$> createJob Runnable (getRecovery (jobUnit job))
      Nothing -> return (Nothing)

  auxHandleAfterExecute :: (Unit b) => a -> Job b -> IO ()
  auxHandleAfterExecute _ _job = return ()

  auxHandleLogging :: (Unit b) => a -> Job b -> IO ()
  auxHandleLogging _ job = do
    hPutStrLn stderr $ show job
    hFlush stderr
    return ()
