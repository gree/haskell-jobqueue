
module Network.JobQueue.Action (
    JobActionState
  , buildActionState
  , runActionState
  , runAction
  , getEnv
  , myname
  , param
  , result
  , next
  , fin
  , none
  , fork
  , forkInTime
  , forkOnTime
  , abort
  , logMsg
  , commitIO
  , Alert(..)
  ) where

import System.Log.Logger

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (catch)
import Control.Exception.Base (PatternMatchFail(..))

import Data.Maybe
import Data.Time.Clock
import Data.Default (Default, def)

import Network.JobQueue.Class
import Network.JobQueue.Types

buildActionState :: (Env e, Unit a) => JobM e a () -> IO (JobActionState e a)
buildActionState jobs = execStateT (runS jobs) (JobActionState [])

runActionState :: (Env e, Unit a) => JobActionState e a -> e -> a -> IO (Maybe (JobResult a))
runActionState (JobActionState { jobActions = actions } ) env ju = do
  mjr <- runActionState' actions
  return (mjr)
  where
    runActionState' actions' = case actions' of
      [] -> return (Nothing)
      (act:acts) -> do
        r <- act env ju `catch` handleFail
        case r of
          Nothing -> runActionState' acts
          Just _ -> return (r)

    handleFail :: PatternMatchFail -> IO (Maybe (JobResult a))
    handleFail (PatternMatchFail msg) = do
      return (Nothing)

runAction :: (Env e, Unit a) => e -> a -> ActionM e a () -> IO (Maybe (JobResult a))
runAction env ju action = do
    (e,r) <- flip runStateT Nothing
           $ flip runReaderT (ActionEnv env ju)
           $ runErrorT
           $ runAM
           $ action `catchError` defaultHandler
    return $ either (const Nothing) (const $ r) e

defaultHandler :: (Env e, Unit a) => ActionError -> ActionM e a ()
defaultHandler (ActionError al msg) = result (Just $ Left $ Failure al msg)

--------------------------------

getEnv :: (Env e, Unit a) => ActionM e a (e)
getEnv = getJobEnv <$> ask

param :: (Env e, Unit a, Read b) => (String, String) -> ActionM e a (b)
param (key, defaultValue) = do
  env <- getEnv
  case maybeRead defaultValue of
    Nothing -> abort Critical $ "internal error. no parse: " ++ show (key, defaultValue)
    Just defaultValue' -> case lookup key (envParameters env) of
      Just value -> return (fromMaybe defaultValue' (maybeRead value))
      Nothing -> return (defaultValue')
  where
    maybeRead = fmap fst . listToMaybe . reads
      
myname :: (Env e, Unit a) => ActionM e a (String)
myname = getEnv >>= return . envName

----------------

commitIO :: (Env e, Unit a) => IO (b) -> ActionM e a (b)
commitIO action = liftIO action

----------------

result :: (Env e, Unit a) => Maybe (JobResult a) -> ActionM e a ()
result = modify . setResult

fork :: (Env e, Unit a) => a -> ActionM e a ()
fork ju = forkWith ju Nothing

forkOnTime :: (Env e, Unit a) => UTCTime -> a -> ActionM e a ()
forkOnTime t ju = forkWith ju (Just t)

forkInTime :: (Env e, Unit a) => NominalDiffTime -> a -> ActionM e a ()
forkInTime tDiff ju = do
  currentTime <- liftIO $ getCurrentTime
  forkWith ju (Just (addUTCTime tDiff currentTime))

next :: (Env e, Unit a) => a -> ActionM e a ()
next ju = modify $ \s -> Just $ setNextJob ju $ fromMaybe def s

fin :: (Env e, Unit a) => ActionM e a ()
fin = result def

none :: (Env e, Unit a) => ActionM e a ()
none = result Nothing

abort :: (Env e, Unit a) => Alert -> String -> ActionM e a b
abort level msg = do
  result $ Just $ Left $ Failure level msg
  throwError $ ActionError level msg

logMsg :: (Env e, Unit a) => Alert -> String -> ActionM e a ()
logMsg level msg = liftIO $ case level of
  Critical -> criticalM "control" msg
  Error -> errorM "control" msg
  Warning -> warningM "control" msg
  Notice -> noticeM "control" msg
  Info -> infoM "control" msg
  -- _ -> infoM "control" msg

---------------------------------------------------------------- PRIVATE

forkWith :: (Env e, Unit a) => a -> Maybe UTCTime -> ActionM e a ()
forkWith ju mt = modify $ \s -> Just $ addForkJob (ju, mt) $ fromMaybe def s

