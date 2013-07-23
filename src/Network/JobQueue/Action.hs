
module Network.JobQueue.Action (
    JobActionState
  , buildActionState
  , runActionState
  , runAction
  , clusterName
  , getEnv
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

import Data.Maybe
import Data.Time.Clock
import Data.Default (Default, def)

import Network.JobQueue.Types
import Network.JobQueue.JobEnv

buildActionState :: (Unit a) => JobM a () -> IO (JobActionState a)
buildActionState jobs = execStateT (runS jobs) (JobActionState [])

runActionState :: (Unit a) => JobActionState a -> JobEnv -> a -> IO (Maybe (JobResult a))
runActionState (JobActionState { jobActions = actions } ) env ju = do
  mjr <- runActionState' actions
  return (mjr)
  where
    runActionState' actions' = case actions' of
      [] -> return (Nothing)
      (act:acts) -> do
        r <- act env ju
        case r of
          Nothing -> runActionState' acts
          Just _ -> return (r)

runAction :: (Unit a) => JobEnv -> a -> ActionM a () -> IO (Maybe (JobResult a))
runAction env ju action = do
    (e,r) <- flip runStateT Nothing
           $ flip runReaderT (ActionEnv env ju)
           $ runErrorT
           $ runAM
           $ action `catchError` defaultHandler
    return $ either (const Nothing) (const $ r) e

defaultHandler :: (Unit a) => ActionError -> ActionM a ()
defaultHandler (ActionError al msg) = result (Just $ Left $ Failure al msg)

--------------------------------

getEnv :: (Unit a) => ActionM a (JobEnv)
getEnv = getJobEnv <$> ask

param :: (Unit a, Read b) => (String, String) -> ActionM a (b)
param (key, defaultValue) = do
  env <- getEnv
  let cp = envConfigParameters env
  let def' = maybeRead defaultValue
  case def' of
    Nothing -> abort Critical $ "internal error. no parse: " ++ show (key, defaultValue)
    Just defaultValue' -> case lookup key cp of
      Just value -> return (fromMaybe defaultValue' (maybeRead value))
      Nothing -> return (defaultValue')
  where
    maybeRead = fmap fst . listToMaybe . reads
      
clusterName :: (Unit a) => ActionM a String
clusterName = do { env <- getEnv; return (envClusterName env) }

----------------

commitIO :: (Unit a) => IO (b) -> ActionM a b
commitIO action = liftIO action

----------------

result :: (Unit a) => Maybe (JobResult a) -> ActionM a ()
result = modify . setResult

fork :: (Unit a) => a -> ActionM a ()
fork ju = forkWith ju Nothing

forkOnTime :: (Unit a) => UTCTime -> a -> ActionM a ()
forkOnTime t ju = forkWith ju (Just t)

forkInTime :: (Unit a) => NominalDiffTime -> a -> ActionM a ()
forkInTime tDiff ju = do
  currentTime <- liftIO $ getCurrentTime
  forkWith ju (Just (addUTCTime tDiff currentTime))

next :: (Unit a) => a -> ActionM a ()
next ju = modify $ \s -> Just $ setNextJob ju $ fromMaybe def s

fin :: (Unit a) => ActionM a ()
fin = result def

none :: (Unit a) => ActionM a ()
none = result Nothing

abort :: (Unit a) => Alert -> String -> ActionM a b
abort level msg = do
  result $ Just $ Left $ Failure level msg
  throwError $ ActionError level msg

logMsg :: (Unit a) => Alert -> String -> ActionM a ()
logMsg level msg = liftIO $ case level of
  Critical -> criticalM "control" msg
  Error -> errorM "control" msg
  Warning -> warningM "control" msg
  Notice -> noticeM "control" msg
  Info -> infoM "control" msg
  -- _ -> infoM "control" msg

---------------------------------------------------------------- PRIVATE

forkWith :: (Unit a) => a -> Maybe UTCTime -> ActionM a ()
forkWith ju mt = modify $ \s -> Just $ addForkJob (ju, mt) $ fromMaybe def s

