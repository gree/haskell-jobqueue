-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.JobQueue.Action (
    JobActionState
  , runActionState
  , runAction
  , getEnv
  , param
  , next
  , orNext
  , fin
  , none
  , fork
  , forkInTime
  , forkOnTime
  , abort
  , commitIO
  , liftIO
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (SomeException(..), toException)
import Control.Exception.Base (PatternMatchFail(..))
import Control.Monad.Logger (runLoggingT)
import Control.Exception.Lifted (catch)
import Control.Monad.Base ()

import Data.Maybe
import Data.Time.Clock
import Data.Default (Default, def)

import Network.JobQueue.Class
import Network.JobQueue.AuxClass
import Network.JobQueue.Types
import Network.JobQueue.Logger

runActionState :: (Env e, Unit a) => JobActionState e a -> ActionFn e a
runActionState (JobActionState { jobActions = actions } ) env ju = runActionState' actions
  where
    runActionState' actions' = case actions' of
      [] -> return $ Right Nothing
      (act:acts) -> do
        r <- act env ju
        case r of
          Right Nothing -> runActionState' acts
          _ -> return r

runAction :: (Aux e, Env e, Unit a) => 
             e -> a -> ActionT e a IO () -> IO (Either Break (Maybe (RuntimeState a)))
runAction env ju action = do
  (e,r) <- flip runLoggingT (auxLogger env)
         $ flip runStateT Nothing
         $ flip runReaderT (ActionEnv env ju)
         $ runExceptT
         $ runAM $ do
             when (toBeLogged ju) $ $(logWarn) "{}" [desc ju]
             action `catch` handlePatternMatchFail `catch` handleSome
  return $ either Left (const $ Right r) e

handlePatternMatchFail :: (Aux e, Env e, Unit a) => PatternMatchFail -> ActionT e a IO ()
handlePatternMatchFail e = do
  s <- get
  if getCommits (fromMaybe def s) > 0
    then do
      ju <- getJobUnit <$> ask
      $(logError) "pattern match fail: ! ({})" [desc ju]
      throwError $ Unhandled (toException e)
    else none

handleSome :: (Aux e, Env e, Unit a) => SomeException -> ActionT e a IO b
handleSome e = do
  $(logError) "unhandled exception: {}" [show e]
  throwError $ Unhandled e

--------------------------------

{- | Get environment in action.
-}
getEnv :: (Env e, Unit a) => ActionM e a e
getEnv = getJobEnv <$> ask

{- | Get a parameter value with a key from the environment in action.
     This is a special function for ParamEnv.
-}
param :: (ParamEnv e, Unit a, Read b) => (String, String) -> ActionM e a b
param (key, defaultValue) = do
  env <- getEnv
  case maybeRead defaultValue of
    Nothing -> do
      $(logCritical) "internal error. no parse: " [show (key, defaultValue)]
      abort
    Just defaultValue' -> case lookup key (envParameters env) of
      Just value -> return (fromMaybe defaultValue' (maybeRead value))
      Nothing -> return (defaultValue')
  where
    maybeRead = fmap fst . listToMaybe . reads
      
----------------

{- | Do a dirty I/O action with a side effect to the external system.
     If it doesn't change the state of the external system, you should use liftIO instead.
-}
commitIO :: (Env e, Unit a) => IO b -> ActionM e a b
commitIO action = do
  do s <- get
     when (getCommits (fromMaybe def s) > 0) $ do
       ju <- getJobUnit <$> ask
       $(logWarn) "commitIO called twice! ({})" [desc ju]
  modify $ \s -> Just $ incrementCommits $ fromMaybe def s
  liftIO action

----------------

{- | Create a job with a unit and schedule it.
-}
fork :: (Env e, Unit a)
        => a -- ^ a unit
        -> ActionM e a ()
fork ju = forkWith ju Nothing

{- | Create a job with a unit and schedule it at a specific time.
-}
forkOnTime :: (Env e, Unit a)
              => UTCTime        -- ^ absolute time in UTC
              -> a              -- ^ a unit
              -> ActionM e a ()
forkOnTime t ju = forkWith ju (Just t)

{- | Create a job with a unit and schedule it after a few micro seconds.
-}
forkInTime :: (Env e, Unit a) => NominalDiffTime -> a -> ActionM e a ()
forkInTime tDiff ju = do
  currentTime <- liftIO $ getCurrentTime
  forkWith ju (Just (addUTCTime tDiff currentTime))

{- | Move to the next state immediately.
     After the execution of the action the job being processed will be
     moved to the given state. The next action will be invoked immediately
     and can continue to work without being interrupted by another job.
     NOTE: This overrides the next state if it is already set.
-}
next :: (Env e, Unit a)
        => a              -- ^ the next state
        -> ActionM e a ()
next ju = modify $ \s -> Just $ setNextJob ju $ fromMaybe def s

{- | Move to the next state immediately.
     This is different from "next" function because this doesn't override
     if the next job is already set.
-}
orNext :: (Env e, Unit a)
         => a              -- ^ the next state
         -> ActionM e a ()
orNext ju = modify $ \s -> Just $ setNextJobIfEmpty ju $ fromMaybe def s

{- | Finish a job.
-}
fin :: (Env e, Unit a) => ActionM e a ()
fin = modify $ \s -> Just $ emptyNextJob $ fromMaybe def s

{- | If the unit passed by the job queue system cannot be processed by the
     action function, the function should call this.
-}
none :: (Env e, Unit a) => ActionM e a ()
none = result Nothing

{- | Abort the execution of a state machine.
     If a critical problem is found and there is a need to switch to the failure state,
     call this function.
-}
abort :: (Env e, Unit a) => ActionM e a b
abort = do
  ju <- getJobUnit <$> ask
  throwError $ Failure ("aborted on " ++ desc ju)

---------------------------------------------------------------- PRIVATE

{- | Set the result of the action. (for internal use)
-}
result :: (Env e, Unit a) => Maybe (RuntimeState a) -> ActionM e a ()
result = modify . setResult

forkWith :: (Env e, Unit a) => a -> Maybe UTCTime -> ActionM e a ()
forkWith ju mt = modify $ \s -> Just $ addForkJob (ju, mt) $ fromMaybe def s
