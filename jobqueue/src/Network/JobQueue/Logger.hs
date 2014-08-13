
{-# LANGUAGE OverloadedStrings #-}

module Network.JobQueue.Logger
  ( LogLevel(..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logNotice
  , logCritical
  ) where

import qualified Data.Text as T
import qualified Control.Monad.Logger as ML
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)

type LogLevel = ML.LogLevel

logDebug :: Q Exp
logDebug = ML.logDebug

logInfo :: Q Exp
logInfo = ML.logInfo

logWarn :: Q Exp
logWarn = ML.logWarn

logError :: Q Exp
logError = ML.logError

logNotice :: Q Exp
logNotice = ML.logOther "notice"

logCritical :: Q Exp
logCritical = ML.logOther "critical"

