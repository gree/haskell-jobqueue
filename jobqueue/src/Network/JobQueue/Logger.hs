
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Text.Lazy as LT
import qualified Control.Monad.Logger as ML
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import Data.Text.Format

type LogLevel = ML.LogLevel

logTH :: LogLevel -> Q Exp
logTH level = [|\a b -> ML.monadLoggerLog $(qLocation >>= ML.liftLoc) (T.pack "") level (LT.toStrict $ format (a :: Format) b)|]

logDebug :: Q Exp
logDebug = logTH ML.LevelDebug

logInfo :: Q Exp
logInfo = logTH ML.LevelInfo

logWarn :: Q Exp
logWarn = logTH ML.LevelWarn

logError :: Q Exp
logError = logTH ML.LevelError

logNotice :: Q Exp
logNotice = logTH (ML.LevelOther "notice")

logCritical :: Q Exp
logCritical = logTH (ML.LevelOther "critical")

