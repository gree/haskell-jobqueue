-- Copyright (c) Gree, Inc. 2013
-- License: MIT-style

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.JobQueue.Param
  ( ParamEnv
  , envParameters
  , Param
  , decodeParam
  , encodeParam
  , param
  ) where

import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import Network.JobQueue.Class
import Network.JobQueue.Types
import Network.JobQueue.Action (getEnv,abort)
import Network.JobQueue.Logger

{- | Environment with a parameter set
-}
class (Env a) => ParamEnv a where
  envParameters :: a -> [(String, String)]
  envParameters _env = []

class Param a where
  decodeParam :: String -> Maybe a
  encodeParam :: a -> String

instance Param String where
  decodeParam str = (fmap fst . listToMaybe . reads) str
  encodeParam val = show val

instance Param Int where
  decodeParam str = (fmap fst . listToMaybe . reads) str
  encodeParam val = show val

instance Param Integer where
  decodeParam str = (fmap fst . listToMaybe . reads) str
  encodeParam val = show val

instance Param Double where
  decodeParam str = (fmap fst . listToMaybe . reads) str
  encodeParam val = show val

instance Param A.Value where
  decodeParam str = A.decode (B.pack str)
  encodeParam val = B.unpack $ A.encode val

{- | Get a parameter value with a key from the environment in action.
     This is a special function for ParamEnv.
-}
param :: (ParamEnv e, Unit a, Param b) => (String, String) -> ActionM e a b
param (key, defaultValue) = do
  env <- getEnv
  case decodeParam defaultValue of
    Nothing -> do
      $(logCritical) "internal error. no parse: " [show (key, defaultValue)]
      abort
    Just defaultValue' -> case lookup key (envParameters env) of
      Just value -> return (fromMaybe defaultValue' (decodeParam value))
      Nothing -> return (defaultValue')
