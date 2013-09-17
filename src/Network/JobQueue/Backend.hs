
{-# LANGUAGE OverloadedStrings #-}

module Network.JobQueue.Backend (openBackend) where

import Prelude hiding (takeWhile)
import Control.Exception (throwIO)
import qualified Data.ByteString.Char8 as BS
import Network.JobQueue.Backend.Class ()
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Zookeeper
import Network.JobQueue.Backend.Sqlite3

import Data.Attoparsec.ByteString
import Control.Applicative

data Locator = Zookeeper String | Sqlite3 String


{- | Open a backend database.
-}
openBackend :: String        -- ^ locator (eg. \"zookeeper://localhost:2181/myapp\", \"sqlite3://myapp.sqlite3\")
               -> IO Backend -- ^ backend
openBackend locator = case parseLocator locator of
  Just (Zookeeper connString) -> openZookeeperBackend connString
  Just (Sqlite3 localPath) -> openSqlite3Backend localPath
  _ -> throwIO $ userError "invalid locator"


---------------------------------------------------------------- PRIVATE

parseLocator :: String -> Maybe Locator
parseLocator v = case parse locatorParser $ BS.pack v of
  Done _ locator -> Just locator
  Partial parse' -> case parse' "" of
    Done _ locator -> Just locator
    _ -> Nothing
  _ -> Nothing
  where
    locatorParser :: Parser (Locator)
    locatorParser = do
      scheme <- takeWhile (/= 58) <* string "://"
      case scheme of
        "zookeeper" -> Zookeeper <$> fmap BS.unpack (takeWhile1 (\_ -> True))
        "sqlite3" -> Sqlite3 <$> fmap BS.unpack (takeWhile1 (\_ -> True))
        _ -> fail "unknown scheme"
  
