
import Test.HUnit
import qualified Data.ByteString.Char8 as BS
import Network.JobQueue.Backend.Sqlite3
import Network.JobQueue.Backend.Types
import Network.JobQueue.Backend.Class

main = do
  backend <- openSqlite3Backend "test.sqlite3"
  doTest backend
  (bClose backend) backend
  return ()

doTest (Backend { bOpenQueue = openQueue }) = do
  q <- openQueue "test"
  _ <- writeQueue q (BS.pack "hoge") 0
  v <- readQueue q
  print v
  
