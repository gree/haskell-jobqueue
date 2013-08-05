
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Network.JobQueue

data JobEnv = JobEnv {
    jeLocator    :: String
  , jeName       :: String
  , jeParameters :: [(String, String)]
  } deriving (Eq, Show)

instance Env JobEnv where
  envParameters = jeParameters
  envName = jeName

data JobUnit =
    InitialStep
  | ComputationStep Integer Integer [Integer]
  deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = InitialStep

instance Desc JobUnit where

main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ do
            process $ \InitialStep -> next $ ComputationStep 0 1 []
            process $ \(ComputationStep a b r) -> do
              if length r > 100
                then liftIO (print (reverse r)) >> fin
                else next $ ComputationStep b (a+b) (a:r)
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv loc name [])
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq InitialStep
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
