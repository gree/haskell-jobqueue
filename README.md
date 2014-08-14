
Haskell JobQueue
================

JobQueue is a simple job queue library based on prioritized FCFS scheduling.

How to install
--------------

Execute the cabal install command and the library and example programs will be placed into your .cabal directory.

    > cd haskell-jobqueue/
    > cabal install --only-dependencies --extra-include-dirs=/usr/local/include/zookeeper # if you use brew on Mac OSX
    > cabal install --fsample

How to use
----------

### Backend

Various backends can be plugged in for use with this library. You can select the backend you wish to use depending on your needs. For example, one may choose Zookeeper for the backend if high availability is a requirement, or one may choose sqlite3 as their backend for a standalone tool.

* Zookeeper
* Sqlite3

### Hello, world

Import JobQueue module in your source file.

```haskell
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import System.Environment hiding (getEnv)
import Network.JobQueue
```

Define your environment data type.

```haskell
data JobEnv = JobEnv {
    jeHello      :: String
  } deriving (Eq, Show)

instance Env JobEnv where
instance Aux JobEnv where
```

Define states that describe your state machine.

```haskell
data JobUnit = HelloStep | WorldStep deriving (Show, Read, Eq, Ord)

instance Unit JobUnit where
  getPriority _ju = 1
  getRecovery _ju = HelloStep

instance Desc JobUnit where
```

Define actions and run the state machine.

```haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    (loc:name:args') -> do
      let withJobQueue = buildJobQueue loc name $ process $ \case
            WorldStep -> commitIO (putStrLn "world") >> fin
            HelloStep -> do
              env <- getEnv
              commitIO (putStr $ (jeHello env) ++ ", ")
              next WorldStep
      case args' of
        ("run":[]) -> withJobQueue $ loop (JobEnv "hello")
        ("init":[]) -> withJobQueue $ \jq -> scheduleJob jq HelloStep
        [] -> putStrLn $ "command not specified."
        (cmd:_) -> putStrLn $ "unknown command: " ++ cmd
    _ -> return ()
  where
    loop env jq = do
      executeJob jq env
      count <- countJobQueue jq
      when (count > 0) $ loop env jq
```

Examples
--------

### Example 1. Hello, World

The "hello" example demonstrates a simple sequential state transition. It consists of just two states, a "Hello" step and a "World" step.

To initialize the state machine, execute it with the "init" command.

    > jobqueue-sample-hello sqlite3://hello.sqlite3 test init

To run the state machine, execute it with the "run" command.

    > jobqueue-sample-hello sqlite3://hello.sqlite3 test run
    hello, world

If you wish to use a Zookeeper cluster, you can specify a Zookeeper address (e.g.  "zookeeper://localhost:2181/") instead of the sqlite3 address.

### Example 2. Fibonacci

The "fibonacci" example demonstrates a slightly more complex case with a loop in the state machine. It consists of an initialization step and a computation step.

To initialize the state machine, execute it with the "init" command.

    > jobqueue-sample-fibonacci sqlite3://fibonacci.sqlite3 test init

To run the state machine, execute it with the "run" command.

    > jobqueue-sample-fibonacci sqlite3://fibonacci.sqlite3 test run
    [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676221,23416728348467685,37889062373143906,61305790721611591,99194853094755497,160500643816367088,259695496911122585,420196140727489673,679891637638612258,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026,354224848179261915075]

