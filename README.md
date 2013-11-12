
Haskell JobQueue
================

JobQueue is a simple job queue library based on prioritized FCFS scheduling.

Prerequisites
-------------

### haskell-zookeeper-client package

haskell-zookeeper-client package is not registered in Hackage database, so please download it from https://github.com/motus/haskell-zookeeper-client.git and install it beforehand.

    > cd haskell-zookeeper-client/
    > cabal install

How to install
--------------

Just execute cabal install command as follows and the library and example programs will be put into your .cabal directory.

    > cd haskell-jobqueue/
    > cabal install

How to use
----------

### Backend

This library provides pluggable backend mechanism to switch the message queue systems used for storing jobs. You can select one of them depending on the requirement of your application. For example, if you need high availability, you may choose Zookeeper as a backend. If you want to make a standalone tool, sqlite3 is preferable.

* Zookeeper
* Sqlite3

Examples
--------

### Hello, World

"Hello" example shows an example of simple sequential state transition. It consists of just two states, "hello" step and "world" step.

To initalize the state machine, execute it with "init" command.

    > jobqueue-sample-hello sqlite3://hello.sqlite3 test init

To run the state machine, execute it with "run" command.

    > jobqueue-sample-hello sqlite3://hello.sqlite3 test run
    hello, world

If you want to use a Zookeeper cluster, you can specify a zookeeper locator like "zookeeper://localhost:2181/" instead of the sqlite3 locator.

### Fibonacci

"Fibonacci" example shows more complex case that has a loop in the state machine. It consists of initialization step and computation step.

To initalize the state machine, execute it with "init" command.

    > jobqueue-sample-fibonacci sqlite3://fibonacci.sqlite3 test init

To run the state machine, execute it with "run" command.

    > jobqueue-sample-fibonacci sqlite3://fibonacci.sqlite3 test run
    [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676221,23416728348467685,37889062373143906,61305790721611591,99194853094755497,160500643816367088,259695496911122585,420196140727489673,679891637638612258,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026,354224848179261915075]

