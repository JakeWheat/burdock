

module Burdock.HsOccasionalTests
    (tests) where

import Burdock.Occasional
    (makeInbox
    ,send
    ,receive
    )

import Control.Concurrent
    (threadDelay
    ,forkIO)
import Control.Monad (void
                     ,forM_)
import Data.Maybe (mapMaybe)

import Text.Show.Pretty (ppShow)

import Test.Tasty as T
import Test.Tasty.HUnit as T

tests :: T.TestTree
tests = T.testGroup "hs occasional tests"
    [inboxSimpleSendAndReceive
    ,inboxSimpleSendAndReceive0Timeout
    ,inboxSimpleReceiveWaitSend
    ,inboxSimpleReceiveWaitSendTimeoutGet
    ,inboxSimpleReceiveWaitSendTimeoutThenGet
    ,mainReceiveTests
    ]

------------------------------------------------------------------------------

-- simple sanity tests, mainly to check understood the behaviour of
-- the underlying primitive - currently TChan, could switch to TQueue,
-- later have to figure out how to one for other processes, and other
-- machines - if it uses generic sockets, then these will be the same
-- I think the types of the inbox and the send and received must be different
-- for the inprocess and non inprocess. The burdock interpreter can
-- hide this difference for burdock users


{-

create an inbox
send it a message
read the message out and check it
-}

inboxSimpleSendAndReceive :: TestTree
inboxSimpleSendAndReceive = T.testCase "inboxSimpleSendAndReceive" $ do
    b <- makeInbox
    send b "test"
    x <- receive b (-1) (const True)
    assertEqual "" (Just "test") x

{-
send a message
read with timeout 0
check the message
-}

inboxSimpleSendAndReceive0Timeout :: TestTree
inboxSimpleSendAndReceive0Timeout = T.testCase "inboxSimpleSendAndReceive0Timeout" $ do
    b <- makeInbox
    send b "test"
    -- not sure if there's a possible race here
    -- may need a tweak if start getting intermittent failures
    -- if running lots of tests concurrently
    -- or on a very busy machine?
    x <- receive b 0 (const True)
    assertEqual "" (Just "test") x

{-
read inbox
sleep 10
send a message
check the message
-}


-- short wait: hopefully more than long enough to mask write then read
-- races, but not so long it makes the tests take forever
-- currently set to 1ms
shortWait :: Int
shortWait = 1000

inboxSimpleReceiveWaitSend :: TestTree
inboxSimpleReceiveWaitSend = T.testCase "inboxSimpleReceiveWaitSend" $ do
    b <- makeInbox
    void $ forkIO $ do
        threadDelay shortWait
        send b "test"
    x <- receive b (-1) (const True)
    assertEqual "" (Just "test") x


{-

read with timeout 1
send a message after 0.5
check the message
-}

inboxSimpleReceiveWaitSendTimeoutGet :: TestTree
inboxSimpleReceiveWaitSendTimeoutGet = T.testCase "inboxSimpleReceiveWaitSendTimeoutGet" $ do
    b <- makeInbox
    void $ forkIO $ do
        threadDelay shortWait
        send b "test"
    x <- receive b (shortWait * 2) (const True)
    assertEqual "" (Just "test") x

{-

read with timeout 1
send a message after 1.5
check get timeout
  check the approx time to timeout
then check get message after 1.5
-}

inboxSimpleReceiveWaitSendTimeoutThenGet :: TestTree
inboxSimpleReceiveWaitSendTimeoutThenGet = T.testCase "inboxSimpleReceiveWaitSendTimeoutThenGet" $ do
    b <- makeInbox
    void $ forkIO $ do
        threadDelay (shortWait * 2)
        send b "test"
    x <- receive b shortWait (const True)
    assertEqual "" Nothing x
    y <- receive b (shortWait * 2) (const True)
    -- todo: saw a rare race with this, how??
    assertEqual "" (Just "test") y


{-

------------------------------------------------------------------------------

tests for selective receive including checking the timeouts

whitebox state transitions

+ some extra to do with timeouts
-> make sure it timesout when messages come in too late
+ doesn't timeout when they come in on time
+ timeout is accurate when there are incoming messages that
  don't match the predicate

the state is:
content of the buffer
content of the tchan
each has matching and non matching item options
0,1,2 of each
state transitions:
always prime with a receive
  this can be selective, and can have a timeout
then optionally a message will come in before the timeout
  or after the timeout
  -> race issues

buffer is
0,1,2 matching messages
0,1,2 non matching messages
chan contents are:
0,1,2 matching messages
0,1,2 non matching messages
receive is:
  matching, non matching
  -1,0,X timeout
action is:
  post matching message before timeout, or after timeout
guard is:
  if there are no matching messages in the buffer or chan,
    and there won't be a matching messages posted before the timeout
  -> reject this test, don't run it because it should hang forever

what to check:
1. does receive timeout or return a value
2. how long it took to get a value or timeout
3. what's left in the buffer and tchan


TODO: extend: number the matching messages in each test uniquely
check if do get a message, it's the right one,
and that the remaining messages in the buffer and chan are in the
right order too

-}


mainReceiveTests :: T.TestTree
mainReceiveTests = T.testGroup "mainReceiveTests" $ map runTest generateTests

data ReceiveTest
    = ReceiveTest
    { -- setup
     numChanMatching :: Int
     -- results
    ,finishes :: Bool
    }
    deriving Show

{-
some steps:
add some non matching
plus the receive matching option
-> needs to check if a test is valid now, some generated will timeout

preseed the buffer too
-> test that this works
  use a testing method that gets the buffer

add timeouts

add posting a message after calling receive

measure the time to run is expected

check what's left in the buffer and chan

check the right message is received and the right messages are in the
buffer and chan in the right order


-}

generateTests :: [ReceiveTest]
generateTests = mapMaybe mk cands
  where
    cands = [n | n <- [0,1,2]]
    mk n = Just $ ReceiveTest
           {numChanMatching = n
           ,finishes = n > 0}

runTest :: ReceiveTest -> T.TestTree
runTest rt = T.testCase (ppShow rt) $ do
    ib <- makeInbox
    forM_ [0..numChanMatching rt - 1] $ \n -> send ib n
    r <- receive ib 0 (const True)
    case (r, finishes rt) of
        (Nothing,False) -> assertBool "" True
        (Just _ ,True) -> assertBool "" True
        -- todo: print this readably
        _ -> assertFailure $ ppShow (r, finishes rt)
    


{-


then test the timeout is stable even when lots of non matching
messages are coming in, two tests:
have non matching items in the buffer?
use a timeout
post lots of non matching messages at different times/delays
then either post a matching message sometime before the timeout
  or just after (enough to avoid races, is this feasible?)
check it either timesout or succeeds
  check the time in both cases: that it succeeds within the timeout
    (a sanity check), or that it timedout close to the exact
  timeout time

-}

------------------------------------------------------------------------------

-- spawn monitor

{-

spawn a process, exchange messages with it by sending the return
address

-}

{-

spawn a process
get the exit value it returns
check it's exited?

-}

{-

spawn a process
throw an exception
get this exception value
check it's exited?

-}

{-

spawn a process
send it the kill message
get the exception value
check it's exited?

-}


{-

check exiting the main process: check the exit value, exception, kill

-}

{-

spawn some processes
exit the main process
check it's prompt, and the spawned processes are all gone

-}


{-

spawn some processes
make one of them non daemon
exit the main process
check it waits for that one process to exit
then check it prompty exits everything and returns

-}
