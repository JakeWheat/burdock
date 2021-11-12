

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
import Control.Monad (void)

import Test.Tasty as T
import Test.Tasty.HUnit as T

tests :: T.TestTree
tests = T.testGroup "hs occasional tests"
    [inboxSimpleSendAndReceive
    ,inboxSimpleSendAndReceive0Timeout
    ,inboxSimpleReceiveWaitSend
    ,inboxSimpleReceiveWaitSendTimeoutGet
    ,inboxSimpleReceiveWaitSendTimeoutThenGet
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
    assertEqual "" (Just "test") y


{-

------------------------------------------------------------------------------

tests for selective receive including checking the timeouts

maybe the selective receive buffering with timeout is a good candidate
to try to do some TLA? It's simple, but there's room to mess it up

-}
{-

put message
selective receive to get it
check it
-}

{-
put message
selective receive doesn't match
check it
selective receive does match
-}

{-
put n messages in order
get them in order
put n messages in order
selective receive one by one in reverse order
-}

{-
check buffered messages match
drain the queue and check it
-}

{-
check buffered messages don't match
drain the queue and check it
-}

{-
skip then match a message being posted after get is called
-}

{-
skip then match messages that are already sitting in the bufffer
-}

{-
arrange so will skip a message, get one
then get another
  post a non matching message after wait
  then post a matching message after another wait
check the get
then drain the queue and check
-}

{-
do a timeout
  have 3 non matching messages posted during the timeout
  then one after the timeout
  check it times out
    check how long it took to time out
    (add these checks everywhere where there's a timeout)
  check after wait that the same get with 0 timeout succeeds
  drain the rest of the queue and check
-}

{-
do a timeout
  have 3 non matching messages posted during the timeout
  then a matching before it times out
  check this message
  drain the rest of the queue and cehck
-}

{-
do get with timeout
  post 1 non matching message in the middle
  check it timed out
loop:
  do another get with timeout
    post a message that doesn't match, but did match the previous
    get which already timed out in the middle
  check it timed out
then do a get with timeout
  in the middle, post a matching message
then drain the queue and check

-}
