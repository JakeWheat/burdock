

module Burdock.HsOccasionalTests
    (tests) where

import Burdock.Occasional
    (makeInbox
    ,send
    ,receive
    ,addr

    ,Inbox
    --,Addr
    
    ,testAddToBuffer
    ,testReceiveWholeBuffer
    )

import Control.Concurrent
    (threadDelay
    ,forkIO)
import Control.Monad (void
                     ,forM_
                     )
--import Data.Maybe (mapMaybe)

import Text.Show.Pretty (ppShow)

import Test.Tasty as T
import Test.Tasty.HUnit as T

import Control.Monad.State
    (State
    ,get
    ,put
    ,evalState
    )

--import Debug.Trace (trace)

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
    send (addr b) "test"
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
    send (addr b) "test"
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
        send (addr b) "test"
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
        send (addr b) "test"
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
        send (addr b) "test"
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


data TestMessage = Matching Int
                 | NotMatching Int
    deriving (Eq,Show)

receivePred :: TestMessage -> Bool
receivePred (Matching {}) = True
receivePred (NotMatching {}) = False

data NoTimeoutReceiveTest
    = NoTimeoutReceiveTest
    { -- setup
     startingChanContents :: [TestMessage]
    ,startingBufferContents :: [TestMessage]
    ,useSelectiveReceive
     -- results
    ,succeedsInGettingAMessage :: Bool
    ,finalChanContents :: [TestMessage]
    ,finalBufferContents :: [TestMessage]
    }
    deriving Show

{-
some steps:

phase 1:
timeout is always 0
add selective receive
ability to preseed the buffer
then have tests which return no matches
get match from the buffer
get match from the tchan
buffer some messages before getting from the tchan
buffer some message before returning no matches
-> test you get the right message
check the content of the buffer and chan is right and in the expected order too

change num chan matching to list of message in the chan
and a list in the buffer at the start
check the message you get, check it's the right one, or not matching
then check the buffer and chan contents are what they should be

todo: add some sanity checks which read multiple messages mixed with
buffering, I think they're not strictly needed iff the state tests are
correctly implemented, but it's a double check that might catch some
situations if there's a mistake in those tests:
drain all tests:
generate 0,1,2 messages of each type in the buf and chan
then read matching until get nothing, and check this list is what's expected
then read const true until get nothing, and check this list is what's expected


then do tests with timeouts
this includes all the tests above
  they should behave the same, apart from taking the timeout time before failing
  -> test all the above + how long the receive call took to return matches
     expectations
  every test that should have a receive value/not a timeout,
    also do with infinite timeout

then do tests with timeouts that have a message posted to the chan
  after half the timeout time which may match or not match
    (some will timeout, some won't)

then do the tests where multiple messages are posted in the timeout
duration, sometimes one matches, sometimes none

-}

mkMsg :: Bool -> State Int TestMessage
mkMsg b = do
    i <- get
    put (i + 1)
    pure $ if b
           then Matching i
           else NotMatching i

mkMsgs :: Num s => State s a -> a
mkMsgs f = evalState f 0

generateTests :: [NoTimeoutReceiveTest]
generateTests = map mk cands
  where
    messageListOptions =
        [] ++ [[True]] ++ [[False]]
        ++ [[a,b]| a <- [True,False], b <- [True,False]]
        ++ [[a,b,c]| a <- [True,False]
                   , b <- [True,False]
                   , c <- [True,False]
                   ]
        
    cands = [(sc,sb,usr)  | sc <- messageListOptions
                          , sb <- messageListOptions
                          , usr <- [True,False]]
    takeMatch usr b = let (p,x,s) = takeMatchSplit usr [] b
                      in  (x,p ++ s)
    -- takeMatchSplit _ a b | trace ("takeMatchSplit:" ++ show (a,b)) False = undefined
    takeMatchSplit _ p [] = (reverse p,Nothing,[])
    takeMatchSplit usr p (x:xs)
        | not usr || receivePred x
        = --let yy =
              (reverse p, Just x, xs)
          --in trace ("tmsret: " ++ show yy) yy
        | otherwise
        = takeMatchSplit usr (x:p) xs
    finalBufChan sb sc usr =
        let (x, finalBufStart) = takeMatch usr sb
        in case x of
                Just _ -> (finalBufStart, sc)
                Nothing ->
                    let (p,_,s) = takeMatchSplit usr [] sc
                    in (finalBufStart ++ p, s)
    mk (sc,sb,usr) =
        let (sbc,scc) =
                mkMsgs ((,) <$> mapM mkMsg sb <*> mapM mkMsg sc)
            succeeds = not usr || or (sc ++ sb)
            (fb,fc) = finalBufChan sbc scc usr
        in NoTimeoutReceiveTest
           {startingChanContents = scc 
           ,startingBufferContents = sbc
           ,useSelectiveReceive = usr
           -- results
           ,succeedsInGettingAMessage = succeeds
           ,finalChanContents = fc
           ,finalBufferContents = fb
           }

runTest :: NoTimeoutReceiveTest -> T.TestTree
-- todo: create a more readable test name?
runTest rt = T.testCase (ppShow rt) $ do
    ib <- makeInbox
    forM_ (startingBufferContents rt) $ testAddToBuffer ib 
    forM_ (startingChanContents rt) $ \m -> send (addr ib) m
    m <- receive ib 0 $ if useSelectiveReceive rt
                        then receivePred
                        else const True
    case (m, succeedsInGettingAMessage rt) of
        (Nothing,False) -> assertBool "" True
        (Just _ ,True) -> assertBool "" True
        -- todo: print this readably
        _ -> assertFailure $ ppShow (m, rt)
    finalBuf <- testReceiveWholeBuffer ib
    assertEqual "" (finalBufferContents rt) finalBuf
    finalChan <- flushInbox ib
    assertEqual "" (finalChanContents rt) finalChan

flushInbox :: Inbox a -> IO [a]
flushInbox ib = do
    x <- receive ib 0 $ const True
    case x of
        Nothing -> pure []
        Just y -> (y:) <$> flushInbox ib

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
