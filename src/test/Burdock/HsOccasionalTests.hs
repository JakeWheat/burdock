

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

    ,spawn
    )

import Control.Concurrent
    (threadDelay
    ,forkIO)
import Control.Monad (void
                     ,forM_
                     ,when
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

import Data.Time.Clock (getCurrentTime
                       ,diffUTCTime
                       )


--import Debug.Trace (trace)

tests :: T.TestTree
tests = T.testGroup "hs occasional tests"
    [inboxSimpleSendAndReceive
    ,inboxSimpleSendAndReceive0Timeout
    ,inboxSimpleReceiveWaitSend
    ,inboxSimpleReceiveWaitSendTimeoutGet
    ,inboxSimpleReceiveWaitSendTimeoutThenGet
    --,mainReceiveTests
    ,testSimpleSpawn
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
-- todo: change this type in the interface to a fixed width type
-- so it's in seconds + supports fractions
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

mostly whitebox state transition based

the main aspects on a test run:
calling receive once on most of the tests
is the timeout infinity, n, or 0
is it a selective receive or not
then, at the start of the test:
  what messages are already in the buffer, already in the chan
then after checking the receive value
check what's now in the buffer and the chan

-}


mainReceiveTests :: T.TestTree
mainReceiveTests = T.testGroup "mainReceiveTests" $ zipWith runTest [0..] generateTests


data TestMessage = Matching Int
                 | NotMatching Int
    deriving (Eq,Show)

receivePred :: TestMessage -> Bool
receivePred (Matching {}) = True
receivePred (NotMatching {}) = False

-- tests which don't send something to the inbox
-- after receive has been called
data NoSendReceiveTest
    = NoSendReceiveTest    { -- setup
     startingChanContents :: [TestMessage]
    ,startingBufferContents :: [TestMessage]
    ,useSelectiveReceive
     -- results
    ,receivesJust :: Bool
    ,finalChanContents :: [TestMessage]
    ,finalBufferContents :: [TestMessage]
    }
    deriving Show

{-
some steps:

test without timeout:
test a combination of different messages in the buffer and chan for the inbox
with and without selective receive, with the timeout 0
it checks if receive correctly returns a Just or not,
and the remaining contents of the buf and chan are correct and in the right order

test with infinite timeout:
the above tests should repeat and work the same
-> do each test at the same time

test with timeout 1 part 1:
the above tests with timeout 1
don't eliminate the tests which won't return
check that these return nothing, and that the time spent is about the
timeout time
do these tests at the same time too

----

then do all the tests above
but also add a variation, for each test that times out above
  it will repeat, but posting a message half way through the timeout
    it will do two versions: a matching and a not matching message
  check the result, and what's in the buf, chan
  + the time it took

then for all these, if it expects a result and not a timeout, repeat
with infinite timeout

add some sanity checks which read multiple messages mixed with
buffering, I think they're not strictly needed iff the state tests are
correctly implemented, but it's a double check that might catch some
situations if there's a mistake in those tests:
generate 0,1,2 messages of each type in the buf and chan
then read matching until get nothing, and check this list is what's expected
then read const true until get nothing, and check this list is what's expected

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

generateTests :: [NoSendReceiveTest]
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
        in NoSendReceiveTest
           {startingChanContents = scc 
           ,startingBufferContents = sbc
           ,useSelectiveReceive = usr
           -- results
           ,receivesJust = succeeds
           ,finalChanContents = fc
           ,finalBufferContents = fb
           }

runTest :: Int -> NoSendReceiveTest -> T.TestTree
-- todo: create a more readable test name?
runTest n rt = T.testCase ("Receive test " ++ show n) $ do
    runTimeout0Test
    runInfiniteTimeoutTest
    runFiniteTimeoutTest
  where
    runTimeout0Test = do
        ib <- makeInbox
        forM_ (startingBufferContents rt) $ testAddToBuffer ib 
        forM_ (startingChanContents rt) $ \m -> send (addr ib) m
        m <- receive ib 0 $ if useSelectiveReceive rt
                            then receivePred
                            else const True
        case (m, receivesJust rt) of
            (Nothing,False) -> assertBool "" True
            (Just _ ,True) -> assertBool "" True
            -- todo: print this readably
            _ -> assertFailure $ ppShow (m, rt)
        finalBuf <- testReceiveWholeBuffer ib
        assertEqual "" (finalBufferContents rt) finalBuf
        finalChan <- flushInbox ib
        assertEqual "" (finalChanContents rt) finalChan
    runInfiniteTimeoutTest = when (receivesJust rt) $ do
        ib <- makeInbox
        forM_ (startingBufferContents rt) $ testAddToBuffer ib 
        forM_ (startingChanContents rt) $ \m -> send (addr ib) m
        m <- receive ib (-1) $ if useSelectiveReceive rt
                            then receivePred
                            else const True
        case (m, receivesJust rt) of
            (Nothing,False) -> assertBool "" True
            (Just _ ,True) -> assertBool "" True
            -- todo: print this readably
            _ -> assertFailure $ ppShow (m, rt)
        finalBuf <- testReceiveWholeBuffer ib
        assertEqual "" (finalBufferContents rt) finalBuf
        finalChan <- flushInbox ib
        assertEqual "" (finalChanContents rt) finalChan
    runFiniteTimeoutTest = do
        ib <- makeInbox
        forM_ (startingBufferContents rt) $ testAddToBuffer ib 
        forM_ (startingChanContents rt) $ \m -> send (addr ib) m
        startTime <- getCurrentTime
        m <- receive ib shortWait $ if useSelectiveReceive rt
                            then receivePred
                            else const True
        endTime <- getCurrentTime                                 
        case (m, receivesJust rt) of
            (Nothing,False) -> assertBool "" True
            (Just _ ,True) -> assertBool "" True
            -- todo: print this readably
            _ -> assertFailure $ ppShow (m, rt)
        finalBuf <- testReceiveWholeBuffer ib
        assertEqual "" (finalBufferContents rt) finalBuf
        finalChan <- flushInbox ib
        assertEqual "" (finalChanContents rt) finalChan
        -- todo: check the time elapsed if got nothing
        when (m == Nothing) $ do
            let elapsed = diffUTCTime endTime startTime
                s x = realToFrac x / 1000 / 1000
                expected = s shortWait
            assertBool ("timeout time: " ++ show expected ++ " ~= " ++ show elapsed)
                -- when shortWait == 0.001s, some of the actual times come in
                -- at just over 0.002s
                (elapsed - expected < s shortWait + 0.001)


flushInbox :: Inbox a -> IO [a]
flushInbox ib = do
    x <- receive ib 0 $ const True
    case x of
        Nothing -> pure []
        Just y -> (y:) <$> flushInbox ib

------------------------------------------------------------------------------

-- spawn monitor

{-

spawn a process, exchange messages with it by sending the return
address

-}

testSimpleSpawn :: T.TestTree
testSimpleSpawn = T.testCase "testSimpleSpawn" $ do
    ib <- makeInbox
    spaddr <- spawn $ \sib -> do
        x <- receive sib (-1) $ const True
        case x of
            Just (ret, msg) -> send ret ("hello " ++ msg)
                 -- putStrLn temp until monitoring stuff works
            _ -> putStrLn ("bad message: " ++ show x)
                 -- >> error ("bad message: " ++ show x)
                 --error "bad message"
    send spaddr (addr ib, "testSimpleSpawn")
    x <- receive ib (-1) $ const True
    assertEqual "" (Just "hello testSimpleSpawn") x

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
