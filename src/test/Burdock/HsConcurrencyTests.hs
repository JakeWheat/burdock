
-- todo: once monitoring is working, see if this can be used
-- to remove most or all of the races which every so often
-- cause a test to fail when it shouldn't

{-# LANGUAGE LambdaCase #-}
module Burdock.HsConcurrencyTests
    (tests) where

import Burdock.HsConcurrency
    (runBConcurrency
    ,spawn
    ,spawnMonitor
    ,send
    ,receive
    --,receiveTimeout
    ,addr
    ,asyncExit

    ,openBConcurrency
    ,closeBConcurrency
    ,spawnExtWait

    ,Addr
    ,Inbox
    ,Down(..)
    ,ExitType(..)
    ,DynValException(..)
    ,extractDynValExceptionVal

    ,testAddToBuffer
    ,testReceiveWholeBuffer
    ,testMakeInbox
    ,testSend
    ,testCloseInbox
    ,testReceive
    ,ibaddr
    )

import Control.Concurrent
    (threadDelay)
import Control.Monad (void
                     ,forM_
                     ,when
                     --,join
                     )
import Data.Maybe (fromJust)

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

import Control.Exception.Safe
    (tryAsync
    ,SomeException
    ,displayException
    ,throwIO
    ,bracket
    )

import Data.List (isInfixOf)

import Control.Concurrent.Async (async,wait)

import Data.Dynamic (Dynamic
                    ,toDyn
                    ,fromDynamic
                    ,Typeable
                    )

import Control.Monad.STM
    (STM
    ,atomically)

import Control.Concurrent.STM.TChan
    (TChan
    ,newTChanIO
    --,readTChan
    ,tryReadTChan
    ,writeTChan
    )



--import Debug.Trace (trace)

tests :: T.TestTree
tests = T.testGroup "hs concurrency tests"
    [T.testGroup "inbox"
        [T.testGroup "basic"
            [inboxSimpleSendAndReceive
            ,inboxSimpleSendAndReceive0Timeout
            ,inboxSimpleReceiveWaitSend
            ,inboxSimpleReceiveWaitSendTimeoutGet
            ,inboxSimpleReceiveWaitSendTimeoutThenGet
            ,inboxSendAfterClose
            ]
            
        ,mainReceiveTests]
    ,T.testGroup "concurrency-api"
        [testSimpleSpawn
        ,testMainProcessReturnValue
        ,catchExceptionExample
        ,testMainProcessException
        ,checkWaitTwice
        ,testSendAfterClose
        ,testDaemonSimple
        ,testSpawnMonitorExitVal
        ,testSpawnMonitorException
        ,testSpawnMonitorTag
        ,testSpawnAsyncExit
        ,testTopProcessSelfAsync
        ,testTopParenticide
        ,testSpawnSelfAsync
        ,testIgnoreUnmonitoredFailure

        ,testSpawnExt
        ,testSelfAsyncExt
        ,testParenticideExt
        ,testRetExt
        ,testThrowExt
        ]
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
    b <- testMakeInbox
    testSend (ibaddr b) $ toDyn "test"
    x <- testReceive b Nothing (const True)
    assertEqual "" (Just "test") $ mtFromDyn x

{-
send a message
read with timeout 0
check the message
-}

inboxSimpleSendAndReceive0Timeout :: TestTree
inboxSimpleSendAndReceive0Timeout = T.testCase "inboxSimpleSendAndReceive0Timeout" $ do
    b <- testMakeInbox
    testSend (ibaddr b) $ toDyn "test"
    -- not sure if there's a possible race here
    -- may need a tweak if start getting intermittent failures
    -- if running lots of tests concurrently
    -- or on a very busy machine?
    x <- testReceive b (Just 0) (const True)
    assertEqual "" (Just "test") $ mtFromDyn x

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
    b <- testMakeInbox
    void $ async $ do
        threadDelay shortWait
        testSend (ibaddr b) $ toDyn "test"
    x <- testReceive b Nothing (const True)
    assertEqual "" (Just "test") $ mtFromDyn x


{-

read with timeout 1
send a message after 0.5
check the message
-}

inboxSimpleReceiveWaitSendTimeoutGet :: TestTree
inboxSimpleReceiveWaitSendTimeoutGet = T.testCase "inboxSimpleReceiveWaitSendTimeoutGet" $ do
    b <- testMakeInbox
    void $ async $ do
        threadDelay shortWait
        testSend (ibaddr b) $ toDyn "test"
    x <- testReceive b (Just (shortWait * 2)) (const True)
    assertEqual "" (Just "test") $ mtFromDyn x

{-

read with timeout 1
send a message after 1.5
check get timeout
  check the approx time to timeout
then check get message after 1.5
-}

inboxSimpleReceiveWaitSendTimeoutThenGet :: TestTree
inboxSimpleReceiveWaitSendTimeoutThenGet = T.testCase "inboxSimpleReceiveWaitSendTimeoutThenGet" $ do
    b <- testMakeInbox
    void $ async $ do
        threadDelay (shortWait * 2)
        testSend (ibaddr b) $ toDyn "test"
    x <- testReceive b (Just shortWait) (const True)
    assertEqual "" Nothing $ mtFromDyn x
    y <- testReceive b (Just $ shortWait * 2) (const True)
    -- todo: saw a rare race with this, how??
    assertEqual "" (Just "test") $ mtFromDyn y


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

receivePredDyn :: Dynamic -> Bool
receivePredDyn x =
    maybe (error $ "wrong type: " ++ show x) receivePred $ fromDynamic x


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
        ib <- testMakeInbox
        forM_ (startingBufferContents rt) (testAddToBuffer ib . toDyn)
        forM_ (startingChanContents rt) $ \m -> testSend (ibaddr ib) $ toDyn m
        m <- testReceive ib (Just 0) $ if useSelectiveReceive rt
                            then receivePredDyn
                            else const True
        case (m, receivesJust rt) of
            (Nothing,False) -> assertBool "" True
            (Just _ ,True) -> assertBool "" True
            -- todo: print this readably
            _ -> assertFailure $ ppShow (m, rt)
        finalBuf <- map tFromDyn <$> testReceiveWholeBuffer ib
        assertEqual "" (finalBufferContents rt) finalBuf
        finalChan <- map tFromDyn <$> flushInbox ib
        assertEqual "" (finalChanContents rt) finalChan
    runInfiniteTimeoutTest = when (receivesJust rt) $ do
        ib <- testMakeInbox
        forM_ (startingBufferContents rt) (testAddToBuffer ib . toDyn)
        forM_ (startingChanContents rt) $ \m -> testSend (ibaddr ib) $ toDyn m
        m <- testReceive ib Nothing $ if useSelectiveReceive rt
                            then receivePredDyn
                            else const True
        case (m, receivesJust rt) of
            (Nothing,False) -> assertBool "" True
            (Just _ ,True) -> assertBool "" True
            -- todo: print this readably
            _ -> assertFailure $ ppShow (m, rt)
        finalBuf <- map tFromDyn <$> testReceiveWholeBuffer ib
        assertEqual "" (finalBufferContents rt) finalBuf
        finalChan <- map tFromDyn <$> flushInbox ib
        assertEqual "" (finalChanContents rt) finalChan
    runFiniteTimeoutTest = do
        ib <- testMakeInbox
        forM_ (startingBufferContents rt) (testAddToBuffer ib . toDyn)
        forM_ (startingChanContents rt) $ \m -> testSend (ibaddr ib) $ toDyn m
        startTime <- getCurrentTime
        m <- mtFromDyn
            <$> testReceive ib (Just shortWait) (if useSelectiveReceive rt
                            then receivePredDyn
                            else const True)
        endTime <- getCurrentTime                                 
        case (m, receivesJust rt) of
            (Nothing,False) -> assertBool "" True
            (Just _ ,True) -> assertBool "" True
            -- todo: print this readably
            _ -> assertFailure $ ppShow (m, rt)
        finalBuf <- map tFromDyn <$> testReceiveWholeBuffer ib
        assertEqual "" (finalBufferContents rt) finalBuf
        finalChan <- map tFromDyn <$> flushInbox ib
        assertEqual "" (finalChanContents rt) finalChan
        -- todo: check the time elapsed if got nothing
        when (m == Nothing) $ do
            let elapsed = diffUTCTime endTime startTime
                s x = realToFrac x / 1000 / 1000
                expected = s shortWait
            assertBool ("timeout time: " ++ show expected ++ " ~= " ++ show elapsed)
                -- when shortWait == 0.001s, some of the actual times come in
                -- at just over 0.002s
                -- the current fudge factor is 2 * shortwait + 0.001
                -- I think 3 * shortwait is not correct if shortwait is changed
                -- maybe shortwait + 0.002 would be better?
                -- review the tests which use this value and see if can reduce
                -- reliance on these times in the testing for test robustness
                (elapsed - expected < s shortWait + 0.001)


flushInbox :: Inbox -> IO [Dynamic]
flushInbox ib = do
    x <- testReceive ib (Just 0) $ const True
    case x of
        Nothing -> pure []
        Just y -> (y:) <$> flushInbox ib


inboxSendAfterClose :: TestTree
inboxSendAfterClose = T.testCase "inboxSendAfterClose" $ do
    b <- testMakeInbox
    testSend (ibaddr b) $ toDyn "test"
    x <- testReceive b (Just 0) (const True)
    assertEqual "" (Just "test") $ mtFromDyn x
    testCloseInbox b
    checkException "tried to use closed queue" $ testSend (ibaddr b) $ toDyn "test"

------------------------------------------------------------------------------

-- concurrency api tests

{-

spawn a process, exchange messages with it by sending the return
address
-}

testSimpleSpawn :: T.TestTree
testSimpleSpawn = T.testCase "testSimpleSpawn" $
    void $ runBConcurrency mainThread
  where
    mainThread ib = do
        spaddr <- spawn ib subThread
        send ib spaddr (toDyn (addr ib, "testSimpleSpawn"))
        x <- receive ib $ const True
        let y :: Maybe (Addr, String) = fromDynamic x
        assertEqual "" (Just "hello testSimpleSpawn") $ fmap snd y
        pure $ toDyn ()
    subThread sib = do
        x <- receive sib (const True)
        case fromDynamic x :: Maybe (Addr, String) of
            Just (ret, msg) -> send sib ret $ toDyn (addr sib, "hello " ++ msg)
            _ -> error $ show x
        pure $ toDyn ()
{-

spawnMonitor a process
check its exit value via monitor

-}

testSpawnMonitorExitVal :: T.TestTree
testSpawnMonitorExitVal = T.testCase "testSpawnMonitorExitVal" $
    void $ runBConcurrency $ \ib -> do
        _spaddr <- spawnMonitor ib Nothing $ \_sib -> do
            pure $ toDyn "I am an exit value"
        x <- receive ib (const True)
        let (a,et,b) = fromJust $ fromDynamic x
            a' = fromJust $ fromDynamic a
            b' = fromJust $ fromDynamic b
        assertEqual "" Down a'
        assertEqual "" ExitValue et
        assertEqual "" "I am an exit value" b'
        pure $ toDyn ()
    
{-

spawnMonitor a process
throw an exception
check this exception value via monitor

-}

data MyVal = MyVal String
    deriving (Eq,Show)

testSpawnMonitorException :: T.TestTree
testSpawnMonitorException = T.testCase "testSpawnMonitorException" $ do
    void $ runBConcurrency $ \ib -> do
        _spaddr <- spawnMonitor ib Nothing $ \_sib ->
            throwIO $ DynValException $ toDyn $ MyVal "custom"
            -- pure $ toDyn () -- "I am an exit value"
        x <- receive ib (const True)
        let (a,et,b) = fromJust $ fromDynamic x
            a' = fromJust $ fromDynamic a
            b' = fromJust $ fromDynamic b
        assertEqual "" Down a'
        assertEqual "" ExitException et
        assertEqual "" (MyVal "custom") b'
        pure $ toDyn ()

{-
repeat both exit val tests with a custom monitor tag
-}

testSpawnMonitorTag :: T.TestTree
testSpawnMonitorTag = T.testCase "testSpawnMonitorTag" $
    void $ runBConcurrency $ \ib -> do
        _spaddr <- spawnMonitor ib (Just $ toDyn $ "tag-a") $ \_sib -> do
            pure $ toDyn "I am an exit value"
        _spaddr <- spawnMonitor ib (Just $ toDyn $ "tag-b") $ \sib -> do
            -- don't exit
            _ <- receive sib (const True)
            pure $ toDyn "I am an exit value"

        x <- receive ib (const True)
        let (a,et,b) = fromJust $ fromDynamic x
            a' = fromJust $ fromDynamic a
            b' = fromJust $ fromDynamic b
        assertEqual "" "tag-a" a'
        assertEqual "" ExitValue et
        assertEqual "" "I am an exit value" b'
        pure $ toDyn ()


{-

spawn a process
send it the kill message
get the exception value

-}


testSpawnAsyncExit :: T.TestTree
testSpawnAsyncExit = T.testCase "testSpawnAsyncExit" $
    void $ runBConcurrency $ \ib -> do

        -- slightly over the top paranoia
        canaryOne <- spawn ib $ \sib -> do
            x <- receive sib (const True)
            case fromDynamic x :: Maybe (Addr, String) of
                Just (ret, msg) -> send sib ret $ toDyn (addr sib, "canary one reporting " ++ msg)
                _ -> error $ show x
            pure $ toDyn ()

        -- the actual test bit
        spaddr <- spawnMonitor ib Nothing $ \sib -> do
            -- don't exit - no one will send it a message
            _ <- receive sib (const True)
            pure $ toDyn "I am an exit value"

        canaryTwo <- spawn ib $ \sib -> do
            x <- receive sib (const True)
            case fromDynamic x :: Maybe (Addr, String) of
                Just (ret, msg) -> send sib ret $ toDyn (addr sib, "canary two reporting " ++ msg)
                _ -> error $ show x
            pure $ toDyn ()

        threadDelay shortWait
        asyncExit ib spaddr $ toDyn "it's time"

        x <- receive ib (const True)
        let (a,et,b) = fromJust $ fromDynamic x
            a' = fromJust $ fromDynamic a
            b' = fromJust $ fromDynamic b
        assertEqual "" Down a'
        assertEqual "" ExitException et
        assertEqual "" "it's time" b'
        
        send ib canaryOne (toDyn (addr ib, "c1"))
        x1 <- receive ib $ const True
        let y1 :: Maybe (Addr, String) = fromDynamic x1
        assertEqual "" (Just "canary one reporting c1") $ fmap snd y1

        send ib canaryTwo (toDyn (addr ib, "c2"))
        x2 <- receive ib $ const True
        let y2 :: Maybe (Addr, String) = fromDynamic x2
        assertEqual "" (Just "canary two reporting c2") $ fmap snd y2

        pure $ toDyn ()

testTopProcessSelfAsync :: T.TestTree
testTopProcessSelfAsync = T.testCase "testTopProcessSelfAsync" $ do
    {-
should the exception coming out of runBConcurrency in this case be
asynchronous or not?
     -}
    (x :: Either SomeException Dynamic) <- tryAsync $ runBConcurrency $ \ib -> do
        asyncExit ib (addr ib) $ toDyn "bye"
        pure $ toDyn ()
    let v = fromJust $ extractDynValExceptionVal $ either id (error . show) x
    assertEqual "" "bye" v



testTopParenticide :: T.TestTree
testTopParenticide = T.testCase "testTopParenticide" $ do
    (x :: Either SomeException Dynamic) <- tryAsync $ runBConcurrency $ \ib -> do
        _ <- spawn ib $ \sib -> do
            asyncExit sib (addr ib) $ toDyn "zz"
            pure $ toDyn ()
        _ <- receive ib (const True)
        pure $ toDyn ()
    let v = fromJust $ extractDynValExceptionVal $ either id (error . show) x
    assertEqual "" "zz" v

testSpawnSelfAsync :: T.TestTree
testSpawnSelfAsync = T.testCase "testSpawnSelfAsync" $ do
    void $ runBConcurrency $ \ib -> do
        _r <- spawnMonitor ib Nothing $ \sib -> do
            asyncExit sib (addr sib) $ toDyn "audi"
            pure $ toDyn "I am an exit value"

        x <- receive ib (const True)
        let (a,et,b) = fromJust $ fromDynamic x
            a' = fromJust $ fromDynamic a
            b' = fromJust $ fromDynamic b
        assertEqual "" Down a'
        assertEqual "" ExitException et
        assertEqual "" "audi" b'
        
        pure $ toDyn ()

{-
check if a thread exits with exception or async exception, and
it's not monitored, it's ignored properly
-}
testIgnoreUnmonitoredFailure :: T.TestTree
testIgnoreUnmonitoredFailure = T.testCase "testIgnoreUnmonitoredFailure" $ do
    void $ runBConcurrency $ \ib -> do
        _ <- spawn ib $ \sib -> do
            asyncExit sib (addr sib) $ toDyn "audi"
            pure $ toDyn "I am an exit value"

        _ <- spawn ib $ \_sib -> do
            void $ throwIO $ DynValException $ toDyn "audi5000"
            pure $ toDyn "I am an exit value"

        -- not sure this is needed
        threadDelay shortWait
        assertBool "" True
        pure $ toDyn ()


{-

spawn a process
exchange a message with it
use monitor to verify the process is down
send another message to it
check you get an error immediately
  instead of it failing silently by going to a chan that no-one will
  ever read now
-}

testSendAfterClose :: T.TestTree
testSendAfterClose = T.testCase "testSendAfterClose" $
    void $ runBConcurrency $ \ib -> do
        spaddr <- spawn ib $ \sib -> do
            x <- receive sib $ const True
            case fromDynamic x of
                Just (ret, msg) -> send sib ret (toDyn ("hello " ++ msg))
                _ -> putStrLn ("bad message: " ++ show x)
            pure $ toDyn ()

        send ib spaddr (toDyn (addr ib, "testSimpleSpawn"))
        x <- receive ib $ const True
        let y = fromDynamic x
        assertEqual "" (Just "hello testSimpleSpawn") y
        -- todo: update using the monitor system
        threadDelay shortWait
        checkException "tried to use closed queue"
            $ send ib spaddr (toDyn (addr ib, "testSimpleSpawn"))
        pure $ toDyn ()
        
{-

check exiting the main process: check the exit value, exception, kill

-}

tFromDyn :: Typeable a => Dynamic -> a
tFromDyn a= maybe (error $ "wrong type of value" ++ show a) id $ fromDynamic a

mtFromDyn :: Maybe Dynamic -> Maybe String
mtFromDyn = fmap tFromDyn

testMainProcessReturnValue :: T.TestTree
testMainProcessReturnValue = T.testCase "testMainProcessReturnValue" $ do
    x <- runBConcurrency $ \_ib -> pure $ toDyn "retval"
    assertEqual "" "retval" $ tFromDyn x

catchExceptionExample :: T.TestTree
catchExceptionExample = T.testCase "catchExceptionExample" $
    checkException "test an error" $
        void $ error "test an error"

checkException :: String -> IO a -> IO ()
checkException msg f =
    tryAsync f >>= \case
        Left e -> chk e
        Right {} -> assertBool ("didn't throw: " ++ msg) False
  where
    chk :: SomeException -> IO ()
    chk e = assertBool ("exception didn't match: " ++ msg ++ "\n" ++ displayException e)
               (msg `isInfixOf` displayException e)

testMainProcessException :: T.TestTree
testMainProcessException = T.testCase "testMainProcessException" $
    checkException "an issue in the main process" runit
  where
    runit = void $ runBConcurrency $ \_ib ->
            error "an issue in the main process"

checkWaitTwice :: T.TestTree
checkWaitTwice = T.testCase "checkWaitTwice" $ do
    a1 <- async $ pure "retval"
    v <- wait a1
    assertEqual "" "retval" v
    v1 <- wait a1
    assertEqual "" "retval" v1

{-

testing the threads are daemon, and they exit:
start a sub thread
it will ping something outside the concurrency handle
  on a loop, every 0.01s or something
can probably just use an inbox for now? if it doesn't work
  use a plan tchan
exit the main thread
check control is immediately returned to the outside code
check the pinging stops after the exit succeeds

-}

flushChan :: TChan a -> STM [a]
flushChan ch = do
    x <- tryReadTChan ch
    case x of
        Nothing -> pure []
        Just i -> (i:) <$> flushChan ch

testDaemonSimple :: T.TestTree
testDaemonSimple = T.testCase "testDaemonSimple" $ do
    ch <- newTChanIO
    void $ runBConcurrency (mainThread ch)
    threadDelay shortWait
    _ <- atomically $ flushChan ch
    -- maybe this is overkill?
    threadDelay $ shortWait * 10
    x <- atomically $ flushChan ch
    assertEqual "no more pings from the subthread" [] x
  where
    mainThread ch ib = do
        spaddr <- spawn ib (subThread ch)
        send ib spaddr (toDyn (addr ib))
        _ <- receive ib $ const True
        pure $ toDyn ()
    subThread ch sib = do
        x <- receive sib (const True)
        case fromDynamic x of
            Just ret -> send sib ret $ toDyn (addr sib)
            _ -> error $ show x
        let loop :: IO ()
            loop = do
                --ping the chan
                atomically $ writeTChan ch True
                threadDelay shortWait
                loop
        loop
        pure $ toDyn ()

{-

other basic features to check:
link, spawn_link
local name registry
catalog:
  threads
  monitors
  links
  local name registry

-}

{-

testing the open/close api
----------------------------

-}

testSpawnExt :: T.TestTree
testSpawnExt = T.testCase "testSpawnExt" $ void $
    bracket openBConcurrency closeBConcurrency $ \oh ->
    spawnExtWait oh $ \ib -> do
        spaddr <- spawn ib subThread
        send ib spaddr (toDyn (addr ib, "testSimpleSpawn"))
        x <- receive ib $ const True
        let y :: Maybe (Addr, String) = fromDynamic x
        assertEqual "" (Just "hello testSimpleSpawn") $ fmap snd y
        pure $ toDyn ()
  where
    subThread sib = do
        x <- receive sib (const True)
        case fromDynamic x :: Maybe (Addr, String) of
            Just (ret, msg) -> send sib ret $ toDyn (addr sib, "hello " ++ msg)
            _ -> error $ show x
        pure $ toDyn ()

testSelfAsyncExt :: T.TestTree
testSelfAsyncExt = T.testCase "testSelfAsyncExt" $
    bracket openBConcurrency closeBConcurrency $ \oh -> do
        (x :: Either SomeException Dynamic) <-
            tryAsync $ spawnExtWait oh $ \ib -> do
            asyncExit ib (addr ib) $ toDyn "bye"
            pure $ toDyn ()
        let v = fromJust $ extractDynValExceptionVal $ either id (error . show) x
        assertEqual "" "bye" v

testParenticideExt :: T.TestTree
testParenticideExt = T.testCase "testParenticideExt" $
    bracket openBConcurrency closeBConcurrency $ \oh -> do
        (x :: Either SomeException Dynamic) <-
            tryAsync $ spawnExtWait oh $ \ib -> do
            _ <- spawn ib $ \sib -> do
                asyncExit sib (addr ib) $ toDyn "zz"
                pure $ toDyn ()
            _ <- receive ib (const True)
            pure $ toDyn ()            
        let v = fromJust $ extractDynValExceptionVal $ either id (error . show) x
        assertEqual "" "zz" v

testRetExt :: T.TestTree
testRetExt = T.testCase "testRetExt" $
    bracket openBConcurrency closeBConcurrency $ \oh -> do
        x <- spawnExtWait oh $ \_ib -> pure $ toDyn "hello"
        assertEqual "" (Just "hello") $ fromDynamic x

testThrowExt :: T.TestTree
testThrowExt = T.testCase "testThrowExt" $
    bracket openBConcurrency closeBConcurrency $ \oh -> do
        (x :: Either SomeException Dynamic) <-
            tryAsync $ spawnExtWait oh $ \_ib -> do
            void $ throwIO $ DynValException $ toDyn $ MyVal "stuff"
            pure $ toDyn ()            
        let v = fromJust $ extractDynValExceptionVal $ either id (error . show) x
        assertEqual "" (MyVal "stuff") v

{-

create a background thread, have it monitor the api call thread, show
it getting the exit message
-}

{-

create a background thread, monitor it from the api call, exit the api call
  show that the monitor entry has disappeared
  make another api call, tell the background thread to exit
-}
{-

create a background thread
  then contact it in a fresh api call
-}
{-

create a background thread, see it pinging after exiting the api call
  then close the handle
  and see that the background thread exited - it stops pinging
  the equiv of testdaemonsimple
-}
{-


concurrency accessing the api:
do a spawnextwait, it will block
then closeBConcurrency in another thread
see the spawnextwait produce an exception which indicates this is what
  happened -> maybe create a specific exception or value for this role
  since it's part of the api now?


-}
