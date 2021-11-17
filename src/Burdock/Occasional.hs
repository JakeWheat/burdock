
{-

Haskell "Occasional" library
an approach to concurrency modelled on a subset of erlang

It only exists to help implement concurrency in Burdock

Plan:

inbox
send and receive messages
typed only for now, since burdock only needs to be able to send a
single type

timeouts
selective receive

there's no implicit self in the haskell layer - too much distraction
to implement something not needed

spawn monitor

runs a function in a new ghc thread which takes an inbox which is
created for the thread
send and receive with the new spawn, sending self address

-}

{-# LANGUAGE MultiWayIf #-}
module Burdock.Occasional
    (runOccasional
    ,spawn
    ,send
    ,receive
    ,addr

    ,Handle
    ,Addr
    ,Inbox

    ,testAddToBuffer
    ,testReceiveWholeBuffer
    ,testMakeInbox
    ,testSend
    ,testCloseInbox
    ,testReceive
    ,ibaddr

    ) where

import System.Timeout (timeout)

import Control.Exception (AssertionFailed(..))
import Control.Exception.Safe (bracket)

import Control.Monad.STM
    (STM
    ,atomically
    ,throwSTM
    )

import Control.Concurrent.STM.TQueue
    (TQueue
    ,newTQueue
    ,readTQueue
    ,tryReadTQueue
    ,writeTQueue
    )

import Control.Concurrent.STM.TVar
    (TVar
    ,newTVar
    ,readTVar
    ,writeTVar
    ,modifyTVar
    ,newTVarIO
    )

import Control.Concurrent.STM.TMVar

import Data.Time.Clock (getCurrentTime
                       ,diffUTCTime
                       ,UTCTime
                       )

import Control.Concurrent
    (myThreadId
    ,ThreadId
    )

import Control.Monad
    ({-void
    ,-}when
    ,unless
    )

import Control.Concurrent.Async
    (async
    --,wait
    --,withAsync
    ,cancel
    ,Async
    )

--import Debug.Trace (traceM, trace)

------------------------------------------------------------------------------

-- types

data Addr a =
    Addr {unAddr :: CQueue a}

-- not sure how to make this better
-- for burdock want erlang style pids which are unique
-- I don't see an easy way to do this in haskell
-- avoiding the show instance makes all sorts of debugging really
-- tedious. maybe some helper functions can be used if messages
-- follow a pattern
instance Show (Addr a) where
    show (Addr {}) = "Addr"

data Handle a
    = Handle
      {inbox :: Inbox a
      ,_threads :: TVar [Async a]
      }

addr :: Handle a -> Addr a
addr = ibaddr . inbox

data Inbox a
    = Inbox
    {ibaddr :: Addr a
    ,_buffer :: TVar [a]
    -- used to check that inboxes are only used in the thread they are created in
    -- todo: apparently this has some not perfect interaction with the garbage
    -- collection of exited threads, so use show on the thread id instead
    -- of saving the ThreadId itself or something
    ,_threadId :: ThreadId
    }


------------------------------------------------------------------------------

-- api functions

runOccasional :: (Handle a -> IO b) -> IO b
runOccasional f = do
    runningThreads <- newTVarIO []
    ib <- makeInbox
    let h = Handle ib runningThreads
    bracket (pure runningThreads) cancelRunningThreads (\_ -> f h)
  where
    cancelRunningThreads rps = do
        ts <- atomically $ readTVar rps
        mapM_ cancel ts

{-

running the main thread:
it sets up the list of running threads
this will be shared between all threads in the occasional instance
when the thread exits, it will cancel all the running threads
issues to follow up on:
add a timeout to this, and then return to user in special way if
any threads didn't exit in the deadline
cancel on an async thread is synchronous, so it may be less quick
use throwTo without the wait to do it concurrently
cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
  then wait on all the thread handles to see they all exited

there's no obvious timeout on cancel or the wait variations:
  maybe use regular io timeout function with stm wait variations?

-}

spawn :: (Handle a) -> (Handle a -> IO a) -> IO (Addr a)
spawn (Handle (Inbox _ _ ibtid) rts) f = do
    assertMyThreadIdIs ibtid
    ch <- newCQueueIO
    -- register the async object in the running thread list
    -- then clean it up in thread when the thread is about to exit
    -- if the thread hasn't exited, when the occasional instance
    -- exits, it needs to call cancel or throwto on the threads
    -- still running -> so this means it needs the async
    -- but the thread itself doesn't know it's async
    -- so who adds the async? and how does the thread remove it's
    -- own async when exiting
    -- the calling thread can pass the async into the running thread
    -- this seems fairly robust and simple
    asyncOnlyCh <- newCQueueIO
    ah <- async $
        let st = atomically $ do
                -- register the async in the handle runningthreads
                ah <- readCQueue asyncOnlyCh
                pure ()
                modifyTVar rts (ah:)
                pure ah
            en ah = atomically $ do
                -- remove the async from the handle runningthreads
                modifyTVar rts (\a -> filter (/= ah) a)
        in bracket st en $ const $ do
            ib <- makeInboxFromCQueue ch
            x <- f (Handle ib rts)
            atomically $ closeCQueue $ unAddr $ ibaddr ib
            pure x
    atomically $ writeCQueue asyncOnlyCh ah
    pure (Addr ch)
  where


send :: (Handle b) -> Addr a -> a -> IO ()
send (Handle (Inbox _ _ ibtid) _) (Addr tgt) v = do
    assertMyThreadIdIs ibtid
    atomically $ writeCQueue tgt v


receive :: --Show a =>
           Handle a
        -> Int -- timeout in microseconds
        -> (a -> Bool) -- accept function for selective receive
        -> IO (Maybe a)
receive (Handle ib _) tme f = receiveInbox ib tme f

---------------------------------------

-- testing only functions, for whitebox and component testing

testAddToBuffer :: Inbox a -> a -> IO ()
testAddToBuffer (Inbox _ buf _) a = atomically $ do
    modifyTVar buf $ (++ [a])

testReceiveWholeBuffer :: Inbox a -> IO [a]
testReceiveWholeBuffer (Inbox _ buf _) = atomically $ do
    x <- readTVar buf
    writeTVar buf []
    pure x

testMakeInbox :: IO (Inbox a)
testMakeInbox = makeInbox

-- send without the local inbox for inbox testing
testSend :: Addr a -> a -> IO ()
testSend (Addr tgt) v = atomically $ writeCQueue tgt v

testCloseInbox :: Inbox a -> IO ()
testCloseInbox ib = atomically $ closeCQueue $ unAddr $ ibaddr ib

testReceive :: Inbox a
            -> Int -- timeout in microseconds
            -> (a -> Bool) -- accept function for selective receive
            -> IO (Maybe a)
testReceive = receiveInbox

------------------------------------------------------------------------------

-- internal components

{-

a queue that can be closed:

queues in this system are each associated with a single thread that reads from them
they are passed around to other threads liberally which can write to them
we want an immediate error if the thread that reads a queues has exited
so: add a close flag to the queue which is checked when you write to it
  (and the other ops for robustness)
this should

-}

data CQueue a
    = CQueue
    {queue :: (TQueue a)
    ,isOpen :: (TVar Bool)}

newCQueue :: STM (CQueue a)
newCQueue = CQueue <$> newTQueue <*> newTVar True

newCQueueIO :: IO (CQueue a)
newCQueueIO = atomically (CQueue <$> newTQueue <*> newTVar True)

checkIsOpen :: CQueue a -> STM ()
checkIsOpen q = do
    o <- readTVar $ isOpen q
    -- come back later and figure out the exception types
    unless o $ throwSTM $ AssertionFailed "tried to use closed queue"

-- todo: could also check that this read call is only used in the
-- thread that owns the queue
readCQueue :: CQueue a -> STM a
readCQueue q = do
    checkIsOpen q
    readTQueue $ queue q

tryReadCQueue :: CQueue a -> STM (Maybe a)
tryReadCQueue q = do
    checkIsOpen q
    tryReadTQueue $ queue q

writeCQueue :: CQueue a -> a -> STM ()
writeCQueue q v = do
    checkIsOpen q
    writeTQueue (queue q) v

closeCQueue :: CQueue a -> STM ()
closeCQueue q = do
    checkIsOpen q
    writeTVar (isOpen q) False

-- receive support

receiveInbox :: Inbox a -> Int -> (a -> Bool) -> IO (Maybe a)
receiveInbox ib@(Inbox (Addr ch) buf ibtid) tme f = do
    assertMyThreadIdIs ibtid
    startTime <- getCurrentTime
    mres <- atomically $ do
        b <- readTVar buf
        --traceM $ "buf at start of receive: " ++ show b
        (newBuf, mres) <- flushInboxForMatch b [] ch f
        writeTVar buf $ newBuf
        pure mres
    case mres of
        Just {} -> pure mres
        Nothing | tme == 0 -> pure mres
                | otherwise -> receiveNoBuffered ib tme f startTime


-- the part of receive that checks the buffer for matching messages
flushInboxForMatch :: --Show a =>
                      [a]
                   -> [a]
                   -> (CQueue a)
                   -> (a -> Bool)
                   -> STM ([a], Maybe a)
flushInboxForMatch buf bdm inQueue prd = go1 buf bdm
  where
    --go1 a b | trace ("go1: " ++ show (a,b)) $ False = undefined
    go1 (x:xs) bufferDidntMatch
        | prd x
        = --trace ("match: " ++ show x) $
          pure (reverse bufferDidntMatch ++ xs, Just x)
        | otherwise
        = go1 xs (x:bufferDidntMatch)
    go1 [] bufferDidntMatch = do
        x <- tryReadCQueue inQueue
        case x of
            Nothing -> pure (reverse bufferDidntMatch, x)
            Just y | prd y -> pure (reverse bufferDidntMatch, x)
                   | otherwise -> go1 [] (y:bufferDidntMatch)


-- receiving a message with timeout and selective receive
-- and buffering, after the existing buffer has been checked
-- and there are no matching messages there
receiveNoBuffered :: Inbox a
        -> Int -- timeout in microseconds
        -> (a -> Bool) -- accept function for selective receive
        -> UTCTime
        -> IO (Maybe a)
receiveNoBuffered ib@(Inbox (Addr ch) buf _) tme f startTime = do
    -- get a message
    -- if it matches, return it
    -- otherwise, buffer
    -- check if more time, if so, loop
    msg <- if | tme < 0 -- todo: < 0 means infinity, wrap it in a data type?
                -> Just <$> atomically (readCQueue ch)
              | otherwise -> smartTimeout tme $ readCQueue ch
    case msg of
        Nothing -> pure Nothing
        Just msg'
            | f msg' -> pure msg
            | otherwise -> do
                atomically $ modifyTVar buf (++[msg'])
                nw <- getCurrentTime
                let elapsed = diffUTCTime nw startTime
                if elapsed > realToFrac tme / 1000 / 1000
                    then pure Nothing
                    else receiveNoBuffered ib tme f startTime

makeInbox :: IO (Inbox a)
makeInbox = do
    tid <- myThreadId
    atomically $ do
      x <- newCQueue
      b <- newTVar []
      pure (Inbox (Addr x) b tid)

makeInboxFromCQueue :: CQueue a -> IO (Inbox a)
makeInboxFromCQueue x = do
    tid <- myThreadId
    atomically $ do
      b <- newTVar []
      pure (Inbox (Addr x) b tid)

assertMyThreadIdIs :: ThreadId -> IO ()
assertMyThreadIdIs ibtid = do
    tid <- myThreadId
    when (tid /= ibtid) $
      error $ "inbox used in wrong thread, created in " ++ show ibtid ++ ", used in " ++ show tid

------------------------------------------------------------------------------

-- utilities


smartTimeout :: Int -> STM a -> IO (Maybe a)
smartTimeout n action = do
   v <- atomically $ newEmptyTMVar
   _ <- timeout n $ atomically $ do
       result <- action
       putTMVar v result
   atomically $ tryTakeTMVar v
