
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

    ,Inbox
    ,Addr

    ,testAddToBuffer
    ,testReceiveWholeBuffer
    ,testMakeInbox
    ,testSend
    ,testCloseInbox
    
    ) where

import System.Timeout (timeout)

import Control.Exception (AssertionFailed(..))

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
    (void
    ,when
    ,unless
    )

import Control.Concurrent.Async
    (async
    --,wait
    --,withAsync
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

data Inbox a
    = Inbox
    {addr :: Addr a
    ,_buffer :: TVar [a]
    -- used to check that inboxes are only used in the thread they are created in
    -- todo: apparently this has some not perfect interaction with the garbage
    -- collection of exited threads, so use show on the thread id instead
    -- of saving the ThreadId itself or something
    ,_threadId :: ThreadId
    }


------------------------------------------------------------------------------

-- api functions

runOccasional :: (Inbox a -> IO b) -> IO b
runOccasional f = do
    ib <- makeInbox
    f ib

spawn :: (Inbox c) -> (Inbox a -> IO b) -> IO (Addr a)
spawn (Inbox _ _ ibtid) f = do
    assertMyThreadIdIs ibtid
    ch <- newCQueueIO
    void $ async $ do
        ib <- makeInboxFromCQueue ch
        void $ f ib
        atomically $ closeCQueue $ unAddr $ addr ib
        pure ()
    pure (Addr ch)

send :: (Inbox b) -> Addr a -> a -> IO ()
send (Inbox _ _ ibtid) (Addr tgt) v = do
    assertMyThreadIdIs ibtid
    atomically $ writeCQueue tgt v


receive :: --Show a =>
           Inbox a
        -> Int -- timeout in microseconds
        -> (a -> Bool) -- accept function for selective receive
        -> IO (Maybe a)
receive ib@(Inbox (Addr ch) buf ibtid) tme f = do
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
testCloseInbox ib = atomically $ closeCQueue $ unAddr $ addr ib

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
