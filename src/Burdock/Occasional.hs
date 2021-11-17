
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
    
    ) where

import System.Timeout (timeout)

import Control.Monad.STM
    (STM
    ,atomically
    )
import Control.Concurrent.STM.TChan
    (TChan
    ,newTChan
    ,newTChanIO
    ,readTChan
    ,writeTChan
    ,tryReadTChan
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

import Control.Monad (void, when)

import Control.Concurrent.Async
    (async
    --,wait
    --,withAsync
    )

--import Debug.Trace (traceM, trace)

------------------------------------------------------------------------------

-- types

data Addr a = Addr (TChan a)

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

{-
create an inbox
spawn the central services process
for now, this will just run the user code
wait for the return value of the spawned thread
return it

then in stage 2:

the central services will spawn the main user process
it will wait for that specific process to exit
and then return it's exit value to the original thread

then in stage 3:
spawn will be implemented by a message to central services,
instead of forkio locally

-}
runOccasional :: (Inbox a -> IO b) -> IO b
runOccasional f = do
    ib <- makeInbox
    f ib

spawn :: (Inbox c) -> (Inbox a -> IO b) -> IO (Addr a)
spawn (Inbox _ _ ibtid) f = do
    assertMyThreadIdIs ibtid
    ch <- newTChanIO
    void $ async $ do
        ib <- makeInboxFromChan ch
        void $ f ib
        pure ()
    pure (Addr ch)

send :: (Inbox b) -> Addr a -> a -> IO ()
send (Inbox _ _ ibtid) (Addr tgt) v = do
    assertMyThreadIdIs ibtid
    atomically $ writeTChan tgt v


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
testSend (Addr tgt) v = atomically $ writeTChan tgt v

------------------------------------------------------------------------------

-- internal components

-- the part of receive that checks the buffer for matching messages
flushInboxForMatch :: --Show a =>
                      [a]
                   -> [a]
                   -> (TChan a)
                   -> (a -> Bool)
                   -> STM ([a], Maybe a)
flushInboxForMatch buf bdm inChan prd = go1 buf bdm
  where
    --go1 a b | trace ("go1: " ++ show (a,b)) $ False = undefined
    go1 (x:xs) bufferDidntMatch
        | prd x
        = --trace ("match: " ++ show x) $
          pure (reverse bufferDidntMatch ++ xs, Just x)
        | otherwise
        = go1 xs (x:bufferDidntMatch)
    go1 [] bufferDidntMatch = do
        x <- tryReadTChan inChan
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
                -> Just <$> atomically (readTChan ch)
              | otherwise -> smartTimeout tme $ readTChan ch
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
      x <- newTChan
      b <- newTVar []
      pure (Inbox (Addr x) b tid)

makeInboxFromChan :: TChan a -> IO (Inbox a)
makeInboxFromChan x = do
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
