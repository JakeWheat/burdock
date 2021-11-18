
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

    ,ThreadHandle
    ,Addr
    ,Inbox
    ,DynValException(..)

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
import Control.Exception.Safe
    (bracket
    ,generalBracket
    ,mask_
    ,finally
    ,displayException
    ,Exception
    ,fromException
    )

import Control.Monad.Catch (ExitCase(..))

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
    (--async
    --,wait
    --,withAsync
    --,cancel
    --,
    uninterruptibleCancel
    ,Async
    ,asyncWithUnmask
    )

import Data.Dynamic (Dynamic
                    --,Typeable
                    ,toDyn
                    --,fromDynamic
                    )

{-import Data.Typeable (cast
                     ,typeOf)-}

{-import Debug.Trace (--traceM
                   --,
                   trace
                   )-}

------------------------------------------------------------------------------

-- types

data Addr =
    Addr {unAddr :: CQueue Dynamic}

-- not sure how to make this better
-- for burdock want erlang style pids which are unique
-- I don't see an easy way to do this in haskell
-- avoiding the show instance makes all sorts of debugging really
-- tedious. maybe some helper functions can be used if messages
-- follow a pattern
instance Show Addr where
    show (Addr {}) = "Addr"

data OccasionalHandle =
    OccasionalHandle
    {globalThreads :: TVar [Async Dynamic]
        -- monitoring process - the one that gets the thread down message,
        -- monitored process,
        -- monitor tag
        --,globalMonitors :: TVar [(Async Dynamic, Async Dynamic, Dynamic)]
    ,globalIsExiting :: TVar Bool
    }

data ThreadHandle
    = ThreadHandle
      {myInbox :: Inbox
      ,occHandle :: OccasionalHandle
      }

addr :: ThreadHandle -> Addr
addr = ibaddr . myInbox

data Inbox
    = Inbox
    {ibaddr :: Addr
    ,ibbuffer :: TVar [Dynamic]
    -- used to check that inboxes are only used in the thread they are created in
    -- todo: apparently this has some not perfect interaction with the garbage
    -- collection of exited threads, so use show on the thread id instead
    -- of saving the ThreadId itself or something
    ,ibThreadId :: ThreadId
    }


------------------------------------------------------------------------------

-- api functions

runOccasional :: (ThreadHandle -> IO Dynamic) -> IO Dynamic
runOccasional f = do
    oh <- OccasionalHandle <$> newTVarIO [] <*> newTVarIO False
    ib <- makeInbox
    let th = ThreadHandle ib oh
    bracket (pure oh) cancelRunningThreads (\_ -> f th)
  where
    cancelRunningThreads oh = do
        ts <- atomically $ do
            -- tell the exiting threads not to do the regular cleanup
            writeTVar (globalIsExiting oh) True
            readTVar (globalThreads oh)
        mapM_ uninterruptibleCancel ts
{-

running the main thread:
issues to follow up on:
add a timeout to this, and then return to user in special way if
any threads didn't exit in the deadline
cancel on an async thread is synchronous, so it may be less quick
use throwTo without the wait to do it concurrently
cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
  then wait on all the thread handles to see they all exited

there's no obvious timeout on the wait variations:
maybe use regular io timeout function with stm wait variations?


-}


{-

spawn, possibly will end up being one of the most fun functions in the
codebase

tricky requirements:

ensure the queue that is the spawned thread's inbox is always closed
even when the spawned thread gets an async exception

implement the monitor which notifies other threads of the spawned
thread's exit value - exception or regular value

and don't do any of this in the specific case that the thread is being
asynchronously exited because the occasional handle it's running under
is being closed (which happens exactly when the main user function
exits by non-exception or exception)

-}

data DynValException = DynValException Dynamic
    deriving Show

instance Exception DynValException

spawn :: ThreadHandle -> (ThreadHandle -> IO Dynamic) -> IO Addr
spawn h f = do
    assertMyThreadIdIs $ ibThreadId $ myInbox h
    -- this queue is used to pass its own async handle
    -- to the spawned thread, so the spawned thread
    -- can register this handle, and deregister it when it exits
    -- it's only used to pass this one value
    asyncOnlyCh <- newCQueueIO
    -- try to ensure that ch is closed if the thread gets an async
    -- message really early in it's lifetime. not 100% sure this
    -- is the right way to do this 
    (ch,ah) <- mask_ $ do
        -- this is the queue that will become the new thread's inbox
        ch <- newCQueueIO
        ah <- asyncWithUnmask $ \unmask ->
            unmask (fst <$> generalBracket (registerRunningThread asyncOnlyCh) 
                    cleanupRunningThread
                    (const $ runThread ch))
            -- this is the bit in the mask that makes sure the cleanup
            -- is registered
            `finally` (atomically $ do
                isExiting <- readTVar $ globalIsExiting $ occHandle h
                unless isExiting $ closeCQueue ch)
        pure (ch,ah)
    atomically $ writeCQueue asyncOnlyCh ah
    pure (Addr ch)
  where
    registerRunningThread asyncOnlyCh = atomically $ do
        -- register the async handle in the occasional handle
        ah <- readCQueue asyncOnlyCh
        modifyTVar (globalThreads $ occHandle h) (ah:)
        pure ah
    cleanupRunningThread ah ev = atomically $ do
        isExiting <- readTVar (globalIsExiting $ occHandle h)
        unless isExiting $ do
            let _exitVal = case ev of
                    ExitCaseSuccess a -> Right a
                    ExitCaseException e -> case fromException e of
                            Just (DynValException v) -> Left v
                            Nothing -> Left $ toDyn $ displayException e
                    ExitCaseAbort -> Left $ toDyn $ "internal issue?: ExitCaseAbort"
            -- send monitor message to each monitoring process
            --trace ("tracehere: " ++ show exitVal) $ pure ()
            -- remove monitoring entries
            -- remove entry from processes table
            modifyTVar (globalThreads $ occHandle h) (\a -> filter (/= ah) a)
    runThread ch = do
        ib <- makeInboxFromCQueue ch
        f (ThreadHandle ib (occHandle h))

send :: ThreadHandle -> Addr -> Dynamic -> IO ()
send h (Addr tgt) v = do
    assertMyThreadIdIs $ ibThreadId $ myInbox h
    atomically $ writeCQueue tgt v


receive :: --Show a =>
           ThreadHandle
        -> Int -- timeout in microseconds
        -> (Dynamic -> Bool) -- accept function for selective receive
        -> IO (Maybe Dynamic)
receive h tme f = receiveInbox (myInbox h) tme f

---------------------------------------

-- testing only functions, for whitebox and component testing

testAddToBuffer :: Inbox -> Dynamic -> IO ()
testAddToBuffer ib a = atomically $ do
    modifyTVar (ibbuffer ib) $ (++ [a])

testReceiveWholeBuffer :: Inbox -> IO [Dynamic]
testReceiveWholeBuffer ib = atomically $ do
    x <- readTVar (ibbuffer ib)
    writeTVar (ibbuffer ib) []
    pure x

testMakeInbox :: IO Inbox
testMakeInbox = makeInbox

-- send without the local inbox for inbox testing
testSend :: Addr -> Dynamic -> IO ()
testSend (Addr tgt) v = atomically $ writeCQueue tgt v

testCloseInbox :: Inbox -> IO ()
testCloseInbox ib = atomically $ closeCQueue $ unAddr $ ibaddr ib

testReceive :: Inbox
            -> Int -- timeout in microseconds
            -> (Dynamic -> Bool) -- accept function for selective receive
            -> IO (Maybe Dynamic)
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

receiveInbox :: Inbox -> Int -> (Dynamic -> Bool) -> IO (Maybe Dynamic)
receiveInbox ib tme f = do
    assertMyThreadIdIs $ ibThreadId ib
    startTime <- getCurrentTime
    mres <- atomically $ do
        b <- readTVar (ibbuffer ib)
        --traceM $ "buf at start of receive: " ++ show b
        (newBuf, mres) <- flushInboxForMatch b [] (unAddr $ ibaddr ib) f
        writeTVar (ibbuffer ib) $ newBuf
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
receiveNoBuffered :: Inbox
        -> Int -- timeout in microseconds
        -> (Dynamic -> Bool) -- accept function for selective receive
        -> UTCTime
        -> IO (Maybe Dynamic)
receiveNoBuffered ib tme f startTime = do
    -- get a message
    -- if it matches, return it
    -- otherwise, buffer
    -- check if more time, if so, loop
    msg <- if | tme < 0 -- todo: < 0 means infinity, wrap it in a data type?
                -> Just <$> atomically (readCQueue $ unAddr $ ibaddr ib)
              | otherwise -> smartTimeout tme $ readCQueue $ unAddr $ ibaddr ib
    case msg of
        Nothing -> pure Nothing
        Just msg'
            | f msg' -> pure msg
            | otherwise -> do
                atomically $ modifyTVar (ibbuffer ib) (++[msg'])
                nw <- getCurrentTime
                let elapsed = diffUTCTime nw startTime
                if elapsed > realToFrac tme / 1000 / 1000
                    then pure Nothing
                    else receiveNoBuffered ib tme f startTime

makeInbox :: IO Inbox
makeInbox = do
    tid <- myThreadId
    atomically $ do
      x <- newCQueue
      b <- newTVar []
      pure (Inbox (Addr x) b tid)

makeInboxFromCQueue :: CQueue Dynamic -> IO Inbox
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
