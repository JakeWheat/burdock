
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
    ,spawnMonitor
    ,send
    ,receive
    ,receiveNoTO
    ,addr

    ,ThreadHandle
    ,Addr
    ,Inbox
    ,DynValException(..)
    ,Down(..)

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
    ,forM_
    )

import Control.Concurrent.Async
    (--async
     wait
    ,withAsync
    --,cancel
    --,
    ,uninterruptibleCancel
    ,Async
    ,asyncWithUnmask
    )

import Data.Dynamic (Dynamic
                    --,Typeable
                    ,toDyn
                    --,fromDynamic
                    )

import Data.Maybe (fromMaybe)

{-import Data.Typeable (cast
                     ,typeOf)-}

{-import Debug.Trace (--traceM
                   --,
                   trace
                   )-}

------------------------------------------------------------------------------

-- types

-- make addr abstract for importers of the module
-- not sure if this is enough?
type Addr = CQueue Dynamic

addr :: ThreadHandle -> Addr
addr = ibaddr . myInbox

data OccasionalHandle =
    OccasionalHandle
    {globalThreads :: TVar [(Async Dynamic, CQueue Dynamic)]
        -- monitoring process - the one that gets the thread down message,
        -- monitored process,
        -- monitor tag
    ,globalMonitors :: TVar [(Async Dynamic, Async Dynamic, Dynamic)]
    ,globalIsExiting :: TVar Bool
    }

data ThreadHandle
    = ThreadHandle
      {myInbox :: Inbox
      ,occHandle :: OccasionalHandle
      ,myAsyncHandle :: Async Dynamic
      }


data Inbox
    = Inbox
    {ibaddr :: CQueue Dynamic
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
    bracket (OccasionalHandle <$> newTVarIO [] <*> newTVarIO [] <*> newTVarIO False)
        cancelRunningThreads
        (spawnMainThread f)
  where
    cancelRunningThreads oh = do
        ts <- atomically $ do
            -- tell the exiting threads not to do the regular cleanup
            writeTVar (globalIsExiting oh) True
            readTVar (globalThreads oh)
        -- it's ok to cancel the main thread in this list since it's
        -- already exited at this point, if this cleanup is moved
        -- inside the main user thread exit, it should skip sending
        -- cancel to the main user thread async handle
        mapM_ (uninterruptibleCancel . fst) ts
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


data DynValException = DynValException Dynamic
    deriving Show

instance Exception DynValException


send :: ThreadHandle -> Addr -> Dynamic -> IO ()
send h tgt v = do
    assertMyThreadIdIs $ ibThreadId $ myInbox h
    atomically $ writeCQueue tgt v

receive :: --Show a =>
           ThreadHandle
        -> Int -- timeout in microseconds
        -> (Dynamic -> Bool) -- accept function for selective receive
        -> IO (Maybe Dynamic)
receive h tme f = receiveInbox (myInbox h) tme f

receiveNoTO :: ThreadHandle -> (Dynamic -> Bool) -> IO Dynamic
receiveNoTO h f = do
    x <- receive h (-1) f
    maybe (error $ "internal error: receiveNoTO got Nothing")
           pure x

spawn :: ThreadHandle -> (ThreadHandle -> IO Dynamic) -> IO Addr
spawn h f = spawnImpl h Nothing f

spawnMonitor :: ThreadHandle -> Maybe Dynamic -> (ThreadHandle -> IO Dynamic) -> IO Addr
spawnMonitor h mtag f =
    spawnImpl h (Just $ fromMaybe (toDyn Down) mtag) f

data Down = Down
          deriving (Eq,Show)
                
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
testSend tgt v = atomically $ writeCQueue tgt v

testCloseInbox :: Inbox -> IO ()
testCloseInbox ib = atomically $ closeCQueue $ ibaddr ib

testReceive :: Inbox
            -> Int -- timeout in microseconds
            -> (Dynamic -> Bool) -- accept function for selective receive
            -> IO (Maybe Dynamic)
testReceive = receiveInbox

------------------------------------------------------------------------------

-- implementation of spawn

{-

possibly will end up being one of the most fun functions in the
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

TODO: reason about what happens when the spawning thread is asynchronously
exited during the call to spawn/spawnMonitor

-}

spawnImpl :: ThreadHandle -> Maybe Dynamic -> (ThreadHandle -> IO Dynamic) -> IO Addr
spawnImpl h ifMonitorTag f = do
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
            -- TODO: still a race where a spawnMonitor process can exit
            -- before the monitoring is setup
            unmask (fst <$> generalBracket (registerRunningThread asyncOnlyCh ch)
                    cleanupRunningThread
                    (runThread ch))
            -- this is the bit in the mask that makes sure the cleanup
            -- is registered
            `finally` (atomically $ do
                isExiting <- readTVar $ globalIsExiting $ occHandle h
                unless isExiting $ closeCQueue ch)
        pure (ch,ah)
    atomically $ writeCQueue asyncOnlyCh ah
    pure ch
  where
    registerRunningThread asyncOnlyCh ch = atomically $ do
        -- register the async handle in the occasional handle
        ah <- readCQueue asyncOnlyCh
        modifyTVar (globalThreads $ occHandle h) ((ah,ch):)
        case ifMonitorTag of
            Nothing -> pure ()
            Just tg -> do
                modifyTVar (globalMonitors $ occHandle h)
                   ((myAsyncHandle h, ah, tg):)
        pure ah
    cleanupRunningThread ah ev = atomically $ do
        isExiting <- readTVar (globalIsExiting $ occHandle h)
        unless isExiting $ do
            let exitVal = case ev of
                    ExitCaseSuccess a -> a
                    ExitCaseException e -> case fromException e of
                            Just (DynValException v) -> v
                            Nothing -> toDyn $ displayException e
                    ExitCaseAbort -> toDyn $ "internal issue?: ExitCaseAbort"
            -- send monitor message to each monitoring process
            --trace ("tracehere: " ++ show exitVal) $ pure ()
            let isMonitoringMe (_,x,_) = x == ah
            ips <- filter isMonitoringMe <$> readTVar (globalMonitors $ occHandle h)
            forM_ ips $ \(mp, _, tg) -> do
                x <- readTVar (globalThreads $ occHandle h)
                case lookup mp x of
                    Nothing -> pure () -- the spawning process already exited?
                    -- send the monitor message
                    -- it ends up as nested dynamic Dynamic (Dynamic tg, Dynamic ev)
                    -- perhaps there is a way to create a Dynamic (tg,ev)?
                    -- this would be a little nicer, but is not a big deal
                    -- for burdock
                    Just mpib -> writeCQueue mpib $ toDyn (tg, exitVal)
            -- remove monitoring entries
            modifyTVar (globalMonitors $ occHandle h)
                $ filter $ \(a,b,_) -> a /= ah && b /= ah
            -- remove entry from processes table
            modifyTVar (globalThreads $ occHandle h) (\a -> filter ((/= ah) . fst) a)
    runThread ch ah = do
        ib <- makeInboxFromCQueue ch
        f (ThreadHandle ib (occHandle h) ah)

-- the system is far easier to work with if the user thread isn't made
-- a special case and we can get it's async handle. the simple way to
-- do this is to run it in a separate thread to the code calling
-- runOccasional
-- keep this in sync with the spawnimpl above
spawnMainThread :: (ThreadHandle -> IO Dynamic) -> OccasionalHandle -> IO Dynamic
spawnMainThread f oh = do
    asyncOnlyCh <- newCQueueIO
    withAsync (runUserThread asyncOnlyCh) $ \a1 -> do
        atomically $ writeCQueue asyncOnlyCh a1
        wait a1
  where
    runUserThread asyncOnlyCh = do
        ib <- makeInbox
        ah <- atomically $ do
            ah <- readCQueue asyncOnlyCh
            -- put the main thread in the thread catalog
            modifyTVar (globalThreads oh) ((ah,ibaddr ib):)
            pure ah
        let th = ThreadHandle ib oh ah
        f th

------------------------------------------------------------------------------

-- receive implementation

-- receive support

receiveInbox :: Inbox -> Int -> (Dynamic -> Bool) -> IO (Maybe Dynamic)
receiveInbox ib tme f = do
    assertMyThreadIdIs $ ibThreadId ib
    startTime <- getCurrentTime
    mres <- atomically $ do
        b <- readTVar (ibbuffer ib)
        --traceM $ "buf at start of receive: " ++ show b
        (newBuf, mres) <- flushInboxForMatch b [] (ibaddr ib) f
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
                -> Just <$> atomically (readCQueue $ ibaddr ib)
              | otherwise -> smartTimeout tme $ readCQueue $ ibaddr ib
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

makeInbox :: IO Inbox
makeInbox = do
    tid <- myThreadId
    atomically $ do
      x <- newCQueue
      b <- newTVar []
      pure (Inbox x b tid)

makeInboxFromCQueue :: CQueue Dynamic -> IO Inbox
makeInboxFromCQueue x = do
    tid <- myThreadId
    atomically $ do
      b <- newTVar []
      pure (Inbox x b tid)

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
