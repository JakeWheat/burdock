{-

Haskell concurrency library

an approach to concurrency modelled on a subset of erlang, uses
haskell async lib threads, which are a thin wrapper around ghc threads

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
module Burdock.HsConcurrency
    (runBConcurrency
    ,spawn
    ,spawnMonitor
    ,send
    ,receive
    ,receiveTimeout
    ,addr
    ,asyncExit
    ,extractDynValExceptionVal

    ,zreceive
    ,zreceiveTimeout

    ,openBConcurrency
    ,closeBConcurrency
    ,spawnExtWait

    ,BConcurrencyHandle
    ,ThreadHandle
    ,Addr
    ,Inbox
    ,DynValException(..)
    ,MonitorDown(..)
    ,ExitType(..)

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
    ,toException
    ,throwTo
    ,AsyncExceptionWrapper(..)
    ,SomeException
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
    (newEmptyTMVar
    ,putTMVar
    ,tryTakeTMVar
    )

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
    ,asyncThreadId
    )

import Data.Dynamic (Dynamic
                    ,Typeable
                    ,toDyn
                    ,fromDynamic
                    )

import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Control.Monad.IO.Class (MonadIO, liftIO)


--import Data.Typeable (typeOf)

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

data BConcurrencyHandle =
    BConcurrencyHandle
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
      ,occHandle :: BConcurrencyHandle
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

-- public api

-- data types

data MonitorDown
    = MonitorDown
    {mdTag :: Dynamic
    ,mdExitType :: ExitType
    ,mdValue :: Dynamic}

data ExitType = ExitValue
              | ExitException
              deriving (Eq,Show)

data DynValException = DynValException Dynamic
    deriving Show

instance Exception DynValException

-- functions

runBConcurrency :: (ThreadHandle -> IO Dynamic) -> IO Dynamic
runBConcurrency f = bracket openBConcurrency closeBConcurrency (flip spawnExtWait f)

openBConcurrency :: IO BConcurrencyHandle
openBConcurrency = BConcurrencyHandle <$> newTVarIO [] <*> newTVarIO [] <*> newTVarIO False

closeBConcurrency :: BConcurrencyHandle -> IO ()
closeBConcurrency oh =  do
    ts <- atomically $ do
        -- tell the exiting threads not to do the regular cleanup
        writeTVar (globalIsExiting oh) True
        readTVar (globalThreads oh)

    -- it's ok to cancel the main thread from runBConcurrency in this
    -- list since it's already exited at this point, if this cleanup
    -- is moved inside the main user thread exit, it should skip
    -- sending cancel to the main user thread async handle
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

send :: ThreadHandle -> Addr -> Dynamic -> IO ()
send h tgt v = do
    assertMyThreadIdIs $ ibThreadId $ myInbox h
    atomically $ writeCQueue tgt v

receiveTimeout :: --Show a =>
                  ThreadHandle
               -> Int -- timeout in microseconds
               -> (Dynamic -> Bool) -- accept function for selective receive
               -> IO (Maybe Dynamic)
receiveTimeout h tme f = receiveInboxTimeout (myInbox h) tme f

zreceiveTimeout :: MonadIO m => 
                   ThreadHandle
                -> Int -- timeout in microseconds
                -> (Dynamic -> m (Maybe Dynamic)) -- accept function for selective receive
                -> m (Maybe Dynamic)
zreceiveTimeout h tme f = zreceiveInboxTimeout (myInbox h) tme f


receive :: ThreadHandle -> (Dynamic -> Bool) -> IO Dynamic
receive h f = receiveInbox (myInbox h) f

zreceive :: MonadIO m => ThreadHandle -> (Dynamic -> m (Maybe Dynamic)) -> m Dynamic
zreceive h f = zreceiveInbox (myInbox h) f


spawn :: ThreadHandle -> (ThreadHandle -> IO Dynamic) -> IO Addr
spawn h f = spawnImpl h Nothing f

spawnMonitor :: ThreadHandle -> Maybe Dynamic -> (ThreadHandle -> IO Dynamic) -> IO Addr
spawnMonitor h mtag f =
    spawnImpl h (Just $ fromMaybe (toDyn ()) mtag) f

asyncExit :: ThreadHandle -> Addr -> Dynamic -> IO ()
asyncExit h target val = do
    ah <- atomically $ do
        ts <- readTVar (globalThreads $ occHandle h)
        case lookup target $ map swap ts of
            -- todo: what exception/fields should this use?
            -- maybe it should duplicate the error you get when sending
            -- to a closed inbox?
            Nothing -> error "thread not found"
            Just ah -> pure ah
    throwTo (asyncThreadId ah) (DynValException val)

-- spawn a thread from outside in an existing handle
-- this function blocks until the spawned function exits, and returns
-- it's exit value (or passes the exception on)
spawnExtWait :: BConcurrencyHandle -> (ThreadHandle -> IO Dynamic) -> IO Dynamic
spawnExtWait oh f = do
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

---------------------------------------

-- testing only functions, for whitebox and component testing
-- maybe the inbox could become a proper usable component on it's own
-- for communicating with concurrency outside the handle

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
            -> (Maybe Int) -- timeout in microseconds
            -> (Dynamic -> Bool) -- accept function for selective receive
            -> IO (Maybe Dynamic)
testReceive ib mtme f =
    case mtme of
        Nothing -> Just <$> receiveInbox ib f
        Just tme -> receiveInboxTimeout ib tme f 

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
asynchronously exited because the concurrency handle it's running under
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
            -- TODO: there's a possible race where a spawnMonitor
            -- process can exit before the monitoring is setup, so the
            -- spawning process will never get a monitor message even
            -- though the spawned process is down
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
        -- register the async handle in the concurrency handle
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
            let (exitType,exitVal) = extractExitVal ev
            -- send monitor message to each monitoring process
            --trace ("tracehere: " ++ show exitVal) $ pure ()
            let isMonitoringMe (_,x,_) = x == ah
            ips <- filter isMonitoringMe <$> readTVar (globalMonitors $ occHandle h)
            forM_ ips $ \(mp, _, tg) -> do
                x <- readTVar (globalThreads $ occHandle h)
                case lookup mp x of
                    Nothing -> pure () -- the spawning process already exited?
                    -- send the monitor message
                    -- it ends up as nested dynamic Dynamic (Dynamic tg, x, Dynamic ev)
                    -- perhaps there is a way to create a Dynamic (tg,ev)?
                    -- this would be a little nicer, but is not a big deal
                    -- for burdock - it's error prone, but only has to be done
                    -- once in the interpreter
                    Just mpib -> writeCQueue mpib $ toDyn $ MonitorDown tg exitType exitVal
            -- remove monitoring entries
            modifyTVar (globalMonitors $ occHandle h)
                $ filter $ \(a,b,_) -> a /= ah && b /= ah
            -- remove entry from processes table
            modifyTVar (globalThreads $ occHandle h) (\a -> filter ((/= ah) . fst) a)
    runThread ch ah = do
        ib <- makeInboxFromCQueue ch
        f (ThreadHandle ib (occHandle h) ah)
    -- eventually, this code manages to dig out the Dynamic value from
    -- a DynValException or from a
    -- AsyncExceptionWrapper-DynValException and turn the exception
    -- into a Dynamic String from displayException if it's another
    -- kind of exception or another kind of exception wrapped in
    -- asyncexceptionwrapper (not sure that asyncexceptionwrapper
    -- doesn't return the displayexception of the wrapped exception
    -- without the extra logic needed)
    extractExitVal ev = case ev of
        ExitCaseSuccess a -> (ExitValue, a)
        ExitCaseException e
            | Just v <- extractDynException e
            -> (ExitException, v)
            | otherwise -> (ExitException, toDyn $ displayException e)
        ExitCaseAbort -> (ExitException, toDyn $ "internal issue?: ExitCaseAbort")

extractDynValExceptionVal :: Typeable a => SomeException -> Maybe a
extractDynValExceptionVal err =
    case () of
        _ | Just v <- extractDynException err -> fromDynamic v
          | otherwise -> Nothing

extractDynException :: SomeException -> Maybe Dynamic
extractDynException e =
    if | Just v <- extract1 -> Just v
       | Just v <- extract2 -> Just v
       | otherwise -> Nothing
  where
    extract1 = case fromException e of
        Just (DynValException v) -> Just v
        Nothing -> Nothing
    extract2 = case fromException e of
        Just (AsyncExceptionWrapper e')
            | Just (DynValException v) <- fromException $ toException e' -> Just v
        Just (AsyncExceptionWrapper e') -> Just $ toDyn $ displayException e'
        Nothing -> Nothing

------------------------------------------------------------------------------

-- receive implementation

-- receive support

receiveInbox :: Inbox -> (Dynamic -> Bool) -> IO Dynamic
receiveInbox ib f = do
    assertMyThreadIdIs $ ibThreadId ib
    mres <- atomically $ do
        b <- readTVar (ibbuffer ib)
        --traceM $ "buf at start of receive: " ++ show b
        (newBuf, mres) <- flushInboxForMatch b [] (ibaddr ib) f
        writeTVar (ibbuffer ib) $ newBuf
        pure mres
    case mres of
        Just res -> pure res
        Nothing -> receiveNoBuffered ib f

receiveInboxTimeout :: Inbox -> Int -> (Dynamic -> Bool) -> IO (Maybe Dynamic)
receiveInboxTimeout ib tme f = do
    assertMyThreadIdIs $ ibThreadId ib
    startTime <- getCurrentTime
    mres <- atomically $ do
        b <- readTVar (ibbuffer ib)
        --traceM $ "buf at start of receive: " ++ show b
        (newBuf, mres) <- flushInboxForMatch b [] (ibaddr ib) f
        writeTVar (ibbuffer ib) $ newBuf
        pure mres
    case (mres, tme) of
        (Just {},_) -> pure mres
        (Nothing, 0)  -> pure mres
        (_, _) -> receiveNoBufferedTimeout ib tme f startTime

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

receiveNoBufferedTimeout :: Inbox
                         -> Int -- timeout in microseconds
                         -> (Dynamic -> Bool) -- accept function for selective receive
                         -> UTCTime
                         -> IO (Maybe Dynamic)
receiveNoBufferedTimeout ib tme f startTime = do
    msg <- smartTimeout tme $ readCQueue $ ibaddr ib
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
                      else receiveNoBufferedTimeout ib tme f startTime

-- no timeout version
receiveNoBuffered :: Inbox
                  -> (Dynamic -> Bool)
                  -> IO Dynamic
receiveNoBuffered ib f = do
    msg <- atomically (readCQueue $ ibaddr ib)
    if f msg
        then pure msg
        else do
            atomically $ modifyTVar (ibbuffer ib) (++[msg])
            receiveNoBuffered ib f

zreceiveInbox:: MonadIO m => Inbox -> (Dynamic -> m (Maybe Dynamic)) -> m Dynamic
zreceiveInbox ib f = do
    liftIO $ assertMyThreadIdIs $ ibThreadId ib
    b <- liftIO $ atomically $ readTVar (ibbuffer ib)
    (newBuf, mres) <- zflushInboxForMatch b [] (ibaddr ib) f
    liftIO $ atomically $ writeTVar (ibbuffer ib) $ newBuf
    case mres of
        Just res -> pure res
        Nothing -> zreceiveNoBuffered ib f

zflushInboxForMatch :: MonadIO m =>
                      [a]
                   -> [a]
                   -> (CQueue a)
                   -> (a -> m (Maybe a))
                   -> m ([a], Maybe a)
zflushInboxForMatch buf bdm inQueue prd = go1 buf bdm
  where
    go1 (x:xs) bufferDidntMatch = do
        r <- prd x
        case r of
            Just {} -> pure (reverse bufferDidntMatch ++ xs, r)
            Nothing -> go1 xs (x:bufferDidntMatch)
    go1 [] bufferDidntMatch = do
        x <- liftIO $ atomically $ tryReadCQueue inQueue
        case x of
            Nothing -> pure (reverse bufferDidntMatch, x)
            Just y -> do
                r <- prd y
                case r of
                    Just {} -> pure (reverse bufferDidntMatch, r)
                    Nothing -> go1 [] (y:bufferDidntMatch)

zreceiveNoBuffered :: MonadIO m =>
                      Inbox
                   -> (Dynamic -> m (Maybe Dynamic))
                   -> m Dynamic
zreceiveNoBuffered ib f = do
    msg <- liftIO $ atomically (readCQueue $ ibaddr ib)
    v <- f msg
    case v of
        Nothing -> do
            liftIO $ atomically $ modifyTVar (ibbuffer ib) (++[msg])
            zreceiveNoBuffered ib f
        Just v' -> pure v'
            

zreceiveInboxTimeout :: MonadIO m =>
                        Inbox
                     -> Int
                     -> (Dynamic -> m (Maybe Dynamic))
                     -> m (Maybe Dynamic)
zreceiveInboxTimeout ib tme f = do
    liftIO $ assertMyThreadIdIs $ ibThreadId ib
    startTime <- liftIO getCurrentTime
    b <- liftIO $ atomically $ readTVar (ibbuffer ib)
    (newBuf, mres) <- zflushInboxForMatch b [] (ibaddr ib) f
    liftIO $ atomically $ writeTVar (ibbuffer ib) $ newBuf
    case (mres, tme) of
        (Just {},_) -> pure mres
        (Nothing, 0)  -> pure mres
        (_, _) -> zreceiveNoBufferedTimeout ib tme f startTime


zreceiveNoBufferedTimeout :: MonadIO m =>
                             Inbox
                         -> Int -- timeout in microseconds
                         -> (Dynamic -> m (Maybe Dynamic)) -- accept function for selective receive
                         -> UTCTime
                         -> m (Maybe Dynamic)
zreceiveNoBufferedTimeout ib tme f startTime = do
    msg <- liftIO $ smartTimeout tme $ readCQueue $ ibaddr ib
    case msg of
        Nothing -> pure Nothing
        Just msg' -> do
            r <- f msg'
            case r of
                Just {} -> pure r
                Nothing -> do
                  liftIO $ atomically $ modifyTVar (ibbuffer ib) (++[msg'])
                  nw <- liftIO $ getCurrentTime
                  let elapsed = diffUTCTime nw startTime
                  if elapsed > realToFrac tme / 1000 / 1000
                      then pure Nothing
                      else zreceiveNoBufferedTimeout ib tme f startTime


------------------------------------------------------------------------------

-- utils

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
    deriving Eq

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

smartTimeout :: Int -> STM a -> IO (Maybe a)
smartTimeout n action = do
   v <- atomically $ newEmptyTMVar
   _ <- timeout n $ atomically $ do
       result <- action
       putTMVar v result
   atomically $ tryTakeTMVar v
