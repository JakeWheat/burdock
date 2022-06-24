{-

Trivial wrappers around queues and asyncs:

queue (called Inbox) with a close, and a receive with a timeout

launch a thread with a callback that's called with an either when it
exits

same for a bound thread

-}
module Burdock.HsConcurrency
    (Inbox
    ,ibQueue
    ,makeInbox
    ,closeInbox
    ,send
    ,receive
    ,receiveTimeout

    ,MAsync
    ,asyncHandle
    ,masync
    ,masyncBound
    )
    where

import qualified Control.Concurrent.Async as A

import Control.Exception
    (SomeException
    ,AssertionFailed(..))

import Control.Concurrent.STM.TQueue
    (TQueue
    ,newTQueue
    ,readTQueue
    ,tryReadTQueue
    ,writeTQueue
    )

import Control.Monad.STM
    (STM
    ,atomically
    ,throwSTM
    )

import Control.Concurrent.STM.TVar
    (TVar
    ,newTVar
    ,readTVar
    ,writeTVar
    )

import Control.Concurrent.STM.TMVar
    (newEmptyTMVar
    ,putTMVar
    ,tryTakeTMVar
    )

import System.Timeout (timeout)

import Control.Monad (unless, void)

---------------------------------------

data Inbox a
    = Inbox
    {ibQueue :: TQueue a
    ,ibIsOpen :: TVar Bool}

makeInbox :: IO (Inbox a)
makeInbox = atomically (Inbox <$> newTQueue <*> newTVar True)

closeInbox :: Inbox a -> IO ()
closeInbox ib = atomically $ writeTVar (ibIsOpen ib) False

checkInboxOpen :: Inbox a -> STM ()
checkInboxOpen ib = do
    o <- readTVar $ ibIsOpen ib
    -- come back later and figure out the exception types
    unless o $ throwSTM $ AssertionFailed "tried to use closed inbox"

send :: Inbox a -> a -> IO ()
send ib v = atomically $ do
    checkInboxOpen ib
    writeTQueue (ibQueue ib) v

receive :: Inbox a -> IO a
receive ib = atomically $ receiveSTM ib

receiveSTM :: Inbox a -> STM a
receiveSTM ib = do
    checkInboxOpen ib
    readTQueue (ibQueue ib)

--- timeout time in microseconds
receiveTimeout :: Inbox a -> Int -> IO (Maybe a)
receiveTimeout ib 0 =
    atomically $ do
        checkInboxOpen ib
        tryReadTQueue (ibQueue ib)
receiveTimeout ib tme =
    smartTimeout tme $ receiveSTM ib

smartTimeout :: Int -> STM a -> IO (Maybe a)
smartTimeout tme action = do
   v <- atomically $ newEmptyTMVar
   _ <- timeout tme $ atomically $ do
       result <- action
       putTMVar v result
   atomically $ tryTakeTMVar v

---------------------------------------

data MAsync a
    = MAsync
    {asyncHandle :: A.Async a
    }

masync :: (Either SomeException a -> IO ()) -> IO a -> IO (MAsync a)
masync cb fn = do
    ia <- A.async fn
    void $ A.async $ do
        r <- A.waitCatch ia
        cb r
    pure $ MAsync ia

masyncBound :: (Either SomeException a -> IO ()) -> IO a -> IO (MAsync a)
masyncBound cb fn = do
    ia <- A.asyncBound fn
    void $ A.async $ do
        r <- A.waitCatch ia
        cb r
    pure $ MAsync ia
