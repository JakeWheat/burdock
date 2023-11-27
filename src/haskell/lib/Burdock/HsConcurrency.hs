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

{-import qualified Data.ByteString as BS
import System.IO (stderr)
import qualified Data.ByteString.UTF8 as BS-}

import Control.Concurrent (ThreadId)

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

send :: a -> Inbox a -> IO ()
send v ib = atomically $ do
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
    deriving Eq

mymasync :: (IO a -> IO (A.Async a))
         -> (ThreadId -> Either SomeException a -> IO ())
         -> IO a
         -> IO (MAsync a)
mymasync myAsync cb fn = do
    ia <- myAsync fn
    void $ A.async $ do
        r <- A.waitCatch ia
        cb (A.asyncThreadId ia) r
    --logit (A.asyncThreadId a1) (A.asyncThreadId ia)
    pure $ MAsync ia
  {-where
    logit mon th = do
        mt <- myThreadId
        BS.hPutStr stderr $ BS.fromString $ "masync from " ++ show mt ++ ": monitor " ++ show mon ++ ", lauched " ++ show th ++ "\n"-}

masync :: (ThreadId -> Either SomeException a -> IO ()) -> IO a -> IO (MAsync a)
masync cb fn = mymasync A.async cb fn
    
masyncBound :: (ThreadId -> Either SomeException a -> IO ()) -> IO a -> IO (MAsync a)
masyncBound cb fn = mymasync A.asyncBound cb fn
