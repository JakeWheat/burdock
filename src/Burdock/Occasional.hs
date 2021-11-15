
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
    (makeInbox
    ,send
    ,receive

    ) where

import System.Timeout (timeout)

import Control.Monad.STM
    (STM
    ,atomically
    )
import Control.Concurrent.STM.TChan
    (TChan
    ,newTChan
    ,readTChan
    ,writeTChan
    ,tryReadTChan
    )

import Control.Concurrent.STM.TMVar

smartTimeout :: Int -> STM a -> IO (Maybe a)
smartTimeout n action = do
   v <- atomically $ newEmptyTMVar
   _ <- timeout n $ atomically $ do
       result <- action
       putTMVar v result
   atomically $ tryTakeTMVar v


data Addr a = Addr (TChan a)

makeInbox :: IO (Addr a)
makeInbox = atomically $ do
    Addr <$> newTChan


send :: Addr a -> a -> IO ()
send (Addr tgt) v = atomically $ do
    writeTChan tgt v


receive :: (Addr a)
          -> Int -- timeout in microseconds
          -> (a -> Bool) -- accept function for selective receive
          -> IO (Maybe a)
receive (Addr ib) to _f = do
    if | to < 0 -- todo: < 0 means infinity, wrap it in a data type?
         -> Just <$> atomically (readTChan ib)
       | to == 0 -> atomically (tryReadTChan ib)
       | otherwise -> smartTimeout to $ readTChan ib

