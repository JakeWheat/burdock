
{-

Haskell "Occasional" library
an approach to concurrency modelled on a subset of erlang

It only exists to help implement concurrency in Burdock

Plan:

inbox demo
  see what the options are, is it worth chasing something
  that will scale to distributed message passing, or ignoring
  that future for now

send and receive messages
typed and untyped
don't actually need untyped for burdock, so skip it for now
anomaly?


timeouts
selective receive
self

spawn monitor
send and receive with it, including self

check exit values with spawn monitor, use a helper
-> regular function exit
  in system exception exit
  arbitrary exception exit
  haskell thread kill

link, spawn_link?


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
    if | to < 0 -- todo: wrap it in a data type?
         -> Just <$> atomically (readTChan ib)
       | to == 0 -> atomically (tryReadTChan ib)
       | otherwise -> smartTimeout to $ readTChan ib

