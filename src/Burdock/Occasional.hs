
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
    ,addr

    ,Inbox
    ,Addr

    ,testAddToBuffer
    ,testReceiveWholeBuffer

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

--import Debug.Trace (traceM, trace)

smartTimeout :: Int -> STM a -> IO (Maybe a)
smartTimeout n action = do
   v <- atomically $ newEmptyTMVar
   _ <- timeout n $ atomically $ do
       result <- action
       putTMVar v result
   atomically $ tryTakeTMVar v


data Addr a = Addr (TChan a)

data Inbox a = Inbox {addr :: Addr a
                     ,_buffer :: TVar [a]}

makeInbox :: IO (Inbox a)
makeInbox = atomically $ do
    x <- newTChan
    b <- newTVar []
    pure (Inbox (Addr x) b)


send :: Addr a -> a -> IO ()
send (Addr tgt) v = atomically $ writeTChan tgt v


-- read through input buffer, then chan
-- buffer non matching message, return nothing
-- if no matches

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

receive :: --Show a =>
           Inbox a
        -> Int -- timeout in microseconds
        -> (a -> Bool) -- accept function for selective receive
        -> IO (Maybe a)
receive ib@(Inbox (Addr ch) buf) tme f = do
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
                | otherwise -> receive1 ib tme f startTime
{-

selective receive with no timeout

run through the buffer, if there's a match
save the new buffer and return the match
then loop:
  try to read the chan
  if it's a match
    return it
  if it isn't, push it to the buffer
    loop
  if there's nothing
    return nothing

-}


-- temp
receive1 :: Inbox a
        -> Int -- timeout in microseconds
        -> (a -> Bool) -- accept function for selective receive
        -> UTCTime
        -> IO (Maybe a)
receive1 ib@(Inbox (Addr ch) buf) tme f startTime = do
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
                    else receive1 ib tme f startTime
                    


-- for whitebox testing

testAddToBuffer :: Inbox a -> a -> IO ()
testAddToBuffer (Inbox _ buf) a = atomically $ do
    modifyTVar buf $ (++ [a])

testReceiveWholeBuffer :: Inbox a -> IO [a]
testReceiveWholeBuffer (Inbox _ buf) = atomically $ do
    x <- readTVar buf
    writeTVar buf []
    pure x

