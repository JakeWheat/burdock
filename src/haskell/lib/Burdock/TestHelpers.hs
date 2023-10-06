


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Burdock.TestHelpers
    (testHelpers
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error,show)

-- todo: make the HaskellModulePlugin into a general haskell ffi api
-- then everything users should access goes through that, and they
-- don't import the runtime directly
import Burdock.Runtime
    (Runtime
    ,Value
    ,extractValue
    ,makeFunction
    ,app
    ,nothingValue
    ,liftIO
    ,makeString
    ,debugShowValue
    ,createBurdockRunner
    ,makeValue
    ,FFITypeTag
    ,makeNumber
    ,makeBurdockList
    )

import Burdock.HaskellModulePlugin
    (HaskellModule(..)
    ,makeHaskellModule
    ,ModuleMember(..)
    ,makeFFIType
    ,opaque
    )

import Burdock.Scientific (Scientific)

import qualified Control.Concurrent.Async as A
import Control.Concurrent
    (threadDelay
    ,throwTo
    ,myThreadId
    )


import Burdock.Scientific
    (extractInt
    )

import Control.Monad
    (void
    ,replicateM_
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T


testHelpers :: Runtime HaskellModule
testHelpers = do

    tempHandle <- makeFFIType "temp-handle" opaque

    mySleep' <- makeFunction mySleep
    spawnSleepThrowTo' <- makeFunction spawnSleepThrowTo
    bmyThreadId' <- makeFunction bmyThreadId
    runCallbackN' <- makeFunction runCallbackN
    runCallbackAsyncN' <- makeFunction (runCallbackAsyncN tempHandle)
    testWait' <- makeFunction testWait
    handleThreadId' <- makeFunction handleThreadId

    split' <- makeFunction split

    bytestringt <- makeFFIType "bytestring" opaque

    makeBytestring' <- makeFunction (makeBytestring bytestringt)
    getBytestringByte' <- makeFunction getBytestringByte

    makeHaskellModule
        [Type tempHandle
        ,Identifier "sleep" mySleep'
        ,Identifier "spawn-sleep-throw-to" spawnSleepThrowTo'
        ,Identifier "my-thread-id" bmyThreadId'
        ,Identifier "run-callback-n" runCallbackN'
        ,Identifier "run-callback-async-n" runCallbackAsyncN'
        ,Identifier "test-wait" testWait'
        ,Identifier "handle-thread-id" handleThreadId'
        ,Identifier "split" split'
        ,Type bytestringt
        ,Identifier "make-bytestring" makeBytestring'
        ,Identifier "get-bytestring-byte" getBytestringByte'
        ]

------------------------------------------------------------------------------

-- quick testing for callstacks

mySleep :: [Value] -> Runtime Value
mySleep [x] = do
    case extractValue x of
        Just (n :: Scientific) -> do
            liftIO $ threadDelay $ floor $ n * 1000 * 1000
            nothingValue
        Nothing -> error $ "bad args to mySleep: " <> debugShowValue x
mySleep _ = error $ "bad args to mySleep"

-- used before threads implemented to test async stack traces
-- it will spawn a thread and return immediately
-- that thread will sleep for the given amount, then asynchronously
-- throw an exception to the original thread
spawnSleepThrowTo :: [Value] -> Runtime Value
spawnSleepThrowTo [x] = do
    case extractValue x of
        Just (n :: Scientific) -> do
            mainTid <- liftIO $ myThreadId
            void $ liftIO $ A.async $ do
                 threadDelay $ floor $ n * 1000 * 1000
                 throwTo mainTid A.AsyncCancelled
            nothingValue
            
        Nothing -> error $ "bad args to spawnSleepThrowTo: " <> debugShowValue x
spawnSleepThrowTo _ = error $ "bad args to spawnSleepThrowTo"

------------------------------------------------------------------------------

-- quick testing for callbacks

bmyThreadId :: [Value] -> Runtime Value
bmyThreadId [] = do
    i <- liftIO myThreadId
    makeString (show i)
bmyThreadId _ = error $ "bad args to bmyThreadId"

runCallbackN :: [Value] -> Runtime Value
runCallbackN [n, fun] = do
    let n' :: Scientific
        n' = maybe (error $ "bad first arg to runCallbackN " <> debugShowValue n) id $ extractValue n
        n'' = maybe (error $ "arg should be int " <> show n') id $ extractInt n'
    replicateM_ n'' $ app Nothing fun []
    nothingValue
runCallbackN _ = error $ "bad args to runCallbackN"

runCallbackAsyncN :: FFITypeTag -> [Value] -> Runtime Value
runCallbackAsyncN htg [n, t, fun] = do
    let n' :: Scientific
        n' = maybe (error $ "bad first arg to runCallbackAsyncN " <> debugShowValue n) id $ extractValue n
        n'' = maybe (error $ "arg should be int " <> show n') id $ extractInt n'
        t' :: Scientific
        t' = maybe (error $ "bad second arg to runCallbackAsyncN " <> debugShowValue t) id $ extractValue t

    (h :: A.Async ()) <- do
        runIt <- createBurdockRunner
        liftIO $ A.async $ runIt $ do
            replicateM_ n'' $ do
                liftIO $ threadDelay $ floor $ t' * 1000 * 1000
                app Nothing fun []
    pure $ makeValue htg h
runCallbackAsyncN _ _ = error $ "bad args to runCallbackAsyncN"

testWait :: [Value] -> Runtime Value
testWait [h] = do
    let h' :: A.Async ()
        h' = maybe (error $ "bad arg to testWait " <> debugShowValue h) id $ extractValue h
    liftIO $ A.wait h'
    nothingValue
testWait _ = error $ "bad args to testWait"

handleThreadId :: [Value] -> Runtime Value
handleThreadId [h] = do
    let h' :: A.Async ()
        h' = maybe (error $ "bad arg to handleThreadId " <> debugShowValue h) id $ extractValue h
    makeString (show $ A.asyncThreadId h')
handleThreadId _ = error $ "bad args to handleThreadId"

------------------------------------------------------------------------------


-- quick testing for tail calls

-- pass a number, get back a bytestring of that length in bytes
makeBytestring :: FFITypeTag -> [Value] -> Runtime Value
makeBytestring btg [x] = do
    let y = maybe (error $ "non number passed to make bytestring" <> debugShowValue x) id $ extractValue x
        z = maybe (error $ "non integer passed to make bytestring: " <> show y) id $ extractInt y
        !bs = BS.replicate z 0
    pure $! makeValue btg $! bs
makeBytestring _ _ = error $ "bad args to makeBytestring"

-- pass bytestring, position, returns the byte
-- value at that position
getBytestringByte :: [Value] -> Runtime Value
getBytestringByte [bsv,pos] = do
    let bs = maybe (error $ "non bytestring passed to get bytestring" <> debugShowValue bsv) id $ extractValue bsv
        ns = maybe (error $ "non number passed to get bytestring" <> debugShowValue pos) id $ extractValue pos
        n = maybe (error $ "non integer passed to make bytestring: " <> show ns) id $ extractInt ns
        b = BS.index bs n
        sb :: Scientific
        sb = fromIntegral b
    makeNumber sb
getBytestringByte _ = error $ "bad args to getBytestringByte"

split :: [Value] -> Runtime Value
split [x] = do
    let t = maybe (error "non string passed to split") id $ extractValue x
        xs = T.splitOn " " t
    --liftIO $ putStrLn $ show xs
    makeBurdockList =<< mapM makeString xs
split _ = error $ "bad args to split"
