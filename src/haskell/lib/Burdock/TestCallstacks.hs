


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.TestCallstacks
    (testCallstacks
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

testCallstacks :: Runtime HaskellModule
testCallstacks = do

    tempHandle <- makeFFIType "temp-handle" opaque

    mySleep' <- makeFunction mySleep
    spawnSleepThrowTo' <- makeFunction spawnSleepThrowTo
    bmyThreadId' <- makeFunction bmyThreadId
    runCallbackN' <- makeFunction runCallbackN
    runCallbackAsyncN' <- makeFunction (runCallbackAsyncN tempHandle)
    testWait' <- makeFunction testWait
    handleThreadId' <- makeFunction handleThreadId

    makeHaskellModule
        [Type tempHandle
        ,Identifier "sleep" mySleep'
        ,Identifier "spawn-sleep-throw-to" spawnSleepThrowTo'
        ,Identifier "my-thread-id" bmyThreadId'
        ,Identifier "run-callback-n" runCallbackN'
        ,Identifier "run-callback-async-n" runCallbackAsyncN'
        ,Identifier "test-wait" testWait'
        ,Identifier "handle-thread-id" handleThreadId'
        ]

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
