
{-

wrap the runtime api to the concurrency features in burdock language namespace/access

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.Concurrency
    (burdockConcurrencyModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error)

import qualified Burdock.Runtime as R

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

burdockConcurrencyModule :: R.Runtime (ModuleMetadata, R.Value)
burdockConcurrencyModule = R.withScope $ do

    ffiTypeInfo <- R.getFFITypeInfoTypeInfo
    threadHandleTI <- makeThreadHandleType
    threadHandleFFITag <- R.makeFFIValue ffiTypeInfo threadHandleTI

    let bs' =
            [(BEType 0, ("_type-thread-handle", threadHandleFFITag))
            ] ++ map (BEIdentifier,)
            [("spawn", R.Fun $ spawn threadHandleTI)
            ,("send", R.Fun $ send threadHandleTI)
            ,("receive-any", R.Fun receive)
            ,("self", R.Fun $ self threadHandleTI)
            ,("wait", R.Fun $ wait threadHandleTI)
            ,("wait-either", R.Fun $ waitEither threadHandleTI)
            ]
        -- todo: what's a nice way to make all the originids unique for ffi code
        -- also, this is rubbish hack
    let (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<concurrency>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))

makeThreadHandleType :: R.Runtime R.FFITypeInfo
makeThreadHandleType = do
    R.makeFFIType ["concurrency", "thread-handle"]
        [R.ToRepr $ \th -> pure $ "<thread-handle:" <> R.debugShowThreadHandle th <>  ">"
        ,R.Equals $ (pure .) . R.compareThreadHandles]

spawn :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
spawn thTI [f] = do
    h <- R.spawn (R.app Nothing f [])
    R.makeFFIValue thTI h
spawn _ _ = error $ "bad args to spawn"

send :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
send thTI [th, v] = do
    th' <- either (const $ error $ "bad value to send") id
           <$> R.extractFFIValue thTI th
    R.send v th'
    pure R.BNothing
send _ _ = error $ "bad args to send"

receive :: [R.Value] -> R.Runtime R.Value
receive [] = R.receive
receive _ = error $ "bad args to receive"

self :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
self thTI [] = R.makeFFIValue thTI =<< R.self
    
self _ _ = error $ "bad args to self"

wait :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
wait thTI [th] = do
    th' <- either (const $ error $ "bad value to wait") id
           <$> R.extractFFIValue thTI th
    R.wait th'
wait _ _ = error $ "bad args to wait"

waitEither :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
waitEither _ _ = error $ "bad args to waitEither"



