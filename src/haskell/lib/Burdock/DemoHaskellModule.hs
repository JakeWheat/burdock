


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.DemoHaskellModule
    (createModule
    ) where

-- todo: make the HaskellModulePlugin into a general haskell ffi api
-- then everything users should access goes through that, and they
-- don't import the runtime directly
import Burdock.Runtime
    (Runtime
    --,makeRecord
    ,makeNumber
    ,Value
    ,extractValue
    ,makeFunction
    ,app
    ,nothingValue
    ,liftIO
    ,getMember
    )

import Burdock.HaskellModulePlugin
    (HaskellModule(..)
    ,makeHaskellModule
    ,importModule
    ,ImportSource(..)
    )

import Burdock.Scientific (Scientific)

import Data.IORef (newIORef, readIORef, modifyIORef)

haskellAddOne :: [Value] -> Runtime Value
haskellAddOne = \case
    [n] | Just (i :: Scientific) <- extractValue n
          -> makeNumber $ i + 1
    _ -> error $ "bad args to haskell add one"

callbackToBurdockDemo :: [Value] -> Runtime Value
callbackToBurdockDemo = \case
    [f] -> do
        nm <- makeNumber 3
        app Nothing f [nm]
    _ -> error $ "bad args to callbackToBurdockDemo"

createModule :: Runtime HaskellModule
createModule = do
    a <- makeNumber 2
    addOne <- makeFunction haskellAddOne
    cbb <- makeFunction callbackToBurdockDemo

    haskellVar <- liftIO $ newIORef 0
    readHaskellVar <- makeFunction $ \case
        [] -> do
            n <- liftIO $ readIORef haskellVar
            makeNumber n
        _ -> error $ "bad args to readhaskellvar"

    incrementHaskellVar <- makeFunction $ \case
        [] -> do
            liftIO $ modifyIORef haskellVar (+1)
            nothingValue
        _ -> error $ "bad args to incrementHaskellVar"

    numbers <- importModule $ ImportName ["numbers"]
    babs <- getMember numbers "num-abs"

    callAbs <- makeFunction $ \case
        [x] -> do
            app Nothing babs [x]
        _ -> error $ "bad args to callAbs"
    
    makeHaskellModule [("a", a)
                      ,("add-one", addOne)
                      ,("callback-burdock", cbb)
                      ,("read-haskell-var", readHaskellVar)
                      ,("increment-haskell-var", incrementHaskellVar)
                      ,("call-abs-haskell", callAbs)
                      ,("reexported-abs", babs)
                      ]
        
