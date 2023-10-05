


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.DemoHaskellModule
    (createModule
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error)

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
    ,makeValue
    ,debugShowValue
    )

import Burdock.HaskellModulePlugin
    (HaskellModule(..)
    ,makeHaskellModule
    ,importModule
    ,ImportSource(..)
    ,ModuleMember(..)
    ,makeFFIType
    ,opaque
    )

import Burdock.Scientific (Scientific)

import Data.IORef (newIORef, readIORef, modifyIORef)

import qualified Data.Text as T

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

data HaskellValue = HaskellValue Scientific

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

    myType <- makeFFIType "my-type" opaque

    makeHaskellValue <- makeFunction $ \case
        [n'] | Just n <- extractValue n' -> do
            let v = HaskellValue n
            pure $ makeValue myType v
        _ -> error $ "bad args to makeHaskellValue"

    -- todo: use better helpers with the tags and stuff being checked
    -- and producing nice error messages
    modifyHaskellValue <- makeFunction $ \case
        [n'] | Just (HaskellValue n) <- extractValue n' -> do
            pure $ makeValue myType $ HaskellValue $ n + 1
        _ -> error $ "bad args to modifyHaskellValue"

    unwrapHaskellValue <- makeFunction $ \case
        [n'] | Just (HaskellValue n) <- extractValue n' -> do
            makeNumber n
        xs -> error $ "bad args to unwrapHaskellValue"
    
    makeHaskellModule [Identifier "a" a
                      ,Identifier "add-one" addOne
                      ,Identifier "callback-burdock" cbb
                      ,Identifier "read-haskell-var" readHaskellVar
                      ,Identifier "increment-haskell-var" incrementHaskellVar
                      ,Identifier "call-abs-haskell" callAbs
                      ,Identifier "reexported-abs" babs
                      ,Type myType
                      ,Identifier "make-haskell-value" makeHaskellValue
                      ,Identifier "modify-haskell-value" modifyHaskellValue
                      ,Identifier "unwrap-haskell-value" unwrapHaskellValue
                      ]
        
