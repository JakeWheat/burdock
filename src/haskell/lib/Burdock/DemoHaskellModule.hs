


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.DemoHaskellModule
    (createModule
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error,show)

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
    ,makeFunctionValue
    ,makeBool
    ,makeString
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
        _ -> error $ "bad args to unwrapHaskellValue"


    let myPairFieldThing f v1 =
            case (f,extractValue v1) of
                ("a", Just (p0::Scientific, _::Scientific)) -> makeNumber p0
                ("b", Just (_,p1)) -> makeNumber p1
                ("_equals", Just x) ->
                    -- equals need to return a function, that takes one more argument
                    -- and compares it to the original x/v1
                    makeFunctionValue $ \case
                            [y] | Just y' <- extractValue y ->
                                  makeBool $ x == y'
                            [_] | otherwise -> makeBool False
                            _ -> error $ "bad args to my-pair _equals"
                
                ("_torepr", Just x) ->
                    -- needs to return a function which doesn't take any args
                    makeFunctionValue $ \case
                            [] -> makeString $ show x
                            _ -> error $ "bad args to my-pair _torepr"
                (x, _) -> error $ "bad args, or unsupported field for my-pair: " <> x

    myPairType <- makeFFIType "my-pair-type" myPairFieldThing

    makeHaskellPair <- makeFunction $ \case
        [p0,p1] | Just (p0'::Scientific) <- extractValue p0
                , Just (p1'::Scientific) <- extractValue p1 -> do
            pure $ makeValue myPairType $ (p0',p1')
        _ -> error $ "bad args to makeHaskellPair"

    let myAdderFieldThing f v1 =
            case (f,extractValue v1) of
                ("_app", Just (n :: Scientific)) ->
                    makeFunctionValue $ \case
                        [m'] | Just m <- extractValue m' -> makeNumber $ n + m
                        _ -> error $ "bad args to myadder._app" 
                         
                _ -> error $ "bad field on my adder " <> f

    myAdder <- makeFFIType "my-adder" myAdderFieldThing

    makeMyAdder <- makeFunction $ \case
        [n'] | Just (n :: Scientific) <- extractValue n' ->
               pure $ makeValue myAdder n
        _ -> error $ "bad args to make my adder"
    
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
                      ,Type myPairType
                      ,Identifier "make-haskell-pair" makeHaskellPair
                      ,Type myAdder
                      ,Identifier "make-my-adder" makeMyAdder
                      ]
        
