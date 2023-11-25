{-

start of the testing plugin, try to make the code more modular
not sure if will make it a user api to be able to swap out testing plugins

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.Testing
    (burdockTestingModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error)

import Data.Text.IO (putStrLn)
import Data.Text (Text)
import qualified Data.Text as T

import Data.IORef
    (IORef
    ,readIORef
    ,modifyIORef
    ,newIORef
    )

import Control.Arrow (first, second)


import qualified Burdock.Runtime as R
import Burdock.Scientific (Scientific)

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

------------------------------------------------------------------------------

burdockTestingModule :: R.Runtime (ModuleMetadata, R.Value)
burdockTestingModule = R.withScope $ do

    imod <- R.getModuleValue (R.ModuleID "haskell" ["_interpreter"])
    bnum <- R.getMember Nothing imod "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (burdockNumberTI :: R.FFITypeInfo) <- R.extractFFIValue ti bnum
    
    testLog <- R.liftIO $ newIORef (0,0)

    let bs' = map (BEIdentifier,)
             [("run-binary-test", R.Fun (bRunBinaryTest testLog))
             ,("get-test-passes", R.Fun (getTestVal testLog 0 burdockNumberTI))
             ,("get-test-failures", R.Fun (getTestVal testLog 1 burdockNumberTI))
             ]
        -- todo: what's a nice way to make all the originids unique for ffi code
        -- also, this is rubbish hack
        (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<testing>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))


------------------------------------------------------------------------------

runBinaryTest :: IORef (Int,Int)
              -> Text
              -> R.Value
              -> R.Value
              -> R.Value
              -> Text
              -> R.Runtime ()
runBinaryTest tally msg lv0 lv1 op opFailString = do
    -- todo: get the original source positions in here
    v0 <- R.runTask $ R.app Nothing lv0 []
    v1 <- R.runTask $ R.app Nothing lv1 []
    case (v0,v1) of
        (Right v0', Right v1') -> do
            expressionsOK v0' v1'
        (Left er0, Left er1) -> do
            R.liftIO $ modifyIORef tally (second (+1))
            er0s <- safeToRepr er0
            er1s <- safeToRepr er1
            R.liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0s
                 ,"RHS raised: " <> er1s]
        (Left er0, Right {}) -> do
            R.liftIO $ modifyIORef tally (second (+1))
            er0s <- safeToRepr er0
            R.liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0s]
        (Right {}, Left er1) -> do
            R.liftIO $ modifyIORef tally (second (+1))
            er1s <- safeToRepr er1
            R.liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"RHS raised: " <> er1s]
  where
    expressionsOK v0 v1 = do
        r <- R.runTask $ R.app Nothing op [v0, v1]
        case r of
            Right rx -> predicateOK rx v0 v1
            Left er -> do
                R.liftIO $ modifyIORef tally (second (+1))
                ers <- safeToRepr er
                R.liftIO $ putStrLn $ T.unlines
                    ["FAIL: " <> msg
                    ,"predicate raised: " <> ers]
    predicateOK r v0 v1 = 
         case r of
            R.Boolean True -> R.liftIO $ do
                R.liftIO $ modifyIORef tally (first (+1))
                R.liftIO $ putStrLn $ "PASS: " <> msg
            R.Boolean False -> do
                R.liftIO $ modifyIORef tally (second (+1))
                sv0 <- safeToRepr v0
                sv1 <- safeToRepr v1
                R.liftIO $ putStrLn $ T.unlines
                    ["FAIL: " <> msg
                    ,sv0
                    ,opFailString
                    ,sv1]
            _ -> do
                R.liftIO $ modifyIORef tally (second (+1))
                er <- safeToRepr r
                error $ "non bool from test predicate: " <> er -- R.debugShowValue r
    safeToRepr v = do
        r <- R.runTask $ do
            tr <- R.getMember Nothing v "_torepr"
            R.app Nothing tr []
        case r of
            Left e -> case e of
                -- todo: another fucking call to safetorepr
                R.BString e' -> pure $ e' <> " : " <> R.debugShowValue v
                e1 -> pure $ R.debugShowValue e1
            Right (R.BString t) -> pure t
            Right x -> pure $ R.debugShowValue v <> " torepr: " <> R.debugShowValue x

getTestVal :: IORef (Int,Int) -> Int -> R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
getTestVal v i nti = \case
    [] -> do
        (a,b) :: (Int,Int) <- R.liftIO $ readIORef v
        if i == 0
            then R.makeFFIValue nti $ ((fromIntegral a) :: Scientific)
            else R.makeFFIValue nti $ ((fromIntegral b) :: Scientific)
    _ -> error $ "bad args to getTestVal"

bRunBinaryTest :: IORef (Int,Int) -> [R.Value] -> R.Runtime R.Value
bRunBinaryTest tally [R.BString msg
                     ,v0
                     ,v1
                     ,op
                     ,R.BString opFailString] = do
    runBinaryTest tally msg v0 v1 op opFailString
    pure R.BNothing
bRunBinaryTest _ _ = error $ "bad args to bRunBinaryTest"

