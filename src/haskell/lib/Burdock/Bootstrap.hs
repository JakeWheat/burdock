
{-

built in ffi module used to bootstrap the interpreter/handle

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Bootstrap
    (burdockBootstrapModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Control.Arrow (first, second)

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Data.IORef
    (IORef
    --,readIORef
    ,modifyIORef
    ,newIORef
    )

import qualified Burdock.Runtime as R
import Burdock.Runtime (Value)
import Burdock.Scientific (Scientific)

------------------------------------------------------------------------------

burdockBootstrapModule :: R.Runtime [(Text, Value)]
burdockBootstrapModule = do

    -- get the ffitypetag burdock type
    -- this is used to make other ffi types
    -- and it's added to a binding in burdock
    ffiTypeInfo <- R.getFFITypeInfoTypeInfo

    -- create number type
    burdockNumberTI <- makeNumber
    testLog <- liftIO $ newIORef (0,0)

    burdockFFITag <- R.makeFFIValue ffiTypeInfo ffiTypeInfo
    burdockNumberTag <- R.makeFFIValue ffiTypeInfo burdockNumberTI

    pure [("_type-ffitag", burdockFFITag)
         ,("_type-number", burdockNumberTag)
         
         ,("run-binary-test", R.Fun (bRunBinaryTest testLog))
         ,("demo-make-val", R.Fun (demoMakeVal burdockNumberTI))
         ,("print", R.Fun bPrint)
         ,("get-test-passes", R.Fun (getTestVal testLog 0))
         ,("get-test-failures", R.Fun (getTestVal testLog 1))
         ]

{-

planning:
ffitypetag
datadecltag
number
string
boolean + true/false pattern matching (will try to implement as datadecl)

the above need is-X variations

+ types, is-X and other support for: tuple, record, module, box?, function, method

nothing

list
either

run-task + variations

raise
load-module

data decl support, roughly
haskell-list ffitype

make-datadecl-tag, make haskell list, make variant, is type, is
variant, check variants equal, show variant

this is roughly the core of the _bootstrap module that's needed to run
the language itself

-}

------------------------------------------------------------------------------

makeNumber :: R.Runtime R.FFITypeInfo
makeNumber = do
    R.makeFFIType ["_bootstrap", "number"]
        [R.ToRepr $ \(v :: Scientific) -> pure $ show v
        ,R.Compare $ (pure .) . compare
        ,R.Arith
            {R.aAdd = (pure .) . (+)
            ,R.aSub = (pure .) . (-)
            ,R.aMult = (pure .) . (*)
            ,R.aDiv = (pure .) . (/)}]
    
------------------------------------------------------------------------------

runBinaryTest :: IORef (Int,Int)
              -> Text
              -> R.Value
              -> R.Value
              -> R.Value
              -> Text
              -> R.Runtime ()
runBinaryTest tally msg v0 v1 op opFailString = do
    -- todo: get the original source positions in here
    v0' <- R.runTask $ R.app Nothing v0 []
    v1' <- R.runTask $ R.app Nothing v1 []
    case (v0',v1') of
        (Right v0'', Right v1'') -> do
            -- todo: have to run-task the call to op
            r <- R.app Nothing op [v0'', v1'']
            case r of
                R.Boolean True -> liftIO $ do
                    liftIO $ modifyIORef tally (second (+1))
                    liftIO $ putStrLn $ "PASS: " <> msg
                R.Boolean False -> do
                    liftIO $ modifyIORef tally (first (+1))
                    -- todo: have to runtask the call to showValue
                    sv0 <- R.showValue v0''
                    sv1 <- R.showValue v1''
                    liftIO $ putStrLn $ T.unlines
                        ["FAIL: " <> msg
                        ,sv0
                        ,opFailString
                        ,sv1]
                _ -> error $ "non bool from test predicate: " <> R.debugShowValue r
        (Left er0, Left er1) ->
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0
                 ,"RHS raised: " <> er1]
        (Left er0, Right {}) ->
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0]
        (Right {}, Left er1) ->
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"RHS raised: " <> er1]

getTestVal :: IORef (Int,Int) -> Int -> [Value] -> R.Runtime Value
getTestVal = undefined

bRunBinaryTest :: IORef (Int,Int) -> [Value] -> R.Runtime Value
bRunBinaryTest tally [R.BText msg
                     ,v0
                     ,v1
                     ,op
                     ,R.BText opFailString] = do
    runBinaryTest tally msg v0 v1 op opFailString
    pure R.BNothing
    
bRunBinaryTest _ _ = error $ "bad args to bRunBinaryTest"

------------------------------------------------------------------------------

demoMakeVal :: R.FFITypeInfo -> [Value] -> R.Runtime Value
demoMakeVal nti [R.Number n] = R.makeFFIValue nti n
demoMakeVal _ _ = error "bad args to demoMakeVal"

--------------------------------------

bPrint :: [Value] -> R.Runtime Value
bPrint [R.BText t] = do
    liftIO $ putStrLn t
    pure R.BNothing
bPrint [v] = do
    f <- R.getMember Nothing v "_torepr"
    -- todo: how do functions get a source position from syntax
    -- if that's how they are called?
    r <- R.app Nothing f []
    case r of
        R.BText t -> liftIO $ putStrLn t
        _ -> error $ "non text returned from x._torepr()" <> R.debugShowValue v <> " " <> R.debugShowValue r
    pure R.BNothing

bPrint _ = error "bad args to print"
