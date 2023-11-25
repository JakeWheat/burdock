
{-

some extra ffi used in the testing for the interpreter itself

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.TestingExtras
    (testingExtrasModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error)

import qualified Burdock.Runtime as R
import Burdock.Runtime (Value)
import Burdock.Scientific (Scientific)
import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

import Control.Monad (void)

------------------------------------------------------------------------------

testingExtrasModule :: R.Runtime (ModuleMetadata, Value)
testingExtrasModule = R.withScope $ do

    imod <- R.getModuleValue (R.ModuleID "haskell" ["_interpreter"])
    bnum <- R.getMember Nothing imod "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (burdockNumberTI :: R.FFITypeInfo) <- R.extractFFIValue ti bnum

    let bs' = map (BEIdentifier,)
             [("simple-haskell-callback", R.Fun $ simpleHaskellCallback burdockNumberTI)
             ,("wrapped-haskell-callback", R.Fun $ wrappedHaskellCallback burdockNumberTI)
             ]
        (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<testing-extras>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))

------------------------------------------------------------------------------

simpleHaskellCallback :: R.FFITypeInfo -> [Value] -> R.Runtime Value
simpleHaskellCallback burdockNumberTI [fn, nm] = do
    Right (nm' :: Scientific) <- R.extractFFIValue burdockNumberTI nm
    nm'' <- R.makeFFIValue burdockNumberTI (nm' * 2)
    void $ R.app Nothing fn [nm'']
    pure R.BNothing
simpleHaskellCallback _ _ = error "bad args to simpleHaskellCallback"

-- get a function value from burdock
-- then wrap it so it can be passed to this haskell function
-- which will call it as a callback without any burdock specific
-- monad or types
myHaskellTripleAndCallback :: Scientific -> (Scientific -> IO ()) -> IO ()
myHaskellTripleAndCallback n f = f (n * 3)

wrappedHaskellCallback :: R.FFITypeInfo -> [Value] -> R.Runtime Value
wrappedHaskellCallback burdockNumberTI [fn, nm] = do
    -- create a wrapper for fn that takes a haskell data type argument
    -- and runs in IO
    myRunBurdock <- R.getRuntimeRunner
    let w :: Scientific -> IO ()
        w n = myRunBurdock $ do
            -- there will be some boilerplate helpers that will make
            -- this wrapping/unwrapping trivial
            n' <- R.makeFFIValue burdockNumberTI n
            void $ R.app Nothing fn [n']
    -- extract the number, and call the native haskell function which
    -- takes a callback
    Right (nm' :: Scientific) <- R.extractFFIValue burdockNumberTI nm
    R.liftIO $ myHaskellTripleAndCallback nm' w
    
    pure R.BNothing
wrappedHaskellCallback _ _ = error "bad args to wrappedHaskellCallback"
