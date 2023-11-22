
{-

some extra ffi used in the testing

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

------------------------------------------------------------------------------

testingExtrasModule :: R.Runtime (ModuleMetadata, Value)
testingExtrasModule = R.withScope $ do

    imod <- R.getModuleValue (R.ModuleID "haskell" ["_interpreter"])
    bnum <- R.getMember Nothing imod "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (burdockNumberTI :: R.FFITypeInfo) <- R.extractFFIValue ti bnum

    let bs' = map (BEIdentifier,)
             [("simple-haskell-callback", R.Fun $ simpleHaskellCallback burdockNumberTI)
             ]
        (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<testing-extras>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))

------------------------------------------------------------------------------


simpleHaskellCallback :: R.FFITypeInfo -> [Value] -> R.Runtime Value
simpleHaskellCallback burdockNumberTI [fn, nm] = do
    Right (nm' :: Scientific) <- R.extractFFIValue burdockNumberTI nm
    nm'' <- R.makeFFIValue burdockNumberTI (nm' * 2)
    R.app Nothing fn [nm'']
    
simpleHaskellCallback _ _ = error "bad args to haskellError"
