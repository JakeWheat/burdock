
{-

grab bag of additional ffi functions for the built ins

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.Internals
    (burdockInternalsModule
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
    ,readIORef
    ,modifyIORef
    ,newIORef
    )

import qualified Burdock.Runtime as R
import Burdock.Runtime (Value)
import Burdock.Scientific (Scientific, showScientific)
import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

------------------------------------------------------------------------------

burdockInternalsModule :: R.Runtime (ModuleMetadata, Value)
burdockInternalsModule = R.withScope $ do

    imod <- R.getModuleValue (R.ModuleID "haskell" ["_interpreter"])
    bnum <- R.getMember Nothing imod "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (burdockNumberTI :: R.FFITypeInfo) <- R.extractFFIValue ti bnum

    testLog <- liftIO $ newIORef (0,0)
    let bs' = map (BEIdentifier,)
             [("tostring", R.Fun bToString)
             ,("print", R.Fun bPrint)
             ,("haskell-error", R.Fun haskellError)
             ]
        (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<internals>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))

------------------------------------------------------------------------------

haskellError :: [Value] -> R.Runtime Value
haskellError [R.BString v] = error v
haskellError _ = error "bad args to haskellError"

-- todo: print should use tostring on a single value
--   so it needs to closure capture the tostring function
bPrint :: [Value] -> R.Runtime Value
bPrint [R.BString t] = do
    liftIO $ putStrLn t
    pure R.BNothing
bPrint [v] = do
    f <- R.getMember Nothing v "_torepr"
    -- todo: how do functions get a source position from syntax
    -- if that's how they are called?
    r <- R.app Nothing f []
    case r of
        R.BString t -> liftIO $ putStrLn t
        _ -> error $ "non text returned from x._torepr()" <> R.debugShowValue v <> " " <> R.debugShowValue r
    pure R.BNothing
bPrint _ = error "bad args to print"

bToString :: [Value] -> R.Runtime Value
bToString [v@(R.BString {})] = pure v
bToString [v] = do
    f <- R.getMember Nothing v "_torepr"
    R.app Nothing f []
bToString _ = error "bad args to tostring"
