{-# LANGUAGE OverloadedStrings #-}
module Burdock.DefaultRuntime
    (initRuntime
    ) where

--import qualified Burdock.Syntax as S
import Burdock.Runtime
    (Value(..)
    --,runBurdock
    ,Runtime
    --,liftIO

    --,ffimethodapp
    --,RuntimeState
    --,emptyRuntimeState
    ,addFFIType

    --,getMember
    --,app

    ,makeValue
    ,extractValue
    ,makeFunctionValue
    ,Type(..)
    ,Scientific
    )

--import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
import qualified Data.Text as T


initRuntime :: Runtime ()
initRuntime = do
    addFFIType "number" (Type scientificFFI)
    pure ()
    
scientificFFI :: Text -> Value -> Runtime Value
scientificFFI "_plus" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error "not a number") id $ extractValue v1
                           n2 = maybe (error "not a number") id $ extractValue v2
                       pure $ makeValue "number" $ n1 + n2
                   _ -> error $ "bad args to number plus"
    makeFunctionValue f

scientificFFI "_equals" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error "not a number") id $ extractValue v1
                           n2 = maybe (error "not a number") id $ extractValue v2
                       pure $ VBool $ n1 == n2
                   _ -> error $ "bad args to number equals"
    makeFunctionValue f

scientificFFI m _ = error $ "unsupported field on number: " ++ T.unpack m
