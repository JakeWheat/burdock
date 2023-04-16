{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.DefaultRuntime
    (initRuntime
    ) where

--import qualified Burdock.Syntax as S
import Burdock.Runtime
    (Value(..)
    --,runBurdock
    ,Runtime
    ,liftIO

    --,ffimethodapp
    --,RuntimeState
    --,emptyRuntimeState
    ,addFFIType
    ,addBinding

    ,debugShowValue

    ,makeList
    ,extractList

    ,getMember
    ,app

    ,makeValue
    ,makeVariant
    ,variantTag
    ,extractValue
    ,makeFunctionValue
    ,Type(..)
    ,Scientific
    )

--import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (when)

initRuntime :: Runtime ()
initRuntime = do
    addFFIType "number" (Type scientificFFI)
    addFFIType "string" (Type stringFFI)
    addFFIType "boolean" (Type booleanFFI)
    addBinding "print" =<< makeFunctionValue myPrint
    addBinding "do-is-test" =<< makeFunctionValue doIsTest
    addBinding "make-list" =<< makeFunctionValue myMakeList
    addBinding "make-variant" =<< makeFunctionValue myMakeVariant
    addBinding "is-variant" =<< makeFunctionValue myIsVariant
    addBinding "debug-print" =<< makeFunctionValue myDebugPrint
    
    -- should true be a built in value (built into the runtime), or an ffi
    -- value, or a agdt?
    addBinding "true" (makeValue "boolean" True)
    addBinding "false" (makeValue "boolean" False)
   
    pure ()

_stubType :: Text -> Text -> Value -> Runtime Value
_stubType nm m _ = error $ "called method " ++ T.unpack m ++ " on type " ++ T.unpack nm

scientificFFI :: Text -> Value -> Runtime Value
scientificFFI "_plus" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " ++ T.unpack (debugShowValue v1)) id $ extractValue v1
                           n2 = maybe (error $ "not a number " ++ T.unpack (debugShowValue v2)) id $ extractValue v2
                       pure $ makeValue "number" $ n1 + n2
                   _ -> error $ "bad args to number plus"
    makeFunctionValue f

scientificFFI "_times" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " ++ T.unpack (debugShowValue v1)) id $ extractValue v1
                           n2 = maybe (error $ "not a number " ++ T.unpack (debugShowValue v2)) id $ extractValue v2
                       pure $ makeValue "number" $ n1 * n2
                   _ -> error $ "bad args to number times"
    makeFunctionValue f


scientificFFI "_equals" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " ++ T.unpack (debugShowValue v1)) id $ extractValue v1
                           mn2 = extractValue v2
                       pure $ maybe (makeValue "boolean" False) (makeValue "boolean" . (n1 ==)) mn2

                       --    n2 = maybe (error $ "not a number " ++ T.unpack (debugShowValue v2)) id $ extractValue v2
                       --pure $ makeValue "boolean" $ n1 == n2
                   _ -> error $ "bad args to number equals"
    makeFunctionValue f
scientificFFI m _ = error $ "unsupported field on number: " ++ T.unpack m


stringFFI :: Text -> Value -> Runtime Value
stringFFI "_plus" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Text
                           n1 = maybe (error "not a string") id $ extractValue v1
                           n2 = maybe (error "not a string") id $ extractValue v2
                       pure $ makeValue "string" $ T.concat [n1,n2]
                   _ -> error $ "bad args to string plus"
    makeFunctionValue f
stringFFI "_equals" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Text
                           n1 = maybe (error "not a string") id $ extractValue v1
                           n2 = maybe (error "not a string") id $ extractValue v2
                       pure $ makeValue "boolean" $ n1 == n2
                   _ -> error $ "bad args to string equals"
    makeFunctionValue f
stringFFI m _ = error $ "unsupported field on string: " ++ T.unpack m

booleanFFI :: Text -> Value -> Runtime Value
booleanFFI "_equals" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Bool
                           n1 = maybe (error "not a boolean") id $ extractValue v1
                           mn2 = extractValue v2
                       pure $ maybe (makeValue "boolean" False) (makeValue "boolean" . (n1 ==)) mn2
                   _ -> error $ "bad args to boolean equals"
    makeFunctionValue f
booleanFFI m _ = error $ "unsupported field on boolean: " ++ T.unpack m




myPrint :: [Value] -> Runtime Value
myPrint [v] = do
    case extractValue v of
        Nothing -> error "wrong type passed to print"
        Just s -> liftIO $ putStrLn $ T.unpack s
    pure VNothing

myPrint _ = error $ "bad args to myPrint"


-- todo: this needs to be desugared completely differently to catch
-- exceptions, etc.
doIsTest :: [Value] -> Runtime Value
doIsTest [v1,v2, m1, m2] = do

    eqm <- getMember v1 "_equals" 
    res <- app eqm [v2]
    let res' = case extractValue res of
                   Just (y :: Bool) -> y
                   Nothing -> error $ "wrong return type for equals"
    let f Nothing = "nothing"
        f (Just t) = T.unpack t
        m1' = f $ extractValue m1
        m2' = f $ extractValue m2
    
    liftIO $ putStrLn $ (if res' then "PASS" else "FAIL") <> " " <> m1' <> " is " <> m2'
    pure VNothing


doIsTest _ = error $ "bad args to doIsTest"

myMakeList :: [Value] -> Runtime Value
myMakeList vs = pure $ makeList vs

myMakeVariant :: [Value] -> Runtime Value
myMakeVariant [nm, flds, es] = do
    let t :: Text
        t = maybe (error $ "bad args to make variant, first arg is not text")
            id $ extractValue nm
        flds' = maybe (error $ "bad args to make variant, second arg is not list")
                id $ extractList flds
        flds'' :: [Text]
        flds'' = flip map flds' $ \f -> maybe (error $ "bad args to make variant, second arg has non string in list")
                 id $ extractValue f
        es' = maybe (error $ "bad args to make variant, third arg is not list")
                id $ extractList es
                                        
    when (length es' /= length flds') $ error $ "wrong number of args to create variant " ++ T.unpack t
    makeVariant t $ zip flds'' es'
myMakeVariant _ = error $ "bad args to makeVariant"

myIsVariant :: [Value] -> Runtime Value
myIsVariant [nm, x] = do
    
    let nm' :: Text
        nm' = maybe (error $ "bad args to is variant, first arg is not text")
            id $ extractValue nm
    x' <- variantTag x
    case x' of
        Nothing -> do
            --liftIO $ putStrLn "wrong type"
            pure $ makeValue "boolean" False
        Just t -> do
            --liftIO $ putStrLn $ "check " <> T.unpack nm' <> " " <> T.unpack t
            pure $ makeValue "boolean" $ nm' == t
        
myIsVariant _ = error $ "bad args to myIsVariant"

myDebugPrint :: [Value] -> Runtime Value
myDebugPrint [x] = do
    liftIO $ putStrLn $ T.unpack $ debugShowValue x
    pure VNothing
myDebugPrint _ = error $ "bad args to myDebugPrint"

