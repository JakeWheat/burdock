{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Burdock.DefaultRuntime
    (initRuntime
    ,prelude
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Data.List (sort)

--import qualified Burdock.Syntax as S
import Burdock.Runtime
    (Value(..)
    --,runBurdock
    ,Runtime
    ,liftIO
    ,addTestFail

    --,ffimethodapp
    --,RuntimeState
    --,emptyRuntimeState
    ,addFFIType
    ,addBinding

    ,debugShowValue
    ,throwValue

    ,nothingValue

    ,getCallStack

    ,makeList
    ,extractList

    ,getMember
    ,app

    ,catchEither
    
    ,makeValue
    ,makeVariant
    ,variantTag
    ,variantValueFields
    ,extractValue
    ,makeFunctionValue
    ,Type(..)
    ,Scientific
    )

--import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.RawString.QQ as R

import Burdock.Scientific (showScientific)

import Control.Monad
    (when
    ,forM
    )

-- temp hack
prelude :: Text
prelude = [R.r|

data Either:
  | left(v)
  | right(v)
end
  |]


initRuntime :: Runtime ()
initRuntime = do
    addFFIType "number" (Type scientificFFI)
    addFFIType "string" (Type stringFFI)
    addFFIType "boolean" (Type booleanFFI)
    addBinding "print" =<< makeFunctionValue myPrint
    addBinding "do-is-test" =<< makeFunctionValue doIsTest
    addBinding "do-is-not-test" =<< makeFunctionValue doIsNotTest
    addBinding "make-list" =<< makeFunctionValue myMakeList
    addBinding "make-variant" =<< makeFunctionValue myMakeVariant
    addBinding "is-variant" =<< makeFunctionValue myIsVariant
    addBinding "debug-print" =<< makeFunctionValue myDebugPrint
    addBinding "check-variants-equal" =<< makeFunctionValue checkVariantsEqual
    addBinding "raise" =<< makeFunctionValue raise
    addBinding "get-call-stack" =<< makeFunctionValue myGetCallStack
    addBinding "torepr" =<< makeFunctionValue myToRepr
    addBinding "tostring" =<< makeFunctionValue myToString
    addBinding "nothing" nothingValue
    addBinding "show-variant" =<< makeFunctionValue showVariant
    addBinding "show-tuple" =<< makeFunctionValue showTuple
    addBinding "show-record" =<< makeFunctionValue showRecord

    -- should true be a built in value (built into the runtime), or an ffi
    -- value, or a agdt?
    addBinding "true" (makeValue "boolean" True)
    addBinding "false" (makeValue "boolean" False)
   
    pure ()

_stubType :: Text -> Text -> Value -> Runtime Value
_stubType nm m _ = error $ "called method " <> m <> " on type " <> nm

scientificFFI :: Text -> Value -> Runtime Value
scientificFFI "_torepr" v1 = do
    let f as = case as of
                   [] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " <> debugShowValue v1) id $ extractValue v1
                       pure $ makeValue "string" $ T.pack $ showScientific n1
                       --pure $ maybe (makeValue "boolean" False) (makeValue "boolean" . (n1 ==)) mn2
                   _ -> error $ "bad args to number _torepr"
    makeFunctionValue f

scientificFFI "_plus" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " <> debugShowValue v1) id $ extractValue v1
                           n2 = maybe (error $ "not a number " <> debugShowValue v2) id $ extractValue v2
                       pure $ makeValue "number" $ n1 + n2
                   _ -> error $ "bad args to number plus"
    makeFunctionValue f

scientificFFI "_times" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " <> debugShowValue v1) id $ extractValue v1
                           n2 = maybe (error $ "not a number " <> debugShowValue v2) id $ extractValue v2
                       pure $ makeValue "number" $ n1 * n2
                   _ -> error $ "bad args to number times"
    makeFunctionValue f


scientificFFI "_equals" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Scientific
                           n1 = maybe (error $ "not a number " <> debugShowValue v1) id $ extractValue v1
                           mn2 = extractValue v2
                       pure $ maybe (makeValue "boolean" False) (makeValue "boolean" . (n1 ==)) mn2
                   _ -> error $ "bad args to number equals"
    makeFunctionValue f
scientificFFI m _ = error $ "unsupported field on number: " <> m


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
                       case extractValue v2 of
                           Nothing -> pure $ makeValue "boolean" False
                           Just n2 -> pure $ makeValue "boolean" $ n1 == n2
                   _ -> error $ "bad args to string equals"
    makeFunctionValue f
stringFFI "_torepr" v1 = do
    let f as = case as of
                   [] -> do
                       let n1 :: Text
                           n1 = maybe (error $ "not a string " <> debugShowValue v1) id $ extractValue v1
                       pure $ makeValue "string" $ show n1
                       --pure $ maybe (makeValue "boolean" False) (makeValue "boolean" . (n1 ==)) mn2
                   _ -> error $ "bad args to number _torepr"
    makeFunctionValue f

stringFFI m _ = error $ "unsupported field on string: " <> m

booleanFFI :: Text -> Value -> Runtime Value
booleanFFI "_torepr" v1 = do
    let f as = case as of
                   [] -> do
                       let n1 :: Bool
                           n1 = maybe (error "not a boolean") id $ extractValue v1
                       pure $ makeValue "string" $ if n1 then ("true" :: Text) else "false"
                   _ -> error $ "bad args to boolean equals"
    makeFunctionValue f


booleanFFI "_equals" v1 = do
    let f as = case as of
                   [v2] -> do
                       let n1 :: Bool
                           n1 = maybe (error "not a boolean") id $ extractValue v1
                           mn2 = extractValue v2
                       pure $ maybe (makeValue "boolean" False) (makeValue "boolean" . (n1 ==)) mn2
                   _ -> error $ "bad args to boolean equals"
    makeFunctionValue f
booleanFFI m _ = error $ "unsupported field on boolean: " <> m

myPrint :: [Value] -> Runtime Value
myPrint [v] = do
    t <- myToString [v]
    case extractValue t of
        Nothing -> liftIO $ putStrLn $ debugShowValue v
        Just s -> liftIO $ putStrLn s
    pure VNothing
myPrint _ = error $ "bad args to myPrint"

-- todo: this needs to be desugared completely differently to catch
-- exceptions, etc.
doIsTest :: [Value] -> Runtime Value
doIsTest [v1,v2, m1, m2] = do
    eres <- catchEither $ compareValues v1 v2
    (res,msg) <- case eres of
        Left e -> do
            t <- safeRenderException e
            pure (False, Just $ indent t)
        Right res -> do
            -- todo: if false, show the two values
            pure (res, Nothing)
    (liftIO . putStrLn) =<< makeResultString "is" res m1 m2
    case msg of
        Nothing -> pure ()
        Just m -> liftIO $ putStrLn m
    when (not res) addTestFail
    pure VNothing
doIsTest _ = error $ "bad args to doIsTest"

doIsNotTest :: [Value] -> Runtime Value
doIsNotTest [v1,v2, m1, m2] = do
    eres <- catchEither $ compareValues v1 v2
    (res,msg) <- case eres of
        Left e -> do
            t <- safeRenderException e
            pure (False, Just $ indent t)
        Right res -> do
            -- todo: if true, show the value? show both?
            pure (not res, Nothing)
    (liftIO . putStrLn) =<< makeResultString "is-not" res m1 m2
    case msg of
        Nothing -> pure ()
        Just m -> liftIO $ putStrLn m
    when (not res) addTestFail
    pure VNothing
doIsNotTest _ = error $ "bad args to doIsNotTest"


indent :: Text -> Text
indent t =
    let ls = T.lines t
        ls' = map ("  " <>) ls
    in T.unlines ls'

safeRenderException :: (Either Text Value) -> Runtime Text
safeRenderException (Left t) = pure t
safeRenderException (Right v) = do
    et <- catchEither $ do
        t <- myToRepr [v]
        case extractValue t of
            Just t' -> pure t'
            Nothing -> error $ "torepr returned non string: " <> debugShowValue t
    case et of
        Right vx -> pure vx
        Left (Left t) -> pure $ "exception when toexpr exception: " <> t
        Left (Right e) -> pure $ "exception when toexpr exception: " <> debugShowValue e

makeResultString :: Text -> Bool -> Value -> Value -> Runtime Text
makeResultString predName res m1 m2 = do
    let f Nothing = "nothing"
        f (Just t) = t
        m1' = f $ extractValue m1
        m2' = f $ extractValue m2
    pure $ (if res then "PASS" else "FAIL") <> " " <> m1' <> " " <> predName <> " " <> m2'
    

compareValues :: Value -> Value -> Runtime Bool
compareValues a b = do
    eqm <- getMember a "_equals"
    res <- app Nothing eqm [b]
    case extractValue res of
                   Just (y :: Bool) -> pure y
                   Nothing -> error $ "wrong return type for equals"

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
                                        
    when (length es' /= length flds') $ error $ "wrong number of args to create variant " <> t
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
    liftIO $ putStrLn $ debugShowValue x
    pure VNothing
myDebugPrint _ = error $ "bad args to myDebugPrint"

-- todo: decide if the flds should be passed, or this function should
-- work them out. Currently, the passed fields are ignored and this
-- function works them out using the hack auxiliary variantValueFields
checkVariantsEqual :: [Value] -> Runtime Value
checkVariantsEqual [_flds, a, b] = do
    --do 
    --    x <- myToRepr [makeList [flds,a,b]]
    --    liftIO $ putStrLn $ "checkVariantsEqual " <> maybe (error $ "f2") id (extractValue x)
    at <- variantTag a
    bt <- variantTag b
    vfldsa <- variantValueFields a
    vfldsb <- variantValueFields b
    -- hack for records:
    let exFlds t l = if t == Just "record"
                     then sort (map fst l)
                     else map fst l
    case (at,bt,vfldsa,vfldsb) of
        (Just at', Just bt',Just vfldsa', Just vfldsb') ->
            if at' == bt' &&
               exFlds at vfldsa' == exFlds bt vfldsb'
            then do
                --liftIO $ putStrLn $ "fields equal:" <> show at'
                fsEq <- forM (map fst vfldsa') $ \f -> do
                    va <- getMember a f
                    vb <- getMember b f
                    eqx <- getMember va "_equals"
                    x <- app Nothing eqx [vb]
                    --let balls = makeList [va, vb, x]
                    --iseq <- myToRepr [balls]
                    --let iseq1 = maybe (error $ "f3") id $ extractValue iseq
                    --liftIO $ putStrLn $ f <> " == " <> iseq1
                    pure x
                -- and together all the fsEq
                let andEm [] = pure $ makeValue "boolean" True
                    andEm (v:vs) = case extractValue v of
                        Just True -> andEm vs
                        Just False -> pure $ makeValue "boolean" False
                        _ -> error $ "non boolean returned from _equals method"
                andEm fsEq
            else do
                --liftIO $ putStrLn "fields not equal:"
                pure $ makeValue "boolean" False
        _ -> pure $ makeValue "boolean" False
checkVariantsEqual _ = error $ "bad args to checkVariantsEqual"

raise :: [Value] -> Runtime Value
raise [x] = throwValue x
raise _ = error $ "bad args to raise"

myGetCallStack :: [Value] -> Runtime Value
myGetCallStack [] = do
    cs <- getCallStack
    let cs' = flip map cs $ \case
            Nothing -> mt "nothing"
            Just x -> mt x
    pure $ makeList cs' -- makeValue "string" $ T.intercalate "," cs'
  where
    mt :: Text -> Value
    mt = makeValue "string"
myGetCallStack _ = error $ "bad args to myGetCallStack"

myToRepr :: [Value] -> Runtime Value
myToRepr [x] = do
    tf <- getMember x "_torepr" 
    app Nothing tf []
myToRepr _ = error $ "bad args to myToRepr"

myToString :: [Value] -> Runtime Value
myToString [x] = do
    case extractValue x of
        Just (_ :: Text) -> pure x
        Nothing -> myToRepr[x]
myToString _ = error $ "bad args to myToString"

showVariant :: [Value] -> Runtime Value
showVariant [x] = do
    at <- variantTag x
    bt <- variantValueFields x
    let trm e = do
            f <- getMember e "_torepr"
            app Nothing f []
    case (at,bt) of
        (Just n, Just fs) -> do
            let fs' = map snd fs
            fs'' <- mapM trm fs'
            let (es :: [Maybe Text]) = map extractValue fs''
            let es' :: [Text]
                es' = maybe (error "showvariant non string from torepr") id $ sequence es
            --liftIO $ putStrLn $ show es'
            pure $ if null es'
                   then makeValue "string" $ n
                   else makeValue "string" $ n <> "(" <> T.intercalate ", " es' <> ")"
        _ -> error $ "show variant called on non variant " <> debugShowValue x
    
showVariant _ = error $ "bad args to showVariant"


showTuple :: [Value] -> Runtime Value
showTuple [x] = do
    at <- variantTag x
    bt <- variantValueFields x
    let trm e = do
            f <- getMember e "_torepr"
            app Nothing f []
    case (at,bt) of
        (Just _n, Just fs) -> do
            let fs' = map snd fs
            fs'' <- mapM trm fs'
            let (es :: [Maybe Text]) = map extractValue fs''
            let es' :: [Text]
                es' = maybe (error "showTuple non string from torepr") id $ sequence es
            --liftIO $ putStrLn $ show es'
            pure $ if null es'
                   then makeValue "string" ("{}" :: Text)
                   else makeValue "string" $ "{" <> T.intercalate ";" es' <> "}"
        _ -> error $ "showTuple called on non variant " <> debugShowValue x
showTuple _ = error $ "bad args to showTuple"

showRecord :: [Value] -> Runtime Value
showRecord [x] = do
    at <- variantTag x
    bt <- variantValueFields x
    let trm e = do
            f <- getMember e "_torepr"
            app Nothing f []
    case (at,bt) of
        (Just _n, Just fs) -> do
            -- hack
            let fs' = map snd fs
                nms = map fst fs
            fs'' <- mapM trm fs'
            let (es :: [Maybe Text]) = map extractValue fs''
            let es' :: [Text]
                es' = maybe (error "showTuple non string from torepr") id $ sequence es
                es'' = zip nms es'
                esx = map (\(a,b) -> a <> ":" <> b) es''
            --liftIO $ putStrLn $ show es'
            pure $ if null es''
                   then makeValue "string" ("{}" :: Text)
                   else makeValue "string" $ "{" <> T.intercalate "," esx <> "}"
        _ -> error $ "showRecord called on non variant " <> debugShowValue x
showRecord _ = error $ "bad args to showRecord"
