{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Burdock.DefaultRuntime
    (initRuntime
    ,prelude
    ,bootstrap
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
    ,addTestPass
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

    ,makeBurdockList
    ,extractBurdockList

    ,getMember
    ,app

    ,runTask
    
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
import qualified Data.Text.Lazy as L

import qualified Text.RawString.QQ as R

import Burdock.Scientific (showScientific)

import Control.Monad
    (when
    ,forM
    ,void
    )

import Data.Dynamic
    (Typeable
    )

import Control.Concurrent
    (threadDelay
    ,throwTo
    ,myThreadId
    )

import Control.Concurrent.Async
    (async
    ,AsyncCancelled(AsyncCancelled)
    )

-- temp hack

bootstrap :: L.Text
bootstrap = [R.r|

_record_torepr = method(self):
   show-record(self)
 end

_record_equals = method(self, b):
   check-variants-equal([list:],self,b)
 end

_tuple_torepr = method(self):
   show-tuple(self)
 end

_tuple_equals = method(self, b):
   check-variants-equal([list:],self,b)
 end
             
             |]

prelude :: L.Text
prelude = [R.r|

data Either:
  | left(v)
  | right(v)
end

_run-task-fixup = lam(x):
    cases x:
      | left(es) => left(es.exception)
      | _ => x
    end
  end

data TestResult:
  | test-pass(name)
  | test-fail(name,msg)
end

my-do-bpred-test = lam(bpredf, pred-string, anti-pred-string, m1, m2, ev1, ev2):
  name = m1 + " " + pred-string + " " + m2

  safe-compare = lam(v1,v2):
    res = run-task(bpredf(v1,v2))
    #print(v1)
    #print(v2)
    #print(bpredf(v1,v2))
    cases res:
      | left(e) => test-fail(name, torepr-debug(v1) + " " + pred-string + " " + torepr-debug(v2) + " raised " + torepr-debug(e))
      | right(true) => test-pass(name)
      | right(false) => test-fail(name, torepr-debug(v1) + "\n" + anti-pred-string + "\n" + torepr-debug(v2))
    end
  end

  cases {ev1;ev2}:
    | {left(e1); left(e2)} =>
      test-fail(name, "LHS error: " + torepr-debug(e1) + "\nRHS error: " + torepr-debug(e2))
    | {left(e1); right(_)} =>
      test-fail(name, "LHS error: " + torepr-debug(e1))
    | {right(_); left(e2)} =>
      test-fail(name, "RHS error: " + torepr-debug(e2))
    | {right(v1); right(v2)} => safe-compare(v1,v2)
  end
       
end

format-test = lam(t):
  cases t:
    | test-pass(name) => "PASS " + name
    | test-fail(name,msg) => "FAIL " + name + "\n" + indent(msg)
  end
end

log-result = lam(t):
  cases t:
    | test-pass(_) => add-test-pass()
    | test-fail(_,_) => add-test-fail()
  end
end

do-is-test = lam(m1,m2,v1,v2):
  r = my-do-bpred-test(lam(a,b): a == b end, "is", "!=", m1, m2, v1, v2)
  log-result(r)
  print(format-test(r))
end

do-is-not-test = lam(m1,m2,v1,v2):
  r = my-do-bpred-test(lam(a,b): not(a == b) end, "is-not", "==", m1, m2, v1, v2)
  log-result(r)
  print(format-test(r))
end

fun list-map(l, f):
  if l == [list:]: [list:]
  else:
    [list: f(l.first)] + list-map(l.rest,f)
  end
end


  |]


initRuntime :: Runtime ()
initRuntime = do
    addFFIType "number" (Type scientificFFI)
    addFFIType "string" (Type stringFFI)
    addFFIType "boolean" (Type booleanFFI)
    addBinding "print" =<< makeFunctionValue myPrint
    addBinding "make-burdock-list" =<< makeFunctionValue myMakeBurdockList
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

    addBinding "not" =<< makeFunctionValue myNot

    addBinding "sleep" =<< makeFunctionValue mySleep
    addBinding "spawn-sleep-throw-to" =<< makeFunctionValue spawnSleepThrowTo

    addBinding "add-test-pass" =<< makeFunctionValue myAddTestPass
    addBinding "add-test-fail" =<< makeFunctionValue myAddTestFail
    addBinding "indent" =<< makeFunctionValue indent

    addBinding "torepr-debug" =<< makeFunctionValue toreprDebug

    addBinding "gremlin" (makeValue "gremlintype" False)

    pure ()

_stubType :: Text -> Text -> Value -> Runtime Value
_stubType nm m _ = error $ "called method " <> m <> " on type " <> nm

unaryMember :: Typeable a => Text -> Text -> Text -> (a -> Text) -> Value -> [Value] -> Runtime Value
unaryMember burName inType outType f v as =
    case as of
        [] -> do
            let v1 = maybe (error $ "u not a " <> inType <> " " <> debugShowValue v) id $ extractValue v
            pure $ makeValue outType $ f v1
        _ -> error $ "bad args to " <> inType <> " " <> burName


binaryMember :: (Typeable a, Typeable b) =>
    Text -> Text -> Text -> (a -> a -> b) -> Value -> [Value] -> Runtime Value
binaryMember burName inType outType f v1 as =
    case as of
        [v2] -> do
            let n1 = maybe (error $ "b0 not a " <> inType <> " " <> debugShowValue v1) id $ extractValue v1
                n2 = maybe (error $ "b1 not a " <> inType <> " " <> debugShowValue v2) id $ extractValue v2
            pure $ makeValue outType $ f n1 n2
        _ -> error $ "bad args to " <> inType <> " " <> burName

binaryMemberLax :: (Typeable a, Typeable b) =>
    Text -> Text -> Text -> (a -> a -> b) -> Value -> [Value] -> Runtime Value
binaryMemberLax burName inType outType f v1 as =
    case as of
        [v2] -> do
            let n1 = maybe (error $ "bl not a " <> inType <> " " <> debugShowValue v1) id $ extractValue v1
                mn2 = extractValue v2
                -- todo: make false an arg?
            pure $ maybe (makeValue outType False) (makeValue outType . (f n1)) mn2
        _ -> error $ "bad args to " <> inType <> " " <> burName


scientificFFI :: Text -> Value -> Runtime Value
scientificFFI "_torepr" v =
    makeFunctionValue $ unaryMember "_torepr" "number" "string" showScientific v
scientificFFI "_plus" v1 =
    makeFunctionValue $ binaryMember "_plus" "number" "number" ((+) :: Scientific -> Scientific -> Scientific) v1
scientificFFI "_minus" v1 = do
    makeFunctionValue $ binaryMember "_plus" "number" "number" ((-) :: Scientific -> Scientific -> Scientific) v1
scientificFFI "_times" v1 = do
    makeFunctionValue $ binaryMember "_plus" "number" "number" ((*) :: Scientific -> Scientific -> Scientific) v1
scientificFFI "_equals" v1 = do
    makeFunctionValue $ binaryMemberLax "_equals" "number" "boolean" ((==) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_lessequal" v1 = do
    makeFunctionValue $ binaryMember "_lessequal" "number" "boolean" ((<=) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_lessthan" v1 = do
    makeFunctionValue $ binaryMember "_lessthan" "number" "boolean" ((<) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_greaterthan" v1 = do
    makeFunctionValue $ binaryMember "_greaterthan" "number" "boolean" ((>) :: Scientific -> Scientific -> Bool) v1
scientificFFI m _ = error $ "unsupported field on number: " <> m


stringFFI :: Text -> Value -> Runtime Value
stringFFI "_plus" v1 =
    makeFunctionValue $ binaryMember "_plus" "string" "string" ((<>) :: Text -> Text -> Text) v1
stringFFI "_equals" v1 =
    makeFunctionValue $ binaryMemberLax "_equals" "string" "boolean" ((==) :: Text -> Text -> Bool) v1
stringFFI "_torepr" v1 = do
    makeFunctionValue $ unaryMember "_torepr" "string" "string" (show :: (Text -> Text)) v1
stringFFI m _ = error $ "unsupported field on string: " <> m

booleanFFI :: Text -> Value -> Runtime Value
booleanFFI "_torepr" v1 = do
    makeFunctionValue $ unaryMember "_torepr" "boolean" "string" disp v1
  where
    disp n1 = if n1 then ("true" :: Text) else "false"
booleanFFI "_equals" v1 = do
    makeFunctionValue $ binaryMemberLax "_equals" "boolean" "boolean" ((==) :: Bool -> Bool -> Bool) v1
booleanFFI m _ = error $ "unsupported field on boolean: " <> m

myPrint :: [Value] -> Runtime Value
myPrint [v] = do
    t <- myToString [v]
    case extractValue t of
        Nothing -> liftIO $ putStrLn $ debugShowValue v
        Just s -> liftIO $ putStrLn s
    pure VNothing
myPrint _ = error $ "bad args to myPrint"

indent :: [Value] -> Runtime Value
indent [x] | Just t <- extractValue x = do
    let ls = T.lines t
        ls' = map ("  " <>) ls
    pure $ makeValue "string" $ T.unlines ls'
indent _ = error $ "bad args to indent"

myMakeBurdockList :: [Value] -> Runtime Value
myMakeBurdockList vs = makeBurdockList vs

myMakeVariant :: [Value] -> Runtime Value
myMakeVariant [nm, flds, es] = do
    let t :: Text
        t = maybe (error $ "bad args to make variant, first arg is not text")
            id $ extractValue nm
        flds' = maybe (error $ "bad args to make variant, second arg is not list")
                id $ extractBurdockList flds
        flds'' :: [Text]
        flds'' = flip map flds' $ \f -> maybe (error $ "bad args to make variant, second arg has non string in list")
                 id $ extractValue f
        es' = maybe (error $ "bad args to make variant, third arg is not list")
                id $ extractBurdockList es
                                        
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

toreprDebug :: [Value] -> Runtime Value
toreprDebug [x] = do
    y <- runTask False $ myToRepr [x]
    case y of
        Left _ -> pure $ makeValue "string" $ debugShowValue x
        Right v -> pure v
toreprDebug _ = error $ "bad args to toreprDebug"

-- todo: decide if the flds should be passed, or this function should
-- work them out. Currently, the passed fields are ignored and this
-- function works them out using the hack auxiliary variantValueFields
checkVariantsEqual :: [Value] -> Runtime Value
checkVariantsEqual [_flds, a, b] = do
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
    makeBurdockList cs' -- makeValue "string" $ T.intercalate "," cs'
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

myNot :: [Value] -> Runtime Value
myNot [x] = case extractValue x of
    Just True -> pure $ makeValue "boolean" False
    Just False -> pure $ makeValue "boolean" True
    _ -> error $ "bad arg to not"
myNot _ = error $ "bad args to raise"

mySleep :: [Value] -> Runtime Value
mySleep [x] = do
    case extractValue x of
        Just (n :: Scientific) -> do
            liftIO $ threadDelay $ floor $ n * 1000 * 1000
            pure VNothing
            
        Nothing -> error $ "bad args to mySleep: " <> debugShowValue x
mySleep _ = error $ "bad args to mySleep"

-- used before threads implemented to test async stack traces
-- it will spawn a thread and return immediately
-- that thread will sleep for the given amount, then asynchronously
-- throw an exception to the original thread
spawnSleepThrowTo :: [Value] -> Runtime Value
spawnSleepThrowTo [x] = do
    case extractValue x of
        Just (n :: Scientific) -> do
            mainTid <- liftIO $ myThreadId
            void $ liftIO $ async $ do
                 threadDelay $ floor $ n * 1000 * 1000
                 throwTo mainTid AsyncCancelled
            pure VNothing
            
        Nothing -> error $ "bad args to spawnSleepThrowTo: " <> debugShowValue x
spawnSleepThrowTo _ = error $ "bad args to spawnSleepThrowTo"

myAddTestPass :: [Value] -> Runtime Value
myAddTestPass [] = do
    addTestPass
    pure VNothing
myAddTestPass _ = error $ "bad args to myAddTestPass"

myAddTestFail :: [Value] -> Runtime Value
myAddTestFail [] = do
    addTestFail
    pure VNothing
myAddTestFail _ = error $ "bad args to myAddTestFail"
