{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Burdock.DefaultRuntime
    (initRuntime
    ,internals
    ,bootstrap
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Data.List (sort)

import Burdock.Runtime
    (Value(..)
    ,Runtime
    ,liftIO
    ,addTestPass
    ,addTestFail

    ,addFFIType
    ,addBinding
    ,tyName

    ,getModuleValue
    ,RuntimeImportSource(..)

    ,debugShowValue
    ,throwValue

    ,nothingValue

    ,getCallStack

    ,makeBurdockList
    ,extractBurdockList
    ,makeTuple
    ,makeString
    ,makeBool
    ,makeNumber
    ,makeDataDeclTag
    ,makeValueName

    ,getMember
    ,app

    ,runTask
    
    ,makeValue
    ,makeVariant
    ,variantName
    ,variantValueFields
    ,extractValue
    ,makeFunctionValue
    ,DataDeclTypeTag(..)
    ,Scientific
    )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import qualified Text.RawString.QQ as R

import Burdock.Scientific
    (showScientific
    )

import Control.Monad
    (when
    ,forM
    ,void
    )

import Data.Dynamic
    (Typeable
    )

import System.Process
    (readProcessWithExitCode
    
    )
import System.Exit
    (ExitCode(..)
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

import Data.IORef
    (newIORef
    ,modifyIORef
    ,readIORef)

------------------------------------------------------------------------------

-- temp hack, before modules implemented, have bootstrap burdock
-- then internals burdock, that are run in a handle when it's created
-- before any user code

bootstrap :: L.Text
bootstrap = [R.r|

_record_torepr = method(self):
   show-record(self)
 end

_record_equals = method(self, b):
   check-variants-equal(self,b)
 end

_tuple_torepr = method(self):
   show-tuple(self)
 end

_tuple_equals = method(self, b):
   check-variants-equal(self,b)
 end

data Nothing: nothing end

data list:
  | link(first, rest)
  | empty
sharing:
  method _torepr(self):
    fun intercalate-items(l):
      cases l:
        | empty => ""
        | link(x, y) =>
          cases y:
            | empty => torepr(x)
            | else => torepr(x) + ", " + intercalate-items(y)
          end
      end
    end
    cases self:
      | empty => "[list: ]"
      | link(x,y) => "[list: "
          + intercalate-items(self) + "]"
    end
  end,
  method _plus(self,b):
    cases self:
      | empty => b
      | link(x,y) => link(x, y + b)
    end
  end,
  method length(self):
    cases self:
      | empty => 0
      | link(_,b) => 1 + b.length()
    end
  end ,
  method map(self,f):
    cases self:
      | empty => empty
      | link(a,b) => link(f(a), b.map(f))
    end
  end
end

             |]

---------------------------------------

internals :: L.Text
internals = [R.r|

##################
# built in stuff
           
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

##################
# testing

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

fun log-and-print-result(r):
  log-result(r)
  print(format-test(r))
end

do-is-test = lam(m1,m2,v1,v2):
  r = my-do-bpred-test(lam(a,b): a == b end, "is", "!=", m1, m2, v1, v2)
  log-and-print-result(r)
end

do-is-not-test = lam(m1,m2,v1,v2):
  r = my-do-bpred-test(lam(a,b): not(a == b) end, "is-not", "==", m1, m2, v1, v2)
  log-and-print-result(r)
end

##################
# more built in stuff

#repeat :: (n :: Number, e :: a) -> List<a>
fun repeat(n,e):
  if n > 0:
    link(e, repeat(n - 1, e))
  else:
    empty
  end
end

# temp
fun make-module(m):
  m
end

  |]


------------------------------------------------------------------------------

-- temp hack again - before modules and proper ffi implemented,
-- make a bunch of quick haskell ffi functions available to burdock code
-- this includes part of the build in language

initRuntime :: Runtime ModuleMetadata
initRuntime = do

    hackMM <- liftIO $ newIORef [("run-task", (Nothing,BEIdentifier))
                                ,("run-task-cs", (Nothing,BEIdentifier))
                                ,("run-task-cs-async", (Nothing,BEIdentifier))
                                -- hack for boolean literal patterns
                                -- todo: renamer should convert boolean literal
                                --   patterns into a booleanliteralpattern variant
                                ,("true", (Nothing, BEVariant 0))
                                ,("false", (Nothing, BEVariant 0))
                                ]
    let addFFIType' nm ty = do
            liftIO $ modifyIORef hackMM ((nm, (Nothing, BEType 0)) : )
            addFFIType nm ty
        addBinding' nm f = do
            liftIO $ modifyIORef hackMM ((nm, (Nothing, BEIdentifier)) : )
            addBinding nm f

    -- todo: divide this up into stuff that's needed to execute bootstrap
    -- and maybe internals
    -- stuff which is part of a built in module or needed to run scripts
    -- which use the whole language and built in modules only
    -- and other bits, used just for testing?

    void $ addFFIType' "ffitypetag" ffitypetagFFI
    void $ addFFIType' "datadecltag" datadecltagFFI

    void $ addFFIType' "number" scientificFFI
    void $ addFFIType' "string" stringFFI
    booleanType <- addFFIType' "boolean" booleanFFI

    addBinding' "tuple" =<< makeDataDeclTag "tuple"
    addBinding' "record" =<< makeDataDeclTag "record"


    void $ addFFIType' "haskell-list" ffitypetagFFI

    addBinding' "make-datadecltag" =<< makeFunctionValue makeDataDeclTag'
    addBinding' "make-burdock-list" =<< makeFunctionValue myMakeBurdockList
    addBinding' "make-haskell-list" =<< makeFunctionValue makeHaskellList
    addBinding' "make-variant" =<< makeFunctionValue myMakeVariant
    addBinding' "is-variant" =<< makeFunctionValue myIsVariant
    addBinding' "check-variants-equal" =<< makeFunctionValue checkVariantsEqual
    addBinding' "raise" =<< makeFunctionValue raise

    -- loading a module needs the interpreter, but the interpreter depends on this
    -- module
    addBinding' "load-module" =<< makeFunctionValue myLoadModule

    -- should true be a built in value (built into the runtime), or an ffi
    -- value, or a agdt?
    
    addBinding' "true" (makeValue booleanType True)
    addBinding' "false" (makeValue booleanType False)

    --------------------------------------

    gremlintype <- addFFIType' "gremlintype" gremlinFFI
    --void $ addFFIType' "bytestring" ffitypetagFFI


    addBinding' "print" =<< makeFunctionValue myPrint
    addBinding' "debug-print" =<< makeFunctionValue myDebugPrint
    addBinding' "debug-show" =<< makeFunctionValue myDebugShow
    addBinding' "get-call-stack" =<< makeFunctionValue myGetCallStack
    addBinding' "torepr" =<< makeFunctionValue myToRepr
    addBinding' "tostring" =<< makeFunctionValue myToString
    addBinding' "show-variant" =<< makeFunctionValue showVariant
    addBinding' "show-tuple" =<< makeFunctionValue showTuple
    addBinding' "show-record" =<< makeFunctionValue showRecord

    addBinding' "not" =<< makeFunctionValue myNot

    addBinding' "add-test-pass" =<< makeFunctionValue myAddTestPass
    addBinding' "add-test-fail" =<< makeFunctionValue myAddTestFail
    addBinding' "indent" =<< makeFunctionValue indent

    addBinding' "torepr-debug" =<< makeFunctionValue toreprDebug

    addBinding' "gremlin" $ makeValue gremlintype False

    addBinding' "read-process" =<< makeFunctionValue myReadProcessWithExitCode

    --addBinding' "make-bytestring" =<< makeFunctionValue makeBytestring
    --addBinding' "get-bytestring-byte" =<< makeFunctionValue getBytestringByte

    -- well hacky, use reverse to shoehorn in the true and false variants
    -- before the binding
    -- all this mess will be fixed after the module system and basic haskell
    -- ffi modules are implemented for user code, before replacing the system
    -- bootstrap with the new approach
    ModuleMetadata <$> reverse <$> liftIO (readIORef hackMM)

------------------------------------------------------------------------------
 
-- some helper functions mainly for operators

unaryMember :: Typeable a => Text -> Text -> Text -> (a -> Text) -> Value -> [Value] -> Runtime Value
unaryMember burName inType outType f v as =
    case as of
        [] -> do
            let v1 = maybe (error $ "u not a " <> inType <> " " <> debugShowValue v) id $ extractValue v
            makeValueName outType $ f v1
        _ -> error $ "bad args to " <> inType <> " " <> burName


binaryMember :: (Typeable a, Typeable b) =>
    Text -> Text -> Text -> (a -> a -> b) -> Value -> [Value] -> Runtime Value
binaryMember burName inType outType f v1 as =
    case as of
        [v2] -> do
            let n1 = maybe (error $ "b0 not a " <> inType <> " " <> debugShowValue v1) id $ extractValue v1
                n2 = maybe (error $ "b1 not a " <> inType <> " " <> debugShowValue v2) id $ extractValue v2
            makeValueName outType $ f n1 n2
        _ -> error $ "bad args to " <> inType <> " " <> burName

binaryMemberLax :: (Typeable a, Typeable b) =>
    Text -> Text -> Text -> (a -> a -> b) -> Value -> [Value] -> Runtime Value
binaryMemberLax burName inType outType f v1 as =
    case as of
        [v2] -> do
            let n1 = maybe (error $ "bl not a " <> inType <> " " <> debugShowValue v1) id $ extractValue v1
                mn2 = extractValue v2
                -- todo: make false an arg?
            maybe (makeValueName outType False) (makeValueName outType . (f n1)) mn2
        _ -> error $ "bad args to " <> inType <> " " <> burName

------------------------------------------------------------------------------

ffitypetagFFI :: Text -> Value -> Runtime Value
ffitypetagFFI "_torepr" v = makeFunctionValue $ \case
    [] -> do
        let y = maybe (error $ "toreprnot a ffitypetag " <> debugShowValue v) id $ extractValue v
        makeString $ tyName y
    xs -> error $ "unsupported args to torepr: " <> T.intercalate "," (map debugShowValue xs)
          <> debugShowValue v
ffitypetagFFI m _ = error $ "unsupported field on ffitypetag: " <> m

datadecltagFFI :: Text -> Value -> Runtime Value
datadecltagFFI "_torepr" v = makeFunctionValue $ \case
    [] -> do
        let y = maybe (error $ "toreprnot a datadecltag" <> debugShowValue v) id $ extractValue v
        makeString $ dtyName y
    xs -> error $ "unsupported args to torepr: " <> T.intercalate "," (map debugShowValue xs)
          <> debugShowValue v
datadecltagFFI m _ = error $ "unsupported field on datadecltag: " <> m

makeDataDeclTag' :: [Value] -> Runtime Value
makeDataDeclTag' [v] | Just v' <- extractValue v = do
    makeDataDeclTag v'
makeDataDeclTag' _ = error $ "bad args to makeDataDeclTag'"

------------------------------------------------------------------------------

-- built in types methods

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
scientificFFI "_greaterequal" v1 = do
    makeFunctionValue $ binaryMember "_greaterequal" "number" "boolean" ((>=) :: Scientific -> Scientific -> Bool) v1
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

gremlinFFI :: Text -> Value -> Runtime Value
gremlinFFI "_torepr" v1 = do
    case extractValue v1 of
        Just (v :: Bool) -> makeString $ "gremlin: " <> show v
        Nothing -> error $ "non gremlintytpe: " <> debugShowValue v1
gremlinFFI "_equals" v1 = do
    makeFunctionValue $ binaryMemberLax "_equals" "gremlintype" "gremlintype" ((==) :: Bool -> Bool -> Bool) v1
gremlinFFI m _ = error $ "unsupported field on gremlin: " <> m

------------------------------------------------------------------------------

-- helpers for the testing

indent :: [Value] -> Runtime Value
indent [x] | Just t <- extractValue x = do
    let ls = T.lines t
        ls' = map ("  " <>) ls
    makeString $ T.unlines ls'
indent _ = error $ "bad args to indent"

myAddTestPass :: [Value] -> Runtime Value
myAddTestPass [] = do
    addTestPass
    nothingValue
myAddTestPass _ = error $ "bad args to myAddTestPass"

myAddTestFail :: [Value] -> Runtime Value
myAddTestFail [] = do
    addTestFail
    nothingValue
myAddTestFail _ = error $ "bad args to myAddTestFail"

------------------------------------------------------------------------------

-- specific language support for agdt

makeHaskellList :: [Value] -> Runtime Value
makeHaskellList vs = makeValueName "haskell-list" vs

myMakeVariant :: [Value] -> Runtime Value
myMakeVariant [nm, flds, es] = do
    let t :: Text
        t = maybe (error $ "bad args to make variant, first arg is not text")
            id $ extractValue nm
        flds' :: [Value]
        flds' = maybe (error $ "bad args to make variant, second arg is not haskell list")
                id $ extractValue flds
        flds'' :: [Text]
        flds'' = flip map flds' $ \f -> maybe (error $ "bad args to make variant, second arg has non string in list")
                 id $ extractValue f
        es' :: [Value]
        es' = maybe (error $ "bad args to make variant, third arg is not list")
                id $ extractValue es
                                        
    when (length es' /= length flds') $ error $ "wrong number of args to create variant " <> t
    -- todo: need the type tag to put in here, later it will be abstract
    -- and it will be used in lots of these auxiliary functions
    makeVariant (DataDeclTypeTag undefined) t $ zip flds'' es'
myMakeVariant _ = error $ "bad args to makeVariant"

myIsVariant :: [Value] -> Runtime Value
myIsVariant [nm, x] = do
    let nm' :: Text
        nm' = maybe (error $ "bad args to is variant, first arg is not text")
            id $ extractValue nm
    x' <- variantName x
    case x' of
        Nothing -> makeBool False
        Just t -> makeBool $ nm' == t
        
myIsVariant _ = error $ "bad args to myIsVariant"

-- todo: decide if the flds should be passed, or this function should
-- work them out. Currently, the passed fields are ignored and this
-- function works them out using the hack auxiliary variantValueFields
checkVariantsEqual :: [Value] -> Runtime Value
checkVariantsEqual [a, b@(VariantV {})] = do
    at <- variantName a
    bt <- variantName b
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
                let andEm [] = makeBool True
                    andEm (v:vs) = case extractValue v of
                        Just True -> andEm vs
                        Just False -> makeBool False
                        _ -> error $ "non boolean returned from _equals method"
                andEm fsEq
            else makeBool False
        _ -> error $ "checkVariantsEqual error: " <> debugShowValue a <> " " <> debugShowValue b
checkVariantsEqual [_,_] = makeBool False
checkVariantsEqual _ = error $ "bad args to checkVariantsEqual"

showVariant :: [Value] -> Runtime Value
showVariant [x] = do
    at <- variantName x
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
            if null es'
                then makeString n
                else makeString $ n <> "(" <> T.intercalate ", " es' <> ")"
        _ -> error $ "show variant called on non variant " <> debugShowValue x
    
showVariant _ = error $ "bad args to showVariant"

------------------------------------------------------------------------------

-- module system support

myLoadModule  :: [Value] -> Runtime Value
myLoadModule [ctx,nm,as]
    | Just (ctx' :: Text)  <- extractValue ctx
    , Just (nm' :: Text) <- extractValue nm
    , Just as' <- extractBurdockList as
    , Just (as'' :: [Text]) <- mapM extractValue as' = do
    getModuleValue (Just ctx') $ RuntimeImportSource nm' as''
myLoadModule _ = error $ "bad args to myLoadModule"

------------------------------------------------------------------------------

-- simple utility functions

myPrint :: [Value] -> Runtime Value
myPrint [v] = do
    t <- myToString [v]
    case extractValue t of
        Nothing -> liftIO $ putStrLn $ debugShowValue v
        Just s -> liftIO $ putStrLn s
    nothingValue
myPrint _ = error $ "bad args to myPrint"

myMakeBurdockList :: [Value] -> Runtime Value
myMakeBurdockList vs = makeBurdockList vs

myDebugPrint :: [Value] -> Runtime Value
myDebugPrint [x] = do
    liftIO $ putStrLn $ debugShowValue x
    nothingValue
myDebugPrint _ = error $ "bad args to myDebugPrint"

myDebugShow :: [Value] -> Runtime Value
myDebugShow [x] = makeString $ debugShowValue x
myDebugShow _ = error $ "bad args to myDebugShow"

toreprDebug :: [Value] -> Runtime Value
toreprDebug [x] = do
    y <- runTask False $ myToRepr [x]
    case y of
        Left _ -> makeString $ debugShowValue x
        Right v -> pure v
toreprDebug _ = error $ "bad args to toreprDebug"

raise :: [Value] -> Runtime Value
raise [x] = throwValue x
raise _ = error $ "bad args to raise"

myGetCallStack :: [Value] -> Runtime Value
myGetCallStack [] = do
    cs <- getCallStack
    cs' <- flip mapM cs $ \case
            Nothing -> makeString "nothing"
            Just x -> makeString x
    makeBurdockList cs' -- makeValue "string" $ T.intercalate "," cs'
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

showTuple :: [Value] -> Runtime Value
showTuple [x] = do
    at <- variantName x
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
            if null es'
                then makeString "{}"
                else makeString $ "{" <> T.intercalate ";" es' <> "}"
        _ -> error $ "showTuple called on non variant " <> debugShowValue x
showTuple _ = error $ "bad args to showTuple"

showRecord :: [Value] -> Runtime Value
showRecord [x] = do
    at <- variantName x
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
            if null es''
                then makeString "{}"
                else makeString $ "{" <> T.intercalate "," esx <> "}"
        _ -> error $ "showRecord called on non variant " <> debugShowValue x
showRecord _ = error $ "bad args to showRecord"

myNot :: [Value] -> Runtime Value
myNot [x] = case extractValue x of
    Just True -> makeBool False
    Just False -> makeBool True
    _ -> error $ "bad arg to not"
myNot _ = error $ "bad args to raise"

------------------------------------------------------------------------------

-- running processes

{-myCallProcess :: [Value] -> Runtime Value
myCallProcess [x] = do
    let as = maybe (error $ "bad args to call-process, arg is not list")
                id $ extractList x
        as' :: [Text]
        as' = flip map as $ \f -> maybe (error $ "bad args to call-process, arg has non string in list")
              id $ extractValue f
        (b:bs) = map T.unpack as'
    liftIO $ callProcess b bs
    pure VNothing
myCallProcess _ = error $ "bad args to myCallProcess"-}

myReadProcessWithExitCode :: [Value] -> Runtime Value
myReadProcessWithExitCode [prog, args, stdinVal] = do
    let prog' = maybe (error "0: bad args to myReadProcessWithExitCode") T.unpack $ extractValue prog
        as = maybe (error $ "bad args to call-process, arg is not list")
                id $ extractBurdockList args
        args' :: [String]
        args' = flip map as $ \f -> maybe (error $ "bad args to read-process, arg has non string in list")
                T.unpack $ extractValue f

        stdinVal' = maybe (error "2: bad args to myReadProcessWithExitCode") T.unpack $ extractValue stdinVal
    (x, so, se) <- liftIO $ readProcessWithExitCode prog' args' stdinVal'
    let x1 :: Scientific
        x1 = fromIntegral $ case x of
                 ExitSuccess -> 0
                 ExitFailure n -> n
    x1' <- makeNumber x1
    so' <- makeString $ T.pack so
    se' <- makeString $ T.pack se
    makeTuple [x1', so', se']
myReadProcessWithExitCode _ = error $ "bad args to myReadProcessWithExitCode"
