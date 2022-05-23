{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Interpreter
    (TestResult(..)
    ,CheckBlockResult(..)
    ,getTestResults
    ,allTestsPassed


    --,Value
    ,valueToStringIO
    --,valueToString

    ,newHandle
    ,closeHandle
    ,Handle
    ,runScript
    ,runLiterateScript
    ,evalExpr
    ,evalFun

    -- temp for ffi
    -- will refactor into a proper abstraction layer for ffi code to
    -- use. want to write ffi code and use it like this to learn
    -- what the abstraction layer needs to support
    ,InterpreterException
    ,formatException
    ,Interpreter
    ,Value(..)
    ,addFFIImpls
    ,FFIPackage(..)
    ,FFITypeInfo(..)
    ,addFFIPackage
    ,makeFFIMemberFunction
    ,ffiSingleArgMethod
    ,ffiNoArgMethod
    ,ffiNoArgValue
    ,FMD(..)
    ,ffiMemberDispatcher
    ,liftIO

    ,nothing
    ,fromBList
    ,makeBList
    ,makeBTuple
    ,fromBTuple
    ,app
    ,makeFFIValue
    ,unmakeFFIValue

    ,setNumCapabilities
    ,getNumProcessors

    ) where

import Control.Concurrent
    (setNumCapabilities
    ,threadDelay
    --,myThreadId
    )

import qualified System.Environment as E

import GHC.Conc (getNumProcessors)

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            )

import Control.Monad (forM_
                     ,void
                     ,when
                     ,zipWithM
                     )

import Data.List (intercalate
                 ,sortOn
                 ,partition
                 ,isInfixOf
                 ,isPrefixOf
                 ,findIndex
                 ,tails
                 )

import Data.IORef (IORef
                  ,newIORef
                  ,readIORef
                  ,modifyIORef
                  ,writeIORef)

import Data.Maybe (catMaybes, isJust, fromJust)
import Text.Read (readMaybe)

import Burdock.Scientific
import Burdock.Syntax
import Burdock.Pretty
import Burdock.Parse (parseScript, parseLiterateScript, parseExpr)
import qualified Burdock.Relational as R

import Burdock.HsConcurrency
    (openBConcurrency
    ,closeBConcurrency
    ,BConcurrencyHandle
    ,ThreadHandle
    ,Addr

    ,spawnExtWait
    ,spawn
    ,spawnMonitor
    ,addr
    ,send
    ,zreceive
    ,zreceiveTimeout
    ,asyncExit

    ,MonitorDown(..)
    ,ExitType(..)
    ,MonitorRef(..)
    )

import Control.Exception.Safe (catch
                              ,SomeException
                              ,Exception
                              ,throwM
                              ,catchAny
                              ,fromException
                              )

import qualified Prettyprinter as P
    (pretty
    ,Doc
    --,parens
    ,nest
    ,(<+>)
    ,sep
    ,punctuate
    --,comma
    --,dquotes
    ,--vsep
    )

import System.FilePath
    (dropExtension
    ,takeFileName
    ,takeBaseName
    ,(</>)
    ,(<.>)
    )

import Data.Dynamic (Dynamic
                    ,toDyn
                    ,fromDynamic
                    ,Typeable
                    )

import Data.Char (isSpace)

--import System.IO.Unsafe (unsafePerformIO)

import Text.Show.Pretty (ppShow)

import Control.Concurrent.STM.TVar
    (TVar
    ,readTVar
    ,modifyTVar
    ,newTVarIO
    --,writeTVar
    )

import Control.Concurrent.STM
    (atomically)

import System.Process (callProcess, readProcess)

import System.Directory as D
--import Control.Monad.IO.Class
--    (MonadIO)

--import Debug.Trace (trace)

------------------------------------------------------------------------------

-- public api

newHandle :: IO Handle
newHandle = do
    x <- newBurdockHandle
    pure $ Handle x

closeHandle :: Handle -> IO ()
closeHandle (Handle h) =
    closeBConcurrency . hsConcurrencyHandle =<< atomically (readTVar h)

runScript :: Handle
          -> Maybe FilePath
          -> [(String,Value)]
          -> String
          -> IO Value
runScript = runScript' False

runLiterateScript :: Handle
          -> Maybe FilePath
          -> [(String,Value)]
          -> String
          -> IO Value
runLiterateScript = runScript' True

runScript' :: Bool
          -> Handle
          -> Maybe FilePath
          -> [(String,Value)]
          -> String
          -> IO Value
runScript' lit h mfn lenv src =
    spawnExtWaitHandle h $ \th -> runInterp th True h $ do
        Script ast <- either error id
            <$> useSource mfn (maybe "runScript" id mfn) src
            (if lit then parseLiterateScript else parseScript)
        -- todo: how to make this local to this call only
        forM_ lenv $ \(n,v) -> letValue n v
        ret <- interpStatements ast
        printTestResults <- interp $ internalsRef "auto-print-test-results"
        case printTestResults of
            BoolV True -> do
                trs <- getTestResultsI
                when (not $ null trs) $
                    liftIO $ putStrLn $ formatTestResults trs
            BoolV False -> pure ()
            x -> _errorWithCallStack $ "bad value in auto-print-test-results (should be boolv): " ++ show x
        pure ret
    
evalExpr :: Handle
         -> Maybe FilePath
         -> [(String,Value)]
         -> String
         -> IO Value
evalExpr h mfn lenv src =
    spawnExtWaitHandle h $ \th -> runInterp th True h $ do
    ast <- either error id <$> useSource mfn (maybe "evalExpr" id mfn) src parseExpr
    -- todo: how to make this local to this call only
    forM_ lenv $ \(n,v) -> letValue n v
    interp ast

evalFun :: Handle
        -> String 
        -> [Value]
        -> IO Value
evalFun h fun args =
    spawnExtWaitHandle h $ \th -> runInterp th True h $ do
    ast <- either error id <$> useSource Nothing "evalFun" fun parseExpr
    f <- interpStatements [StmtExpr ast]
    let as = zipWith (\i x -> ("aaa-" ++ show i, x)) [(0::Int)..] args
        src' = "fff(" ++ intercalate "," (map fst as) ++ ")"
    ast' <- either error id <$> useSource Nothing "evalFun" src' parseExpr
    forM_ (("fff", f):as) $ \(n,v) -> letValue n v
    interp ast'

valueToStringIO :: Handle -> Value -> IO (Maybe String)
valueToStringIO h v =
    spawnExtWaitHandle h $ \th -> runInterp th True h $ valueToString v

formatException :: Handle -> Bool -> InterpreterException -> IO String
formatException h includeCallstack e =
    spawnExtWaitHandle h $ \th ->
        runInterp th True h $ formatExceptionI includeCallstack e

addFFIImpls :: Handle -> [(String, [Value] -> Interpreter Value)] -> IO ()
addFFIImpls h ffis =
    spawnExtWaitHandle h $ \th -> runInterp th True h $ addFFIImpls' ffis

addFFIPackage :: Handle -> String -> FFIPackage -> IO ()
addFFIPackage h nm ffipkg =
    spawnExtWaitHandle h $ \th -> runInterp th True h $ addFFIPackage' nm ffipkg

allTestsPassed :: Handle -> IO Bool
allTestsPassed h = spawnExtWaitHandle h $ \th -> runInterp th True h $ do
    trs <- getTestResultsI
    pure $ and $ map isPass $ concatMap tr $ concatMap snd trs
  where
    tr (CheckBlockResult _ x) = x
    isPass (TestPass {}) = True
    isPass (TestFail {}) = False
    
---------------------------------------

-- testing, uses haskell values atm, will move to burdock
-- values at some point in the future

data TestResult = TestPass String
                | TestFail String String
                deriving Show

data CheckBlockResult = CheckBlockResult String [TestResult]
                      deriving Show

getTestResults :: Handle -> IO [(String, [CheckBlockResult])]
getTestResults h =
    spawnExtWaitHandle h $ \th -> runInterp th True h getTestResultsI

getTestResultsI :: Interpreter [(String, [CheckBlockResult])]
getTestResultsI = do
    st <- ask
    v <- liftIO $ atomically $ do
        a <- readTVar $ tlHandleState st
        readTVar $ hsTestResults a
    let perModules :: [(String, [(String,TestResult)])]
        perModules = partitionN $ map (\(m,cb,tr) -> (m,(cb,tr))) v
        collectCBs :: [(String, [(String,[TestResult])])]
        collectCBs = map (\(mb,cbs) -> (mb,partitionN cbs)) perModules
    pure $ map (\(a,b) -> (a, map (uncurry CheckBlockResult) b)) collectCBs

partitionN :: Eq a => [(a,b)] -> [(a,[b])]
partitionN [] = []
partitionN vs@((k,_):_) =
    let (x,y) = partition ((==k) . fst) vs
    in (k,map snd x) : partitionN y

-- todo: this function is a complete mess
formatTestResults :: [(String, [CheckBlockResult])] -> String
formatTestResults ms =
    intercalate "\n\n" (map (uncurry showModuleTests) ms)
    ++ "\n" ++ summarizeAll (concatMap snd ms)
  where
    showModuleTests mnm cbs =
        heading ("Module: " ++ mnm) ++ "\n"
        ++ intercalate "\n\n" (map showCheckBlockResult cbs)
        ++ "\n" ++ summarizeCheckBlocks mnm cbs
    heading s = s ++ "\n" ++ replicate (length s) '-'
    getCheckBlockTotals ts = 
        let f p t [] = (p,t)
            f p t (TestPass {} : s) = f (p + 1) (t + 1) s
            f p t (TestFail {} : s) = f p (t + 1) s
        in f (0 :: Int) (0 :: Int) ts
    summarizeAll rs =
        let (passed,total) = getCheckBlocksTotals rs
        in showOutOf passed total ++ " in all modules"
    showOutOf passed total =
        show passed ++ "/" ++ show total ++ " tests passed"
    sumPairs ps = 
        let f t1 t2 [] = (t1,t2)
            f t1 t2 ((a,b):rs) = f (t1 + a) (t2 + b) rs
        in f (0 :: Int) (0 :: Int) ps
    getCheckBlocksTotals cbs = 
        sumPairs $ map (\(CheckBlockResult _ ts) -> getCheckBlockTotals ts) cbs
    summarizeCheckBlocks mnm cbs =
        let (passed, total) = getCheckBlocksTotals cbs
        in showOutOf passed total ++ " in module: " ++ mnm
    summarizeCheckBlock nm ts =
        let (passed, total) = getCheckBlockTotals ts
        in "  " ++ showOutOf passed total ++ " in check block: " ++ nm
    showCheckBlockResult (CheckBlockResult nm ts) =
        "Check block: " ++ nm ++ "\n"
        ++ unlines (map showTestResult ts)
        ++ summarizeCheckBlock nm ts
    showTestResult (TestPass nm) =
        "  test (" ++ nest 8 nm ++ "): OK"
    showTestResult (TestFail nm msg) =
        "  test (" ++ nest 8 nm ++ "): failed, reason:\n" ++ indent 4 msg
    indent n txt = unlines $ map (replicate n ' ' ++) $ lines txt
    nest n txt =
        case lines $ txt of
            (f:r) -> trim $ unlines (f : map (replicate n ' ' ++) r)
            _ -> trim txt
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------------

-- runtime language values

data Value = NumV Scientific
           -- bool could also be a variant
           -- easier like this since it's used a lot
           | BoolV Bool
           | TextV String
           | FunV [Binding] Expr Env
           | ForeignFunV String
           | VariantV TypeInfo String [(String,Value)]
           | BoxV TypeInfo (IORef Value)
           | FFIValue (String,String) Dynamic
           | MethodV Value
           | TemplateV SourcePosition -- todo: this is a hack before source positions become better integrated

instance Ord Value where
    NumV a <= NumV b = a <= b
    BoolV a <= BoolV b = a <= b
    TextV a <= TextV b = a <= b
    ForeignFunV a <= ForeignFunV b = a <= b
    VariantV tg nm fs <= VariantV tgb nmb fsb = (tg,nm,fs) <= (tgb,nmb,fsb)
    a <= b = error $ "cannot ord values: " ++ show (a,b)

-- todo: revert back to automatically derived show
-- and use torepr' everywhere you want a readable value that
-- isn't the haskell ast syntax
instance Show Value where
  show (NumV n) = "NumV " ++ show n
  show (TextV n) = "TextV " ++ show n
  show (BoolV n) = "BoolV " ++ show n
  -- todo: add the last element of the t?
  show (VariantV _t nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
  show (FunV as bdy _env) = "FunV " ++ show as ++ "\n" ++ prettyExpr bdy
  show (ForeignFunV n) = "ForeignFunV " ++ show n
  show (BoxV _ _n) = "BoxV XX" -- ++ show n
  show (FFIValue _ r) | Just (r' :: R.Relation Value) <- fromDynamic r = R.showRel r'
  show (FFIValue _ v) = "FFIValue " ++ show v
  show (MethodV {}) = "<method>"
  show (TemplateV sp) = "Template at " ++ show sp


-- needs some work
-- not even sure value should have an eq instance
-- because comparing values is not a pure operation in general
-- so having an eq instance is surely very error prone
instance Eq Value where
    NumV a == NumV b = a == b
    BoolV a == BoolV b = a == b
    TextV a == TextV b = a == b
    VariantV tg "record" as == VariantV tg' "record" bs
        | tg == bootstrapType "Record" && tg == tg' 
        = sortOn fst as == sortOn fst bs
    VariantV tg nm fs == VariantV tg' lm gs = tg == tg' && (nm,fs) == (lm,gs)
    ForeignFunV x == ForeignFunV y = x == y
    FFIValue _ a == FFIValue _ b
        | Just (a' :: R.Relation Value) <- fromDynamic a
        , Just b' <- fromDynamic b = either (error . show) id $ R.relEqual a' b'
    FFIValue _ a == FFIValue _ b
        | Just (a' :: Addr) <- fromDynamic a
        , Just b' <- fromDynamic b = a' == b'
    -- todo: funv, boxv ..., ffivalue
    -- ioref has an eq instance for pointer equality
    --    this is very useful
    --    pyret has some other concepts of equality
    --    not sure if should write alternative equals in the right monad
    --    and try to stick to them
    _ == _ = False

valueToString :: Value -> Interpreter (Maybe String)
valueToString v = case v of
    VariantV tg "nothing" [] | tg == bootstrapType "Nothing" -> pure $ Nothing
    _ -> Just <$> torepr' v

-- temp?
nothing :: Value
nothing = VariantV (bootstrapType "Nothing") "nothing" []

makeFFIValue :: Typeable v => String -> v -> Interpreter Value
makeFFIValue localTg v =
    pure $ FFIValue ("_pkg", localTg) $ toDyn v

unmakeFFIValue :: Typeable v => String -> Value -> Interpreter (Maybe v)
unmakeFFIValue localTg (FFIValue (_pkg,vtg) a)
    | localTg == vtg = pure $ fromDynamic a
unmakeFFIValue _ _ = pure Nothing

-- represents info for a "type" at runtime
-- not sure if this is a type-tag, or something different,
-- because it can be abstract/partial
-- TODO: make sure the code makes a clear distinction between
-- the name of a type, and the description of a type
-- -> don't identify types by their description, use their name

data TypeInfo
    = SimpleTypeInfo
      {tiID :: [String] -- TODO: path to the system module [_system.modules.module-name.type-name]
      }
    | TupleTypeInfo [TypeInfo]
    | RecordTypeInfo [(String,TypeInfo)]
    | ParamTypeInfo TypeInfo [TypeInfo]
    | ArrowTypeInfo [TypeInfo] TypeInfo
    deriving (Eq, Show, Ord)

bootstrapType :: String -> TypeInfo
bootstrapType x = SimpleTypeInfo ["_system","modules","_bootstrap",x]

internalsType :: String -> TypeInfo
internalsType x = SimpleTypeInfo ["_system","modules","_internals",x]

-- todo: split the runtime type tag and the type annotation syntax
-- types
-- check the type referred to by the given type annotation is a valid
-- type (e.g. it exists, and it doesn't have the wrong type params, ...
typeOfTypeSyntax :: Ann -> Interpreter TypeInfo
typeOfTypeSyntax (TName x) = do
    v <- interp (makeDotPathExpr $ prefixLast "_typeinfo-" x)
    case v of
        FFIValue _ffitag v'
            | Just z <- fromDynamic v' -> pure (z :: TypeInfo)
            | otherwise -> _errorWithCallStack $ "type info reference isn't a type info: " ++ show v'
        _ -> _errorWithCallStack $ "type info reference isn't a type info: " ++ show v
typeOfTypeSyntax (TTuple xs) = do
    fs <- mapM typeOfTypeSyntax xs
    pure $ TupleTypeInfo fs
typeOfTypeSyntax (TRecord xs) = do
    fs <- mapM (secondM typeOfTypeSyntax) xs
    pure $ RecordTypeInfo fs

typeOfTypeSyntax (TParam nm ps) = do
    -- check the ps are valid
    pst <- mapM typeOfTypeSyntax ps
    -- check the nm is valid
    nmt <- typeOfTypeSyntax (TName nm)
    -- check the number of ps matches what nm allows
    pure $ ParamTypeInfo nmt pst

typeOfTypeSyntax (TArrow as r) = do
    as' <- mapM typeOfTypeSyntax as
    r' <- typeOfTypeSyntax r
    pure $ ArrowTypeInfo as' r'

typeOfTypeSyntax (TParens t) = typeOfTypeSyntax t

typeOfTypeSyntax x = _errorWithCallStack $ "typeOfTypeSyntax: " ++ show x

secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f (a,b) = (a,) <$> f b

-- shrink types according to default pyret type checking,
-- e.g. checking a value against a List<Number>
-- only actually checks it's a List
-- this is so the typeIsCompatibleWith will be able to do
-- complete type checks, but if wanted, the default
-- language checks will just be shallow checks like pyret does it
shallowizeType :: TypeInfo -> TypeInfo
shallowizeType (ParamTypeInfo nmt _) = nmt
shallowizeType x = x

typeIsCompatibleWith :: Value -> TypeInfo -> Bool

--typeIsCompatibleWith a@(VariantV tg "empty" _) b
--    | trace (show ("typeIsCompatibleWith", a, b)) False = undefined

typeIsCompatibleWith _ b | b == bootstrapType "Any" = True

typeIsCompatibleWith (NumV {}) b | b == bootstrapType "Number" = True
typeIsCompatibleWith (TextV {}) b | b == bootstrapType "String" = True
typeIsCompatibleWith (BoolV {}) b | b == bootstrapType "Boolean" = True

typeIsCompatibleWith (VariantV tg "tuple" fs) (TupleTypeInfo bs)
    | tg == bootstrapType "Tuple" =
    -- todo: do this efficiently
    -- if there's an issue, collect all the incompatible messages
    -- instead of just reporting the first
    length fs == length bs && and (zipWith typeIsCompatibleWith (map snd fs) bs)
typeIsCompatibleWith (VariantV tg "tuple" _) b
    | b == bootstrapType "Tuple"
    , tg == bootstrapType "Tuple" = True

typeIsCompatibleWith (VariantV tg "record" fs) (RecordTypeInfo bs)
    | tg == bootstrapType "Record" =
    -- todo: do this efficiently
    -- if there's an issue, collect all the incompatible messages
    -- instead of just reporting the first
    let f = sortOn fst
        tc a b = fst a == fst b && typeIsCompatibleWith (snd a) (snd b)
    in length fs == length bs && and (zipWith tc (f fs) (f bs))

typeIsCompatibleWith (VariantV tg "record" _) b
    | b == bootstrapType "Record"
    , tg == bootstrapType "Record" = True

typeIsCompatibleWith v b
    | b == bootstrapType "Function"
    , isFunVal v = True
  where
    isFunVal (FunV {}) = True
    isFunVal (ForeignFunV {}) = True
    isFunVal _ = False

typeIsCompatibleWith (VariantV tg _ _) b
    | tg == b = True

typeIsCompatibleWith (FunV as _ _) (ArrowTypeInfo ts _) =
    length as == length ts

typeIsCompatibleWith _ (ParamTypeInfo {}) =
    error $ "todo: typeIsCompatibleWith param type info not supported"
typeIsCompatibleWith (FFIValue (_pkg,tg) _) (SimpleTypeInfo ty) = [tg] == ty
typeIsCompatibleWith _ _ = False

assertPatternTagMatchesCasesTag :: TypeInfo -> TypeInfo -> Interpreter ()
assertPatternTagMatchesCasesTag a b
    | a == bootstrapType "Any" = pure ()
    | otherwise =
          when (a /= b) $ _errorWithCallStack $ "type not compatible: type of pattern not the same as the type of the case: " ++ show (b,a)

prefixLast :: [a] -> [[a]] -> [[a]]
prefixLast pre is = f is
  where
    f [] = []
    f [x] = [pre ++ x]
    f (x:xs) = x : f xs


------------------------------------------------------------------------------

-- environment, interpreter state, intepreter monad helper functions


type Env = [(String, Value)]
type ForeignFunctionEntry = (String, [Value] -> Interpreter Value)

{-

Guidelines for the state:

if something can possibly be readonly, and overridden using local, do
this

otherwise -> if it's local to a thread, use an ioref
mostly these are things that should preserve updates when moving to
the next statement in a statement list, which local isn't able to do
because of the structure of the interpreter (it could be done so that
local works for this, but when this was done previously, it made a lot
other things much more awkward)

if something can be updated, and is read by multiple threads (or is
updated by multiple threads), put it in a tvar

-}

data BurdockHandleState
    = BurdockHandleState
    {hsConcurrencyHandle :: BConcurrencyHandle
    ,hsLoadedModules :: [(FilePath,(String,Value))] -- modulename, module value (as a record)
    ,hsTempBaseEnv :: [(String,Value)]
    ,hsForeignFunctionImpls :: [ForeignFunctionEntry]
    ,hsForeignTypes :: [(String, FFITypeInfo)]
    ,hsSourceCache :: [(FilePath, String)]
    ,hsLoadedPackages :: [(String,FilePath)]
    ,hsUniqueSourceCtr :: Int
    -- temp, to be split by module
    -- module path, check block name, result
    ,hsTestResults :: TVar [(String,String,TestResult)]
    }

data ModuleState
    = ModuleState
    {msModuleName :: String
    ,_msModuleSourcePath :: FilePath
    }

data ThreadLocalState
    = ThreadLocalState
    {tlHandleState :: TVar BurdockHandleState
    ,tlModuleState :: IORef ModuleState
    ,tlThreadHandle :: ThreadHandle
    -- collect the provides, used at end of module loading
    ,tlProvides :: IORef [ProvideItem]
    ,tlCallStack :: CallStack
    -- this is used for the bindings in the current scope
    -- which weren't introduced using a prelude statement
    ,tlLocallyCreatedBindings :: IORef [(String,Value)]
    -- hack for the repl - save bindings from run script
    -- so they are preserved from call to call
    ,tlReplCreatedBindings :: IORef [(String,Value)]
    -- these are the local bindings that were introduced using
    -- a prelude statement
    ,tlIncludedBindings :: IORef [(String,Value)]
    -- support for testing - identify current check block
    -- if there is one, and generating unique names for
    -- anonymous checkblocks
    ,tlCurrentCheckblock :: Maybe String
    ,tlNextAnonCheckblockNum :: IORef Int
    ,_tlIsModuleRootThread :: Bool
    }

type InterpreterState = ThreadLocalState 

type Interpreter = ReaderT InterpreterState IO

-- it's a bit crap that have variables nested within variables
-- but it seems like the least worst option for now
-- it uses reader + ioref because this is more straightforward
-- that using rwst/state
-- but then it needs an extra ioref so that you can preserve the
-- handle state conveniently in the handle wrapper when you
-- run api functions
data Handle = Handle (TVar BurdockHandleState)


type CallStack = [SourcePosition]

data InterpreterException = InterpreterException CallStack String
                          | ValueException CallStack Value

instance Show InterpreterException where
    show (InterpreterException _ s) = s
    -- todo: create a pure version of torepr' just to use here
    show (ValueException _ v) = show v -- unsafePerformIO $ torepr' v

instance Exception InterpreterException where

interpreterExceptionToValue :: InterpreterException -> Value
interpreterExceptionToValue (InterpreterException _ s) = TextV s
interpreterExceptionToValue (ValueException _ v) = v

interpreterExceptionCallStack :: InterpreterException -> CallStack
interpreterExceptionCallStack (InterpreterException cs _) = cs
interpreterExceptionCallStack (ValueException cs _) = cs

---------------------------------------

-- interpreter state functions

newBurdockHandle :: IO (TVar BurdockHandleState)
newBurdockHandle = do
    ch <- openBConcurrency (Just extractValueException)
    rs <- newTVarIO []
    h <- newTVarIO $ BurdockHandleState ch [] baseEnv builtInFF builtInFFITypes [] [] 0 rs
    spawnWaitCast ch $ \th -> runInterp th False (Handle h) initBootstrapModule
    ip <- getBuiltInModulePath "_internals"
    spawnWaitCast ch $ \th -> runInterp th False (Handle h) $ loadAndRunModule False "_internals" ip
    pure h

newSourceHandle :: ThreadHandle -> TVar BurdockHandleState -> IO ThreadLocalState
newSourceHandle th bs =
    ThreadLocalState bs
        <$> newIORef (ModuleState "unknown" "unknown") -- todo: generate unique names
        <*> pure th
        <*> newIORef []
        <*> pure []
        <*> newIORef []
        <*> newIORef []
        <*> newIORef []
        <*> pure Nothing
        <*> newIORef 1
        <*> pure True

{-

to make concurrency efficient, want to share data as little as possible
is it a bit premature to start worrying about this?

currently used:
get test results for the api
  unavoidable, and unlikely to be performance sensitive
read source cache:
  could cache the cache in local threads
  but it's not super performance sensitive, since it's only use to
  render error messages. the only time I can think of that this would
  be an issue is that if you are in production and logging error
  messages at a very high rate, and you don't want the additional
  problem of the system running even slower

adding a loaded module:
  unavoidable, and unlikely to be performance sensitive

askFF:
  this is potentially a performance issue
    figure out a pattern of caching and cache invalidation
    to minimise coordination when the list of ffimpls is not changing

askbindings:
  this is the biggest issue
  do something similar to the notes on askff

addtestresult:
  unavoidable when having tests running concurrently,
  but the coordination will be reduced when test results are logged
    in a tvar per module/source, instead of a single tvar
    for the whole handle
  it's probably not going to be an issue even without this

addffiimpls:
  unavoidable, and unlikely to be performance sensitive

use source:
  it's a shame to do two transactions when loading a module instead of one
  but it's no big deal

uniquesourcename:
  goes with use source/load module

-}
    
readSourceCache :: Interpreter [(FilePath, String)]
readSourceCache = do
    st <- ask
    liftIO $ atomically
        (hsSourceCache <$> readTVar (tlHandleState st))

readCallStack :: Interpreter CallStack
readCallStack = tlCallStack <$> ask

readModuleName :: Interpreter String
readModuleName = msModuleName <$> (liftIO . readIORef =<< (tlModuleState <$> ask))

modifyAddModule :: FilePath -> String -> Value -> Interpreter ()
modifyAddModule fn modName modv = do
    st <- ask
    liftIO $ atomically $ do
        modifyTVar (tlHandleState st) $ \x ->
            x {hsLoadedModules =
                (fn,(modName,modv)) : hsLoadedModules x}

askFF :: Interpreter [ForeignFunctionEntry]
askFF = do
    st <- ask
    liftIO $ atomically
        (hsForeignFunctionImpls <$> readTVar (tlHandleState st))

askFFTInfo :: String -> Interpreter (Maybe FFITypeInfo)
askFFTInfo nm = do
    st <- ask
    mp <- liftIO $ atomically
        (hsForeignTypes <$> readTVar (tlHandleState st))
    pure $ lookup nm mp

modifyScriptProvides :: ([ProvideItem] -> [ProvideItem]) -> Interpreter ()
modifyScriptProvides f = do
    st <- ask
    liftIO $ modifyIORef (tlProvides st) f

askScriptProvides :: Interpreter [ProvideItem]
askScriptProvides = do
    st <- ask
    liftIO $ readIORef (tlProvides st)

modifyBindings :: (Env -> Env) -> Interpreter ()
modifyBindings f = do
    st <- ask
    liftIO $ modifyIORef (tlLocallyCreatedBindings st) f

modifyIncludedBindings :: (Env -> Env) -> Interpreter ()
modifyIncludedBindings f = do
    st <- ask
    liftIO $ modifyIORef (tlIncludedBindings st) f

modifyReplCreatedBindings :: (Env -> Env) -> Interpreter ()
modifyReplCreatedBindings f = do
    st <- ask
    liftIO $ modifyIORef (tlReplCreatedBindings st) f


putModule :: ModuleState -> Interpreter ()
putModule m = do
    st <- ask
    liftIO $ writeIORef (tlModuleState st) m

lookupBinding :: String -> Interpreter (Maybe Value)
lookupBinding k = lookup k <$> askBindings

askBindings :: Interpreter [(String,Value)]
askBindings = do
    -- combine the local bindings with the global ones
    l <- askLocallyCreatedBindings
    lt <- askReplCreatedBindings
    lo <- askIncludedBindings
    st <- ask
    hs <- liftIO $ atomically (readTVar (tlHandleState st))
    let mods = map snd $ hsLoadedModules hs
    -- todo: make sure new bindings override old ones in all situations
    -- do some tests, it's not right at the moment
    let l1 = l ++ lt ++ lo ++ hsTempBaseEnv hs
             ++ [("_system", VariantV (bootstrapType "Record") "record"
                       [("modules", VariantV (bootstrapType "Record") "record" mods)])]
    pure l1
    
-- split like this to support provides
-- later it will also need to be able to not include autogenerated
-- bindings that are local only and should not be provideable
askLocallyCreatedBindings :: Interpreter [(String,Value)]
askLocallyCreatedBindings = liftIO . readIORef =<< (tlLocallyCreatedBindings <$> ask)

askReplCreatedBindings :: Interpreter [(String,Value)]
askReplCreatedBindings = liftIO . readIORef =<< (tlReplCreatedBindings <$> ask)

askIncludedBindings :: Interpreter [(String,Value)]
askIncludedBindings = liftIO . readIORef =<< (tlIncludedBindings <$> ask)

localBindings :: (Env -> Env) -> Interpreter a -> Interpreter a
localBindings m f = do
    st <- ask
    e' <- liftIO $ m <$> readIORef (tlLocallyCreatedBindings st)
    e1 <- liftIO $ newIORef e'
    eo <- liftIO $ m <$> readIORef (tlIncludedBindings st)
    eo1 <- liftIO $ newIORef eo
    p <- liftIO $ newIORef []
    local (const $ st {tlLocallyCreatedBindings = e1
                      ,tlIncludedBindings = eo1
                      ,tlProvides = p}) f

extendBindings :: [(String,Value)] -> Env -> Env
extendBindings bs env = bs ++ env
    
{-
throw an error if already in a check block
generate a name if it's maybe
run the check block in a local bindings
-}
enterCheckBlock :: Maybe String -> Interpreter a -> Interpreter a
enterCheckBlock mcbnm f = do
    st <- ask
    when (isJust $ tlCurrentCheckblock st)
        $ _errorWithCallStack $ "nested checkblock " ++ show (tlCurrentCheckblock st) ++ " / " ++ show mcbnm
    -- generate unique name if anon
    -- todo: incorporate thread id if not root thread
    cbnm <- case mcbnm of
        Just nm -> pure nm
        Nothing -> do
            i <- liftIO $ readIORef (tlNextAnonCheckblockNum st)
            liftIO $ writeIORef (tlNextAnonCheckblockNum st) (i + 1)
            pure $ "check-block-" ++ show i
    local (\st' -> st' {tlCurrentCheckblock = Just cbnm}) $
        localBindings id f
    
-- get the check block name, error if there isn't one
-- update the test results tvar
addTestResult :: TestResult -> Interpreter ()
addTestResult tr = do
    st <- ask
    ms <- liftIO $ readIORef $ tlModuleState st
    cbnm <- case tlCurrentCheckblock st of
        Just n -> pure n
        -- todo: this check should be done before the test is executed
        -- this can happen once implement static checks on the syntax
        -- before interpretation
        Nothing -> _errorWithCallStack $ "test predicate not in check block: " ++ show tr
    liftIO $ atomically $ do
        v <- readTVar (tlHandleState st)
        -- nested tvar will be removed when test results per module
        -- is implemented
        modifyTVar (hsTestResults v) ((msModuleName ms, cbnm, tr):)

localPushCallStack :: SourcePosition -> Interpreter a -> Interpreter a
localPushCallStack sp = do
    local (\tl -> tl { tlCallStack = sp : tlCallStack tl})

showState :: InterpreterState -> IO String
showState _s = undefined {-do
    p1 <- showTab "local created" =<< readIORef (tlLocallyCreatedBindings s)
    p2 <- showTab "imported"  =<< readIORef (tlIncludedBindings s)
    pure $ unlines [p1,p2]
    --pure "TODO: show state"
    {-do
    e1 <- readIORef $ isSystemEnv s
    e2 <- readIORef $ isScriptEnv s
    p1 <- showTab "System" e1
    p2 <- showTab "User" e2
    pure $ p1 ++ "\n" ++ p2-}
  where
    showTab t vs = do
        ps <- mapM (\(n,v) -> ((n ++ " = ") ++) <$> torepr' v) vs
        pure $ t ++ "\n" ++ unlines ps-}

{-
show the system env
then the script env
in tables
left side: binding name
right side: binding value
values: simple: num, bool, text: but use ``` display, no '\n'
     foreignfunv
     boxv: look inside the box, mark it as a box though
     variantv:
       special show for list, record, tuple
  funv: show the env
        show the param names and the expr (using pretty)
        -> show as a lam syntax
        the env needs shrinking to not be awful

then figure out how to show values as bindings to other bindings
  the only way to do this is to save this info at time of adding to the env
  it only works for bindings which are a = _system_path.module.iden
    which will display like this instead of with the actual value on the lhs


TODO: add test results -> but these will end up in the above before
long
foreign functions 

-}
    
addFFIImpls' :: [(String, [Value] -> Interpreter Value)] -> Interpreter ()
addFFIImpls' fs = do
    st <- ask
    liftIO $ atomically $
        modifyTVar (tlHandleState st) $ \x ->
        x {hsForeignFunctionImpls = fs ++ hsForeignFunctionImpls x}

addFFITypes' :: [(String, FFITypeInfo)] -> Interpreter ()
addFFITypes' ts = do
    st <- ask
    liftIO $ atomically $
        modifyTVar (tlHandleState st) $ \x ->
        x {hsForeignTypes = ts ++ hsForeignTypes x}

addFFIPackage' :: String -> FFIPackage -> Interpreter ()
addFFIPackage' _nm pkg = do
    addFFITypes' $ ffiPackageTypes pkg
    addFFIImpls' $ ffiPackageFunctions pkg 

---------------------------------------

-- concurrency support

spawnExtWaitHandle :: Typeable a => Handle -> (ThreadHandle -> IO a) -> IO a
spawnExtWaitHandle (Handle h) f = do
    st <- atomically $ readTVar h
    spawnWaitCast (hsConcurrencyHandle st) f

spawnWaitCast :: Typeable a => BConcurrencyHandle -> (ThreadHandle -> IO a) -> IO a
spawnWaitCast h f = do
    r <- spawnExtWait h (\th -> toDyn <$> f th)
    pure (fromJust $ fromDynamic r)

askThreadHandle :: Interpreter ThreadHandle
askThreadHandle = tlThreadHandle <$> ask

extractValueException :: SomeException -> Maybe Dynamic
extractValueException e = case fromException e of
    Just (InterpreterException cs s) -> f (TextV s) cs
    Just (ValueException cs v) -> f v cs
    _ -> Nothing
  where
    f v cs = Just $ toDyn $ VariantV (bootstrapType "Tuple") "tuple"
                  [("0", v), ("1", FFIValue ("_system","callstack") $ toDyn cs)]

bSpawn :: [Value] -> Interpreter Value
bSpawn [f] = do
    st <- ask
    x <- liftIO $ spawn (tlThreadHandle st) $ \th ->
        runInterpInherit st th True $ toDyn <$> app f []
    pure $ FFIValue ("_system","addr") $ toDyn x
bSpawn x = _errorWithCallStack $ "wrong args to bSpawn: " ++ show x

bSpawnMonitor :: [Value] -> Interpreter Value
bSpawnMonitor [f] =  spawnMonitorWrap Nothing f
bSpawnMonitor x = _errorWithCallStack $ "wrong args to bSpawnMonitor: " ++ show x

bSpawnMonitorTag :: [Value] -> Interpreter Value
bSpawnMonitorTag [tg,f] = spawnMonitorWrap (Just tg) f
bSpawnMonitorTag x = _errorWithCallStack $ "wrong args to bSpawnMonitorTag: " ++ show x

spawnMonitorWrap :: Maybe Value -> Value -> Interpreter Value
spawnMonitorWrap tag f = do
    st <- ask
    (saddr,ref) <- liftIO $ spawnMonitor (tlThreadHandle st) (toDyn <$> tag) $ \th ->
        runInterpInherit st th True $ do
        toDyn <$> app f []
    pure $ VariantV (bootstrapType "Tuple") "tuple"
        [("0",FFIValue ("_system","addr") $ toDyn saddr)
        ,("1", convertHsMonitorRef ref)]

bAsyncExit :: [Value] -> Interpreter Value
bAsyncExit [FFIValue _ffitag to,val] = do
    let toaddr = maybe (error $ "async-exit to non addr: " ++ show to) id $ fromDynamic to
    th <- askThreadHandle
    liftIO $ asyncExit th toaddr $ toDyn val
    pure nothing
bAsyncExit x = _errorWithCallStack $ "wrong args to bAsyncExit: " ++ show x

convertHsMonitorRef :: MonitorRef -> Value
convertHsMonitorRef (MonitorRef s i) =
    VariantV (internalsType "MonitorRef") "monitor-ref"
        [("a", TextV s)
        ,("b", NumV $ fromIntegral i)
        ]

bSelf :: [Value] -> Interpreter Value
bSelf [] = do
    x <- askThreadHandle
    pure $ FFIValue ("_system","addr") $ toDyn $ addr x
bSelf x = _errorWithCallStack $ "wrong args to bSelf: " ++ show x

bSleep :: [Value] -> Interpreter Value
bSleep [NumV t] = do
    liftIO $ threadDelay (floor (t * 1000 * 1000))
    pure nothing
bSleep x = _errorWithCallStack $ "wrong args to sleep: " ++ show x

bSend :: [Value] -> Interpreter Value
bSend [FFIValue _ffitag to, val] = do
    let toaddr = maybe (error $ "send to non addr: " ++ show to) id $ fromDynamic to
    th <- askThreadHandle
    liftIO $ send th toaddr $ toDyn val
    pure nothing
bSend x = _errorWithCallStack $ "wrong args to bSend: " ++ show x

{-bReceive :: [Value] -> Interpreter Value
bReceive [] = do
    th <- askThreadHandle
    v <- liftIO $ zreceive th (\x -> pure $ Just x)
    let x = maybe (error $ "got non value from receive: " ++ show v) id $ fromDynamic v
    pure x
bReceive x = error $ "wrong args to bReceive: " ++ show x-}


-- stubs - this is what the new api in hsconcurrency should be modified too
-- needs a bunch of rewriting because the code currently runs the prd
-- in the stm monad, which is too inconvenient now
-- try to hack it first using unsafeperformio
-- then can check everything else works before doing this big rewrite
-- xreceive :: MonadIO m => ThreadHandle -> (Dynamic -> m (Maybe Dynamic)) -> m Dynamic
{-xreceive :: ThreadHandle -> (Dynamic -> Interpreter (Maybe Dynamic)) -> Interpreter Dynamic
xreceive th prd = do
    st <- ask
    let prd' :: Dynamic -> Bool
        prd' d = unsafePerformIO $ do
            let f x = runReaderT x st
            x <- f $ prd d
            case x of
                Nothing -> pure False
                Just {} -> pure True
    x <- liftIO $ receive th prd'
    x' <- prd x
    case x' of
        Nothing -> error $ "receive hack: predicate was not consistent?"
        Just v -> pure v
    
xreceiveTimeout :: MonadIO m =>
                   ThreadHandle
                -> Int
                -> (Dynamic -> m (Maybe Dynamic))
                -> m (Maybe Dynamic)
xreceiveTimeout = undefined-}


---------------------------------------

-- new handles, running interpreter functions using a handle

baseEnv :: [(String,Value)]
baseEnv =
    [
    -- come back to dealing with these properly when decide how to
    -- do ad hoc polymorphism
    -- but definitely also want a way to refer to these as regular
    -- values in the syntax like haskell does (==)
    -- so they can be passed as function values, and appear in
    -- provides, include lists
    -- these operators should be in a module

     ("==", ForeignFunV "equal-always")
    ,("+", ForeignFunV "+")
    ,("-", ForeignFunV "-")
    ,("*", ForeignFunV "*")
    ,("/", ForeignFunV "/")
    ,("<", ForeignFunV "_lessthan")
    ,(">", ForeignFunV "_greaterthan")
    ,("<=", ForeignFunV "_lessequal")
    ,(">=", ForeignFunV "_greaterequal")
    ,("^", ForeignFunV "reverse-app")
    ,("|>", ForeignFunV "chain-app")

    ]

runInterp :: ThreadHandle -> Bool -> Handle -> Interpreter a -> IO a
runInterp th incG (Handle h) f = do
    sh <- newSourceHandle th h
    flip runReaderT sh $ do
        -- will become the use context thing
        -- only bootstrapping a handle with the bootstrap and _internals
        -- loading skips including globals atm
        when incG $ void $ interpStatement (Include (ImportName "globals"))
        f

-- used for a local thread - one that isn't for a new api call or
-- new module/source file - it preserves the right part of the
-- enclosing state
runInterpInherit :: ThreadLocalState -> ThreadHandle -> Bool -> Interpreter a -> IO a
runInterpInherit parentSh th incG f = do
    p <- newIORef []
    b <- newIORef =<< readIORef (tlLocallyCreatedBindings parentSh)
    bo <- newIORef =<< readIORef (tlIncludedBindings parentSh)
    rb <- newIORef =<< readIORef (tlReplCreatedBindings parentSh)
    cbn <- newIORef 0
    let sh = parentSh
            {tlThreadHandle = th
            ,tlProvides = p
            ,tlLocallyCreatedBindings = b
            ,tlReplCreatedBindings = rb
            ,tlIncludedBindings = bo
            ,tlNextAnonCheckblockNum = cbn
            ,_tlIsModuleRootThread = False
            }
    flip runReaderT sh $ do
        when incG $ void $ interpStatement (Include (ImportName "globals"))
        f

---------------------------------------

-- other interpreter monad support functions

-- convert a burdock either value to a haskell one

_bToHEither :: Value -> Either Value Value
_bToHEither (VariantV tg "left" [(_,e)]) | tg == internalsType "Either" = Left e
_bToHEither (VariantV tg "right" [(_,r)]) | tg == internalsType "Either" = Right r
_bToHEither x = error $ "non either passed to btoheither: " ++ show x

formatExceptionI :: Bool -> InterpreterException -> Interpreter String
formatExceptionI includeCallstack e = do
    (st,m) <- case e of
            ValueException st (TextV m) -> pure (st,m)
            ValueException st m -> (st,) <$> torepr' m
            InterpreterException st m -> pure (st,m)
    stf <- if includeCallstack
           then ("\ncall stack:\n" ++) <$> formatCallStack st
           else pure ""
    pure $ m ++ stf

{-
partial idea -> save sources as they are parsed, so can refer to them
in later error messages and stuff
todo: track original filenames plus generated unique filenames better
something robust will track repeated loads of the same file
which changes each load
and be able to identify which version of that source a particular
call stack refers to
can think about generating generated names specific to the api, and
to the repl (e.g. repl-line-x for repl instead of unknown-arbitrary-ctr)

-}

useSource :: Maybe FilePath
          -> String
          -> String
          -> (FilePath -> String -> Either String a)
          -> Interpreter (Either String a)
useSource mfn modName src p = do
    fn <- case mfn of
              Nothing -> uniqueSourceName
              Just x -> pure x
    st <- ask
    liftIO $ atomically $ do
        modifyTVar (tlHandleState st) $ \x ->
            x {hsSourceCache = (fn,src) : hsSourceCache x }
    putModule (ModuleState modName fn)
    pure $ p fn src

-- needs some checking
uniqueSourceName :: Interpreter String
uniqueSourceName = do
    st <- ask
    i <- liftIO $ atomically $ do
        i <- hsUniqueSourceCtr <$> readTVar (tlHandleState st)
        modifyTVar (tlHandleState st) $ \x ->
            x {hsUniqueSourceCtr = i + 1}
        pure i
    pure $ "unknown-" ++ show (i :: Int)
    
formatCallStack :: CallStack -> Interpreter String
formatCallStack cs = do
    sc <- readSourceCache
    pure $ unlines $ map (f sc) cs
  where
    f :: [(FilePath, String)] -> SourcePosition -> String
    f _ Nothing = "<unknown>"
    f sc (Just (fn,l,c)) = either show id $ do
        -- todo: add the caret for the column
        src <- maybe (Left $ "source not found in cache: " ++ fn) Right $ lookup fn sc
        -- todo: add error handling for this
        -- plus memoize running lines on the source?
        let lne = lines src !! (l - 1)
            caret = replicate (c - 1) ' ' ++ "^"
        pure $ fn ++ ":" ++ show l ++ ":\n" ++ lne ++ "\n" ++ caret

_undefinedWithCallStack :: Interpreter a
_undefinedWithCallStack = _errorWithCallStack "undefined"

_errorWithCallStack :: String -> Interpreter a
_errorWithCallStack msg = do
    cs <- readCallStack
    css <- formatCallStack cs
    error $ msg ++ "\n" ++ css

---------------------------------------

-- converting values to strings

torepr' :: Value -> Interpreter String
torepr' x = show <$> toreprx x

toreprx :: Value -> Interpreter (P.Doc a)
toreprx (NumV n) = pure $ case extractInt n of
                             Just x -> P.pretty $ show x
                             Nothing ->  P.pretty $ show n
toreprx (BoolV n) = pure $ P.pretty $ if n then "true" else "false"
toreprx (FunV {}) = pure $ P.pretty "<Function>" -- todo: print the function value
toreprx (MethodV {}) = pure $ P.pretty "<Method>"
toreprx (ForeignFunV f) = pure $ P.pretty $ "<ForeignFunction:" ++ f ++ ">"
toreprx (TextV s) = pure $ P.pretty $ "\"" ++ s ++ "\""
-- todo: add the type
toreprx (BoxV _ b) = do
    bv <- liftIO $ readIORef b
    v <- toreprx bv
    pure $ P.pretty "<Box:" <> v <> P.pretty ">"

toreprx (VariantV tg "tuple" fs) | tg == bootstrapType "Tuple" = do
    vs <- mapM (toreprx . snd) fs
    pure $ P.pretty "{" <> P.nest 2 (xSep ";" vs) <> P.pretty "}"
toreprx (VariantV tg "record" fs) | tg == bootstrapType "Record" = do
    vs <- mapM f fs
    pure $ P.pretty "{" <> P.nest 2 (xSep "," vs) <> P.pretty "}"
  where
    f (a,b) = ((P.pretty a P.<+> P.pretty "=") P.<+>) <$> toreprx b
-- todo: should this try to use the local name of the variant
-- including qualifier if there is one
-- how can this work?
toreprx (VariantV _ nm []) = pure $ P.pretty nm
toreprx (VariantV _ nm fs) = do
    vs <- mapM (toreprx . snd) fs
    pure $ P.pretty nm <> P.pretty "(" <> P.nest 2 (xSep "," vs) <> P.pretty ")"

toreprx x@(FFIValue {}) = do
    fn' <- interpDotExprE x "_torepr"
    case fn' of
        Left {} -> pure $ P.pretty $ show x
        Right fn -> do
            y <- app fn []
            case y of
                TextV t -> pure $ P.pretty t
                _ -> error $ "_torepr didn't return a text value: " ++ show y
toreprx (TemplateV sp) = evalTemplate [sp]

xSep :: String -> [P.Doc a] -> P.Doc a
xSep x ds = P.sep $ P.punctuate (P.pretty x) ds

------------------------------------------------------------------------------

-- ffi

-- package system

data FFIPackage
    = FFIPackage
    {ffiPackageTypes :: [(String,FFITypeInfo)] 
    ,ffiPackageFunctions :: [(String, [Value] -> Interpreter Value)]
    }
-- built in ffi types

data FFITypeInfo
    = FFITypeInfo
    {ffiMember :: Maybe (Interpreter Value)
    }

makeFFIMemberFunction :: String -> Maybe (Interpreter Value)
makeFFIMemberFunction f = Just $ pure $ MethodV $ ForeignFunV f

ffiSingleArgMethod :: String -> Value -> Interpreter Value
ffiSingleArgMethod nm v =
        pure $ FunV [NameBinding "xx"]
            (App Nothing (Iden nm) [Iden "yy", Iden "xx"])
            [("yy", v)
            ,(nm, ForeignFunV nm)]

ffiNoArgMethod :: String -> Value -> Interpreter Value
ffiNoArgMethod nm v =
        pure $ FunV []
            (App Nothing (Iden nm) [Iden "yy"])
            [("yy", v)
            ,(nm, ForeignFunV nm)]

ffiNoArgValue :: String -> String -> Value -> Interpreter Value
ffiNoArgValue nm fld v = app f []
  where
    f = FunV []
            (App Nothing (Iden nm) [Text fld, Iden "yy"])
            [("yy", v)
            ,(nm, ForeignFunV nm)]

{-liftEquals :: Typeable t =>
              String
           -> (t -> t -> Interpreter Bool)
           -> Dynamic
           -> Dynamic -> Interpreter Bool
liftEquals tyname f a b =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> f a' b'
        _ -> error $ "expected two " ++ tyname ++ ", got " ++ show (a,b)-}

-- this is definitely way too much boilerplate
addrEquals :: [Value] -> Interpreter Value
addrEquals [FFIValue _ a, FFIValue _ b] =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> BoolV <$> e a' b'
        _ -> error $ "expected two addrs, got " ++ show (a,b)
  where
    e :: Addr -> Addr -> Interpreter Bool
    e x y = pure $ (==) x y
addrEquals _ = error "bad args to addr equals"

addrToRepr :: [Value] -> Interpreter Value
addrToRepr [FFIValue _ a] =
    case fromDynamic a of
        Just (_ :: Addr) -> pure $ TextV $ show a
        _ -> error $ "bad arg to addr torepr " ++ show a
addrToRepr _ = error "bad args to addr torepr"


{-addrEquals :: Dynamic -> Dynamic -> Interpreter Bool
addrEquals a b =
    liftEquals "addr" e a b
  where
    e :: Addr -> Addr -> Interpreter Bool
    e x y = pure $ (==) x y-}

relationFFIEquals :: [Value] -> Interpreter Value
relationFFIEquals [FFIValue _ a, FFIValue _ b] =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> BoolV <$> e a' b'
        _ -> error $ "expected two relations, got " ++ show (a,b)
  where
    e :: R.Relation Value -> R.Relation Value -> Interpreter Bool
    e x y = either (error . show) pure $ R.relEqual x y
relationFFIEquals _ = error "bad args to relation equals"

{-relationToRepr :: Dynamic -> Interpreter String
relationToRepr a = case fromDynamic a of
    Just (r' :: R.Relation Value) -> pure $ R.showRel r'
    Nothing -> error $ "expected relation, got " ++ show a-}

relationToRepr :: [Value] -> Interpreter Value
relationToRepr [FFIValue _ a] = case fromDynamic a of
    Just (r' :: R.Relation Value) -> pure $ TextV $ R.showRel r'
    Nothing -> error $ "expected relation, got " ++ show a
relationToRepr _ = error "bad args to relationToRepr"

data FMD
    = FMD
    {fmdLkp :: [(String, Value -> Interpreter Value)]
    ,fmdFallback :: Maybe (String -> Value -> Interpreter Value)
    }

ffiMemberDispatcher :: FMD -> [Value] -> Interpreter Value
ffiMemberDispatcher fmd [TextV fld, v] = do
    case lookup fld (fmdLkp fmd) of
        Just f -> f v
        _ | Just fb <- fmdFallback fmd -> fb fld v
        _ -> error $ "unsupported method: " ++ show v ++ " " ++ fld
ffiMemberDispatcher _ _ = error "bad args to ffiMemberDispatcher"

builtInFFITypes :: [(String,FFITypeInfo)]
builtInFFITypes =
    [("callstack", FFITypeInfo Nothing)
    ,("addr", FFITypeInfo (makeFFIMemberFunction "_member-addr"))
    ,("relation", FFITypeInfo (makeFFIMemberFunction "_member-relation"))
    ,("unknown", FFITypeInfo Nothing)
    ,("burdockast", FFITypeInfo  Nothing)
    ,("typeinfo", FFITypeInfo Nothing)
    ,("casepattern", FFITypeInfo Nothing)
    ]

-- built in functions implemented in haskell:

builtInFF :: [(String, [Value] -> Interpreter Value)]
builtInFF =
    [("ffi-function", ffiFunction)
    ,("is-specific-ffi-type", isSpecificFFIType)

    ,("equal-always", equalAlways)
    ,("_lessthan", lessThan)
    ,("_greaterthan", bGT)
    ,("_lessequal", bLTE)
    ,("_greaterequal", bGTE)

    ,("+", \case
             [NumV a,NumV b] -> pure $ NumV $ a + b
             [TextV a, TextV b] -> pure $ TextV $ a ++ b
             as -> _errorWithCallStack $ "unsupported args to + :" ++ show as)
    ,("-", \case
             [NumV a] -> pure $ NumV (- a)
             [NumV a,NumV b] -> pure $ NumV $ a - b
             as -> _errorWithCallStack $ "unsupported args to - :" ++ show as)
    ,("*", \case
             [NumV a,NumV b] -> pure $ NumV $ a * b
             xs -> _errorWithCallStack $ "bad args to * " ++ show xs)
    ,("/", \case
             [NumV a,NumV b] -> pure $ NumV $ divideScientific a b
             xs -> error $ "bad args to / " ++ show xs)

    ,("reverse-app", reverseApp)
    ,("chain-app", chainApp)
    
    ,("not", \case
             [BoolV b] -> pure $ BoolV $ not b
             xs -> error $ "bad args to not " ++ show xs)

    ,("make-variant", makeVariant)
    ,("is-variant", isVariant)
    ,("is-specific-ffi-type", isSpecificFFIType)

    ,("equal-by-field", equalByField)

    ,("load-module", bLoadModule)
    ,("show-handle-state", bShowHandleState)
    ,("haskell-error", haskellError)
    ,("haskell-undefined", haskellUndefined)

    ,("is-nothing", isNothing)
    ,("is-Nothing", isNothing)

    ,("is-tuple", isTuple)
    ,("is-record", isRecord)
    ,("is-boolean", isBoolean)
    ,("is-string", isString)
    ,("is-number", isNumber)
    ,("is-function", isFunction)
    ,("string-to-number", stringToNumber)
    ,("string-index-of", stringIndexOf)
    ,("num-abs", numAbs)

    ,("print", bPrint)
    ,("torepr", torepr)
    ,("tostring", toString)
    ,("raise", raise)

    ,("parse-file", bParseFile)
    ,("show-haskell-ast", bShowHaskellAst)
    ,("get-call-stack", getCallStack)
    ,("format-call-stack", bFormatCallStack)
    ,("run-script", bRunScript)

    ,("_member-relation", ffiMemberDispatcher $ FMD
         {fmdLkp = [("_equals", ffiSingleArgMethod "relation-equals")
                   ,("_torepr", ffiNoArgMethod "relation-to-repr")]
         ,fmdFallback = Nothing})
    ,("relation-equals", relationFFIEquals)
    ,("relation-to-repr", relationToRepr)
    ,("rel-to-list", relToList)
    ,("rel-from-list", relFromList)
    ,("rel-from-table", relFromTable)
    ,("table-dee", \case
             [] -> pure $ FFIValue ("_system","relation") $ toDyn (R.tableDee :: R.Relation Value)
             _ -> error $ "bad args to table-dee maker")
    ,("table-dum", \case
             [] -> pure $ FFIValue ("_system","relation") $ toDyn (R.tableDum :: R.Relation Value)
             _ -> _errorWithCallStack $ "bad args to table-dee maker")
    ,("rel-union", relUnion)
    ,("rel-where", relWhere)
    ,("rel-update", relUpdate)
    ,("rel-project", relProject)
    ,("rel-rename", relRename)
    ,("rel-join", relJoin)
    ,("rel-not-matching", relNotMatching)
    ,("rel-group", relGroup)
    ,("rel-ungroup", relUngroup)
    ,("rel-summarize", relSummarize)
    ,("union-recs", unionRecs)

    ,("spawn", bSpawn)
    ,("spawn-monitor", bSpawnMonitor)
    ,("spawn-monitor-tag", bSpawnMonitorTag)
    ,("self", bSelf)
    ,("sleep", bSleep)
    ,("send", bSend)
    ,("async-exit", bAsyncExit)
    ,("addr-equals", addrEquals)
    ,("addr-to-repr", addrToRepr)
    ,("_member-addr", ffiMemberDispatcher $ FMD
         {fmdLkp = [("_equals", ffiSingleArgMethod "addr-equals")
                   ,("_torepr", ffiNoArgMethod "addr-to-repr")
                   ]
         ,fmdFallback = Nothing})

    ,("get-args", bGetArgs)

    ,("read-process", bReadProcess)
    ,("call-process", bCallProcess)
    ,("list-directory", bListDirectory)
     
    ]

ffiFunction :: [Value] -> Interpreter Value
ffiFunction [TextV nm] = do
    -- todo: check the value exists in the ffi catalog
    pure $ ForeignFunV nm
ffiFunction _ = _errorWithCallStack "wrong args to ffi-function"

fromBList :: Value -> Maybe [Value]
fromBList (VariantV tg "empty" [])
    | tg == internalsType "List" = Just []
fromBList (VariantV tg "link" [("first",f),("rest",r)])
    | tg == internalsType "List" =
      (f:) <$> fromBList r
fromBList _ = Nothing

fromBTuple :: Value -> Maybe [Value]
fromBTuple (VariantV tg "tuple" fs)
    | tg == bootstrapType "Tuple" = Just $ map snd fs
fromBTuple _ = Nothing


makeVariant :: [Value] -> Interpreter Value
makeVariant [FFIValue _ffitag ftg, TextV nm, ms', as']
    | Just tg <- fromDynamic ftg
    , Just ms <- fromBList ms'
    , Just as <- fromBList as' =
    VariantV tg nm <$> f (as ++ ms)
  where
    f [] = pure []
    f (BoolV False : TextV fnm : v : asx) =
        ((fnm,v) :) <$> f asx
    f (BoolV True : TextV fnm : v : asx) = do
        vb <- BoxV (internalsType "Any") <$> liftIO (newIORef v)
        ((fnm,vb):) <$> f asx
    f x = _errorWithCallStack $ "wrong args to make-variant: " ++ show x
makeVariant x = _errorWithCallStack $ "wrong args to make-variant: " ++ show x

isVariant :: [Value] -> Interpreter Value
isVariant [FFIValue _ffitag ftg, TextV nm, VariantV vtg vt _]
    | Just tg <- fromDynamic ftg
    = pure $ BoolV $ tg == vtg && nm == vt
isVariant [FFIValue {}, TextV _nm, _] = pure $ BoolV False
isVariant _ = _errorWithCallStack $ "wrong args to is-variant"

isSpecificFFIType :: [Value] -> Interpreter Value
isSpecificFFIType [TextV nm, FFIValue (_pkg,ffitag) _]
    = pure $ BoolV $ nm == ffitag
isSpecificFFIType [TextV _, _] = pure $ BoolV False
isSpecificFFIType _ = _errorWithCallStack $ "wrong args to is specific ffi type"

isNothing :: [Value] -> Interpreter Value
isNothing [VariantV tg "nothing" _] | tg == bootstrapType "Nothing" = pure $ BoolV True
isNothing [_] = pure $ BoolV False
isNothing _ = _errorWithCallStack $ "wrong args to is-nothing/is-Nothing"

isTuple :: [Value] -> Interpreter Value
isTuple [VariantV tg "tuple" _] | tg == bootstrapType "Tuple" = pure $ BoolV True
isTuple [_] = pure $ BoolV False
isTuple _ = _errorWithCallStack $ "wrong args to is-tuple"

isRecord :: [Value] -> Interpreter Value
isRecord [VariantV tg "record" _] | tg == bootstrapType "Record" = pure $ BoolV True
isRecord [_] = pure $ BoolV False
isRecord _ = _errorWithCallStack $ "wrong args to is-record"

isBoolean :: [Value] -> Interpreter Value
isBoolean [BoolV _] = pure $ BoolV True
isBoolean [_] = pure $ BoolV False
isBoolean _ = _errorWithCallStack $ "wrong args to is-boolean"

isString :: [Value] -> Interpreter Value
isString [TextV _] = pure $ BoolV True
isString [_] = pure $ BoolV False
isString _ = _errorWithCallStack $ "wrong args to is-string"

isNumber :: [Value] -> Interpreter Value
isNumber [NumV _] = pure $ BoolV True
isNumber [_] = pure $ BoolV False
isNumber _ = _errorWithCallStack $ "wrong args to is-number"

isFunction :: [Value] -> Interpreter Value
isFunction [FunV {}] = pure $ BoolV True
isFunction [ForeignFunV {}] = pure $ BoolV True
isFunction [_] = pure $ BoolV False
isFunction _ = _errorWithCallStack $ "wrong args to is-function"

reverseApp :: [Value] -> Interpreter Value
reverseApp [a, f] = app f [a]
reverseApp as = _errorWithCallStack $ "wrong args to ^ " ++ show as

chainApp :: [Value] -> Interpreter Value
chainApp [f, a] = app f [a]
chainApp as = _errorWithCallStack $ "wrong args to |> " ++ show as

bGT :: [Value] -> Interpreter Value
bGT [a,b] = do
    f0 <- interp (Iden "<")
    e0 <- app f0 [a,b]
    case e0 of
        BoolV x -> pure $ BoolV $ not x
        _ -> error $ "expected bool from <, got " ++ show e0
bGT as = _errorWithCallStack $ "unsupported args to > (todo?) " ++ show as

bLTE :: [Value] -> Interpreter Value
bLTE [a,b] = do
    f0 <- interp (Iden "<")
    e0 <- app f0 [a,b]
    f1 <- interp (Iden "==")
    e1 <- app f1 [a,b]
    case (e0,e1) of
        (BoolV True, _) -> pure $ BoolV True
        (BoolV False, x@(BoolV {})) -> pure x
        _ -> error $ "unexpected return value to < or ==: " ++ show (e0,e1)
bLTE as = _errorWithCallStack $ "unsupported args to <= (todo?) " ++ show as

bGTE :: [Value] -> Interpreter Value
bGTE [a,b] = do
    f0 <- interp (Iden ">")
    e0 <- app f0 [a,b]
    f1 <- interp (Iden "==")
    e1 <- app f1 [a,b]
    case (e0,e1) of
        (BoolV True, _) -> pure $ BoolV True
        (BoolV False, x@(BoolV {})) -> pure x
        _ -> error $ "unexpected return value to > or ==: " ++ show (e0,e1)
bGTE as = _errorWithCallStack $ "unsupported args to >= (todo?) " ++ show as

bPrint :: [Value] -> Interpreter Value
bPrint [v] = do
    v' <- case v of
              TextV s -> pure s
              _ -> torepr' v
    liftIO $ putStrLn v'
    pure nothing

bPrint _ = _errorWithCallStack $ "wrong number of args to print"

-- load module at filepath
bLoadModule :: [Value] -> Interpreter Value
bLoadModule [TextV moduleName, TextV fn] = do
    loadAndRunModule True moduleName fn
    pure nothing
bLoadModule _ = _errorWithCallStack $ "wrong args to load module"

bShowHandleState :: [Value] -> Interpreter Value
bShowHandleState [] = do
    st <- ask
    v <- liftIO $ showState st
    pure $ TextV v
    
bShowHandleState _ = _errorWithCallStack $ "wrong args to show-handle-state"

torepr :: [Value] -> Interpreter Value
torepr [x] = TextV <$> torepr' x
torepr _ = _errorWithCallStack "wrong number of args to torepr"

toString :: [Value] -> Interpreter Value
toString [s@(TextV {})] = pure s
toString [x] = torepr [x]
toString _ = _errorWithCallStack "wrong number of args to tostring"

stringToNumber :: [Value] -> Interpreter Value
stringToNumber [TextV t] = case readMaybe t of
    Just n ->
        pure $ VariantV (internalsType "Option") "some" [("value", NumV n)]
    Nothing ->
        pure $ VariantV (internalsType "Option") "none" []
stringToNumber _ = _errorWithCallStack "wrong args to string-to-number"

stringIndexOf :: [Value] -> Interpreter Value
stringIndexOf [TextV t, TextV s] =
    case (s `isPrefixOf`) `findIndex` (tails t) of
        Just i -> pure $ NumV $ fromIntegral i
        Nothing -> pure $ NumV (-1)
stringIndexOf _ = _errorWithCallStack "wrong args to string-index-of"

numAbs :: [Value] -> Interpreter Value
numAbs [NumV v] = pure $ NumV $ abs v
numAbs _ = _errorWithCallStack $ "wrong args to num-abs"

raise :: [Value] -> Interpreter Value
raise [v] = do
    cs <- readCallStack
    throwM $ ValueException cs v
raise _ = _errorWithCallStack "wrong args to raise"

haskellError :: [Value] -> Interpreter Value
haskellError [TextV v] = error v
haskellError _ = _errorWithCallStack $ "wrong args to haskell-error"

haskellUndefined :: [Value] -> Interpreter Value
haskellUndefined [] = undefined
haskellUndefined _ = _errorWithCallStack $ "wrong args to haskell-undefined"

bParseFile :: [Value] -> Interpreter Value
bParseFile [TextV fn] = do
    src <- liftIO $ readFile fn
    let ast = either error id $ parseScript fn src
    pure $ FFIValue ("_system","burdockast") $ toDyn $ ast
    
bParseFile _ = _errorWithCallStack $ "wrong args to parse-file"

bShowHaskellAst :: [Value] -> Interpreter Value
bShowHaskellAst [FFIValue _ffitag v] = do
    let ast :: Script
        ast = maybe (error "bad arg type to haskell-show-ast") id $ fromDynamic v
    pure $ TextV $ ppShow ast
bShowHaskellAst _ = _errorWithCallStack $ "wrong args to show-haskell-ast"
    
getCallStack :: [Value] -> Interpreter Value
getCallStack [] = (FFIValue ("_system","callstack") . toDyn) <$> readCallStack
getCallStack _ = _errorWithCallStack $ "wrong args to get-call-stack"

bFormatCallStack :: [Value] -> Interpreter Value
bFormatCallStack [FFIValue _ffitag cs] = do
    let hcs = maybe (error "bad arg type to format-call-stack") id $ fromDynamic cs
    TextV <$> formatCallStack hcs
bFormatCallStack _ = _errorWithCallStack $ "wrong args to format-call-stack"

bRunScript :: [Value] -> Interpreter Value
bRunScript [TextV src] = do
    Script ast <- either error id <$> useSource Nothing "run-script" src parseScript
    x <- interpStatements ast
    -- todo: need to only get the new bindings created in the interpstatements
    -- just called
    stuff <- askLocallyCreatedBindings
    modifyReplCreatedBindings (extendBindings stuff)
    pure x

bRunScript _ = _errorWithCallStack $ "wrong args to run-script"

equalAlways :: [Value] -> Interpreter Value
equalAlways [a,b] = BoolV <$> hEqualAlways a b

equalAlways _ = _errorWithCallStack $ "wrong args to equal-always"

hEqualAlways :: Value -> Value -> Interpreter Bool
hEqualAlways a' b' =
    case (a',b') of
        (TemplateV sp, _) -> evalTemplate [sp]
        (_,TemplateV sp) -> evalTemplate [sp]
        (BoolV a, BoolV b) -> pure $ a == b
        (BoolV {}, _) -> pure False
        (NumV a, NumV b) -> pure $ a == b
        (NumV {}, _) -> pure False
        (TextV a, TextV b) -> pure $ a == b
        (TextV {}, _) -> pure False
        -- variant with equal method
        -- this includes records with an equal-always
        (VariantV _ _ fs, _)
            | Just _ <- lookup "_equals" fs
              -> do
                  fn <- interpDotExpr a' "_equals"
                  unwrapBool <$> app fn [b']
        (VariantV tg nm fs, VariantV tg1 nm1 fs1)
            -> ((tg == tg1 && nm == nm1) &&)
                <$> fieldsEqual fs fs1
        (VariantV {} , _) -> pure False
        -- pointer equality on IORef
        (BoxV _ a, BoxV _ b) -> pure $ a == b
        (BoxV {}, _) -> pure False
        -- ffivalue with equals member
        (FFIValue {}, _)
            -> do
              fn <- interpDotExpr a' "_equals"
              unwrapBool <$> app fn [b']
        -- ffivalue with old equals function
        {-(FFIValue (_pkga,tga) a, FFIValue (_pkgb,tgb) b)
            | tga == tgb -> do
                  ti <- askFFTInfo tga
                  maybe (pure False) (\f' -> f' a b) (ffiTypeEquals =<< ti)
        (FFIValue {}, _) -> pure False-}
        _ | isFn a' && isFn b' -> incomparable
        (FunV {}, _) -> pure False
        (ForeignFunV {}, _) -> pure False
        (MethodV {}, _) -> pure False
  where
    fieldsEqual as bs =
        if length as /= length bs
        then pure False
        else do
        let as' = sortOn fst as
            bs' = sortOn fst bs
            fieldEquals (na,va) (nb,vb) =
                          ((na == nb) &&) <$> unwrapBool <$> equalAlways [va, vb]
        and <$> zipWithM fieldEquals as' bs'
    isFn = \case
        FunV {} -> True
        ForeignFunV {} -> True
        MethodV {} -> True
        _ -> False
    incomparable = error $ "Attempted to compare two incomparable values:\n" ++ show a' ++ "\n" ++ show b'
    unwrapBool = \case
        BoolV True -> True
        BoolV False -> False
        x -> error $ "expected boolean from equal always, got  " ++ show x

equalByField :: [Value] -> Interpreter Value
equalByField [fs', VariantV tga nma a, VariantV tgb nmb b]
    | Just fs <- mapM unText =<< fromBList fs'
    = -- check tags and names
      if not (tga == tgb && nma == nmb)
      then pure $ BoolV False
      else do
          -- check the fields are present in both values
          let proj k m = (k,) <$> lookup k m
              vs1 = mapM (flip proj a) fs
              vs2 = mapM (flip proj b) fs
          case (vs1, vs2) of
              -- compare their values
              (Just vs1', Just vs2') ->
                  BoolV <$> hEqualAlways (VariantV (bootstrapType "Record") "record" vs1')
                               (VariantV (bootstrapType "Record") "record" vs2')
              _ -> pure $ BoolV False
  where
    unText (TextV t) = Just t
    unText _ = Nothing
equalByField _ = _errorWithCallStack $ "wrong args to equalByField"

lessThan :: [Value] -> Interpreter Value
lessThan [a,b] = BoolV <$> hLessThan a b
lessThan as = error $ "unsupported args to < (todo?) " ++ show as

hLessThan :: Value -> Value -> Interpreter Bool
hLessThan a' b' =
    case (a',b') of
        (TemplateV sp, _) -> evalTemplate [sp]
        (_,TemplateV sp) -> evalTemplate [sp]
        (BoolV a, BoolV b) -> pure $ a < b
        (BoolV {}, _) -> error "incompatible args to <"
        (NumV a, NumV b) -> pure $ a < b
        (NumV {}, _) -> error "incompatible args to <"
        (TextV a, TextV b) -> pure $ a < b
        (TextV {}, _) -> error "incompatible args to <"
        -- variant with equal method
        -- this includes records with an equal-always
        (VariantV _ _ fs, _)
            | Just _ <- lookup "_lessthan" fs
              -> do
                  fn <- interpDotExpr a' "_lessthan"
                  unwrapBool <$> app fn [b']
        (VariantV {} , _) -> error "incompatible args to <"
        (BoxV {}, _) -> error "incompatible args to <"
        -- can you create a new unique id for each box
        -- so they can have an arbitrary order?
                --_ a, BoxV _ b) -> pure $ a < b -- error "incompatible args to <"
        -- ffivalue with equals function
        (FFIValue {}, _)
            -> do
              fn <- interpDotExpr a' "_lessthan"
              unwrapBool <$> app fn [b']
        {-(FFIValue (_pkga,tga) a, FFIValue (_pkgb,tgb) b)
            | tga == tgb -> do
                  ti <- askFFTInfo tga
                  maybe (error "incompatible args to <")
                        (\f' -> f' a b)
                        (ffiTypeLT =<< ti)
        (FFIValue {}, _) -> error "incompatible args to <"-}
        _ | isFn a' && isFn b' -> incomparable
        (FunV {}, _) -> error "incompatible args to <"
        (ForeignFunV {}, _) -> error "incompatible args to <"
        (MethodV {}, _) -> error "incompatible args to <"
  where
    isFn = \case
        FunV {} -> True
        ForeignFunV {} -> True
        MethodV {} -> True
        _ -> False
    incomparable = error $ "Attempted to compare two incomparable values:\n" ++ show a' ++ "\n" ++ show b'
    unwrapBool = \case
        BoolV True -> True
        BoolV False -> False
        x -> error $ "expected boolean from equal always, got  " ++ show x


-------------------

relToList :: [Value] -> Interpreter Value
relToList [FFIValue _ffitag v]
    | Just v' <- fromDynamic v
    = either (error . show) (pure . mkl) $ R.toList v'
  where
    mkl = makeBList . map mkr
    mkr = VariantV (bootstrapType "Record") "record"
relToList _ = _errorWithCallStack "bad args to relToList"

relFromList :: [Value] -> Interpreter Value
relFromList [l]
    | Just l' <- fromBList l
    , Just l'' <- mapM unr l'
    = either (error . show) (pure . FFIValue ("_system","relation") . toDyn) $ R.fromList l''
  where
    unr (VariantV tg "record" fs)
        | tg == bootstrapType "Record" = Just fs
    unr _ = Nothing
relFromList x = _errorWithCallStack $ "bad args to relFromList: " ++ show x

relFromTable :: [Value] -> Interpreter Value
relFromTable = relFromList

relUnion :: [Value] -> Interpreter Value
relUnion [FFIValue _ffitag a, FFIValue _ffitag2 b]
    | Just (a' :: R.Relation Value) <- fromDynamic a
    , Just b' <- fromDynamic b
    = either (error . show) (pure . FFIValue ("_system","relation") . toDyn) $ R.relUnion a' b'
relUnion _ = _errorWithCallStack "bad args to relUnion"

relWhere :: [Value] -> Interpreter Value
relWhere [FFIValue _ffitag a, f]
    | Just (a' :: R.Relation Value) <- fromDynamic a
    = either (error . show) (FFIValue ("_system","relation") . toDyn) <$>
      R.relWhere a' (wrapBPredicate f)

relWhere _ = _errorWithCallStack "bad args to relWhere"

relUpdate :: [Value] -> Interpreter Value
relUpdate [FFIValue _ffitag a, uf, pf]
    | Just (a' :: R.Relation Value) <- fromDynamic a
    =  either (error . show) (FFIValue ("_system","relation") . toDyn) <$>
       R.relUpdate a' (wrapBRecFn uf) (wrapBPredicate pf)
relUpdate _ = _errorWithCallStack "bad args to relUpdate"

relProject :: [Value] -> Interpreter Value
relProject [l, FFIValue _ffitag a]
    | Just cs <- mapM unText =<< fromBList l
    , Just (a' :: R.Relation Value) <- fromDynamic a
    =  either (error . show) (pure . FFIValue ("_system","relation") . toDyn) $
       R.relProject cs a'
  where
    unText (TextV t) = Just t
    unText _ = Nothing
relProject _ = _errorWithCallStack "bad args to relProject"

relRename :: [Value] -> Interpreter Value
relRename [l, FFIValue _ffitag a]
    | Just cs <- mapM unTextPair =<< fromBList l
    , Just (a' :: R.Relation Value) <- fromDynamic a
    =  either (error . show) (pure . FFIValue ("_system","relation") . toDyn) $
       R.relRename cs a'
  where
    unTextPair (VariantV tg "tuple" [(_, TextV x), (_, TextV y)])
        | tg == bootstrapType "Tuple" = Just (x,y)
    unTextPair _ = Nothing
relRename _ = _errorWithCallStack "bad args to relRename"

relJoin :: [Value] -> Interpreter Value
relJoin [FFIValue _ffitag a, FFIValue _ffitag2 b]
    | Just (a' :: R.Relation Value) <- fromDynamic a
    , Just b' <- fromDynamic b
    = either (error . show) (pure . FFIValue ("_system","relation") . toDyn)
      $ R.relJoin a' b'
relJoin _ = _errorWithCallStack "bad args to relJoin"

relNotMatching :: [Value] -> Interpreter Value
relNotMatching [FFIValue _ffitag a, FFIValue _ffitag2 b]
    | Just (a' :: R.Relation Value) <- fromDynamic a
    , Just b' <- fromDynamic b
    = either (error . show) (pure . FFIValue ("_system","relation") . toDyn)
      $ R.relNotMatching a' b'
relNotMatching _ = error "bad args to relNotMatching"

relGroup :: [Value] -> Interpreter Value
relGroup [a, TextV b, FFIValue _ffitag c]
    | Just (c' :: R.Relation Value) <- fromDynamic c
    , Just ks <- mapM unText =<< fromBList a
    = either (error . show) (pure . FFIValue ("_system","relation") . toDyn)
      $ R.relGroup makeRelVal ks b c'
  where
    makeRelVal :: R.Relation Value -> Value
    makeRelVal rs = FFIValue ("_system","relation") . toDyn $ rs
    unText (TextV v) = Just v
    unText _ = Nothing
relGroup _ = error "bad args to relGroup"

relUngroup :: [Value] -> Interpreter Value
relUngroup [TextV a, FFIValue _ffitag b]
    | Just (b' :: R.Relation Value) <- fromDynamic b
    = either (error . show) (pure . FFIValue ("_system","relation") . toDyn)
      $ R.relUngroup unmakeRel a b'
  where
    unmakeRel :: Value -> [[(String,Value)]]
    unmakeRel (FFIValue _ffitag x) | Just (y :: R.Relation Value) <- fromDynamic x
       = either (error . show) id $ R.toList y
    unmakeRel x = error $ "ungroup unmake rel, not a relation:" ++ show x
relUngroup _ = _errorWithCallStack "bad args to relUngroup"


relSummarize :: [Value] -> Interpreter Value
relSummarize [p, TextV c, FFIValue _ffitag r]
    | Just (r' :: R.Relation Value) <- fromDynamic r
    , Just p' <- extractPair p
    = either (error . show) id <$> R.relSummarize p' c r'
  where
    extractPair (VariantV tg "tuple" [(_,z),(_,bo)])
        | tg == bootstrapType "Tuple" =
          Just (z,(\a b -> app bo [a,b]))
    extractPair _ = Nothing
relSummarize _ = _errorWithCallStack "bad args to relSummarize"


unionRecs :: [Value] -> Interpreter Value
unionRecs [VariantV tg "record" as, VariantV tg' "record" bs]
    | tg == bootstrapType "Record" && tg' == tg
    = pure $ VariantV tg "record" (as ++ bs)
    
unionRecs x = _errorWithCallStack $ "bad args to unionRecs" ++ show x

wrapBRecFn :: Value -> [(String,Value)] -> Interpreter [(String,Value)]
wrapBRecFn f r = do
    let rc = VariantV (bootstrapType "Record") "record" r
    x <- app f [rc]
    case x of
        VariantV tg "record" r' | tg == bootstrapType "Record" -> pure r'
        _ -> _errorWithCallStack $ "expected record result from predicate, got " ++ show x

wrapBPredicate :: Value -> [(String,Value)] -> Interpreter Bool
wrapBPredicate f r = do
    let rc = VariantV (bootstrapType "Record") "record" r
    x <- app f [rc]
    case x of
        BoolV v -> pure v
        _ -> _errorWithCallStack $ "expected bool result from predicate, got " ++ show x

bGetArgs :: [Value] -> Interpreter Value
bGetArgs _ = do
    as <- liftIO $ E.getArgs
    pure $ makeBList $ map TextV as

bReadProcess :: [Value] -> Interpreter Value
bReadProcess [TextV pn, l, TextV stdin]
    | Just as <- mapM getText =<< fromBList l = do
          x <- liftIO $ readProcess pn as stdin
          pure $ TextV x
  where
    getText (TextV t) = Just t
    getText _ = Nothing
bReadProcess x = error $ "bad args to read-process" ++ show x

bCallProcess :: [Value] -> Interpreter Value
bCallProcess [TextV pn, l]
    | Just as <- mapM getText =<< fromBList l = do
          liftIO $ callProcess pn as
          pure $ nothing
  where
    getText (TextV t) = Just t
    getText _ = Nothing
bCallProcess x = error $ "bad args to call-process" ++ show x

bListDirectory :: [Value] -> Interpreter Value
bListDirectory [TextV pn] = do
    is <- liftIO $ D.listDirectory pn
    pure $ makeBList $ map TextV is
bListDirectory x = error $ "bad args to list-directory" ++ show x



------------------------------------------------------------------------------

-- the interpreter itself

interp :: Expr -> Interpreter Value
--interp x | trace ("trace: "  ++ prettyExpr x) False = undefined
interp (Num n) = pure $ NumV n
interp (Text s) = pure $ TextV s
interp (Parens e) = interp e
interp (Iden "_") = _errorWithCallStack $ "wildcard/partial application not supported in this context"
interp (Iden a) = do
    mv <- lookupBinding a
    case mv of
        -- hack for self for concurrency
        Just f@(ForeignFunV "self") -> app f []
        Just (BoxV _ vr) -> liftIO $ readIORef vr
        Just v -> pure v
        Nothing -> _errorWithCallStack $ "identifier not found: " ++ a

interp (UnboxRef e f) = do
    v <- interp e
    case v of
        VariantV _ _ fs
            | Just (BoxV _ vr) <- lookup f fs -> liftIO $ readIORef vr
            | Just _  <- lookup f fs -> _errorWithCallStack $ "set ref on non ref: " ++ f ++ " in " ++ show v
        _ -> _errorWithCallStack $ "setref on non variant: " ++ show v

interp (InstExpr e ts) = do
    -- todo: add type check - check the ty param list has valid types
    -- and the type of e accepts this many type params
    mapM_ typeOfTypeSyntax ts
    interp e

-- special case for partial app
interp (App sp f es) | f == Iden "_" || any (== Iden "_") es =
    interp $ makeCurriedApp sp f es
    
interp (App sp f es) = do
    fv <- interp f
    -- special case apps
    case fv of
        ForeignFunV "run-task" ->
            -- todo: maintain call stack
            catchAsEither es
        _ -> do
            -- TODO: refactor to check the value of fv before
            -- evaluating the es?
            vs <- mapM interp es
            localPushCallStack sp $ app fv vs

-- special case binops
interp (BinOp _ x _)
    | x `elem` ["is","raises","raises-satisfies", "satisfies"]
    = _errorWithCallStack $ "'" ++ x ++ "' test predicate only allowed in check block"

interp (BinOp e0 "and" e1) = do
    x <- interp e0
    case x of
        BoolV False -> pure x
        BoolV True -> interp e1
        _ -> _errorWithCallStack $ "bad value type to 'and' operator: " ++ show x

interp (BinOp e0 "or" e1) = do
    x <- interp e0
    case x of
        BoolV True -> pure x
        BoolV False -> interp e1
        _ -> _errorWithCallStack $ "bad value type to 'or' operator: " ++ show x

interp (BinOp e0 op e1) = interp (App Nothing (Iden op) [e0,e1])

interp (UnaryMinus e) = do
    opv <- interp $ Iden "-"
    v <- interp e
    app opv [v]

{-
Type checking for lam:

lam(x :: T) -> U: [bdy] end
->

lam(x):
   assert-type-compat(block:
     assert-type-compat(x :: T)
     [bdy]
   end :: U)
end
-}

interp (Lam (FunHeader dpms bs rt) e) =
    typeLet tls $ do
    env <- askBindings
    pure $ FunV bs wrapRt env
  where
    tls = flip map dpms $ \t -> TypeDecl t [] (TName ["Any"])
    wrapRt = maybe (Block e)
             (\t -> (AssertTypeCompat (Block e) t))
             rt 

interp (CurlyLam fh e) = interp (Lam fh e)

interp (Let bs e) = do
    let newEnv [] = interpStatements e
        newEnv ((b,ex):bs') = do
            v <- interp ex
            lbs <- matchBindingOrError b v
            localBindings (extendBindings lbs) $ newEnv bs'
    newEnv bs

interp (Block ss) = localBindings id $ interpStatements ss

interp (If bs e) = do
    let f ((c,t):bs') = do
            c' <- interp c
            case c' of
                BoolV True -> interpStatements t
                BoolV False -> f bs'
                _ -> _errorWithCallStack $ "throwExpectedType 'Boolean'" ++ show c'
        f [] = case e of
                   Just x -> interpStatements x
                   Nothing -> _errorWithCallStack "NoBranchesSatisfied"
    f bs
interp (Ask bs e) = interp (If bs e)

interp (LetRec bs e) =
    let sts = doLetRec bs
    in interp $ Block (sts ++ e)

interp (DotExpr e f) = do
    v <- interp e
    interpDotExpr v f

{-
run through each branch
if the variant tag in the value matches the variant tag in the casebranch
run that branch
if there are members with names, bind these before running the branch

-}
interp (Cases e ty cs els) = do
    v <- interp e
    case ty of
        Just ty' -> do
            ty'' <- shallowizeType <$> typeOfTypeSyntax ty'
            void $ assertTypeCompat v ty''
            -- todo: looks up the patterns twice, optimise this?
            mapM_ (\(b,_,_) -> bindingMatchesType ty'' b) cs
        Nothing -> pure ()
    matchb v cs
  where
    bindingMatchesType a _ | a == bootstrapType "Any" = pure ()
    bindingMatchesType _ (ShadowBinding {}) = pure ()
    bindingMatchesType t (TypedBinding _ a) = do
        ta <- typeOfTypeSyntax a
        when (t /= ta) $ _errorWithCallStack $ "type not compatible: type of pattern not the same as the type of the case: " ++ show (t,ta)
    bindingMatchesType t (VariantBinding nm _) = checkVariantType t nm
    bindingMatchesType t (NameBinding nm) = checkVariantType t [nm]
    bindingMatchesType _ WildcardBinding = pure ()
    bindingMatchesType t (AsBinding b _) = bindingMatchesType t b
    bindingMatchesType _t (TupleBinding {}) = error $ "todo: implement tuple binding type check"
    bindingMatchesType _t (NumberLitBinding {}) = error $ "todo: implement numberlit binding type check"
    bindingMatchesType _t (StringLitBinding {}) = error $ "todo: implement numberlit binding type check"

    checkVariantType t ss = do
        let s = prefixLast "_casepattern-" ss
        caseName <- interp $ makeDotPathExpr s
        case caseName of
            FFIValue _ffitag fgt | Just (ptg::TypeInfo, _::String) <- fromDynamic fgt ->
                assertPatternTagMatchesCasesTag t ptg
            _ -> _errorWithCallStack $ "casepattern lookup returned " ++ show caseName
    matchb v [] = case els of
                      Just ee -> interpStatements ee
                      Nothing -> _errorWithCallStack $ "no cases match and no else " ++ show v ++ " " ++ show cs
    matchb v ((b,tst,ce) : cs') = do
        r <- matchBindingMaybe True b v
        case r of
            Nothing -> matchb v cs'
            Just bbs -> do
                let rbbs = localBindings (extendBindings bbs)
                tstv <- case tst of
                    Nothing -> pure $ BoolV True
                    Just tste -> rbbs $ interp tste
                case tstv of
                    BoolV True -> rbbs $ interpStatements ce
                    BoolV False -> matchb v cs'
                    _ -> _errorWithCallStack $ "non bool value in when clause: " ++ show tstv

interp (TupleSel es) = do
    vs <- mapM interp es
    pure $ VariantV (bootstrapType "Tuple") "tuple" $ zipWith (\n v -> (show n, v)) [(0::Int)..] vs

interp (RecordSel fs) = do
    vs <- mapM (\(n,e) -> (n,) <$> interp e) fs
    pure $ VariantV (bootstrapType "Record") "record" vs

interp (TableSel cs rs) = do
    -- todo: check each row has the right number of fields
    rs' <- mapM (\(RowSel es) -> mapM interp es) rs
    -- table is a list of records for now
    -- todo: fix this because a row is not the same as a record
    -- a row is more dynamic, and the field order is significant
    let mkr es = VariantV (bootstrapType "Record") "record" $ zip cs es
    pure $ makeBList $ map mkr rs'

interp (TupleGet e f) = do
    v <- interp e
    case v of
        VariantV tg "tuple" fs | tg == bootstrapType "Tuple" ->
            maybe (error $ "tuple field not found: " ++ show f ++ ", " ++ show v) pure
                 $ lookup (show f) fs
        _ -> _errorWithCallStack $ "tuple get called on non tuple value: " ++ show v

interp (Construct c es) = do
    maker <- interp (makeDotPathExpr c)
    case maker of
        VariantV tg "record" fs
            | tg == bootstrapType "Record" ->
              -- see if there's a makeN
              case lookup ("make" ++ show (length es)) fs of
                  Just f ->
                      app f =<< mapM interp es
                  _ -> case lookup "make" fs of
                           Just f -> do
                               vs <- makeBList <$> mapM interp es
                               app f [vs]
                           Nothing -> _errorWithCallStack $ "no matching construct make: " ++ show c ++ ": " ++ show maker ++ " for " ++ show (length es) ++ " args"
              -- otherwise try to call the make
        _ -> _errorWithCallStack $ "non construct record used in construct " ++ show c ++ ": " ++ show maker

interp (For fn args mty bdy) =
    interp (App Nothing fn (f : map snd args))
  where
    f = Lam (FunHeader [] (map fst args) mty) bdy

interp (AssertTypeCompat e t) = do
    v <- interp e
    ty' <- shallowizeType <$> typeOfTypeSyntax t
    assertTypeCompat v ty'

interp (TypeLet tds e) = typeLet tds $ interpStatements e

interp (Receive cs aft) = do
    -- turn the cs into a function which returns a maybe x
    -- put the branches in a cases
    -- each branch is wrapped in some()
    -- add an else which return none
    -- create a wrapper which unlifts the some/none to Maybe
    -- the branches are wrapped in a lambda, so that the bodies
    -- are run after the hsconcurrency receive is finished,
    -- so that nested receives work correctly
    let rv = "receiveval"
        some = internalsRef "some"
        none = internalsRef "none"
        cs' = flip map cs $ \(cb, tst, e) -> (cb, tst, [StmtExpr $ App Nothing some [lam0 $ Block e]])
        prdf = Lam (FunHeader [] [NameBinding rv] Nothing)
               [StmtExpr $ Cases (Iden rv) Nothing cs' (Just [StmtExpr none])]
    prdfv <- interp prdf
    let prd :: Dynamic -> Interpreter (Maybe Dynamic)
        prd v = do
                let v' = case fromDynamic v of
                        Just w -> w
                        _ -> case fromDynamic v of
                            Just (MonitorDown tg et mv r) -> --trace(show (tg,et,mv,r)) $
                                makeMonitorDown tg et mv r
                            _ -> error $ "receive pred got something that wasn't a value: " ++ show v
                r <- app prdfv [v']
                case r of
                    VariantV tg "none" [] | tg == internalsType "Option" -> pure Nothing
                    VariantV tg "some" [(_,x)] | tg == internalsType "Option" -> pure $ Just $ toDyn x
                    _ -> _errorWithCallStack $ "expected Option type in prdfv ret, got " ++ show r
    th <- askThreadHandle
    let getVal :: Dynamic -> Interpreter Value
        getVal v | Just v' <- fromDynamic v = app v' []
                 | otherwise = _errorWithCallStack $ "expected <<Value>> from receive, got " ++ show v
    case aft of
        Nothing -> getVal =<< zreceive th prd
        Just (a, e) -> do
            tme <- interp a
            case tme of
                VariantV tg "infinity" []
                    | tg == internalsType "Infinity"
                      -> getVal =<< zreceive th prd                   
                NumV tme' -> do
                    v <- zreceiveTimeout th (floor (tme' * 1000 * 1000)) prd
                    case v of
                        Just v' -> getVal v'
                        Nothing -> interpStatements e
                _ -> _errorWithCallStack $ "after timeout value not number: " ++ show tme

  where
    lam0 e = Lam (FunHeader [] [] Nothing) [StmtExpr e]
    makeMonitorDown tg et v r = --trace (show (fromDynamic v :: Maybe String)) $
        let tg' = case fromDynamic tg of
                      Just vx -> vx
                      _ -> case fromDynamic tg of
                          Just () -> nothing
                          _ -> FFIValue ("_system","unknown") tg
            et' = case et of
                      ExitValue -> VariantV (internalsType "ExitType") "exit-value" []
                      ExitException -> VariantV (internalsType "ExitType") "exit-exception" []
            v' = case fromDynamic v of
                     Just vx -> vx
                     _ -> case fromDynamic v of
                              Just s -> TextV s
                              _ -> FFIValue ("_system","unknown") v
            r' = convertHsMonitorRef r
        in VariantV (internalsType "MonitorDown") "monitor-down"
               [("tag", tg')
               ,("exit-type", et')
               ,("v", v')
               ,("mref", r')]
interp (Template sp) =
    pure $ TemplateV sp

interp (MethodExpr m) = makeMethodValue m

interpDotExpr :: Value -> String -> Interpreter Value
interpDotExpr v f = do
    x <- interpDotExprE v f
    case x of
        Right x' -> pure x'
        Left e -> error e

interpDotExprE :: Value -> String -> Interpreter (Either String Value)
interpDotExprE v f =
    case v of
        VariantV _ _ fs
            | Just fv <- lookup f fs ->
              -- not quite sure about this?
              -- it's needed for referencing vars in a module
              -- (including fun which is desugared to a var)
              -- one improvement would be to representing a module value
              -- as a distinct variant type, then only doing this behaviour
              -- on that variant type at least
              case fv of
                  -- the hack for modules bit
                  BoxV _ vr -> Right <$> (liftIO $ readIORef vr)
                  -- regular support for methods
                  MethodV m -> Right <$> app m [v]
                  _ -> pure $ Right fv
            | otherwise -> pure $ Left $ "field not found in dotexpr: " ++ f
              ++"\nlooking in value:\n" ++ show v
              ++ "\nit's fields are: " ++ show fs
        FFIValue (_,tg) _ -> do
            (ti :: Maybe FFITypeInfo) <- askFFTInfo tg
            let vv :: Maybe (Interpreter Value)
                vv = do
                    x <- ffiMember =<< ti
                    pure x -- $ x f
            case vv of
                Nothing -> pure $ Left $ "dot called on ffi value which doesn't support dot " ++ show tg ++ ":" ++ show  v
                Just x -> do
                    x' <- x
                    case x' of
                        MethodV m -> Right <$> app m [TextV f, v]
                        nm -> pure $ Right nm
        _ -> pure $ Left $ "dot called on non variant: " ++ show v


-- todo: deal with ts and mty
makeMethodValue :: Method -> Interpreter Value
makeMethodValue (Method (FunHeader _ts (a:as) _mty) bdy) =
    MethodV <$> interp (Lam (FunHeader [] [a] Nothing)
                        [StmtExpr $ Lam (FunHeader [] as Nothing) bdy])
makeMethodValue m@(Method (FunHeader _ts [] _mty) _bdy) =
    error $ "method declaration should accept at least one argument: " ++ show m

makeBList :: [Value] -> Value
makeBList [] = VariantV (internalsType "List") "empty" []
makeBList (x:xs) = VariantV (internalsType "List") "link" [("first", x),("rest", makeBList xs)]

makeBTuple :: [Value] -> Value
makeBTuple vs =
    VariantV (bootstrapType "Tuple") "tuple" $ zipWith (\n v -> (show n, v)) [(0::Int)..] vs


sbindingName :: SimpleBinding -> String
sbindingName (SimpleBinding _ nm _) = nm

makeCurriedApp :: SourcePosition -> Expr -> [Expr] -> Expr
makeCurriedApp sp f es =
    -- create some names for the _ args
    let (nms',newEs) = unzip $ makeNewEs (1::Int) es
        nms = catMaybes nms'
        (newf,nms2) = case f of
                        Iden "_" -> (Iden "_p0", ("_p0":nms))
                        _ -> (f, nms)
        bs = flip map nms2 $ \n -> NameBinding n
    in Lam (FunHeader [] bs Nothing) [StmtExpr $ App sp newf newEs]
  where
    makeNewEs _ [] = []
    makeNewEs n (Iden "_":es') =
        let nm = "_p" ++ show n
        in (Just nm, Iden nm) : makeNewEs (n + 1) es'
    makeNewEs n (x:es') = (Nothing, x) : makeNewEs n es'

app :: Value -> [Value] -> Interpreter Value
app _ vs | ts@(_:_) <- catMaybes (map extractTemplate vs) =
    evalTemplate ts
  where
    extractTemplate (TemplateV sp) = Just sp
    extractTemplate _ = Nothing
app fv vs =
    case fv of
        FunV ps bdy env -> do
            as <- safeZip ps vs
            let fbas acc [] = do
                    let got = concat $ reverse acc
                    localBindings (extendBindings got) $ interp bdy
                fbas acc ((b,v):as') = do
                    lbs <- matchBindingOrError b v
                    fbas (lbs:acc) as'
            localBindings (const env) $ fbas [] as
        ForeignFunV nm -> do
            ffs <- askFF
            case lookup nm ffs of
                Just f -> f vs
                Nothing -> error $ "internal error, foreign function not found: " ++ nm
        FFIValue {} -> do
            fn <- interpDotExpr fv "_app"
            app fn [makeBList vs]
        TemplateV sp -> evalTemplate [sp]
        _ -> error $ "app called on non function value: " ++ show fv
  where
    safeZip ps xs | length ps == length xs  = pure $ zip ps xs
                  | otherwise = error $ "wrong number of args: " ++ show ps ++ ", " ++ show xs

evalTemplate :: [SourcePosition] -> Interpreter a
evalTemplate (t:_) =
    -- todo: the source position should be part of the error
    -- not pushed to the call stack
    localPushCallStack t $ 
    throwValueWithStacktrace $ TextV $ "template-not-finished: " ++ show t
evalTemplate [] = error $ "eval template on 0 templates"

typeLet :: [TypeDecl] -> Interpreter a -> Interpreter a
typeLet tds f = do
    let newEnv [] = f
        newEnv (TypeDecl _ (_:_) _ : _) = _errorWithCallStack $ "todo: parameterized type-let"
        newEnv (TypeDecl n _ t:tds') = do
            ty <- typeOfTypeSyntax t
            localBindings (extendBindings [("_typeinfo-" ++ n,FFIValue ("_system","typeinfo") $ toDyn ty)])
                 $ newEnv tds'
    newEnv tds

-- pyret spells this run-task
catchAsEither :: [Expr] -> Interpreter Value
catchAsEither [e] = do
    (v0 :: Either (Value,a) Value) <- catchAll (interp e)
    let (f,v1) = case v0 of
            Right v -> (internalsRef "right", v)
            Left (er,_) -> (internalsRef "left", er)
    fv <- interp f
    app fv [v1]
catchAsEither _ = _errorWithCallStack $ "wrong args to run-task"

catchAll :: Interpreter a -> Interpreter (Either (Value, Maybe CallStack) a)
catchAll f =
    ca $ catchit f
  where
    catchit f' = catch (Right <$> f') iToV
    iToV e = pure $ Left (interpreterExceptionToValue e
                         ,Just $ interpreterExceptionCallStack e)
    -- catch any haskell exception, for dealing with error and undefined
    -- not sure about this, but definitely wanted for testing
    ca f' = catchAny f' (pure . Left . (,Nothing) . TextV . show)

showExceptionCallStackPair :: (Value, Maybe CallStack) -> Interpreter String
showExceptionCallStackPair (v,cs) = do
    s1 <- torepr' v
    s2 <- case cs of
        Nothing -> pure ""
        Just cs' -> formatCallStack cs'
    pure $ s1 ++ s2

assertTypeCompat :: Value -> TypeInfo -> Interpreter Value
assertTypeCompat v ti = 
    if v `typeIsCompatibleWith` ti
        then pure v
        else do
            r <- torepr' v
            throwValueWithStacktrace
                $ TextV $ "type not compatible (marker) " ++ r ++ " :: " ++ show v ++ " // " ++ show ti

throwValueWithStacktrace :: Value -> Interpreter a
throwValueWithStacktrace v = do
    cs <- readCallStack
    throwM $ ValueException cs v

assertTypeAnnCompat :: Value -> Ann -> Interpreter Value
assertTypeAnnCompat v t = do
    ti <- shallowizeType <$> typeOfTypeSyntax t
    assertTypeCompat v ti

-- takes an expression and wraps it with a typelet
-- mapping strings to Any
-- this is a way to implement syntax<NAME LIST> ...
-- checks for dynamic mode
-- the syntax is used in a few places
typeLetWrapper :: [String] -> Expr -> Expr
typeLetWrapper [] x = x
typeLetWrapper ps e = TypeLet (map (\a -> TypeDecl a [] (TName ["Any"])) ps) [StmtExpr e]

---------------------------------------


interpStatements :: [Stmt] -> Interpreter Value

interpStatements = interpStatements' . desugarContracts


{-

The full Pyret rules about where a contract can go in relation to the
binding it relates to:

a contract can be for one of the following bindings (all statements):
letdecl
vardecl
recdecl
fundecl

A contract must appear before the binding it refers to

There can be other statements in between the contract and it's binding:

if it's binding is a let or var:
you can have other contracts, check/example blocks and other let/var decls
in between the contract and its let/var decl. any other statement is an error

same replacing let/var with rec/fun

not implementing these full rules yet

currently a contract must immediately precede its let/var/rec/fun decl

-}

desugarContracts :: [Stmt] -> [Stmt]
desugarContracts
    (Contract cnm ta : x : sts)
    | Just (ctor, b@(NameBinding bnm), e) <- letorrec x
    , cnm == bnm
    = ctor (TypedBinding b ta) e : desugarContracts sts
  where
    letorrec (LetDecl b e) = Just (LetDecl,b,e)
    letorrec (RecDecl b e) = Just (RecDecl,b,e)
    letorrec _ = Nothing

desugarContracts
    (Contract cnm ta : VarDecl (SimpleBinding sh bnm lta) e : sts)
    | cnm == bnm
    = case lta of
          Just _ -> error $ "contract for binding that already has type annotation: " ++ cnm
          Nothing -> VarDecl (SimpleBinding sh bnm (Just ta)) e : desugarContracts sts

desugarContracts
    (Contract cnm ta : s@(FunDecl (SimpleBinding _ fnm _) _ _ _ _) : sts)
        | cnm == fnm
        , ta `elem` [TName ["Function"] -- todo: need to look these up in the env
                    ,TName ["Any"]]
        = s : desugarContracts sts

desugarContracts
    (cd@(Contract cnm (TArrow tas trt)) :
     fd@(FunDecl nb@(SimpleBinding _ fnm _) (FunHeader ps as rt) ds e chk) :
     sts)
    | cnm == fnm
    =
    -- check for annotations in the funheader
    if (not $ null $ catMaybes $ map getTa as) || isJust rt
    then error $ "contract for fun that already has type annotations: " ++ cnm
    else FunDecl nb (FunHeader ps bindArgs (Just trt)) ds e chk : desugarContracts sts
  where
    bindArgs | length as /= length tas = error $ "type not compatible: different number of args in contract and fundecl" ++ show (cd,fd)
             | otherwise = zipWith (\ta b -> TypedBinding b ta) tas as
    getTa (TypedBinding _ ta) = Just $ ta
    getTa _ = Nothing

desugarContracts
    (c@(Contract {}) : s@(FunDecl {}) : _sts) =
    error $ "type not compatible: non function contract for function: " ++ show (c,s)

desugarContracts (s : ss) = s : desugarContracts ss
desugarContracts [] = []


interpStatements' :: [Stmt] -> Interpreter Value

-- gather adjacent fun and rec can be mutually recursive
-- to desugar all together
interpStatements' ss | (recbs@(_:_),chks, ss') <- getRecs [] [] ss = do
    interpStatements' (doLetRec recbs ++ chks ++ ss')
  where
    getRecs accdecls accchks (RecDecl nm bdy : ss') = getRecs ((nm,bdy):accdecls) accchks ss'
    getRecs accdecls accchks (FunDecl (SimpleBinding _ nm mty) fh _ds bdy whr : ss') =
        let accchks' = maybe accchks (\w -> Check (Just nm) w : accchks) whr
            b = case mty of
                    Nothing -> NameBinding nm
                    Just ty -> TypedBinding (NameBinding nm) ty
        in getRecs ((b, Lam fh bdy):accdecls) accchks' ss'
    getRecs accdecls accchks ss' = (reverse accdecls, reverse accchks, ss')

-- collect a contract with a following letdecl

-- the interpreter for all other statements don't need access to the
-- statement list so they delegate to interpStatement

-- return value if it's the last statement
interpStatements' [s] = interpStatement s
interpStatements' (s:ss) = interpStatement s >> interpStatements' ss
interpStatements' [] = pure $ VariantV (bootstrapType "Nothing") "nothing" []


interpStatement :: Stmt -> Interpreter Value

-- interpStatement x | trace ("interp: " ++ show x) False = undefined

interpStatement (RecDecl {}) = _errorWithCallStack $ "internal error, rec decl not captured by letrec desugaring"
interpStatement (FunDecl {}) = _errorWithCallStack $ "internal error, fun decl not captured by letrec desugaring"

{-

TODO: notes on how tests execute
the quick version is that
a is b
becomes something like
run-is-test(lam(): a, lam(): b)
and run-is-test will catch any exceptions from evaluating a or b and
  report this, as well as reporting if they are equal if they eval without
  throwing exceptions


-}

interpStatement s@(StmtExpr (BinOp e0 "is" e1)) =
    testIs (prettyStmt s) e0 e1

interpStatement s@(StmtExpr (BinOp e0 "is-not" e1)) =
    testIsNot (prettyStmt s) e0 e1

interpStatement s@(StmtExpr (BinOp e0 "raises" e1)) =
    testRaises (prettyStmt s) e0 e1

interpStatement s@(StmtExpr (BinOp e0 "satisfies" e1)) =
    testSatisfies (prettyStmt s) e0 e1

interpStatement s@(StmtExpr (BinOp e0 "raises-satisfies" f)) =
    testRaisesSatisfies (prettyStmt s) e0 f

interpStatement (StmtExpr e) = do
    v <- interp e
    case v of
        TemplateV sp -> evalTemplate [sp]
        _ -> pure v
interpStatement (When t b) = do
    tv <- interp t
    case tv of
        BoolV True -> interpStatements b
        BoolV False -> pure nothing
        _ -> _errorWithCallStack $ "expected when test to have boolean type, but is " ++ show tv

interpStatement (Check cbnm ss) = do
    runTestsEnabled <- interp $ internalsRef "auto-run-tests"
    case runTestsEnabled of
        BoolV True -> enterCheckBlock cbnm $ do
            x <- catchAll $ interpStatements ss
            case x of
                Right v -> pure v
                Left e -> do
                    msg <- showExceptionCallStackPair e
                    addTestResult (TestFail "check block threw an exception, some tests may not have executed" msg)
                    pure nothing
        BoolV False -> pure nothing
        x -> _errorWithCallStack $ "auto-run-tests not set right (should be boolv): " ++ show x
    
interpStatement (LetDecl b e) = do
    v <- interp e
    bs <- matchBindingOrError b v
    mapM_ (uncurry letValue) bs
    pure nothing

interpStatement (VarDecl b e) = do
    v <- interp e
    -- todo: it should only preserve the type of the var itself
    -- if the user used a contract, and not if they used a type
    -- annotation on the value the var is initialized with?
    (v',ty) <- case b of
              SimpleBinding _ _ (Just ta) -> do
                  (,) <$> assertTypeAnnCompat v ta
                  <*> typeOfTypeSyntax ta
              _ -> pure (v, bootstrapType "Any")
    vr <- liftIO $ newIORef v'
    letValue (sbindingName b) (BoxV ty vr)
    pure nothing

interpStatement (SetVar nm e) = do
    mv <- lookupBinding nm
    let (ty,vr) = case mv of
                 Just (BoxV vty b) -> (vty,b)
                 Just x -> error $ "attempt to assign to something which isn't a var: " ++ show x
                 Nothing -> error $ "identifier not found: " ++ nm
    v <- interp e
    void $ assertTypeCompat v ty
    liftIO $ writeIORef vr v
    pure nothing

interpStatement (SetRef e fs) = do
    vs <- mapM (secondM interp) fs
    v <- interp e
    case v of
        VariantV _ _ vfs -> mapM_ (setfield vfs) vs
        _ -> error $ "set ref on non variant"
    pure nothing
  where
    setfield :: [(String,Value)] -> (String,Value) -> Interpreter ()
    setfield vfs (nm, v) = case lookup nm vfs of
        Just b -> setBox b v
        Nothing -> error $ "setref field not found: " ++ nm
    setBox bx v = case bx of
        BoxV _ b -> liftIO $ writeIORef b v
        _ -> _errorWithCallStack $ "attempt to assign to something which isn't a ref: " ++ show bx

{-

data decl:

an is-x function for each variant
an is-dat function for the data type
a make function for each variant, with the name of the variant, e.g.
pt(...)
a support binding for cases, for each variant x:
_casepattern-x = ...
support for the dynamic type tags:
_typeinfo-Pt = simpletypeinfo ["_system","modules",currentModuleName, "Pt"]
list the variant names for import support:
_datainfo-Pt = ["pt"]


-}

interpStatement (DataDecl dnm dpms vs shr whr) = do
    let makeIs (VariantDecl vnm _ _meth) = 
            letDecl ("is-" ++ vnm)
            $ lam ["x"]
            [StmtExpr $ App appSourcePos (bootstrapRef "is-variant") [Iden typeInfoName, Text vnm, Iden "x"]]
        callIs (VariantDecl vnm _ _) = App appSourcePos (Iden $ "is-" ++ vnm) [Iden "x"]
        -- todo: use the type tag instead
        makeIsDat =
            letDecl ("is-" ++ dnm)
            $ lam ["x"]
           [StmtExpr $ foldl1 orE $ map callIs vs]
        chk = maybe [] (\w -> [Check (Just dnm) w]) whr
    moduleName <- readModuleName
    letValue typeInfoName $ FFIValue ("_system","typeinfo") $ toDyn
        $ SimpleTypeInfo ["_system", "modules", moduleName, dnm]
    letValue dataInfoName $ FFIValue ("_system","datainfo") $ toDyn
        $ map varName vs
    -- todo: use either a burdock value or a proper ffi value for the casepattern
    -- instead of a haskell tuple
    forM_ vs $ \(VariantDecl vnm _ _) -> letValue ("_casepattern-" ++ vnm)
        $ FFIValue ("_system","typeinfo") $ toDyn (SimpleTypeInfo ["_system", "modules", moduleName, dnm], vnm)
    makeVs <- mapM makeV vs
    let desugared = (makeVs ++ map makeIs vs ++ [makeIsDat] ++ chk)
    interpStatements desugared
  where
    typeInfoName = "_typeinfo-" ++ dnm
    dataInfoName = "_datainfo-" ++ dnm
    me (menm,m) = [Iden "false", Text menm, MethodExpr m]
    varName (VariantDecl vnm _ _) = vnm
    makeV (VariantDecl vnm fs ms) = do
        -- make an _equals method which compares the non method fields
        -- if the variant has methods
        -- todo: don't override if the user supplies an explicit _equals method
        -- todo: update this if start supporting equal-now, to add the equal-rec field
        let cnms = flip map fs $ \(_,SimpleBinding _ nm _) -> nm
            eqFlds = Construct ["_system", "modules", "_internals", "list"] $ map Text cnms
            equalsWithoutMethods =
                if null (ms ++ shr)
                then []
                else [("_equals"
                      ,Method (FunHeader [] [NameBinding "self", NameBinding "x"] Nothing)
                          [StmtExpr $ App appSourcePos (internalsRef "equal-by-field")
                              [eqFlds, Iden "self", Iden "x"]])]
        let appMakeV = App appSourcePos (bootstrapRef "make-variant")
                       [Iden typeInfoName
                       ,Text vnm
                       ,Construct ["list"] $ concatMap me (ms ++ shr ++ equalsWithoutMethods)
                       ,Construct ["list"] $ concatMap mvf fs]
        pure $ recDecl vnm
           $ typeLetWrapper dpms
           $ (if null fs then id else \e -> Lam (fh $ map (simpleBindingToBinding . snd) fs) [StmtExpr e])
           $ appMakeV
    mvf (r, b) = let nm = sbindingName b
                 in ([case r of
                          Ref -> Iden "true"
                          Con -> Iden "false"
                     ,Text nm
                     ,Iden nm])
    letDecl nm v = LetDecl (mnm nm) v
    recDecl nm v = RecDecl (mnm nm) v
    lam as e = Lam (fh $ map mnm as) e
    fh as = FunHeader [] as Nothing
    orE a b = BinOp a "or" b
    mnm x = NameBinding x

interpStatement (FFITypeStmt nm ty) = do
    -- check the name is in registry
    -- create the _typeinfo value
    letValue ("_typeinfo-" ++ nm) $ FFIValue ("_system","typeinfo") $ toDyn $ SimpleTypeInfo [ty]
    -- create the is-X type tester function
    interpStatement
        (LetDecl (NameBinding ("is-" ++ nm))
         (Lam (FunHeader [] [NameBinding "x"] Nothing)
          [StmtExpr $ App appSourcePos (internalsRef "is-specific-ffi-type") [Text ty, Iden "x"]]))
    --pure nothing is-specific-ffi-type

interpStatement (TypeStmt {}) = _errorWithCallStack $ "TODO: interp typestmt"
interpStatement c@(Contract {}) = _errorWithCallStack $ "contract without corresponding statement: " ++ show c

---------------------------------------

-- prelude statements

{-
import file("file.bur") as X
import string-dict as X

->
resolve the path to the module, if it isn't loaded, load it
alias it locally

TODO:
1. import from packages like this?
import package-name.module-name ...
2. import modules in an fs hierarchy like haskell does
import package-name.dir-name.module-name ...
doesn't apply to importing files
3. import modules as if they were in a package my-p
import my-p.x.y
 -> import file("x/y.bur") ...
4. when importing using the raw name instead of file, there's a default qualifier:

import xx
-> import xx as xx
import xx.yy
-> import xx.yy as yy ?
it's slightly annoying otherwise - maybe not annoying enough?
  but it is the kind of annoying boilerplate that beginners and newcomers
  will see and can often judge over harshly on

-}

interpStatement (Import is al) =  do
    (modName,fn) <- resolveImportPath is
    ensureModuleLoaded modName fn
    as <- aliasModule modName [ProvideAll]
    letIncludedValue al $ VariantV (bootstrapType "Record") "record" as
    pure nothing
{-
include from X: a end
include from X: * end
include from X: a,b end
include from X: a as b end

makes names in the already visible module alias X available without a
qualifier

-}
interpStatement (IncludeFrom nm pis) = do
    v <- interp (Iden nm)
    case v of
        VariantV tg "record" fs | tg == bootstrapType "Record" -> do
            as <- aliasSomething fs [] pis
            mapM_ (uncurry letIncludedValue) as
        _ -> _errorWithCallStack $ "trying to alias from something that isn't a record: " ++ show v
    pure nothing

{-
include file("file.bur")
include string-dict
include m == include from m: * end
-}

interpStatement (Include is) = do
    (modName,fn) <- resolveImportPath is
    ensureModuleLoaded modName fn
    ls <- aliasModule modName [ProvideAll]
    mapM_ (uncurry letIncludedValue) ls
    pure nothing

{-

import from file(x):
  y,
  z as t
end

-> short hand to import the import source with a temp alias,
then includefrom the items written

-}

interpStatement (ImportFrom is pis) =
    -- not quite right, it introduces the is name which
    -- will stick around
    let al = show is
    in interpStatements [Import is al, IncludeFrom al pis]


{-
provide:

logs the provide pis to an internal record in the current script env
the load module knows how to turn these into the provided module value

provide: * end
provide: a end
provide: a,b end
provide: a as b end

-}

interpStatement (Provide pis) = do
    -- todo: turn the provides into burdock values and save as a regular burdock list?
    modifyScriptProvides (++pis)
    pure nothing

interpStatement (UsePackage dr) = do
    st <- ask
    let packageName = takeBaseName dr
    liftIO $ atomically $ modifyTVar (tlHandleState st)
        (\r -> r {hsLoadedPackages = (packageName,dr) : hsLoadedPackages r})
    pure nothing

-- hsLoadedPackages :: [(String,FilePath)]

internalsRef :: String -> Expr
internalsRef nm = makeDotPathExpr ["_system", "modules", "_internals", nm]

bootstrapRef :: String -> Expr
bootstrapRef nm = makeDotPathExpr ["_system", "modules", "_bootstrap", nm]

makeDotPathExpr :: [String] -> Expr
makeDotPathExpr [] = error "empty makedotpathexpr"
makeDotPathExpr [n] = Iden n
makeDotPathExpr (n':nms') = f (Iden n') nms'
  where
    f e (n:nms) = f (DotExpr e n) nms
    f e [] = e

------------------------------------------------------------------------------

-- test predicates
testIs :: String -> Expr -> Expr -> Interpreter Value
testIs msg e0 e1 = do
    (v0,v1,atr) <- testPredSupport e0 e1
    case (v0,v1) of
        (Right v0', Right v1') -> do
            t <- hEqualAlways v0' v1'
            if t
            then atr $ TestPass msg
            else do
                p0 <- torepr' v0'
                p1 <- torepr' v1'
                atr $ TestFail msg (p0 ++ "\n!=\n" ++ p1)
        (Left (_,er0), Right {}) -> do
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0)
        (Right {}, Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1)
        (Left (_,er0), Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0
                                ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1)
    pure nothing

testIsNot :: String -> Expr -> Expr -> Interpreter Value
testIsNot msg e0 e1 = do
    (v0,v1,atr) <- testPredSupport e0 e1
    case (v0,v1) of
        (Right v0', Right v1') -> do
            t <- hEqualAlways v0' v1'
            if not t
            then atr $ TestPass msg
            else do
                p0 <- torepr' v0'
                p1 <- torepr' v1'
                atr $ TestFail msg (p0 ++ "\n==\n" ++ p1)
        (Left (_,er0), Right {}) -> do
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0)
        (Right {}, Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1)
        (Left (_,er0), Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0
                                ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1)
    pure nothing

testRaises :: String -> Expr -> Expr -> Interpreter Value
testRaises msg e0 e1 = do
    (v0,v1,atr) <- testPredSupport e0 e1
    case (v0,v1) of
        (Right _, Right _) ->
            atr $ TestFail msg (prettyExpr e0 ++ " didn't raise")
        (_, Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1)
        (Left (er0,er0'), Right (TextV v)) -> do
            val <- torepr' er0
            if v `isInfixOf` val
                then atr $ TestPass msg
                else atr $ TestFail msg ("failed: " ++ val ++ ", expected " ++ "\"" ++ v ++ "\"" ++ "\n" ++ er0')
        (Left _, Right v) -> do
            atr $ TestFail msg (prettyExpr e1 ++ " failed, expected String, got: " ++ show v)
    pure nothing

testRaisesSatisfies :: String -> Expr -> Expr -> Interpreter Value
testRaisesSatisfies msg e0 f = do
    (v0,fv,atr) <- testPredSupport e0 f
    case (v0,fv) of
        (Right _, Right _) ->
            atr $ TestFail msg (prettyExpr e0 ++ " didn't raise")
        (_, Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr f ++ " failed: " ++ er1)
        (Left (er0,er0'), Right fvv) -> do
            r <- app fvv [er0]
            case r of
                BoolV True -> atr $ TestPass msg
                BoolV False -> atr $ TestFail msg ("failed: " ++ show er0 ++ ", didn't satisfy predicate " ++ show f ++ "\n" ++ show er0')
                _ -> atr $ TestFail msg ("failed: predicted didn't return a bool: " ++ show f ++ " - " ++ show r)
    pure nothing

testSatisfies :: String -> Expr -> Expr -> Interpreter Value
testSatisfies msg e0 f = do
    (v0,fv,atr) <- testPredSupport e0 f
    case (v0,fv) of
        (Right v', Right f') -> do
            r <- app f' [v']
            case r of
                BoolV True -> atr $ TestPass msg
                BoolV False -> atr $ TestFail msg ("failed: " ++ show v' ++ ", didn't satisfy predicate " ++ show f)
                _ -> atr $ TestFail msg ("failed: predicted didn't return a bool: " ++ show f ++ " - " ++ show r)
        (Left (_,er0), Right {}) -> do
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0)
        (Right {}, Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr f ++ " failed: " ++ er1)
        (Left (_,er0), Left (_,er1)) -> do
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0
                                ++ "\n" ++ prettyExpr f ++ " failed: " ++ er1)
    pure nothing

testPredSupport :: Expr
                -> Expr
                -> Interpreter (Either (Value,String) Value
                               ,Either (Value,String) Value
                               ,TestResult -> Interpreter ())
testPredSupport e0 e1 = do
    let w :: Expr -> Interpreter (Either (Value,String) Value)
        w x = do
            b0 <- catchAll (interp x)
            case b0 of
                Left (v,cs) -> do
                    vs <- torepr' v
                    vext <- case cs of
                            Nothing -> pure ""
                            Just cs' -> formatCallStack cs'
                    pure $ Left (v,vs ++ vext)
                    -- undefined -- Left . TextV <$> showExceptionCallStackPair er
                Right v -> pure $ Right $ v
    v0 <- w e0
    v1 <- w e1
    pure (v0, v1, addTestResult)

------------------------------------------------------------------------------

-- module loading support


getBuiltInModulePath :: String -> IO FilePath
getBuiltInModulePath nm =
    -- later will need to use the cabal Paths_ thing and stuff
    pure ("built-ins" </> nm <.> "bur")

-- todo: add importing relative to the current file path
-- check import sources for ambiguity
resolveImportPath :: ImportSource -> Interpreter (String, FilePath)
resolveImportPath is = do
    case is of
        ImportSpecial "file" [fn] ->
            pure (dropExtension $ takeFileName fn, fn)
        ImportSpecial {} -> _errorWithCallStack "unsupported import"
        -- todo: set this path in a sensible and flexible way
        ImportName nm -> do
            let (p,nm') = break (=='.') nm
            if nm' == ""
                then (nm,) <$> liftIO (getBuiltInModulePath nm)
                else do
                    ps <- getPackages
                    case lookup p ps of
                        Nothing -> (nm,) <$> liftIO (getBuiltInModulePath nm)
                        Just fn -> pure (nm,fn </> "bur" </> drop 1 nm' <.> "bur")
  where
    getPackages = do
        st <- ask
        hsLoadedPackages <$> (liftIO $ atomically $ readTVar (tlHandleState st))

ensureModuleLoaded :: String -> FilePath -> Interpreter ()
ensureModuleLoaded moduleName moduleFile = do
    modRec <- interp $ makeDotPathExpr ["_system", "modules"]
    case modRec of
        VariantV tg "record" fs
            | tg == bootstrapType "Record"
            , moduleName `notElem` map fst fs -> do
                --liftIO $ putStrLn $ "loading module: " ++ moduleFile
                loadAndRunModule True moduleName moduleFile
            | tg == bootstrapType "Record"
            , otherwise -> pure ()
        _ -> _errorWithCallStack $ "_system.modules is not a record??"

-- bootstrap is a special built in module which carefully creates
-- the minimal in language infrastructure for the language itself to work
-- a big reason is then to be able to use any part of the language
-- in the _internals module which is used to bootstrap the built in modules
-- that uses use, including globals

initBootstrapModule :: Interpreter ()
initBootstrapModule = runModule "BOOTSTRAP" "_bootstrap" $ do
    mapM_ (uncurry letValue)
        [("true", BoolV True)
        ,("false", BoolV False)
        ,("nothing", VariantV (bootstrapType "Nothing") "nothing" [])
        ,("is-nothing", ForeignFunV "is-nothing")
        ,("is-Nothing", ForeignFunV "is-Nothing")
        ,("_casepattern-nothing", FFIValue ("_system","casepattern") $ toDyn (bootstrapType "Nothing", "nothing"))
        ,("_typeinfo-Nothing", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Nothing"])
        ,("_datainfo-Nothing", FFIValue ("_system","datainfo") $ toDyn ["nothing"])
        -- todo: complete the boolean (and nothing?) types
        ,("ffi-function", ForeignFunV "ffi-function")
        ,("make-variant", ForeignFunV "make-variant")
        ,("is-variant", ForeignFunV "is-variant")
        ,("_typeinfo-Any", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Any"])
        ,("_typeinfo-Number", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Number"])
        ,("_typeinfo-String", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","String"])
        ,("_typeinfo-Boolean", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Boolean"])
        ,("_typeinfo-Tuple", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Tuple"])
        ,("_typeinfo-Record", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Record"])
        ,("_typeinfo-Function", FFIValue ("_system","typeinfo") $ toDyn
             $ SimpleTypeInfo ["_system","modules","_bootstrap","Function"])
        ]

runModule :: String -> String -> Interpreter () -> Interpreter ()        
runModule filename moduleName f =
    localBindings (const []) $ do
        f
        localModuleEnv <- askLocallyCreatedBindings
        allModuleEnv <- askIncludedBindings
        -- get the pis and make a record from the env using them
        pis <- (\case [] -> [ProvideAll]
                      x -> x) <$> askScriptProvides
        moduleRecord <- VariantV (bootstrapType "Record") "record"
                        <$> aliasSomething localModuleEnv allModuleEnv pis
        modifyAddModule filename moduleName moduleRecord

-- todo: replace the includeGlobals flag with use context
loadAndRunModule :: Bool -> String -> FilePath -> Interpreter ()
loadAndRunModule includeGlobals moduleName fn = spawnExtWaitI $ \_ -> do
    src <- liftIO $ readFile fn
    -- auto include globals, revisit when use context added
    let incG = if includeGlobals && moduleName /= "globals"
               then (Include (ImportName "globals") :)
               else id
    Script ast' <- either error id <$> useSource (Just fn) moduleName src parseScript
    let ast = incG ast'
    -- todo: track generated unique names better?
    runModule fn moduleName $ void $ interpStatements ast

-- todo: refactor all these spawn thread and runreadert wrapper variations
-- this resets the interpreter state for a fresh thread
-- there are two kinds of spawn: api level, and regular subthread spawn
-- which modify the new state differently
spawnExtWaitI :: Typeable a => (ThreadHandle -> Interpreter a) -> Interpreter a
spawnExtWaitI f = do
    st <- ask
    h <- hsConcurrencyHandle <$> liftIO (atomically $ readTVar (tlHandleState st))
    liftIO $ spawnWaitCast h $ \th -> do
        ms <- newIORef =<< readIORef (tlModuleState st)
        p <- newIORef []
        b <- newIORef []
        rb <- newIORef []
        bo <- newIORef []
        cbn <- newIORef 0
        let st1 = st
                {tlModuleState = ms
                ,tlThreadHandle = th
                ,tlProvides = p
                ,tlCallStack = []
                ,tlLocallyCreatedBindings = b
                ,tlReplCreatedBindings = rb
                ,tlIncludedBindings = bo
                ,tlCurrentCheckblock = Nothing
                ,tlNextAnonCheckblockNum = cbn
                ,_tlIsModuleRootThread = True
                }
        flip runReaderT st1 (f th)

-- TODO: not really happy with how messy these functions are

-- create the aliases which are letdecls
-- to point the names in the provide items to the internal
-- loaded module/ module member names
aliasModule :: String -> [ProvideItem] -> Interpreter [(String,Value)]
aliasModule modName pis = do
    modEnv <- ex <$> lkp
    aliasSomething modEnv [] pis
    -- pure $ concat $ map (apis modEnv) pis
  where

    ex (VariantV tg "record" r) | tg == bootstrapType "Record" = r
    ex x = error $ "aliasmodule: module value is not a record: " ++ show x
    lkp = interp $ makeDotPathExpr $ modulePath modName

aliasSomething :: [(String,Value)] -> [(String,Value)] -> [ProvideItem]
               -> Interpreter [(String,Value)]
aliasSomething localBds extraBds pis = concat <$> mapM apis pis
  where
    allBds = localBds ++ extraBds
    apis ProvideAll = pure $ localBds
    apis (ProvideAlias k al) = case lookup k allBds of
        Nothing -> error $ "provide alias source not found: " ++ k
        Just v ->
            -- todo: check for magic casepattern
            pure $ [(al,v)]
    apis (ProvideName k) = case lookup k allBds of
        Nothing -> error $ "provide alias source not found: " ++ k
        Just v -> do
            -- check for magic casepattern
            let c = "_casepattern-" ++ k
            pure $ case lookup c allBds of
                Nothing ->  [(k,v)]
                Just v' -> [(k,v),(c,v')]
    apis (ProvideType t) =
        let ti = "_typeinfo-" ++ t
            ist = "is-" ++ t
        in pure $ case (,) <$> lookup ti allBds <*> lookup ist allBds of
            Nothing -> error $ "provide type source not found: " ++ t
            Just (tiv, istv) -> [(ti,tiv), (ist, istv)]
    apis (ProvideData t) = do
        bs <- askBindings
        let di = "_datainfo-" ++ t
            t0 = maybe (error $ "type not found: " ++ t) id $ lookup di bs
            ti :: [String]
            ti = case t0 of
                     FFIValue ("_system","datainfo") v
                         | Just v' <- fromDynamic v -> v'
                     _ -> error "not datainfo"
        r0 <- apis (ProvideType t)
        let r00 = [(di, t0)]
        r1 <- mapM (apis . ProvideName) ti
        r2 <- mapM (\x -> apis (ProvideName ("is-" ++ x))) ti
        pure $ r0 ++ r00 ++ concat r1 ++ concat r2


-- says where to find the named module in the system path
-- _system.modules.[nm]
modulePath :: String -> [String]
modulePath nm = ["_system", "modules", nm]

---------------------------------------


letValue :: String -> Value -> Interpreter ()
--letValue "_" _ = pure ()
letValue nm v = modifyBindings (extendBindings [(nm,v)])

-- used for prelude statement support
letIncludedValue :: String -> Value -> Interpreter ()
--letIncludedValue "_" _ = pure ()
letIncludedValue nm v = modifyIncludedBindings (extendBindings [(nm,v)])



{-
letrec:
  a = ...
  b = ...
  ...
  ->
  # want to replace the lam with a memoize thing
  var a = lam():raise("internal error: uninitialized letrec")
  var b = lam():raise("internal error: uninitialized letrec")
  ...
  # then for each binding that doesn't have a lam on the lhs:
  a := memoize(lam(): original_expr)
  # if it has a lam on the lhs, then do the original expr
  b := ...
  ...
  memoize-force(a)
  # this is needed for every non lam binding
  # although it has to ignore non memoize values(since it might be
  # called more than once), so it should work if called on all the values
  ...

this approach roughly ensures every binding has a lazy value
before any value is evaluated
and these lazy values are all evaluated before continuing (e.g. to make
sure side effects happen at the right time, and the performance behaves
the way you'd expect)

every non fun value will remain a variable, if you turn them into
constants, will it work?

a memothunk has to be in a variable, otherwise it's an error
when you get the value of the variable, it evaluates the contained lam
and updates the variable and returns the value
there's also a ffi function memoize-eval, which if passed a variable
with a memothunk in it, it will eval it and save it
if passed any other kind of value, it will do nothing without error

types:

for a lam
a :: ty = ...
->
a = raise ...
...
a := assert-type-compat( ... :: ty)

for a val, put the type inside the memoize lam:
a :: ty = ...
->
a := memoize(lam(): assert-type-compat(... :: ty))
then the type will be checked the first time the value is evaluated


how will this work with the new flexible bindings?
no idea
maybe the answer is the same as with other non lambda vals?
only allow simple bindings in letrec for now

-}

doLetRec :: [(Binding, Expr)] -> [Stmt]
doLetRec bs = 
    let vars = map (makeVar . fst) bs
        assigned = map makeAssign bs
    in vars ++ assigned
  where
    makeVar (TypedBinding b _) = makeVar b
    makeVar (ShadowBinding nm) = makeVar1 (Shadow, nm)
    makeVar (NameBinding nm) = makeVar1 (NoShadow, nm)
    makeVar x = error $ "unsupported binding in recursive let: " ++ show x
    makeVar1 (sh, nm) =
        VarDecl (SimpleBinding sh nm Nothing) $ Lam (FunHeader [] [] Nothing)
        [StmtExpr $ App appSourcePos (Iden "raise")
            [Text "internal error: uninitialized letrec implementation var"]]
    makeAssign (ShadowBinding nm, v) = makeAssign1 (nm,Nothing,v)
    makeAssign (NameBinding nm, v) = makeAssign1 (nm,Nothing,v)
    makeAssign (TypedBinding (NameBinding nm) ty, v) = makeAssign1 (nm,Just ty,v)
    makeAssign (TypedBinding (ShadowBinding nm) ty, v) = makeAssign1 (nm,Just ty,v)
    makeAssign x = error $ "unsupported binding in recursive let: " ++ show x
    makeAssign1 (nm,ty,v)=
        SetVar nm $ (case ty of
                         Nothing -> id
                         Just ta -> flip AssertTypeCompat ta) v

-- placeholder to mark the places where need to fix the source
-- position

appSourcePos :: SourcePosition
appSourcePos = Nothing

---------------------------------------
-- bindings

-- when bindings can fail, this will throw an error
-- the other version is for cases and similar, it will return a maybe
matchBindingOrError :: Binding -> Value -> Interpreter [(String,Value)]
matchBindingOrError b v = do
    x <- matchBindingMaybe False b v
    case x of
        Nothing -> error $ "value doesn't match binding: " ++ show v ++ ", " ++ show b
        Just y -> pure y

-- bool is if this is a variant context, so treat a namebinding
-- as a unqualified zero arg variant match
matchBindingMaybe :: Bool -> Binding -> Value -> Interpreter (Maybe [(String,Value)])

-- todo: will turn it to wildcard in the parser
matchBindingMaybe _ (WildcardBinding) _ = pure $ Just []


-- temp until boolean becomes an actual agdt
matchBindingMaybe _ (NameBinding "true") (BoolV True) = pure $ Just []
matchBindingMaybe _ (NameBinding "true") _ = pure Nothing
matchBindingMaybe _ (NameBinding "false") (BoolV False) = pure $ Just []
matchBindingMaybe _ (NameBinding "false") _ = pure Nothing

matchBindingMaybe _ (ShadowBinding nm) v = do
    pure $ Just [(nm,v)]

matchBindingMaybe True (NameBinding nm) v = matchBindingMaybe False (VariantBinding [nm] []) v
matchBindingMaybe False (NameBinding nm) v = do
    pure $ Just [(nm,v)]

matchBindingMaybe _ (VariantBinding cnm bs) (VariantV tg vnm fs) = do
    let s = prefixLast "_casepattern-" cnm
    caseName <- interp $ makeDotPathExpr s
    case caseName of
            FFIValue _ffitag fgt | Just (ptg, pnm) <- fromDynamic fgt ->
                if ptg == tg && pnm == vnm
                then if length bs == length fs
                     then do
                         newbs <- zipWithM (matchBindingMaybe False) bs $ map snd fs
                         case sequence newbs of
                             Nothing -> pure Nothing
                             Just nbs -> pure $ Just $ concat nbs
                     else _errorWithCallStack $ "wrong number of args to pattern, expected " ++ show (map fst fs) ++ ", got " ++ show bs
                else pure Nothing
            _ -> _errorWithCallStack $ "casepattern lookup returned " ++ show caseName

matchBindingMaybe _ (VariantBinding {}) _ = pure Nothing

matchBindingMaybe c (TypedBinding b ty) v = do
    r <- matchBindingMaybe c b v
    case r of
        Just {} -> do
            void $ assertTypeAnnCompat v ty
            pure r
        Nothing -> pure Nothing

matchBindingMaybe c (AsBinding b nm) v = do
    r <- matchBindingMaybe c b v
    pure $ case r of
        Nothing -> Nothing
        Just cs -> Just ((nm,v) : cs)

matchBindingMaybe c (TupleBinding bs) (VariantV tg "tuple" fs)
    | tg == bootstrapType "Tuple" = do
          when (length bs /= length fs) $
              error $ "mismatched tuple sizes: " ++ show bs ++ " " ++ show fs
          xs <- zipWithM (matchBindingMaybe c) bs $ map snd fs
          pure (concat <$> sequence xs)
          
matchBindingMaybe _ (TupleBinding {}) _ = pure Nothing

matchBindingMaybe _ (NumberLitBinding n) (NumV m) | n == m = pure $ Just []
matchBindingMaybe _ (NumberLitBinding {}) _ = pure Nothing

matchBindingMaybe _ (StringLitBinding t) (TextV s) | t == s = pure $ Just []
matchBindingMaybe _ (StringLitBinding {}) _ = pure Nothing

          
simpleBindingToBinding :: SimpleBinding -> Binding
simpleBindingToBinding = \case
    SimpleBinding Shadow nm ty -> wrapTy ty (ShadowBinding nm)
    SimpleBinding NoShadow nm ty -> wrapTy ty (NameBinding nm)
  where
    wrapTy Nothing b = b
    wrapTy (Just ty) b = TypedBinding b ty
