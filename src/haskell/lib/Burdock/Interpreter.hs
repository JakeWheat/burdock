{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Burdock.Interpreter
    (TestResult(..)
    ,CheckBlockResult(..)
    ,takeTestResults
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
    ,myThreadId
    ,ThreadId
    ,throwTo
    )

import qualified System.Environment as E

import GHC.Conc (getNumProcessors)

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            ,MonadIO
                            )

import Control.Monad (forM_
                     ,void
                     ,when
                     ,zipWithM
                     ,forM
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

import Data.Maybe (catMaybes
                  ,isJust
                  --,fromJust
                  )
import Text.Read (readMaybe)

import Burdock.Scientific
import Burdock.Syntax
import Burdock.Pretty
import Burdock.Parse (parseScript, parseLiterateScript, parseExpr)
import qualified Burdock.Relational as R

import qualified Burdock.HsConcurrency as HC
import qualified Control.Concurrent.Async as A

import Control.Exception.Safe (catch
                              ,SomeException
                              ,Exception
                              ,throwM
                              ,catchAny
                              ,fromException
                              ,try
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
    ,takeExtension
    ,splitExtension
    ,takeDirectory
    ,(</>)
    ,(<.>)
    )

import Data.Dynamic (Dynamic
                    ,toDyn
                    ,fromDynamic
                    ,Typeable
                    )

import Data.Char (isSpace, toLower)

--import System.IO.Unsafe (unsafePerformIO)

import Text.Show.Pretty (ppShow)

import Control.Concurrent.STM.TVar
    (TVar
    ,readTVar
    ,modifyTVar
    ,newTVarIO
    ,writeTVar
    ,newTVar
    )

import Control.Concurrent.STM
    (atomically
    ,STM)

import System.Process (callProcess, readProcess)

import System.Directory as D

import qualified System.FilePath.Glob as Glob

import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))

import qualified Burdock.GeneratedBuiltins

import qualified Burdock.Version as Version

import qualified Data.ByteString as BS
import System.IO (stderr)
import qualified Data.ByteString.UTF8 as BS

import Data.Either (isLeft)

import Data.Time.Clock (getCurrentTime
                       ,diffUTCTime
                       --,UTCTime
                       )


--import Control.Monad.IO.Class
--    (MonadIO)

--import Debug.Trace (trace)


-- todo: make this dynamic

logConcurrency :: Bool
logConcurrency = False

------------------------------------------------------------------------------

-- public api

newHandle :: IO Handle
newHandle = do
    x <- newBurdockHandle
    pure $ Handle x

closeHandle :: Handle -> IO ()
closeHandle (Handle _) = pure ()

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
    runInterp True h $ do
        Script ast <- either error id
            <$> useSource mfn (maybe "runScript" id mfn) src
            (if lit then parseLiterateScript else parseScript)
        localModule (maybe "runScript" id mfn) mfn $ do
            -- todo: how to make this local to this call only
            forM_ lenv $ \(n,v) -> letValue' Shadow n v
            ret <- interpStatements ast
            pure ret
    
evalExpr :: Handle
         -> Maybe FilePath
         -> [(String,Value)]
         -> String
         -> IO Value
evalExpr h mfn lenv src =
    runInterp True h $ evalI mfn lenv src

evalI :: Maybe FilePath
      -> [(String,Value)]
      -> String
      -> Interpreter Value
evalI mfn lenv src = do
    ast <- either error id <$> useSource mfn (maybe "evalExpr" id mfn) src parseExpr
    localModule (maybe "evalExpr" id mfn) mfn $ do
        -- todo: how to make this local to this call only
        forM_ lenv $ \(n,v) -> letValue' Shadow n v
        interp ast

evalFun :: Handle
        -> String 
        -> [Value]
        -> IO Value
evalFun h fun args =
    runInterp True h $ evalFunI fun args

evalFunI :: String -> [Value] -> Interpreter Value
evalFunI fun args = do
    ast <- either error id <$> useSource Nothing "evalFun" fun parseExpr
    localModule "evalFun" Nothing $ do
        f <- interpStatements [StmtExpr ast]
        let as = zipWith (\i x -> ("aaa-" ++ show i, x)) [(0::Int)..] args
            src' = "fff(" ++ intercalate "," (map fst as) ++ ")"
        ast' <- either error id <$> useSource Nothing "evalFun" src' parseExpr
        forM_ (("fff", f):as) $ \(n,v) -> letValue' Shadow n v
        interp ast'

valueToStringIO :: Handle -> Value -> IO (Maybe String)
valueToStringIO h v =
    runInterp True h $ valueToString v

formatException :: Handle -> Bool -> InterpreterException -> IO String
formatException h includeCallstack e =
    runInterp True h $ formatExceptionI includeCallstack e

addFFIImpls :: Handle -> [(String, [Value] -> Interpreter Value)] -> IO ()
addFFIImpls h ffis =
    runInterp True h $ addFFIImpls' ffis

addFFIPackage :: Handle -> String -> FFIPackage -> IO ()
addFFIPackage h nm ffipkg =
    runInterp True h $ do
        addFFIPackage' nm ffipkg
        st <- ask
        liftIO $ atomically $ do
            modifyTVar (tlHandleState st) $ \x ->
                x {hsPackages =
                   ((nm,ffipkg) : hsPackages x)}

allTestsPassed :: Handle -> IO Bool
allTestsPassed h = runInterp True h $ do
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

takeTestResults :: Handle -> IO [(String, [CheckBlockResult])]
takeTestResults h = runInterp True h takeTestResultsI

takeTestResultsI :: Interpreter [(String, [CheckBlockResult])]
takeTestResultsI = do
    st <- ask
    v <- liftIO $ atomically $ do
        a <- readTVar $ tlHandleState st
        r <- readTVar $ hsTestResults a
        writeTVar (hsTestResults a) []
        pure r
    pure $ convertTestLog v

getTestResultsI :: Interpreter [(String, [CheckBlockResult])]
getTestResultsI = do
    st <- ask
    v <- liftIO $ atomically $ do
        a <- readTVar $ tlHandleState st
        readTVar $ hsTestResults a
    pure $ convertTestLog v


convertTestLog :: [(String, String, TestResult)]
               -> [(String, [CheckBlockResult])]
convertTestLog v = 
    let perModules :: [(String, [(String,TestResult)])]
        perModules = partitionN $ map (\(m,cb,tr) -> (m,(cb,tr))) $ reverse v
        collectCBs :: [(String, [(String,[TestResult])])]
        collectCBs = map (\(mb,cbs) -> (mb,partitionN cbs)) perModules
    in map (\(a,b) -> (a, map (uncurry CheckBlockResult) b)) collectCBs

partitionN :: Eq a => [(a,b)] -> [(a,[b])]
partitionN [] = []
partitionN vs@((k,_):_) =
    let (x,y) = partition ((==k) . fst) vs
    in (k,map snd x) : partitionN y

formatTestResults :: [(String, [CheckBlockResult])] -> Bool -> String
formatTestResults ms hideSucc =
    intercalate "\n" [intercalate "\n\n" (filter (/="") $ map formatModule ms)
                     ,summarizeAll (concatMap snd ms)]
  where
    formatTestResult tr =
        case tr of
            TestPass nm | hideSucc -> ""
                        | otherwise -> "  test (" ++ nest 8 nm ++ "): OK"
            TestFail nm msg -> "  test (" ++ nest 8 nm ++ "): failed, reason:\n" ++ indent 4 msg
    formatCheckBlock (CheckBlockResult nm trs) =
        let rs = intercalate "\n" $ filter (/= "") $ map formatTestResult trs
        in if hideSucc && rs == ""
           then ""
           else let (passed, total) = getCheckBlockTotals trs
                in "Check block: " ++ nm ++ "\n"
                   ++ rs
                   ++ "\n  " ++ showOutOf passed total ++ " in check block: " ++ nm
    formatModule (mnm, cbs) =
        let cbsf = intercalate "\n\n" $ filter (/= "") $ map formatCheckBlock cbs
        in if hideSucc && cbsf == ""
           then ""
           else heading ("Module: " ++ mnm) ++ "\n"
                ++ cbsf
                ++ summarizeCheckBlocks mnm cbs 

    summarizeAll rs =
        let (passed,total) = getCheckBlocksTotals rs
        in showOutOf passed total ++ " in all modules"

    showOutOf passed total =
        show passed ++ "/" ++ show total ++ " tests passed"
    heading s = s ++ "\n" ++ replicate (length s) '-'
    -- queries
    getCheckBlockTotals ts = 
        let f p t [] = (p,t)
            f p t (TestPass {} : s) = f (p + 1) (t + 1) s
            f p t (TestFail {} : s) = f p (t + 1) s
        in f (0 :: Int) (0 :: Int) ts
    summarizeCheckBlocks mnm cbs =
        let (passed, total) = getCheckBlocksTotals cbs
        in "\n" ++ showOutOf passed total ++ " in module: " ++ mnm

    getCheckBlocksTotals cbs = 
        sumPairs $ map (\(CheckBlockResult _ ts) -> getCheckBlockTotals ts) cbs
    sumPairs ps = 
        let f t1 t2 [] = (t1,t2)
            f t1 t2 ((a,b):rs) = f (t1 + a) (t2 + b) rs
        in f (0 :: Int) (0 :: Int) ps
    -- formatting
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
    {-FFIValue _ a == FFIValue _ b
        | Just (a' :: Addr) <- fromDynamic a
        , Just b' <- fromDynamic b = a' == b'-}
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
    {hsLoadedModules :: [(FilePath,(String,Value))] -- modulename, module value (as a record)
    ,hsTempBaseEnv :: [(String,Value)]
    ,hsForeignFunctionImpls :: [ForeignFunctionEntry]
    ,hsForeignTypes :: [(String, FFITypeInfo)]
    ,hsSourceCache :: [(FilePath, String)]
    ,hsLoadedPackages :: [(String,FilePath)]
    ,hsUniqueSourceCtr :: Int
    ,hsPackages :: [(String,FFIPackage)] -- todo: combine with loadedpackages?
    -- temp, to be split by module
    -- module path, check block name, result
    ,hsTestResults :: TVar [(String,String,TestResult)]
    ,hsTempMonitorId :: TVar Int
    }

data ModuleState
    = ModuleState
    {msModuleName :: String
    ,msModuleSourcePath :: FilePath
    }

data ThreadLocalState
    = ThreadLocalState
    {tlHandleState :: TVar BurdockHandleState
    ,tlConcurrencyHandle :: ConcurrencyHandle
    ,tlModuleState :: ModuleState
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
                          | ScopedExitException

instance Show InterpreterException where
    show (InterpreterException _ s) = s
    -- todo: create a pure version of torepr' just to use here
    show (ValueException _ v) = show v -- unsafePerformIO $ torepr' v
    show ScopedExitException = "ScopedExitException"

instance Exception InterpreterException where

interpreterExceptionToValue :: InterpreterException -> Value
interpreterExceptionToValue (InterpreterException _ s) = TextV s
interpreterExceptionToValue (ValueException _ v) = v
interpreterExceptionToValue ScopedExitException = TextV "ScopedExitException"

interpreterExceptionCallStack :: InterpreterException -> CallStack
interpreterExceptionCallStack (InterpreterException cs _) = cs
interpreterExceptionCallStack (ValueException cs _) = cs
interpreterExceptionCallStack ScopedExitException = []

---------------------------------------

-- interpreter state functions

newBurdockHandle :: IO (TVar BurdockHandleState)
newBurdockHandle = do
    rs <- newTVarIO []
    i <- newTVarIO 0
    h <- newTVarIO $ BurdockHandleState [] baseEnv builtInFF builtInFFITypes [] [] 0 [] rs i
    runInterp False (Handle h) initBootstrapModule
    runInterp False (Handle h) $ do
        (internalsFn, internalsSrc) <- readImportSource (ImportName "_internals")
        loadAndRunModuleSource False "_internals" internalsFn internalsSrc
    debugLogConcurrency Nothing "new handle"
    pure h

newSourceHandle :: TVar BurdockHandleState -> IO ThreadLocalState
newSourceHandle bs = do
    tid <- myThreadId
    ThreadLocalState bs
        <$> (ConcurrencyHandle
             (Left tid)
             <$> HC.makeInbox
             <*> newIORef []
             <*> (atomically defaultThreadExitData))
        <*> pure (ModuleState "unknown" "") -- todo: generate unique names
        <*> newIORef []
        <*> pure []
        <*> newIORef []
        <*> newIORef []
        <*> newIORef []
        <*> pure Nothing
        <*> newIORef 0
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
readModuleName = msModuleName <$> (tlModuleState <$> ask)

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

modifyBindings :: (Env -> Interpreter Env) -> Interpreter ()
modifyBindings f = do
    st <- ask
    v <- liftIO $ readIORef (tlLocallyCreatedBindings st)
    v' <- f v
    liftIO $ writeIORef (tlLocallyCreatedBindings st) v'

modifyIncludedBindings :: (Env -> Interpreter Env) -> Interpreter ()
modifyIncludedBindings f = do
    st <- ask
    v <- liftIO $ readIORef (tlIncludedBindings st)
    v' <- f v
    liftIO $ writeIORef (tlIncludedBindings st) v'

modifyReplCreatedBindings :: (Env -> Interpreter Env) -> Interpreter ()
modifyReplCreatedBindings f = do
    st <- ask
    v <- liftIO $ readIORef (tlReplCreatedBindings st)
    v' <- f v
    liftIO $ writeIORef (tlReplCreatedBindings st) v'

localModule :: String -> Maybe String -> Interpreter a -> Interpreter a
localModule modName mfn f = do
    n <- case mfn of
              Nothing -> uniqueSourceName
              Just x -> pure x
    local (\x -> x {tlModuleState = ModuleState modName n}) f

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

localBindings :: (Env -> Interpreter Env) -> Interpreter a -> Interpreter a
localBindings m f = do
    st <- ask
    e' <- m =<< liftIO (readIORef (tlLocallyCreatedBindings st))
    e1 <- liftIO $ newIORef e'
    eo <- m =<< liftIO (readIORef (tlIncludedBindings st))
    eo1 <- liftIO $ newIORef eo
    p <- liftIO $ newIORef []
    local (const $ st {tlLocallyCreatedBindings = e1
                      ,tlIncludedBindings = eo1
                      ,tlProvides = p}) f

extendBindings :: [(String,Value)] -> Env -> Interpreter Env
extendBindings bs env = pure $ bs ++ env

extendBindings' :: [(Shadow,String,Value)] -> Env -> Interpreter Env
extendBindings' bs env = do
    -- get the list of no shadow
    let gn (_,n,_) = n
        isNoShad (Shadow, _ ,_ ) = False
        isNoShad (NoShadow, _ ,_ ) = True
        nsh = map gn $ filter isNoShad bs
    -- check if any already resolve
    -- this is an incredibly slow way to do this if there are many
    -- bindings
    -- todo, get the list of binding names then do a head
    -- of the set intersection
    forM_ nsh $ \nm -> do
         msv <- lookupBinding nm
         case msv of
             Just {} -> error $ "declaration of name " ++ nm ++ " conflicts with earlier declaration"
             Nothing -> pure ()
    -- return the new env
    pure (map (\(_,n,v) -> (n,v)) bs ++ env)

    
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
        localBindings pure f
    
-- get the check block name, error if there isn't one
-- update the test results tvar
addTestResult :: TestResult -> Interpreter ()
addTestResult tr = do
    st <- ask
    let ms = tlModuleState st
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
        modifyTVar (hsTestResults v) ((msModuleSourcePath ms, cbnm, tr):)

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

data ConcurrencyHandle
    = ConcurrencyHandle
    { -- maybe because the handle for the calling thread
      -- doesn't have one of these, only an inbox
     chThreadHandle :: Either ThreadId (HC.MAsync Value)
    ,chInbox :: HC.Inbox Value
    ,chInboxBuffer :: IORef [Value]
    ,chExit :: ThreadExitData
    }

chThreadId :: ConcurrencyHandle -> ThreadId
chThreadId = either id (A.asyncThreadId . HC.asyncHandle) . chThreadHandle

debugLogConcurrency :: MonadIO m => Maybe ThreadId -> String -> m ()
debugLogConcurrency mti msg = when logConcurrency $ do
    tid <- maybe (show <$> liftIO myThreadId) (pure . show) mti
                 {-(\x -> case chThreadHandle x of
                         Nothing -> show <$> liftIO myThreadId
                         Just a -> pure $ show $ A.asyncThreadId $ HC.asyncHandle a)
                 mch-}
    liftIO $ BS.hPutStr stderr $ BS.fromString $ show tid ++ ": " ++ msg ++ "\n"


{-
When a thread exits, it does the following:
send a message to each thread monitoring it
send an exit message to each thread that is linked to it
send an exit message to each thread that is scoped to it
-}
data ThreadExitData
    = ThreadExitData
    -- reference, tag, monitoring thread
    {tedMonitors :: TVar [(Value, Value, ConcurrencyHandle)]
    ,tedLinked :: TVar [ConcurrencyHandle]
    ,tedScoped :: TVar [ConcurrencyHandle]
    -- set to have each thread exited exactly once
    -- when being asynchronously exited
    ,tedIsExiting :: TVar Bool
    }

defaultThreadExitData :: STM ThreadExitData
defaultThreadExitData =
    ThreadExitData
    <$> newTVar []
    <*> newTVar []
    <*> newTVar []
    <*> newTVar False

extractValueException :: SomeException -> Maybe Value
extractValueException e = case fromException e of
    Just (InterpreterException _cs s) -> Just $ TextV s
    Just (ValueException _cs v) -> Just v
    _ -> Nothing

bSleep :: [Value] -> Interpreter Value
bSleep [NumV t] = do
    liftIO $ threadDelay (floor (t * 1000 * 1000))
    pure nothing
bSleep x = _errorWithCallStack $ "wrong args to sleep: " ++ show x

inboxTag :: (String,String)
inboxTag = ("_system","inbox")
threadHandleTag :: (String,String)
threadHandleTag = ("_system","thread-handle")

type Inbox = HC.Inbox Value

makeInbox :: [Value] -> Interpreter Value
makeInbox [] = do
    (x :: Inbox) <- liftIO $ HC.makeInbox
    pure $ FFIValue inboxTag $ toDyn x
makeInbox x = error $ "wrong args to make-inbox: " ++ show x

sendInbox :: [Value] -> Interpreter Value
sendInbox [FFIValue _ffitag to, v]
    | Just ib <- fromDynamic to
    = do
          liftIO $ HC.send ib v
          pure nothing
sendInbox x = do
    error $ "wrong args to send-inbox: " ++ show x

receiveInbox :: [Value] -> Interpreter Value
receiveInbox [FFIValue _ffitag frm]
    | Just ib <- fromDynamic frm
    = liftIO $ HC.receive ib
receiveInbox x = error $ "wrong args to receive-inbox: " ++ show x

closeInbox :: [Value] -> Interpreter Value
closeInbox [FFIValue _ffitag frm]
    | Just (ib :: HC.Inbox Value) <- fromDynamic frm
    = liftIO (HC.closeInbox ib) >> pure nothing
closeInbox x = error $ "wrong args to close-inbox: " ++ show x

bThreadHandleEquals :: [Value] -> Interpreter Value
bThreadHandleEquals [FFIValue _ x, FFIValue _ y]
    | Just (x' :: ConcurrencyHandle) <- fromDynamic x
    , Just y' <- fromDynamic y
    = pure $ BoolV $
      HC.ibQueue (chInbox x') == HC.ibQueue (chInbox y') 
bThreadHandleEquals x = error $ "wrong args to thread-handle-equals: " ++ show x

bThreadHandleToRepr :: [Value] -> Interpreter Value
bThreadHandleToRepr [FFIValue _ x]
    | Just (x' :: ConcurrencyHandle) <- fromDynamic x
    = pure $ TextV $ case chThreadHandle x' of
          Left tid -> show tid
          Right z -> show $ A.asyncThreadId $ HC.asyncHandle z

bThreadHandleToRepr x = error $ "wrong args to thread-handle-to-repr: " ++ show x

{-
todo: what are the race conditions if e.g. the thread is cancelled
from the calling thread between the masync call and the local $ app f []?
-}

spawnOpts :: Bool -> Bool -> Maybe Value -> Value -> Interpreter Value
spawnOpts isLinked isScoped monitorTag f = do

    -- create the exit info for the launched thread
    st <- ask
    let slf = tlConcurrencyHandle st
    (mref,subted) <- liftIO $ atomically $ do
        x <- defaultThreadExitData
        when isLinked $ modifyTVar (tedLinked x) (slf:)
        mref <- case monitorTag of
            Nothing -> pure Nothing
            Just tg -> do
                hs <- readTVar $ tlHandleState st
                v <- readTVar $ hsTempMonitorId hs
                writeTVar (hsTempMonitorId hs) (v + 1)
                let mref = TextV $ show v
                modifyTVar (tedMonitors x) ((mref,tg,slf):)
                pure $ Just mref
        pure (mref,x)

    -- helpers for the exit handler for the lauched thread
    scopedCancelled <- interp (Iden "scoped-cancelled")
    let linkedCancelled :: SomeException -> IO Value
        linkedCancelled a = flip runReaderT st $ do
            let a' = maybe (TextV $ show a) id $ extractValueException a
            lc <- interp (Iden "linked-cancelled")
            app lc [a']
    let makeMonitorDown vs = flip runReaderT st $ do
            mmd <- interp (Iden "monitor-down")
            app mmd vs
    let makeEither x = flip runReaderT st $ do
            case x of
                Left e -> do
                    let v = maybe (TextV $ show e) id $ extractValueException e
                    l <- interp (Iden "left")
                    app l [v]
                Right v -> do
                    l <- interp (Iden "right")
                    app l [v]

    -- inbox for the launched thread
    subInbox <- liftIO $ HC.makeInbox

    -- launch the thread
    h <- liftIO $ HC.masync (ext subInbox
                                 scopedCancelled
                                 linkedCancelled
                                 makeMonitorDown
                                 makeEither
                                 subted)
        $ runInterpInherit st True $ do
        -- get the self concurrency handle, it's done like this because
        -- it contains this thread's own async handle, which we can't
        -- from the thread itself easily
        x <- liftIO $ HC.receive subInbox
        let myCh = case x of
              FFIValue _ffitag a | Just a' <- fromDynamic a -> a'
              _ -> error $ "expected async handle, got " ++ show x
        local (\z -> z {tlConcurrencyHandle = myCh}) $ app f []
    -- create the concurrency handle for the launched thread
    
    subCh <- do
        b <- liftIO $ newIORef []
        pure $ ConcurrencyHandle (Right h) subInbox b subted
    -- tell the launched thread its concurrency handle
    liftIO $ HC.send subInbox $ FFIValue ("temp", "temp") $ toDyn subCh

    -- modify this thread's exit handle to add links and scoped stuff
    liftIO $ atomically $ do
        let myTed = chExit $ tlConcurrencyHandle st
        when isLinked $ do
            modifyTVar (tedLinked myTed) (subCh:)
        when isScoped $ do
            modifyTVar (tedScoped myTed) (subCh:)

    -- what what's been done
    debugLogConcurrency Nothing (unwords ["spawn"
                                 ,if isLinked
                                     then "linked"
                                     else "unlinked"
                                 ,show $ chThreadId subCh])
    let bh = FFIValue threadHandleTag $ toDyn subCh
    pure $ case mref of
               Nothing -> bh
               Just mr -> VariantV (bootstrapType "Tuple") "tuple" $ zipWith (\n v -> (show n, v)) [(0::Int)..] [bh,mr]
  where
    ext subInbox scopedCancelled linkedCancelled makeMonitorDown makeEither ted tid x = do
        debugLogConcurrency (Just tid) $ "starting exit " ++ show x

        liftIO $ HC.closeInbox subInbox

        (monitors, scopedExits, linkedExits) <- atomically $ do
             -- get the list of monitors to send messages to
             monitors <- readTVar $ tedMonitors ted
             -- run through each scoped thread
             --   if it isn't already exiting, mark it to be exited
             let g xch = do
                     e <- readTVar $ tedIsExiting $ chExit xch
                     if e
                         then pure $ Left xch
                         else do
                             writeTVar (tedIsExiting $ chExit xch) True
                             pure $ Right xch 
             scopedExits <- mapM g =<< readTVar (tedScoped ted)
             -- same for linked threads, but only exit linked
             -- threads if it's an error value exit
             linkedExits <-
                 if isLeft x
                 then mapM g =<< readTVar (tedLinked ted)
                 else pure []
             pure (monitors, scopedExits, linkedExits)
        -- signal monitoring threads
        forM_ monitors $ \(ref,tg,och) -> do
            x' <- makeEither x
            v <- makeMonitorDown [tg,x',ref]
            HC.send (chInbox och) v

        {- waits until the scoped threads have exited this is for
        better troubleshooting, so you see a hang instead of it
        silently failing. Could make it asynchronous with a timeout
        callback instead in the future
        need a check like this for linked threads
        -}
        let scopeExit ch = case chThreadHandle ch of
                Left xtid -> 
                    throwTo xtid (ValueException [] scopedCancelled)
                Right sch ->
                    let a = HC.asyncHandle sch
                        xtid = A.asyncThreadId a
                    in throwTo xtid (ValueException [] scopedCancelled)
                       <* A.waitCatch a
        forM_ scopedExits $ \case
            Left {} -> pure () -- todo: log it
            Right ch -> scopeExit ch
        lc <- case x of
                  Left x' -> linkedCancelled x'
                  _ -> pure nothing
        let linkExit ch = throwTo (chThreadId ch) (ValueException [] lc)
        forM_ linkedExits $ \case
            Left {} -> pure () -- todo: log it
            Right ch -> linkExit ch
        -- todo: check a global flag to see if to output
        -- ideally, want to output when it's a left, and no-one
        -- notices - no scopers, no linkers, no monitorers
        debugLogConcurrency (Just tid) $ "exit with " ++ show x 
{-how to log thread exit actions nicely
  want to state the list of linked threads that are being signaled to exit
    the list of linked threads which are already exiting
  the list of scoped threads that are being signaled to exit
    the list of scoped threads which are already exiting-}
        -- putStrLn $ "exit callback: " ++ show x --  ++ "\n" ++ show info

bThreadCancel :: [Value] -> Interpreter Value
bThreadCancel [FFIValue _ffitag h']
    | Just h <- fromDynamic h' = do
          threadCancelWith h nothing
bThreadCancel x = error $ "wrong args to thread-cancel: " ++ show x

bThreadCancelWith :: [Value] -> Interpreter Value
bThreadCancelWith [FFIValue _ffitag h', v]
    | Just h <- fromDynamic h' = do
          threadCancelWith h v
bThreadCancelWith x = error $ "wrong args to thread-cancel-with: " ++ show x

threadCancelWith :: ConcurrencyHandle -> Value -> Interpreter Value
threadCancelWith h v = do
    ac <- interp (Iden "cancelled")
    ac' <- app ac [v]
    case chThreadHandle $ h of
        Left tid -> liftIO $ throwTo tid $ ValueException [] ac'
        Right ch' -> liftIO $ throwTo (A.asyncThreadId $ HC.asyncHandle ch') $ ValueException [] ac'
    pure nothing



bSpawn :: [Value] -> Interpreter Value
bSpawn [f] = spawnOpts True True Nothing f
bSpawn x = error $ "wrong args to spawn: " ++ show x

bSpawnNoLink :: [Value] -> Interpreter Value
bSpawnNoLink [f] = spawnOpts False True Nothing f
bSpawnNoLink x = error $ "wrong args to spawn-nolink: " ++ show x

bSpawnUnscoped :: [Value] -> Interpreter Value
bSpawnUnscoped [f] = spawnOpts True False Nothing f
bSpawnUnscoped x = error $ "wrong args to spawn-unscoped: " ++ show x

bSpawnMonitor :: [Value] -> Interpreter Value
bSpawnMonitor [f] = spawnOpts False True (Just nothing) f
bSpawnMonitor x = error $ "wrong args to spawn-monitor: " ++ show x

bSpawnMonitorTag :: [Value] -> Interpreter Value
bSpawnMonitorTag [f,tg] = spawnOpts False True (Just tg) f
bSpawnMonitorTag x = error $ "wrong args to spawn-monitor-tag: " ++ show x

bWait :: [Value] -> Interpreter Value
bWait [FFIValue _ffitag h']
    | Just h'' <- fromDynamic h' = do
    case chThreadHandle h'' of
        Left {} -> error "can't wait on main/api call thread"
        Right h -> liftIO $ A.wait $ HC.asyncHandle h
bWait x = error $ "wrong args to wait: " ++ show x

bWaitEither :: [Value] -> Interpreter Value
bWaitEither [FFIValue _ffitag h']
    | Just h'' <- fromDynamic h' = do
    let h = either (error "can't wait on main/api call thread")
             id $ chThreadHandle h''
    (x :: Either SomeException Value) <- liftIO $ A.waitCatch $ HC.asyncHandle h
    case x of
        Right v -> do
            r <- interp (Iden "right")
            app r [v]
        Left e -> do
            l <- interp (Iden "left")
            case extractValueException e of
                Nothing -> app l [TextV $ show e]
                Just v -> app l [v]
bWaitEither x = error $ "wrong args to wait: " ++ show x

bSend :: [Value] -> Interpreter Value
bSend [FFIValue _ffitag h', v]
    | Just h <- fromDynamic h' = do
          liftIO $ HC.send (chInbox h) v
          pure nothing
bSend x = error $ "wrong args to send: " ++ show x

receiveFromInboxBufferElse :: Interpreter Value -> Interpreter Value
receiveFromInboxBufferElse f = do
    ibR <- (chInboxBuffer . tlConcurrencyHandle) <$> ask
    ib <- liftIO $ readIORef ibR
    case ib of
        [] -> f
        (x:xs) -> do
            liftIO $ writeIORef ibR xs
            pure x

bReceiveAny :: [Value] -> Interpreter Value
bReceiveAny [] =
    receiveFromInboxBufferElse $ do
    ch <- (chInbox . tlConcurrencyHandle) <$> ask
    liftIO $ HC.receive ch
bReceiveAny x = error $ "wrong args to receive-any: " ++ show x

bReceiveAnyTimeout :: [Value] -> Interpreter Value
bReceiveAnyTimeout [NumV n] =
    receiveFromInboxBufferElse $ do
    ch <- (chInbox . tlConcurrencyHandle) <$> ask
    ret <- liftIO $ HC.receiveTimeout ch (floor $ n * 1000 * 1000)
    case ret of
        Nothing -> pure nothing
        Just v -> do
            i <- interp (Iden "some")
            app i [v]
bReceiveAnyTimeout x = error $ "wrong args to receive-any-timeout: " ++ show x

{-
rough pseudocode:

there's a buffer which represents items read from the queue which
  were not matching in a selective receive
and the spent buffer
the spent buffer is items popped from the buffer
  that don't match during a single receive
when returning, the new buffer is reverse the spent buffer + the
  unlooked at buffer


  record the start time
  check the match function against each item in the buffer
  if one matches, return it
  then check the match function against each item you can get with a receive with 0 timeout
    if one matches, return it
    if one doesn't match, add it to the end of the buffer
  if the timeout was 0, call the after if there is one, else return none
  loop:
    if timeout was infinity, do receive with no timeout
    otherwise calculate the time left on the timeout
      receive with this timeout
    -> if timeout, call the after if there is one, else return none
    else check the new item with the match function
    if matched, return it
    else put it in the spent buffer and loop

TODO: sync up receive-any and receive-any-timeout to the buffer
-> get them to call this function

-}

fromBOption :: Value -> Maybe (Maybe Value)
fromBOption (VariantV _ "some" [(_,x)]) = Just $ Just x
fromBOption (VariantV _ "none" []) = Just Nothing
fromBOption _ = Nothing

receiveSyntaxHelper :: Value -> Maybe (Scientific, Value) -> Interpreter Value
receiveSyntaxHelper matchFn maft = do
    startTime <- liftIO getCurrentTime
    ch <- (chInbox . tlConcurrencyHandle) <$> ask
    ibR <- (chInboxBuffer . tlConcurrencyHandle) <$> ask
    inboxBuffer <- liftIO $ readIORef ibR

    let ret sb ib x = do
            liftIO $ writeIORef ibR (reverse sb ++ ib)
            pure x
        checkForMatch v sb ib = do
            res <- app matchFn [v]
            case fromBOption res of
                Nothing -> error $ "receive match function returned non option: " ++ show res
                Just Nothing -> doOneTick (v:sb) ib
                Just (Just x) -> ret sb ib x
        timedOut sb ib aftf = do
            liftIO $ writeIORef ibR (reverse sb ++ ib)
            app aftf []
        -- check through the buffer
        doOneTick sb (x:xs) = checkForMatch x sb xs
        doOneTick sb [] = doOneTickIb sb
        -- check any messages already in the inbox
        -- TODO: this calls receivetimeout with 0 redundantly
        -- when the inbox has already been drained, if/when the code
        -- knows it's going to call it with a non zero timeout
        -- next
        doOneTickIb sb = do
            v <- liftIO $ HC.receiveTimeout ch 0
            case v of
                Just v' -> checkForMatch v' sb []
                Nothing -> doOneTickWait sb
        doOneTickWait sb =
            case maft of
                Nothing -> do
                    -- no timeout
                    newv <- liftIO $ HC.receive ch
                    checkForMatch newv sb []
                Just (tmo, aftf) -> do
                    togo <- microsecondsLeft tmo
                    if togo < 0
                        then timedOut sb [] aftf
                        else do
                            v <- liftIO $ HC.receiveTimeout ch togo
                            case v of
                                Nothing -> timedOut sb [] aftf
                                Just v' -> checkForMatch v' sb []
        microsecondsLeft tmo = do
            nw <- liftIO getCurrentTime
            let elapsed = diffUTCTime nw startTime
                togo :: Int
                togo = floor ((tmo - realToFrac elapsed) * 1000 * 1000)
            pure togo
            
    doOneTick [] inboxBuffer

bSelf :: [Value] -> Interpreter Value
bSelf [] = do
    ch <- tlConcurrencyHandle <$> ask
    pure $ FFIValue threadHandleTag $ toDyn ch

bSelf x = error $ "wrong args to self: " ++ show x


bThreadId :: [Value] -> Interpreter Value
bThreadId [] = (TextV . show) <$> liftIO myThreadId
bThreadId x = error $ "wrong args to haskell-thread-id: " ++ show x

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

runInterp :: Bool -> Handle -> Interpreter a -> IO a
runInterp incG (Handle h) f = do
    sh <- newSourceHandle h
    flip runReaderT sh $ do
        -- will become the use context thing
        -- only bootstrapping a handle with the bootstrap and _internals
        -- loading skips including globals atm
        when incG $ void $ interpStatement (Include (ImportName "globals"))
        f

-- used for a local thread - one that isn't for a new haskell api call or
-- new module/source file - it preserves the right part of the
-- enclosing state
runInterpInherit :: ThreadLocalState -> Bool -> Interpreter a -> IO a
runInterpInherit parentSh incG f = do
    p <- newIORef []
    b <- newIORef =<< readIORef (tlLocallyCreatedBindings parentSh)
    bo <- newIORef =<< readIORef (tlIncludedBindings parentSh)
    rb <- newIORef =<< readIORef (tlReplCreatedBindings parentSh)
    cbn <- newIORef 0
    let sh = parentSh
            {tlProvides = p
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
            ScopedExitException -> pure ([], "ScopedExitException")
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
useSource mfn _modName src p = do
    fn <- case mfn of
              Nothing -> uniqueSourceName
              Just x -> pure x
    st <- ask
    liftIO $ atomically $ do
        modifyTVar (tlHandleState st) $ \x ->
            x {hsSourceCache = (fn,src) : hsSourceCache x }
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
{-addrEquals :: [Value] -> Interpreter Value
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
addrToRepr _ = error "bad args to addr torepr"-}


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
    ,("thread-handle", FFITypeInfo (makeFFIMemberFunction "_member-thread-handle"))
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
             [a, b] | Just a' <- fromBList a
                    , Just b' <- fromBList b
                    -> pure $ makeBList $ a' ++ b'
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
    ,("haskell-show", haskellShow)
    ,("sysexit", sysExit)
    ,("burdock-version", bBurdockVersion)

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

    ,("sleep", bSleep)
    ,("make-inbox", makeInbox)
    ,("send-inbox", sendInbox)
    ,("receive-inbox", receiveInbox)
    ,("close-inbox", closeInbox)
    ,("_member-thread-handle"
     ,ffiMemberDispatcher $ FMD
      {fmdLkp = [("_equals", ffiSingleArgMethod "thread-handle-equals")
                ,("_torepr", ffiNoArgMethod "thread-handle-to-repr")]
      ,fmdFallback = Nothing})
    ,("thread-handle-equals", bThreadHandleEquals)
    ,("thread-handle-to-repr", bThreadHandleToRepr)

    
    ,("spawn", bSpawn)
    ,("spawn-nolink", bSpawnNoLink)
    ,("spawn-unscoped", bSpawnUnscoped)
    ,("spawn-monitor", bSpawnMonitor)
    ,("spawn-monitor-tag", bSpawnMonitorTag)
    ,("wait", bWait)
    ,("wait-either", bWaitEither)
    ,("haskell-thread-id", bThreadId)
    ,("thread-cancel", bThreadCancel)
    ,("thread-cancel-with", bThreadCancelWith)
    
    ,("send", bSend)
    ,("self", bSelf)
    ,("receive-any", bReceiveAny)
    ,("receive-any-timeout", bReceiveAnyTimeout)
    
    ,("get-args", bGetArgs)

    ,("read-process", bReadProcess)
    ,("call-process", bCallProcess)
    ,("list-directory", bListDirectory)
    ,("glob", bGlob)
    
    ,("run-tests-with-opts", bRunTestsWithOpts)
    ,("take-test-results", bTakeTestResults)
    ,("format-test-results", bFormatTestResults)
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
    let is = ImportSpecial "file" [fn]
    (fn',src) <- readImportSource is
    _moduleName' <- moduleNameOf is
    loadAndRunModuleSource True moduleName fn' src
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

haskellShow :: [Value] -> Interpreter Value
haskellShow args = pure $ TextV $ show args

-- todo: work out how this should work with threads
sysExit :: [Value] -> Interpreter Value
sysExit [NumV n'] | Just n <- extractInt n' =
    liftIO $ if n == 0
             then exitSuccess
             else exitWith (ExitFailure n)
sysExit _ = error "wrong args tosysExit"

bBurdockVersion :: [Value] -> Interpreter Value
bBurdockVersion [] =
    pure $ TextV $ Version.version
bBurdockVersion _ = error "wrong args burdock-version"

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
    localModule "run-script" Nothing $ do
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

bGlob :: [Value] -> Interpreter Value
bGlob [TextV pn] = do
    is <- liftIO $ Glob.globDir1 (Glob.compile pn) "."
    pure $ makeBList $ map TextV is
bGlob x = error $ "bad args to glob" ++ show x


bRunTestsWithOpts :: [Value] -> Interpreter Value
bRunTestsWithOpts [opts] = do
    testSrcs :: [Either FilePath (FilePath,String)] <- do
        tsrcs' <- getAttr opts "test-sources"
        let tsrcs = maybe (error $ "bad args to run-tests-with-opts" ++ show opts) id
                    $ fromBList tsrcs'
        forM tsrcs $ \tsrc -> do
            TextV nm <- getAttr tsrc "file-name"
            if isV tsrc "file-test"
                then pure $ Left nm
                else do
                    TextV s <- getAttr tsrc "source"
                    pure $ Right (nm, s)
    BoolV showProgressLog <- getAttr opts "show-progress-log"
    _testRunPredicate :: Maybe Value <- do
        v <- getAttr opts "test-run-predicate"
        case v of
            VariantV _ "none" [] -> pure Nothing
            _ -> error $ "test run predicate not supported"
    BoolV hideSuccesses <- getAttr opts "hide-successes"
    BoolV autoPrintResults <- getAttr opts "auto-print-results"

    cbs <- concat <$>
        (forM testSrcs $ \testSrc -> runIsolated showProgressLog hideSuccesses $ do
            r <- try $ case testSrc of
                Left fn -> do
                    (fn',src) <- readImportSource (ImportSpecial "file" [fn])
                    loadAndRunModuleSource True fn' fn' src
                Right (fn, src) -> loadAndRunModuleSource True fn fn src
            case r of
                Right {} -> pure ()
                Left (e :: SomeException) -> do
                    let nm = "load module " ++ show testSrc
                    local (\st' -> st' {tlCurrentCheckblock = Just nm}) $
                        addTestResult (TestFail nm (show e))
                    --liftIO $ putStrLn $ show e
            takeTestResultsI)
    rs <- testResultsToB cbs
    when autoPrintResults $ void $
        evalI Nothing [("rs1234", rs)
                      ,("hs1234", BoolV hideSuccesses)
                      ]
            "print(format-test-results(rs1234, hs1234))"
    pure rs
  where
    runIsolated showProgressLog hideSuccesses f = do
{-
hack: create a new state as if it's a new handle
run loadandrunmodule with this new state, nested in a new monad context
then extract the hstestresults from it
-}
        subH <- liftIO $ newBurdockHandle
        let h = Handle subH
        -- add the loaded packages to the new handle
        pkgs <- do
            st <- ask
            hs <- liftIO $ atomically $ readTVar $ tlHandleState st
            pure $ reverse $ hsPackages hs
        forM_ pkgs $ \(nm,p) -> liftIO $ addFFIPackage h nm p

        liftIO $ void $ runScript h Nothing [] "_system.modules._internals.set-auto-run-tests(true)"
        if showProgressLog
            then liftIO $ void $ runScript h Nothing [] "_system.modules._internals.set-show-test-progress-log(true)"
            else liftIO $ void $ runScript h Nothing [] "_system.modules._internals.set-show-test-progress-log(false)"
        if hideSuccesses
            then liftIO $ void $ runScript h Nothing [] "_system.modules._internals.set-hide-test-successes(true)"
            else liftIO $ void $ runScript h Nothing [] "_system.modules._internals.set-hide-test-successes(false)"
        liftIO $ runInterp True h f
            
    isV v nm = case v of
        VariantV _ nm' _ -> nm == nm'
        _ -> False
    getAttr v nm = evalI Nothing [("v", v)] ("v." ++ nm)

bRunTestsWithOpts x = error $ "bad args to run-tests-with-opts" ++ show x

bTakeTestResults :: [Value] -> Interpreter Value
bTakeTestResults [] = do
    st <- ask
    cbs <- liftIO $ atomically $ do
        a <- readTVar $ tlHandleState st
        r <- readTVar $ hsTestResults a
        writeTVar (hsTestResults a) []
        pure r
    testResultsToB $ convertTestLog cbs
bTakeTestResults x = error $ "bad args to take-test-results" ++ show x

testResultsToB :: [(String, [CheckBlockResult])] -> Interpreter Value
testResultsToB cbs = do
    mkModuleCheckResults <- interp (Iden "module-check-results")
    mkCheckResults <- interp (Iden "check-results")
    mkTestPass <- interp (Iden "test-pass")
    mkTestFail <- interp (Iden "test-fail")
    let makeMcr nm cbsx = do
            cs <- mapM makeCbr cbsx
            app mkModuleCheckResults [TextV nm, makeBList cs]
        makeCbr (CheckBlockResult nm trs) = do
            trs' <- forM trs $ \case
                        TestPass tnm -> app mkTestPass [TextV tnm]
                        TestFail tnm msg -> app mkTestFail [TextV tnm, TextV msg]
            app mkCheckResults [TextV nm, makeBList trs']
            
    x <- mapM (uncurry makeMcr) cbs
    pure $ makeBList x
    

bFormatTestResults :: [Value] -> Interpreter Value
bFormatTestResults as@[lst', BoolV hs] | Just lst <- fromBList lst' = do
    let extractMod v = do
            TextV mn <- getAttr v "module-name"
            cbs <- getAttr v "mcheck-results"
            let cbs' = maybe (error $ "bad args to format-test-results" ++ show as) id $ fromBList cbs
            (mn,) <$> mapM extractCB cbs'
        extractCB v = do
            TextV cbn <- getAttr v "block-name"
            trs <- getAttr v "results"
            let trs' = maybe (error $ "bad args to format-test-results" ++ show as) id $ fromBList trs
            CheckBlockResult cbn <$> mapM extractTr trs'
        extractTr v = do
            TextV nm <- getAttr v "name"
            case v of
                VariantV _ "test-fail" _ -> do
                    TextV msg <- getAttr v "msg"
                    pure $ TestFail nm msg
                _ -> pure $ TestPass nm
    v <- mapM extractMod lst
    let res = formatTestResults v hs
    pure $ TextV res
  where
    getAttr v nm = evalI Nothing [("v", v)] ("v." ++ nm)
bFormatTestResults x = error $ "bad args to format-test-results" ++ show x
    

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
            localBindings (extendBindings' lbs) $ newEnv bs'
    newEnv bs

interp (Block ss) = localBindings pure $ interpStatements ss

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
    bindingMatchesType t (AsBinding b _ _) = bindingMatchesType t b
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
                let rbbs = localBindings (extendBindings' bbs)
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

interp (Extend ev fs) = do
    v <- interp ev
    case v of
        -- todo: if the fields being updated aren't already in the variant,
        -- then convert the variant to a record?
        VariantV tg vr vs -> do
            vs' <- mapM (\(n,e) -> (n,) <$> interp e) fs
            let vs'' = filter ((`notElem` map fst vs') . fst) vs ++ vs'
            pure $ VariantV tg vr vs''
        _ -> error $ "extend only supported for variants, got " ++ show v

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

{-

the match function is built from the receive like this:

receive
  c1 => t1
  c2 => t2
  ...
  [after ...]
end

->
fun matchfn(x):
  cases x:
    c1 => some(t1)
    c2 => some(t2)
    ...
    else => none
  end
end

-}
interp (Receive as aft) = do
    let mpCase (b,x,bdy) = (b,x,[StmtExpr $ App Nothing (Iden "some") [Block bdy]])
        els = Just [StmtExpr $ Iden "none"]
        matchFnS = Lam (FunHeader[] [NameBinding "receivepayload"] Nothing)
                  [StmtExpr $ Cases (Iden "receivepayload") Nothing (map mpCase as) els]
    matchFn <- interp matchFnS
    aft' <- case aft of
            -- TODO: this isn't correct, infinity should be a value,
            -- not a bit of syntax
            Just (Iden "infinity", _fn) -> pure Nothing
            Just (e, fn) -> do
                aftx <- interp (Lam (FunHeader [] [] Nothing) fn)
                v <- interp e
                case v of
                    NumV b -> pure $ Just (b, aftx)
                    _ -> error $ "receive after value is not a number: " ++ show v
            Nothing -> pure Nothing
    receiveSyntaxHelper matchFn aft'

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
              ++"\nlooking in value with fields: " ++ show (map fst fs)
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
        bs = flip map nms2 $ \n -> ShadowBinding n
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
                    localBindings (extendBindings' got) $ interp bdy
                fbas acc ((b,v):as') = do
                    lbs <- matchBindingOrError b v
                    fbas (lbs:acc) as'
            localBindings (\_ -> pure env) $ fbas [] as
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
    getRecs accdecls accchks (FunDecl (SimpleBinding sh nm mty) fh _ds bdy whr : ss') =
        let accchks' = maybe accchks (\w -> Check (Just nm) w : accchks) whr
            b1 = case sh of
                     Shadow -> ShadowBinding nm
                     NoShadow -> NameBinding nm
            b = case mty of
                    Nothing -> b1
                    Just ty -> TypedBinding b1 ty
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
    mapM_ (\(x0,x1,x2) -> letValue' x0 x1 x2) bs
    pure nothing

interpStatement (VarDecl b e) = do
    v <- interp e
    -- todo: it should only preserve the type of the var itself
    -- if the user used a contract, and not if they used a type
    -- annotation on the value the var is initialized with?
    (sh, v',ty) <- case b of
              SimpleBinding sh _ (Just ta) -> do
                  (sh,,) <$> assertTypeAnnCompat v ta
                  <*> typeOfTypeSyntax ta
              SimpleBinding sh _ _ -> pure (sh, v, bootstrapType "Any")
              --_ -> pure (Shadow, v, bootstrapType "Any")
    vr <- liftIO $ newIORef v'
    
    letValue' sh (sbindingName b) (BoxV ty vr)
    pure nothing

interpStatement (SetVar (Iden nm) e) = do
    mv <- lookupBinding nm
    let (ty,vr) = case mv of
                 Just (BoxV vty b) -> (vty,b)
                 Just x -> error $ "attempt to assign to something which isn't a var: " ++ show x
                 Nothing -> error $ "identifier not found: " ++ show nm
    v <- interp e
    void $ assertTypeCompat v ty
    liftIO $ writeIORef vr v
    pure nothing

interpStatement (SetVar (DotExpr r@(Iden _) nm) e) = do
    r1 <- interp r
    case r1 of
        VariantV _ _ fs | Just mv <- lookup nm fs -> do
            let (ty,vr) = case mv of
                         BoxV vty b -> (vty,b)
                         x -> error $ "attempt to assign to something which isn't a var: " ++ show x
            v <- interp e
            void $ assertTypeCompat v ty
            liftIO $ writeIORef vr v
            pure nothing
        _ -> error $ "field not found in dot expr: " ++ nm
interpStatement (SetVar x _) = error $ ":= not supported for " ++ show x

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
    letValue' NoShadow typeInfoName $ FFIValue ("_system","typeinfo") $ toDyn
        $ SimpleTypeInfo ["_system", "modules", moduleName, dnm]
    letValue' NoShadow dataInfoName $ FFIValue ("_system","datainfo") $ toDyn
        $ map varName vs
    -- todo: use either a burdock value or a proper ffi value for the casepattern
    -- instead of a haskell tuple
    forM_ vs $ \(VariantDecl vnm _ _) -> letValue' NoShadow ("_casepattern-" ++ vnm)
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
    letValue' NoShadow ("_typeinfo-" ++ nm) $ FFIValue ("_system","typeinfo") $ toDyn $ SimpleTypeInfo [ty]
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
    modName <- ensureModuleLoaded is
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
    modName <- ensureModuleLoaded is
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
testIs msg e0 e1 =
    testPredSupport msg e0 e1 $ \v0 v1 -> do
    case (v0,v1) of
        (Right v0', Right v1') -> do
            t <- hEqualAlways v0' v1'
            if t
            then pure $ TestPass msg
            else do
                p0 <- torepr' v0'
                p1 <- torepr' v1'
                pure $ TestFail msg (p0 ++ "\n!=\n" ++ p1)
        (Left (_,er0), Right {}) -> do
            pure $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0)
        (Right {}, Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1)
        (Left (_,er0), Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0
                                ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1)

testIsNot :: String -> Expr -> Expr -> Interpreter Value
testIsNot msg e0 e1 =
    testPredSupport msg e0 e1 $ \v0 v1 -> do
    case (v0,v1) of
        (Right v0', Right v1') -> do
            t <- hEqualAlways v0' v1'
            if not t
            then pure $ TestPass msg
            else do
                p0 <- torepr' v0'
                p1 <- torepr' v1'
                pure $ TestFail msg (p0 ++ "\n==\n" ++ p1)
        (Left (_,er0), Right {}) -> do
            pure $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0)
        (Right {}, Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1)
        (Left (_,er0), Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0
                                ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1)

testRaises :: String -> Expr -> Expr -> Interpreter Value
testRaises msg e0 e1 =
    testPredSupport msg e0 e1 $ \v0 v1 -> do
    case (v0,v1) of
        (Right _, Right _) ->
            pure $ TestFail msg (prettyExpr e0 ++ " didn't raise")
        (_, Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1)
        (Left (er0,er0'), Right (TextV v)) -> do
            val <- torepr' er0
            if v `isInfixOf` val
                then pure $ TestPass msg
                else pure $ TestFail msg ("failed: " ++ val ++ ", expected " ++ "\"" ++ v ++ "\"" ++ "\n" ++ er0')
        (Left _, Right v) -> do
            pure $ TestFail msg (prettyExpr e1 ++ " failed, expected String, got: " ++ show v)

testRaisesSatisfies :: String -> Expr -> Expr -> Interpreter Value
testRaisesSatisfies msg e0 f =
    testPredSupport msg e0 f $ \v0 fv -> do
    case (v0,fv) of
        (Right _, Right _) ->
            pure $ TestFail msg (prettyExpr e0 ++ " didn't raise")
        (_, Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr f ++ " failed: " ++ er1)
        (Left (er0,er0'), Right fvv) -> do
            r <- app fvv [er0]
            case r of
                BoolV True -> pure $ TestPass msg
                BoolV False -> pure $ TestFail msg ("failed: " ++ show er0 ++ ", didn't satisfy predicate " ++ show f ++ "\n" ++ show er0')
                _ -> pure $ TestFail msg ("failed: predicted didn't return a bool: " ++ show f ++ " - " ++ show r)

testSatisfies :: String -> Expr -> Expr -> Interpreter Value
testSatisfies msg e0 f =
    testPredSupport msg e0 f $ \v0 fv -> do
    case (v0,fv) of
        (Right v', Right f') -> do
            r <- app f' [v']
            case r of
                BoolV True -> pure $ TestPass msg
                BoolV False -> pure $ TestFail msg ("failed: " ++ show v' ++ ", didn't satisfy predicate " ++ show f)
                _ -> pure $ TestFail msg ("failed: predicted didn't return a bool: " ++ show f ++ " - " ++ show r)
        (Left (_,er0), Right {}) -> do
            pure $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0)
        (Right {}, Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr f ++ " failed: " ++ er1)
        (Left (_,er0), Left (_,er1)) -> do
            pure $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0
                                ++ "\n" ++ prettyExpr f ++ " failed: " ++ er1)

testPredSupport :: String
                -> Expr
                -> Expr
                -> (Either (Value,String) Value
                    -> Either (Value,String) Value
                    -> Interpreter TestResult)
                -> Interpreter Value
testPredSupport testPredName e0 e1 testPredCheck = do
    -- todo 1: check the test-run-p predicate to see if this test should run
    -- todo 2: check the progress log to see if should output a log message
    -- todo 3: if progress log enabled, output OK or FAIL + the message
    le <- isProgressLogEnabled
    hideSucc <- isHideSuccesses
    when (le && not hideSucc) logit
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
                Right v -> pure $ Right $ v
    v0 <- w e0
    v1 <- w e1
    res <- testPredCheck v0 v1
    when le $ case res of
        TestPass {} ->
            if hideSucc
            then pure ()
            else liftIO $ putStrLn $ " OK"
        TestFail _ msg -> do
            when hideSucc logit
            liftIO $ putStrLn $ " FAIL\n" ++ msg
    addTestResult res
    pure nothing
  where
    logit = do
        st <- ask
        let ms = msModuleSourcePath $ tlModuleState st
        cbnm <- case tlCurrentCheckblock st of
            Just n -> pure n
            Nothing -> _errorWithCallStack $ "test predicate not in check block: " ++ testPredName
        liftIO $ putStr $ "test " ++ ms ++ "/" ++ cbnm ++ "/" ++ testPredName
    isProgressLogEnabled = do
        e <- interp $ internalsRef "show-test-progress-log"
        case e of
            BoolV True -> pure True
            _ -> pure False
    isHideSuccesses = do
        e <- interp $ internalsRef "hide-test-successes"
        case e of
            BoolV True -> pure True
            _ -> pure False

------------------------------------------------------------------------------

-- import handlers

fileImportHandler :: [String] -> Interpreter (String,String)
fileImportHandler [fn] = do
    cm <- msModuleSourcePath <$> (tlModuleState <$> ask)
    --liftIO $ putStrLn $ "loading " ++ fn ++ " from " ++ cm
    let fn1 = takeDirectory cm </> fn
    src <- liftIO $ readFile fn1
    pure (fn1, src)
fileImportHandler x = error $ "bad args to file import handler " ++ show x


builtinsImportHandler :: [String] -> Interpreter (String,String)
builtinsImportHandler [nm] =
    let fn = -- hack for bundled package files
            if '.' `elem` nm
            then case splitExtension nm of
                (p, ('.':nm')) -> "src/packages" </> p </> "bur" </> nm' <.> "bur"
                _ -> error $ "bad import name: " ++ nm
            else "src/burdock/built-ins" </> nm <.>"bur"
    in case lookup fn Burdock.GeneratedBuiltins.builtins of
        Nothing -> error $ "built in module not found: " ++ nm ++ "\n" ++ fn ++ "\n" ++ intercalate "\n" (map fst Burdock.GeneratedBuiltins.builtins)
        Just src -> pure (fn, src)
builtinsImportHandler x = error $ "bad args to built ins import handler " ++ show x

-- todo: get rid of the module name concept, it's not real
moduleNameOf :: ImportSource -> Interpreter String
moduleNameOf (ImportName nm) = pure nm
moduleNameOf (ImportSpecial "file" [nm]) = pure $ dropExtension $ takeFileName nm
moduleNameOf x = error $ "import source not supported " ++ show x

readImportSource :: ImportSource -> Interpreter (String,String)
readImportSource is = case is of
    ImportName nm -> builtinsImportHandler [nm]
    ImportSpecial "file" [nm] -> fileImportHandler [nm]
    _ -> error $ "unsupported import source: " ++ show is

------------------------------------------------------------------------------

-- module loading support

ensureModuleLoaded :: ImportSource -> Interpreter String
ensureModuleLoaded is = do
    modRec <- interp $ makeDotPathExpr ["_system", "modules"]
    moduleName <- moduleNameOf is
    case modRec of
        VariantV tg "record" fs
            | tg == bootstrapType "Record"
            , moduleName `notElem` map fst fs -> do
                --liftIO $ putStrLn $ "loading module: " ++ moduleFile
                (fn,src) <- readImportSource is
                loadAndRunModuleSource True moduleName fn src
                pure moduleName
            | tg == bootstrapType "Record"
            , otherwise -> pure moduleName
        _ -> _errorWithCallStack $ "_system.modules is not a record??"

-- bootstrap is a special built in module which carefully creates
-- the minimal in language infrastructure for the language itself to work
-- a big reason is then to be able to use any part of the language
-- in the _internals module which is used to bootstrap the built in modules
-- that uses use, including globals

initBootstrapModule :: Interpreter ()
initBootstrapModule = runModule "BOOTSTRAP" "_bootstrap" $ do
    mapM_ (uncurry (letValue' NoShadow))
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
    localBindings (\_ -> pure []) $ do
        f
        localModuleEnv <- askLocallyCreatedBindings
        allModuleEnv <- askIncludedBindings
        -- get the pis and make a record from the env using them
        pis <- (\case [] -> [ProvideAll]
                      x -> x) <$> askScriptProvides
        moduleRecord <- VariantV (bootstrapType "Record") "record"
                        <$> aliasSomething localModuleEnv allModuleEnv pis
        modifyAddModule filename moduleName moduleRecord

loadAndRunModuleSource :: Bool -> String -> FilePath -> String -> Interpreter ()
loadAndRunModuleSource includeGlobals moduleName fn src = do
    -- auto include globals, revisit when use context added
    let incG = if includeGlobals && moduleName /= "globals"
               then (Include (ImportName "globals") :)
               else id
    let p = if map toLower (takeExtension fn) == ".rst"
            then parseLiterateScript
            else parseScript
    Script ast' <- either error id <$> useSource (Just fn) moduleName src p
    localModule moduleName (Just fn) $ localX $ do
        let ast = incG ast'
        -- todo: track generated unique names better?
        runModule fn moduleName $ void $ interpStatements ast
  where
    localX f = do
        x1 <- liftIO $ newIORef 0
        local (\x -> x {tlNextAnonCheckblockNum = x1}) f

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


letValue' :: Shadow -> String -> Value -> Interpreter ()
letValue' s nm v = modifyBindings (extendBindings' [(s,nm,v)])

-- used for prelude statement support
-- so the system can tell what was declared locally, and what was
-- imported
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
        SetVar (Iden nm) $ (case ty of
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
matchBindingOrError :: Binding -> Value -> Interpreter [(Shadow,String,Value)]
matchBindingOrError b v = do
    x <- matchBindingMaybe False b v
    case x of
        Nothing -> error $ "value doesn't match binding: " ++ show v ++ ", " ++ show b
        Just y -> pure y

-- bool is if this is a variant context, so treat a namebinding
-- as a unqualified zero arg variant match
-- needs some design work
matchBindingMaybe :: Bool -> Binding -> Value -> Interpreter (Maybe [(Shadow,String,Value)])
-- todo: will turn it to wildcard in the parser
matchBindingMaybe _ (WildcardBinding) _ = pure $ Just []


-- temp until boolean becomes an actual agdt
matchBindingMaybe _ (NameBinding "true") (BoolV True) = pure $ Just []
matchBindingMaybe _ (NameBinding "true") _ = pure Nothing
matchBindingMaybe _ (NameBinding "false") (BoolV False) = pure $ Just []
matchBindingMaybe _ (NameBinding "false") _ = pure Nothing

matchBindingMaybe _ (ShadowBinding nm) v = do
    pure $ Just [(Shadow,nm,v)]

matchBindingMaybe True n@(NameBinding nm) v = do
    -- try to match a 0 arg variant
    x <- matchBindingMaybe False (VariantBinding [nm] []) v
    case x of
        Just {} -> pure x
        -- no matching 0 arg variant, match it as a binding
        -- as long as the name doesn't match a 0 arg variant in scope
        -- how will this interact with shadowing? does shadowing cover it?
        -- is it mainly about what error message you get - shadowing, or
        -- wrong number of args to ctor
        
        Nothing -> do
            vn <- isVariantName
            if vn
                then pure Nothing
                else matchBindingMaybe False n v
  where
    isVariantName = do
        mv <- lookupBinding $ "_casepattern-" ++ nm
        case mv of
            Just {} -> pure True
            _ -> pure False
        
        
matchBindingMaybe False (NameBinding nm) v = do
    pure $ Just [(NoShadow, nm,v)]

matchBindingMaybe _ (VariantBinding cnm bs) (VariantV tg vnm fs) = do
    let s = prefixLast "_casepattern-" cnm
    caseName <- catchAll $ interp $ makeDotPathExpr s
    case caseName of
            -- hack because there's an ambiguity in the ast over zero arg
            -- variants and new names
            Left {} -> pure Nothing
            Right (FFIValue _ffitag fgt) | Just (ptg, pnm) <- fromDynamic fgt -> do
                if ptg == tg && pnm == vnm
                then if length bs == length fs
                     then do
                         newbs <- zipWithM (matchBindingMaybe True) bs $ map snd fs
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

matchBindingMaybe c (AsBinding b s nm) v = do
    r <- matchBindingMaybe c b v
    pure $ case r of
        Nothing -> Nothing
        Just cs -> Just ((s,nm,v) : cs)

matchBindingMaybe _ (TupleBinding bs) (VariantV tg "tuple" fs)
    | tg == bootstrapType "Tuple"
    , length bs == length fs = {-trace "got here" $-} do
          xs <- zipWithM (matchBindingMaybe True) bs $ map snd fs
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
