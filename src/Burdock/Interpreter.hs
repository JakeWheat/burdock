
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Burdock.Interpreter
    (TestResult(..)
    ,CheckBlockResult(..)
    ,getTestResults

    --,Value
    ,valueToString

    ,newHandle
    ,Handle
    ,runScript
    ,evalExpr
    ,evalFun

    -- temp for ffi
    ,InterpreterException
    ,formatException
    ,Interpreter
    ,Value(..)
    ,addFFI

    ) where

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            )

import Control.Monad (forM_
                     ,void
                     ,when
                     )

import Data.List (intercalate
                 ,sortOn
                 ,partition
                 )

import Data.IORef (IORef
                  ,newIORef
                  ,readIORef
                  ,modifyIORef
                  ,writeIORef)

import Burdock.Scientific
import Burdock.Syntax
import Burdock.Pretty
import Burdock.Parse (parseScript, parseExpr)

import Control.Exception.Safe (catch
                              --,SomeException
                              ,Exception
                              ,throwM
                              ,catchAny
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
    ,(</>)
    ,(<.>)
    )

import Data.Dynamic (Dynamic
                    ,toDyn
                    ,fromDynamic
                    )

import Data.Char (isSpace)

import System.IO.Unsafe (unsafePerformIO)

import Text.Show.Pretty (ppShow)

--import Debug.Trace (trace)

------------------------------------------------------------------------------

-- public api

-- newHandle below with the rest of handle stuff

runScript :: Handle
          -> Maybe FilePath
          -> [(String,Value)]
          -> String
          -> IO Value
runScript h mfn lenv src = runInterp h $ do
    (fn, Script ast) <- either error id <$> iParseScript mfn src
    -- todo: how to make this local to this call only
    forM_ lenv $ \(n,v) -> letValue n v
    ret <- localTestInfo (\cti -> cti {tiCurrentSource = fn}) $
           interpStatements ast
    printTestResults <- interp $ internalsRef "auto-print-test-results"
    case printTestResults of
        BoolV True -> do
            trs <- getTestResultsI
            when (not $ null trs) $
                liftIO $ putStrLn $ formatTestResults trs
        BoolV False -> pure ()
        x -> error $ "bad value in auto-print-test-results (should be boolv): " ++ show x
    pure ret

evalExpr :: Handle
         -> Maybe FilePath
         -> [(String,Value)]
         -> String
         -> IO Value
evalExpr h mfn lenv src = runInterp h $ do
    let ast = either error id $ parseExpr (maybe "" id mfn) src
    -- todo: how to make this local to this call only
    forM_ lenv $ \(n,v) -> letValue n v
    interp ast

evalFun :: Handle
        -> String 
        -> [Value]
        -> IO Value
evalFun h fun args = runInterp h $ do
    let ast = either error id $ parseExpr "" fun
    f <- interpStatements [StmtExpr ast]
    let as = zipWith (\i x -> ("aaa-" ++ show i, x)) [(0::Int)..] args
        src' = "fff(" ++ intercalate "," (map fst as) ++ ")"
        ast' = either error id $ parseExpr "" src'
    forM_ (("fff", f):as) $ \(n,v) -> letValue n v
    interp ast'


addFFI :: Handle -> [(String, [Value] -> Interpreter Value)] -> IO ()
addFFI h ffis = runInterp h $ do
    st <- ask
    liftIO $ modifyIORef (isForeignFunctions st) (ffis ++)

formatException :: Handle -> Bool -> InterpreterException -> IO String
formatException h includeCallstack e = runInterp h $ formatExceptionI includeCallstack e

---------------------------------------

-- testing, uses haskell values atm, will move to burdock
-- values at some point in the future

data TestResult = TestPass String
                | TestFail String String
                deriving Show

data CheckBlockResult = CheckBlockResult String [TestResult]
                      deriving Show

getTestResults :: Handle -> IO [(String, [CheckBlockResult])]
getTestResults h = runInterp h getTestResultsI

getTestResultsI :: Interpreter [(String, [CheckBlockResult])]
getTestResultsI = do
    st <- ask
    v <- liftIO (reverse <$> readIORef (isTestResults st))
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
           | FunV [String] Expr Env
           | ForeignFunV String
           | VariantV String [(String,Value)]
           | BoxV (IORef Value)
           | FFIValue Dynamic

-- todo: revert back to automatically derived show
-- and use torepr' everywhere you want a readable value that
-- isn't the haskell ast syntax
instance Show Value where
  show (NumV n) = "NumV " ++ show n
  show (TextV n) = "TextV " ++ show n
  show (BoolV n) = "BoolV " ++ show n
  show (VariantV nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
  show (FunV as bdy _env) = "FunV " ++ show as ++ "\n" ++ prettyExpr bdy
  show (ForeignFunV n) = "ForeignFunV " ++ show n
  show (BoxV _n) = "BoxV XX" -- ++ show n
  show (FFIValue _v) = "FFIValue"


-- needs some work
-- not even sure value should have an eq instance
-- because comparing values is not a pure operation in general
-- so having an eq instance is surely very error prone
instance Eq Value where
    NumV a == NumV b = a == b
    BoolV a == BoolV b = a == b
    TextV a == TextV b = a == b
    VariantV "record" as == VariantV "record" bs =
        sortOn fst as == sortOn fst bs
    VariantV nm fs == VariantV lm gs = (nm,fs) == (lm,gs)
    ForeignFunV x == ForeignFunV y = x == y
    -- todo: funv, boxv ..., ffivalue
    -- ioref has an eq instance for pointer equality
    --    this is very useful
    --    pyret has some other concepts of equality
    --    not sure if should write alternative equals in the right monad
    --    and try to stick to them
    _ == _ = False

valueToString :: Value -> IO (Maybe String)
valueToString v = case v of
    VariantV "nothing" [] -> pure $ Nothing
    _ -> Just <$> torepr' v

-- temp?
nothing :: Value
nothing = VariantV "nothing" []

------------------------------------------------------------------------------

-- environment, interpreter state, intepreter monad helper functions


{-

system env: contains the bindings for the loaded modules and system
support internals

script env: contains the bindings for the runscript of the handle

this split allows modules to load without reference to any of the
runscript bindings as they should
and allows persistence of bindings in the runscript context between
  calls -> especially needed for the repl

there's no straightforward way to query only one of them
typically, you'll create a localscriptenv which replaces the scriptenv
  when loading modules to hide the runscript bindings

keeps the foreign functions separate, so that the main env can easily
be show completely, not sure if this is worth this kind of split.
it also allows two pointers to the same foreign function to be compared,
which wouldn't be the case without the indirection

todo: namespace the foreign functions when there's an api for them:
  want to support the idea that two files from two different people,
  that coincidentally use the same name for a foreign function id,
  can still be run together without having to edit one

-}

type Env = [(String, Value)]
type ForeignFunctions = [(String, [Value] -> Interpreter Value)]

data InterpreterState =
    InterpreterState {isSystemEnv :: IORef Env
                     ,isScriptEnv :: IORef Env
                     ,isProvides :: IORef [ProvideItem]
                     ,isTestInfo :: IORef TestInfo
                     ,isTestResults :: IORef [(String,String,TestResult)]
                     ,isForeignFunctions :: IORef ForeignFunctions
                     ,isCallStack :: CallStack
                     ,isSourceCache :: IORef [(FilePath,String)]
                     ,isUniqueSourceCtr :: IORef Int
                     }
type Interpreter = ReaderT InterpreterState IO

-- it's a bit crap that have variables nested within variables
-- but it seems like the least worst option for now
-- it uses reader + ioref because this is more straightforward
-- that using rwst/state
-- but then it needs an extra ioref so that you can preserve the
-- handle state conveniently in the handle wrapper when you
-- run api functions
data Handle = Handle (IORef InterpreterState)


type CallStack = [SourcePosition]

data InterpreterException = InterpreterException CallStack String
                          | ValueException CallStack Value

instance Show InterpreterException where
    show (InterpreterException _ s) = s
    -- todo: create a pure version of torepr' just to use here
    show (ValueException _ v) = unsafePerformIO $ torepr' v

instance Exception InterpreterException where

interpreterExceptionToValue :: InterpreterException -> Value
interpreterExceptionToValue (InterpreterException _ s) = TextV s
interpreterExceptionToValue (ValueException _ v) = v


---------------------------------------

-- interpreter state functions


emptyEnv :: Env
emptyEnv = []

extendEnv :: [(String,Value)] -> Env -> Env
extendEnv bs env = bs ++ env

showState :: InterpreterState -> IO String
showState s = do
    e1 <- readIORef $ isSystemEnv s
    e2 <- readIORef $ isScriptEnv s
    p1 <- showTab "System" e1
    p2 <- showTab "User" e2
    pure $ p1 ++ "\n" ++ p2
  where
    showTab t vs = do
        ps <- mapM (\(n,v) -> ((n ++ " = ") ++) <$> torepr' v) vs
        pure $ t ++ "\n" ++ unlines ps 

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

    
localScriptEnv :: (Env -> Env) -> Interpreter a -> Interpreter a
localScriptEnv m f = do
    st <- ask
    e' <- liftIO $ m <$> readIORef (isScriptEnv st)
    e1 <- liftIO $ newIORef e'
    p <- liftIO $ newIORef []
    local (const $ st {isScriptEnv = e1
                      ,isProvides = p}) f

localTestInfo :: (TestInfo -> TestInfo) -> Interpreter a -> Interpreter a
localTestInfo m f = do
    st <- ask
    ti <- liftIO $ m <$> readIORef (isTestInfo st)
    ti' <- liftIO $ newIORef ti
    local (const $ st {isTestInfo = ti'}) f

    
askEnv :: Interpreter Env
askEnv = do
    st <- ask
    e1 <- liftIO $ readIORef $ isSystemEnv st
    e2 <- liftIO $ readIORef $ isScriptEnv st
    pure $ e1 ++ e2

askScriptEnv :: Interpreter Env
askScriptEnv =  do
    st <- ask
    liftIO $ readIORef $ isScriptEnv st

lookupEnv :: String -> Interpreter (Maybe Value)
lookupEnv k = do
    e <- askEnv
    --liftIO $ putStrLn $ "-------------\n" ++ ppShow e ++ "\n-------------"
    pure $ lookup k e 

modifyScriptEnv :: (Env -> Env) -> Interpreter ()
modifyScriptEnv f = do
    st <- ask
    liftIO $ modifyIORef (isScriptEnv st) f

modifyScriptProvides :: ([ProvideItem] -> [ProvideItem]) -> Interpreter ()
modifyScriptProvides f = do
    st <- ask
    liftIO $ modifyIORef (isProvides st) f

askScriptProvides :: Interpreter [ProvideItem]
askScriptProvides = do
    st <- ask
    liftIO $ readIORef (isProvides st)

askFF :: Interpreter ForeignFunctions
askFF = do
    st <- ask
    liftIO $ readIORef $ isForeignFunctions st

-- saves a test result to the interpreter state
addTestResult :: String -> String -> TestResult -> Interpreter ()
addTestResult moduleName checkBlockName tr = do
    st <- ask
    liftIO $ modifyIORef (isTestResults st) ((moduleName,checkBlockName,tr):)

-- the state that the testing framework needs to track in the interpreter
data TestInfo
    = TestInfo
    {tiCurrentSource :: String
    ,tiCurrentCheckblock :: Maybe String
    ,tiNextAnonCheckblockNum :: Int
    }

readCallStack :: Interpreter CallStack
readCallStack = do
    st <- ask
    pure $ isCallStack st

emptyInterpreterState :: IO InterpreterState
emptyInterpreterState = do
    sysenv <- newIORef emptyEnv
    scenv <- newIORef emptyEnv
    provs <- newIORef []
    ti <- newIORef $ TestInfo
        {tiCurrentSource = "runScript-api"
        ,tiCurrentCheckblock = Nothing
        ,tiNextAnonCheckblockNum = 1
        }
    trs <- newIORef []
    ffs <- newIORef []
    sc <- newIORef []
    ctr <- newIORef 0
    pure $ InterpreterState sysenv scenv provs ti trs ffs [] sc ctr

---------------------------------------

-- new handles, running interpreter functions using a handle

baseEnv :: [(String,Value)]
baseEnv =
    [("_system", VariantV "record" [("modules", VariantV "record" [])])

    -- come back to dealing with these properly when decide how to
    -- do ad hoc polymorphism
    -- but definitely also want a way to refer to these as regular
    -- values in the syntax like haskell does (==)
    -- so they can be passed as function values, and appear in
    -- provides, include lists

    ,("==", ForeignFunV "==")
    ,("+", ForeignFunV "+")
    ,("-", ForeignFunV "-")
    ,("*", ForeignFunV "*")

    ]


newHandle :: IO Handle
newHandle = do
    -- create a completely empty state
    s <- emptyInterpreterState
    -- add the built in ffi functions
    modifyIORef (isForeignFunctions s) (builtInFF ++)
    -- bootstrap the getffivalue function and other initial values
    -- including the _system thing
    -- temp: this is what will mostly be factored into _internals and globals
    modifyIORef (isSystemEnv s) (baseEnv ++)
    h <- Handle <$> newIORef s
    runInterp h $ do
        initBootstrapModule
        internalsPth <- liftIO $ getBuiltInModulePath "_internals"
        loadModule False "_internals" internalsPth
        -- for now, include globals module, later this will depend
        -- on if a use context is used
        -- todo: write some tests to show the system works when you don't load
        -- the globals module explicitly or via include globals
        void $ interpStatement (Include (ImportName "globals"))
    pure h
    


runInterp :: Handle -> Interpreter a -> IO a
runInterp (Handle h) f = do
    h' <- readIORef h
    (v,h'') <- runReaderT (do
          res <- f
          st <- ask
          pure (res,st)) h'
    writeIORef h h''
    pure v


---------------------------------------

-- other interpreter monad support functions

-- convert a burdock either value to a haskell one

bToHEither :: Value -> Either Value Value
bToHEither (VariantV "left" [(_,e)]) = Left e
bToHEither (VariantV "right" [(_,r)]) = Right r
bToHEither x = error $ "non either passed to btoheither: " ++ show x


formatExceptionI :: Bool -> InterpreterException -> Interpreter String
formatExceptionI includeCallstack e = do
    (st,m) <- case e of
            ValueException st (TextV m) -> pure (st,m)
            ValueException st m -> (st,) <$> liftIO (torepr' m)
            InterpreterException st m -> pure (st,m)
    stf <- if includeCallstack
           then ("\ncall stack:\n" ++) <$> formatCallStack st
           else pure ""
    pure $ m ++ stf

-- partial idea -> save sources as4 they are parsed, so can refer to them
-- in later error messages and stuff
-- todo: track original filenames plus generated unique filenames better
-- something robust will track repeated loads of the same file
-- which changes each load
-- and be able to identify which version of that source a particular
-- call stack refers to
-- can think about generating generated names specific to the api, and
-- to the repl (e.g. repl-line-x for repl instead of unknown-arbitrary-ctr)
iParseScript :: Maybe FilePath -> String -> Interpreter (Either String (FilePath, Script))
iParseScript mfn src = do
    fn <- case mfn of
              Nothing -> uniqueSourceName
              Just x -> pure x
    st <- ask
    liftIO $ modifyIORef (isSourceCache st) ((fn,src):)
    pure $ (fn,) <$> parseScript fn src

-- needs some checking
uniqueSourceName :: Interpreter String
uniqueSourceName = do
    st <- ask
    i <- liftIO $ readIORef (isUniqueSourceCtr st)
    liftIO $ writeIORef (isUniqueSourceCtr st) (i + 1)
    pure $ "unknown-" ++ show i
    
formatCallStack :: CallStack -> Interpreter String
formatCallStack cs = do
    st <- ask
    sc <- liftIO $ readIORef (isSourceCache st)
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


---------------------------------------

-- converting values to strings

torepr' :: Value -> IO String
torepr' x = show <$> toreprx x

toreprx :: Value -> IO (P.Doc a)
toreprx (NumV n) = pure $ case extractInt n of
                             Just x -> P.pretty $ show x
                             Nothing ->  P.pretty $ show n
toreprx (BoolV n) = pure $ P.pretty $ if n then "true" else "false"
toreprx (FunV {}) = pure $ P.pretty "<Function>" -- todo: print the function value
toreprx (ForeignFunV f) = pure $ P.pretty $ "<ForeignFunction:" ++ f ++ ">"
toreprx (TextV s) = pure $ P.pretty $ "\"" ++ s ++ "\""
toreprx (BoxV b) = do
    bv <- readIORef b
    v <- toreprx bv
    pure $ P.pretty "<Box:" <> v <> P.pretty ">"

toreprx (VariantV "tuple" fs) = do
    vs <- mapM (toreprx . snd) fs
    pure $ P.pretty "{" <> P.nest 2 (xSep ";" vs) <> P.pretty "}"
toreprx (VariantV "record" fs) = do
    vs <- mapM f fs
    pure $ P.pretty "{" <> P.nest 2 (xSep "," vs) <> P.pretty "}"
  where
    f (a,b) = ((P.pretty a P.<+> P.pretty "=") P.<+>) <$> toreprx b
toreprx (VariantV nm []) = pure $ P.pretty nm
toreprx (VariantV nm fs) = do
    vs <- mapM (toreprx . snd) fs
    pure $ P.pretty nm <> P.pretty "(" <> P.nest 2 (xSep "," vs) <> P.pretty ")"

toreprx (FFIValue {}) = pure $ P.pretty "<ffi-value>"

xSep :: String -> [P.Doc a] -> P.Doc a
xSep x ds = P.sep $ P.punctuate (P.pretty x) ds

------------------------------------------------------------------------------

-- built in functions implemented in haskell:

builtInFF :: [(String, [Value] -> Interpreter Value)]
builtInFF =
    [("get-ffi-value", getFFIValue)
    ,("==", \[a,b] -> pure $ BoolV $ a == b)
    ,("+", \[NumV a,NumV b] -> pure $ NumV $ a + b)
    ,("-", \[NumV a,NumV b] -> pure $ NumV $ a - b)
    ,("*", \[NumV a,NumV b] -> pure $ NumV $ a * b)

    ,("make-variant", makeVariant)
    ,("is-variant", isVariant)

    ,("load-module", bLoadModule)
    ,("show-handle-state", bShowHandleState)
    ,("haskell-error", haskellError)

    ,("is-nothing", isNothing)
    ,("is-Nothing", isNothing)

    ,("is-tuple", isTuple)
    ,("is-record", isRecord)

    ,("print", bPrint)
    ,("torepr", torepr)
    ,("raise", raise)

    ,("parse-file", bParseFile)
    ,("show-haskell-ast", bShowHaskellAst)
    ,("get-call-stack", getCallStack)
    ,("format-call-stack", bFormatCallStack)
    ]

getFFIValue :: [Value] -> Interpreter Value
getFFIValue [TextV nm] = do
    -- todo: check the value exists in the ffi catalog
    pure $ ForeignFunV nm
getFFIValue _ = error "wrong args to get-ffi-value"
    

makeVariant :: [Value] -> Interpreter Value
makeVariant (TextV nm : as) =
    pure $ VariantV nm (f as)
  where
    f [] = []
    f (TextV fnm : v : as') = (fnm,v): f as'
    f x = error $ "wrong args to make-variant: " ++ show x
makeVariant x = error $ "wrong args to make-variant: " ++ show x

isVariant :: [Value] -> Interpreter Value
isVariant [TextV nm, VariantV vt _] = pure $ BoolV $ nm == vt
isVariant [TextV _nm, _] = pure $ BoolV False
isVariant _ = error $ "wrong args to is-variant"

isNothing :: [Value] -> Interpreter Value
isNothing [VariantV "nothing" _] = pure $ BoolV True
isNothing [_] = pure $ BoolV False
isNothing _ = error $ "wrong args to is-nothing/is-Nothing"

isTuple :: [Value] -> Interpreter Value
isTuple [VariantV "tuple" _] = pure $ BoolV True
isTuple [_] = pure $ BoolV False
isTuple _ = error $ "wrong args to is-tuple"

isRecord :: [Value] -> Interpreter Value
isRecord [VariantV "record" _] = pure $ BoolV True
isRecord [_] = pure $ BoolV False
isRecord _ = error $ "wrong args to is-record"


bPrint :: [Value] -> Interpreter Value
bPrint [v] = do
    v' <- case v of
              TextV s -> pure s
              _ -> liftIO $ torepr' v
    liftIO $ putStrLn v'
    pure nothing

bPrint _ = error $ "wrong number of args to print"

-- load module at filepath
bLoadModule :: [Value] -> Interpreter Value
bLoadModule [TextV moduleName, TextV fn] = do
    loadModule True moduleName fn
    pure nothing
bLoadModule _ = error $ "wrong args to load module"

bShowHandleState :: [Value] -> Interpreter Value
bShowHandleState [] = do
    st <- ask
    v <- liftIO $ showState st
    pure $ TextV v
    
bShowHandleState _ = error $ "wrong args to show-handle-state"

torepr :: [Value] -> Interpreter Value
torepr [x] = TextV <$> liftIO (torepr' x)
torepr _ = error "wrong number of args to torepr"


raise :: [Value] -> Interpreter Value
raise [v] = do
    cs <- readCallStack
    throwM $ ValueException cs v
raise _ = error "wrong args to raise"

haskellError :: [Value] -> Interpreter Value
haskellError [TextV v] = error v
haskellError _ = error $ "wrong args to haskell-error"

bParseFile :: [Value] -> Interpreter Value
bParseFile [TextV fn] = do
    src <- liftIO $ readFile fn
    let ast = either error id $ parseScript fn src
    pure $ FFIValue $ toDyn $ ast
    
bParseFile _ = error $ "wrong args to parse-file"

bShowHaskellAst :: [Value] -> Interpreter Value
bShowHaskellAst [FFIValue v] = do
    let ast :: Script
        Just ast = fromDynamic v
    pure $ TextV $ ppShow ast
bShowHaskellAst _ = error $ "wrong args to show-haskell-ast"
    
getCallStack :: [Value] -> Interpreter Value
getCallStack [] = (FFIValue . toDyn) <$> readCallStack
getCallStack _ = error $ "wrong args to get-call-stack"

bFormatCallStack :: [Value] -> Interpreter Value
bFormatCallStack [FFIValue cs] = do
    let Just hcs = fromDynamic cs
    TextV <$> formatCallStack hcs
bFormatCallStack _ = error $ "wrong args to format-call-stack"



------------------------------------------------------------------------------

-- the interpreter itself

interp :: Expr -> Interpreter Value
--interp x | trace ("trace: "  ++ prettyExpr x) False = undefined
interp (Num n) = pure $ NumV n
interp (Text s) = pure $ TextV s
interp (Parens e) = interp e
interp (Iden a) = do
    mv <- lookupEnv a
    case mv of
        Just (BoxV vr) -> liftIO $ readIORef vr
        Just v -> pure v
        Nothing -> error $ "identifier not found: " ++ a

interp (App sp f es) = do
    fv <- interp f
    -- special case apps
    if fv == ForeignFunV "catch-as-either"
        then do
            -- todo: maintain call stack
            catchAsEither es
        else do
            -- TODO: refactor to check the value of fv before
            -- evaluating the es?
            vs <- mapM interp es
            local (\m -> m { isCallStack = sp : isCallStack m}) $ app fv vs

-- special case binops
interp (BinOp _ "is" _) = error $ "'is' test predicate only allowed in check block"

interp (BinOp e0 "and" e1) = do
    x <- interp e0
    case x of
        BoolV False -> pure x
        BoolV True -> interp e1
        _ -> error $ "bad value type to 'and' operator: " ++ show x

interp (BinOp e0 "or" e1) = do
    x <- interp e0
    case x of
        BoolV True -> pure x
        BoolV False -> interp e1
        _ -> error $ "bad value type to 'or' operator: " ++ show x

interp (BinOp e0 op e1) = do
    opv <- interp $ Iden op
    v0 <- interp e0
    v1 <- interp e1
    app opv [v0,v1]

interp (Lam ps e) = do
    env <- askEnv
    pure $ FunV (map unPat ps) e env

interp (Let bs e) = do
    let newEnv [] = interp e
        newEnv ((b,ex):bs') = do
            v <- interp ex
            localScriptEnv (extendEnv [(unPat b,v)]) $ newEnv bs'
    newEnv bs

interp (Block ss) =
    localScriptEnv id $ interpStatements ss

interp (If bs e) = do
    let f ((c,t):bs') = do
            c' <- interp c
            case c' of
                BoolV True -> interp t
                BoolV False -> f bs'
                _ -> error $ "throwExpectedType 'Boolean'" ++ show c'
        f [] = case e of
                   Just x -> interp x
                   Nothing -> error "NoBranchesSatisfied"
    f bs

interp (LetRec bs e) =
    let sts = doLetRec bs
    in interp $ Block (sts ++ [StmtExpr e])

interp (DotExpr e f) = do
    v <- interp e
    case v of
        VariantV _ fs | Just fv <- lookup f fs ->
                            -- not quite sure about this?
                            -- it's needed for referencing vars in a module
                            -- (including fun which is desugared to a var)
                            case fv of
                                BoxV vr -> liftIO $ readIORef vr
                                _ -> pure fv
                      | otherwise -> error $ "field not found in dotexpr " ++ show v ++ " . " ++ f
                                             ++ "\nfields are: " ++ show fs
        _ -> error $ "dot called on non variant: " ++ show v


{-
run through each branch
if the variant tag in the value matches the variant tag in the pattern
run that branch
if there are members with names, bind these before running the branch

-}
interp (Cases _ty e cs els) = do
    v <- interp e
    matchb v cs
  where
    matchb v ((p,ce) : cs') = do
        x <- matches p v ce
        case x of
            Just f -> f
            Nothing -> matchb v cs'
    matchb v [] = case els of
                      Just ee -> interp ee
                      Nothing -> error $ "no cases match and no else " ++ show v ++ " " ++ show cs
    matches (CaseBinding s nms) v ce = doMatch s nms v ce
    doMatch s' nms (VariantV tag fs) ce = do
        -- todo: lookup the full path
        let s = last s'
        pat <- interp (Iden $ "_pattern-" ++ s)
        case pat of
            TextV nm ->
                if nm == tag
                then let letvs = zipWith (\(NameBinding _ n _) (_,v) -> (n,v)) nms fs
                     in pure $ Just $ localScriptEnv (extendEnv letvs) $ interp ce
                else pure Nothing
            _ -> error $ "pattern lookup returned " ++ show pat
    doMatch _ _ _ _ = pure Nothing

interp (TupleSel es) = do
    vs <- mapM interp es
    pure $ VariantV "tuple" $ zipWith (\n v -> (show n, v)) [(0::Int)..] vs

interp (RecordSel fs) = do
    vs <- mapM (\(n,e) -> (n,) <$> interp e) fs
    pure $ VariantV "record" vs

interp (TupleGet e f) = do
    v <- interp e
    case v of
        VariantV "tuple" fs ->
            maybe (error $ "tuple field not found: " ++ show f ++ ", " ++ show v) pure
                 $ lookup (show f) fs
        _ -> error $ "tuple get called on non tuple value: " ++ show v

interp (Construct (Iden "list") es) = do
    vs <- mapM interp es
    pure $ makeBList vs

interp (Construct {}) = error "todo: construct for non lists"

interp (TypeSel {}) = error "todo: typeselector"

makeBList :: [Value] -> Value
makeBList [] = VariantV "empty" []
makeBList (x:xs) = VariantV "link" [("first", x),("rest", makeBList xs)]



unPat :: Binding -> String
unPat (NameBinding _ nm _) = nm

app :: Value -> [Value] -> Interpreter Value
app fv vs =
    case fv of
        FunV ps bdy env -> do
            as <- safeZip ps vs
            localScriptEnv (const $ extendEnv as env) $ interp bdy
        ForeignFunV nm -> do
            ffs <- askFF
            case lookup nm ffs of
                Just f -> f vs
                Nothing -> error $ "internal error, foreign function not found: " ++ nm
        _ -> error $ "app called on non function value: " ++ show fv
  where
    safeZip ps xs | length ps == length xs  = pure $ zip ps xs
                  | otherwise = error $ "wrong number of args: " ++ show ps ++ ", " ++ show xs

catchAsEither :: [Expr] -> Interpreter Value
catchAsEither [e] = do
    v0 <- ca $ catchit (interp e)
    let (f,v1) = case v0 of
            Right v -> (internalsRef "right", v)
            Left er -> (internalsRef "left", er)
    fv <- interp f
    app fv [v1]
  where
    catchit :: Interpreter Value -> Interpreter (Either Value Value)
    catchit f = catch (Right <$> f) iToV
    iToV = pure . Left . interpreterExceptionToValue
    -- catch any haskell exception, for dealing with error and undefined
    -- not sure about this, but definitely wanted for testing
    ca f = catchAny f (pure . Left . TextV . show)
catchAsEither _ = error $ "wrong args to catch as either"

{-
errorWithCallStack :: String -> Interpreter a
errorWithCallStack msg = do
    cs <- readCallStack
    csf <- formatCallStack cs
    error $ msg ++ "\nCALLSTACK:\n" ++ csf ++ "\n-------"
-}

---------------------------------------


interpStatements :: [Stmt] -> Interpreter Value

-- gather adjacent fun and rec can be mutually recursive
-- to desugar all together
interpStatements ss | (recbs@(_:_),chks, ss') <- getRecs [] [] ss = do
    interpStatements (doLetRec recbs ++ chks ++ ss')
  where
    getRecs accdecls accchks (RecDecl nm bdy : ss') = getRecs ((nm,bdy):accdecls) accchks ss'
    getRecs accdecls accchks (FunDecl nm args bdy whr : ss') =
        let accchks' = maybe accchks (\w -> Check (Just $ unPat nm) w : accchks) whr
        in getRecs ((nm, Lam args bdy):accdecls) accchks' ss'
    getRecs accdecls accchks ss' = (reverse accdecls, reverse accchks, ss')

-- the interpreter for all other statements don't need access to the
-- statement list so they delegate to interpStatement

-- return value if it's the last statement
interpStatements [s] = interpStatement s
interpStatements (s:ss) = interpStatement s >> interpStatements ss
interpStatements [] = pure $ VariantV "nothing" []


interpStatement :: Stmt -> Interpreter Value

-- interpStatement x | trace ("interp: " ++ show x) False = undefined

interpStatement (RecDecl {}) = error $ "internal error, rec decl not captured by letrec desugaring"
interpStatement (FunDecl {}) = error $ "internal error, fun decl not captured by letrec desugaring"

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

interpStatement s@(StmtExpr (BinOp e0 "is" e1)) = do
    v0 <- bToHEither <$> catchAsEither [e0]
    v1 <- bToHEither <$> catchAsEither [e1]
    st <- ask
    ti <- liftIO $ readIORef (isTestInfo st)
    let cbn = maybe (error "is precated not in check block")
              id $ tiCurrentCheckblock ti
    let atr = addTestResult (tiCurrentSource ti) cbn

    case (v0,v1) of
        (Right v0', Right v1') ->
            if v0' == v1'
            then atr $ TestPass msg
            else do
                p0 <- liftIO $ torepr' v0'
                p1 <- liftIO $ torepr' v1'
                atr $ TestFail msg (p0 ++ "\n!=\n" ++ p1)
        (Left er0, Right {}) -> do
            er0' <- liftIO $ torepr' er0
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0')
        (Right {}, Left er1) -> do
            er1' <- liftIO $ torepr' er1
            atr $ TestFail msg (prettyExpr e1 ++ " failed: " ++ er1')
        (Left er0, Left er1) -> do
            er0' <- liftIO $ torepr' er0
            er1' <- liftIO $ torepr' er1
            atr $ TestFail msg (prettyExpr e0 ++ " failed: " ++ er0'
                                ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1')
    pure nothing
  where
    msg = prettyStmt s

interpStatement (StmtExpr e) = interp e
interpStatement (Check mnm ss) = do

    runTestsEnabled <- interp $ internalsRef "auto-run-tests"
    case runTestsEnabled of
        BoolV True -> do
            st <- ask
            ti <- liftIO $ readIORef (isTestInfo st)
            let (cnm, newcbn) = case mnm of
                    Just n -> (n, id)
                    Nothing ->
                        let i = tiNextAnonCheckblockNum ti
                        in ("check-block-" ++ show i
                           ,\cti -> cti {tiNextAnonCheckblockNum = i + 1})
            localTestInfo (\cti -> newcbn $ cti {tiCurrentCheckblock = Just cnm})
                $ interpStatements ss
        BoolV False -> pure nothing
        x -> error $ "auto-run-tests not set right (should be boolv): " ++ show x
    
interpStatement (LetDecl b e) = do
    v <- interp e
    letValue (unPat b) v
    pure nothing

interpStatement (VarDecl b e) = do
    v <- interp e
    vr <- liftIO $ newIORef v
    letValue (unPat b) (BoxV vr)
    pure nothing

interpStatement (SetVar nm e) = do
    mv <- lookupEnv nm
    let vr = case mv of
                 Just (BoxV b) -> b
                 Just x -> error $ "attempt to assign to something which isn't a var: " ++ show x
                 Nothing -> error $ "identifier not found: " ++ nm
    v <- interp e
    liftIO $ writeIORef vr v
    pure nothing

{-

data decl:

an is-x function for each variant
an is-dat function for the data type
a make function for each variant, with the name of the variant, e.g.
pt(...)
a support binding for pattern matching variants, for each variant x:
_pattern-x = ...


-}

interpStatement (DataDecl dnm vs whr) = do
    let makeIs (VariantDecl vnm _) = 
            letDecl ("is-" ++ vnm)
            $ lam ["x"]
            $ App appSourcePos (bootstrapRef "is-variant") [Text vnm, Iden "x"]
        callIs (VariantDecl vnm _) = App appSourcePos (Iden $ "is-" ++ vnm) [Iden "x"]
        makeIsDat =
            letDecl ("is-" ++ dnm)
            $ lam ["x"]
            $ foldl1 orE $ map callIs vs
        chk = maybe [] (\w -> [Check (Just dnm) w]) whr
    interpStatements (concatMap makeV vs ++ map makeIs vs ++ [makeIsDat] ++ chk)
  where
    makeV :: VariantDecl -> [Stmt]
    makeV (VariantDecl vnm fs) =
        [letDecl vnm
         $ (if null fs then id else lam (map snd fs))
         $ App appSourcePos (bootstrapRef "make-variant")
                (Text vnm : concat (map ((\x -> [Text x, Iden x]) . snd) fs))
        ,letDecl ("_pattern-" ++ vnm) $ Text vnm
        ]
    letDecl nm v = LetDecl (NameBinding NoShadow nm Nothing) v
    lam as e = Lam (map (\x -> NameBinding NoShadow x Nothing) as) e
    orE a b = BinOp a "or" b

---------------------------------------

-- prelude statements

{-
import file("file.tea") as X
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
    (modName,fn) <- resolveImportPath [] is
    ensureModuleLoaded modName fn
    as <- aliasModule modName [ProvideAll]
    letValue al $ VariantV "record" as
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
        VariantV "record" fs -> do
            let as = aliasSomething fs pis
            mapM_ (uncurry letValue) as
        _ -> error $ "trying to alias from something that isn't a record: " ++ show v
    pure nothing

{-
include file("file.tea")
include string-dict
include m == include from m: * end
-}

interpStatement (Include is) = do
    (modName,fn) <- resolveImportPath [] is
    ensureModuleLoaded modName fn
    ls <- aliasModule modName [ProvideAll]
    mapM_ (uncurry letValue) ls
    pure nothing

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

internalsRef :: String -> Expr
internalsRef nm = DotExpr (DotExpr (DotExpr (Iden "_system") "modules") "_internals") nm

bootstrapRef :: String -> Expr
bootstrapRef nm = DotExpr (DotExpr (DotExpr (Iden "_system") "modules") "_bootstrap") nm

------------------------------------------------------------------------------

-- module loading support


getBuiltInModulePath :: String -> IO FilePath
getBuiltInModulePath nm =
    -- later will need to use the cabal Paths_ thing and stuff
    pure ("built-ins" </> nm <.> "bur")

resolveImportPath :: [FilePath] -> ImportSource -> Interpreter (String, FilePath)
resolveImportPath _moduleSearchPath is = do
    case is of
        ImportSpecial "file" [fn] ->
            pure (dropExtension $ takeFileName fn, fn)
        ImportSpecial {} -> error "unsupported import"
        -- todo: set this path in a sensible and flexible way
        ImportName nm -> (nm,) <$> liftIO (getBuiltInModulePath nm)

ensureModuleLoaded :: String -> FilePath -> Interpreter ()
ensureModuleLoaded moduleName moduleFile = do
    modRec <- interp (DotExpr (Iden "_system") "modules")
    case modRec of
        VariantV "record" fs
            | moduleName `notElem` map fst fs -> do
                --liftIO $ putStrLn $ "loading module: " ++ moduleFile
                loadModule True moduleName moduleFile
            | otherwise -> pure ()
        _ -> error $ "_system.modules is not a record??"

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
        ,("nothing", VariantV "nothing" [])
        ,("is-nothing", ForeignFunV "is-nothing")
        ,("is-Nothing", ForeignFunV "is-Nothing")
        -- todo: complete the boolean (and nothing?) types
        ,("get-ffi-value", ForeignFunV "get-ffi-value")
        ,("make-variant", ForeignFunV "make-variant")
        ,("is-variant", ForeignFunV "is-variant")
        ]

runModule :: String -> String -> Interpreter () -> Interpreter ()        
runModule filename moduleName f =
    localScriptEnv (const emptyEnv)
        $ localTestInfo (\cti -> cti {tiCurrentSource = filename}) $ do
        f
        moduleEnv <- askScriptEnv
        -- get the pis and make a record from the env using them
        pis <- (\case [] -> [ProvideAll]
                      x -> x) <$> askScriptProvides
        let moduleRecord = VariantV "record" $ aliasSomething moduleEnv pis
            sp = modulePath moduleName
        modifySystemExtendRecord sp moduleRecord

-- todo: replace the includeGlobals flag with use context
loadModule :: Bool -> String -> FilePath -> Interpreter ()
loadModule includeGlobals moduleName fn = do
    src <- liftIO $ readFile fn
    -- auto include globals, revisit when use context added
    let incG = if includeGlobals && moduleName /= "globals"
               then (Include (ImportName "globals") :)
               else id
    (fn',Script ast') <- either error id <$> iParseScript (Just fn) src
    let ast = incG ast'
    runModule fn' moduleName $ void $ interpStatements ast

-- TODO: not really happy with how messy these functions are

-- create the aliases which are letdecls
-- to point the names in the provide items to the internal
-- loaded module/ module member names
aliasModule :: String -> [ProvideItem] -> Interpreter [(String,Value)]
aliasModule modName pis = do
    modEnv <- ex <$> lkp
    pure $ aliasSomething modEnv pis
    -- pure $ concat $ map (apis modEnv) pis
  where
    mk [] = error $ "alias module: empty path"
    mk [p] = Iden p
    mk [r,p] = DotExpr (Iden r) p
    mk [s,r,p] = DotExpr (DotExpr (Iden s) r) p
    mk _ = error $ "todo: aliasmodule, find someone who is better at programming than me"
    ex (VariantV "record" r) = r
    ex x = error $ "aliasmodule: module value is not a record: " ++ show x
    lkp = interp $ mk $ modulePath modName

aliasSomething :: [(String,Value)] -> [ProvideItem] -> [(String,Value)]
aliasSomething rc pis = concat $ map apis pis
  where
    apis ProvideAll = rc
    apis (ProvideAlias k al) = case lookup k rc of
        Nothing -> error $ "provide alias source not found: " ++ k
        Just v -> [(al,v)]
    apis (ProvideName k) = case lookup k rc of
        Nothing -> error $ "provide alias source not found: " ++ k
        Just v -> [(k,v)]

-- says where to find the named module in the system path
-- _system.modules.[nm]
modulePath :: String -> [String]
modulePath nm = ["_system", "modules", nm]

modifySystemExtendRecord :: [String] -> Value -> Interpreter ()
modifySystemExtendRecord pth v = do
    st <- ask
    e <- liftIO $ readIORef (isSystemEnv st)
    -- dodgy code to descend into a path of records until
    -- you find the place to add the new entry
    let f :: [(String,Value)] -> [String] -> [(String,Value)]
        f _ [] = error $ "internal error modifySystemExtendRecord no path"
        f r [p] =
            case lookup p r of
                Just _ -> error $ "internal error: modifySystemExtendRecord extend record field already present: " ++ show pth
                Nothing -> (p,v) : r
        f r (p:ps) =
            case lookup p r of
                Nothing -> error $ "internal error: modifySystemExtendRecord path not found: " ++ show pth ++ " " ++ p
                Just (VariantV "record" s) ->
                    let s' = VariantV "record" $ f s ps
                    in updateEntry p s' r
                Just x -> error $ "internal error: modifySystemExtendRecord non record " ++ show pth ++ ": " ++ show x

    liftIO $ writeIORef (isSystemEnv st) $ f e pth
  where
    updateEntry k u mp = 
        (k,u) : filter ((/= k) . fst) mp
---------------------------------------


letValue :: String -> Value -> Interpreter ()
letValue nm v = do
    modifyScriptEnv (extendEnv [(nm,v)])


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

-}

doLetRec :: [(Binding, Expr)] -> [Stmt]
doLetRec bs = 
    let vars = map makeVar bs
        assigned = map makeAssign bs
    in vars ++ assigned
  where
    makeVar (v,_) = VarDecl v $ Lam []
        $ App appSourcePos (Iden "raise")
            [Text "internal error: uninitialized letrec implementation var"]
    makeAssign (b,v) = SetVar (unPat b) v

-- placeholder to mark the places where need to fix the source
-- position

appSourcePos :: SourcePosition
appSourcePos = Nothing
