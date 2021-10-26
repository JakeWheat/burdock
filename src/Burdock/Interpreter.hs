
{-# LANGUAGE TupleSections #-}
module Burdock.Interpreter
    (TestResult(..)
    ,getTestResults

    ,Value
    ,valueToString

    ,newHandle
    ,Handle
    ,runScript
    ,evalExpr
    ,evalFun

    ) where

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            )

import Control.Monad (forM_)

import Data.List (intercalate
                 ,sortOn)

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
                              ,SomeException)


-- import Text.Show.Pretty (ppShow)

--import Debug.Trace (trace)

------------------------------------------------------------------------------

-- public api

-- newHandle below with the rest of handle stuff

runScript :: Handle
          -> Maybe FilePath
          -> [(String,Value)]
          -> String
          -> IO Value
runScript h mfn lenv src = do
    --hh <- showHandleState h
    --putStrLn $ "before:\n" ++ hh ++ "\n----\n"
    ret <- runInterp h $ do
        let Script ast = either error id $ parseScript (maybe "" id mfn) src
        -- todo: how to make this local to this call only
        forM_ lenv $ \(n,v) -> letValue n v
        interpStatements ast
    --hh1 <- showHandleState h
    --putStrLn $ "after:\n" ++ hh1 ++ "\n----\n"
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

-- temp testing
-- todo: store the test results as burdock values
-- use a function to get these stored test results
-- and a helper to convert to haskell value (if needed)

data TestResult = TestResult String Bool

getTestResults :: Handle -> IO [TestResult]
getTestResults h = runInterp h $ do
    st <- ask
    liftIO (reverse <$> readIORef (isTestResults st))

------------------------------------------------------------------------------

-- runtime language values

data Value = NumV Scientific
           -- could also be a variant
           -- easier like this since it's used a lot
           | BoolV Bool
           | TextV String
           | FunV [String] Expr Env
           | ForeignFunV String
           | VariantV String [(String,Value)]
           | BoxV (IORef Value)

-- todo: the rough idea is to convert values back to syntax
-- and pretty them
instance Show Value where
  show (NumV n) = "NumV " ++ show n
  show (TextV n) = "TextV " ++ show n
  show (BoolV n) = "BoolV " ++ show n
  show (VariantV nm fs) = "VariantV " ++ nm ++ "[" ++ intercalate "," (map show fs) ++ "]"
  show (FunV as bdy _env) = "FunV " ++ show as ++ "\n" ++ prettyExpr bdy
  show (ForeignFunV n) = "ForeignFunV " ++ show n
  show (BoxV _n) = "BoxV XX" -- ++ show n

instance Eq Value where
    NumV a == NumV b = a == b
    BoolV a == BoolV b = a == b
    TextV a == TextV b = a == b
    VariantV "record" as == VariantV "record" bs =
        sortOn fst as == sortOn fst bs
    VariantV nm fs == VariantV lm gs = (nm,fs) == (lm,gs)
    _ == _ = False

-- do instance eq, show

valueToString :: Value -> Maybe String
valueToString v = case v of
    VariantV "nothing" [] -> Nothing
    _ -> Just $ show v -- todo: torepr

-- temp?
nothing :: Value
nothing = VariantV "nothing" []

------------------------------------------------------------------------------

-- environment, interpreter state, intepreter monad helper functions

type Env = [(String, Value)]

type ForeignFunctions = [(String, [Value] -> Interpreter Value)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: [(String,Value)] -> Env -> Env
extendEnv bs env = bs ++ env

data InterpreterState =
    InterpreterState {isEnv :: IORef Env
                     ,isTestResults :: IORef [TestResult]
                     ,isForeignFunctions :: IORef ForeignFunctions
                     }

showState :: InterpreterState -> IO String
showState s = do
    e <- readIORef $ isEnv s
    pure $ show e

localEnv :: (Env -> Env) -> Interpreter a -> Interpreter a
localEnv m f = do
    st <- ask
    e' <- liftIO $ m <$> readIORef (isEnv st)
    e1 <- liftIO $ newIORef e'
    local (const $ st {isEnv = e1}) f

askEnv :: Interpreter Env
askEnv = do
    st <- ask
    liftIO $ readIORef $ isEnv st

lookupEnv :: String -> Interpreter (Maybe Value)
lookupEnv k = do
    st <- ask
    e <- liftIO $ readIORef (isEnv st)
    --liftIO $ putStrLn $ "-------------\n" ++ ppShow e ++ "\n-------------"
    pure $ lookup k e 

modifyEnv :: (Env -> Env) -> Interpreter ()
modifyEnv f = do
    st <- ask
    liftIO $ modifyIORef (isEnv st) f

askFF :: Interpreter ForeignFunctions
askFF = do
    st <- ask
    liftIO $ readIORef $ isForeignFunctions st


-- saves a test result to the interpreter state
addTestResult :: TestResult -> Interpreter ()
addTestResult tr = do
    st <- ask
    liftIO $ modifyIORef (isTestResults st) (tr:)

emptyInterpreterState :: IO InterpreterState
emptyInterpreterState = do
    a <- newIORef emptyEnv
    b <- newIORef []
    c <- newIORef []
    pure $ InterpreterState a b c

type Interpreter = ReaderT InterpreterState IO


-- it's a bit crap that have variables nested within variables
-- but it seems like the least worst option for now
-- it uses reader + ioref because this is more straightforward
-- that using rwst/state
-- but then it needs an extra ioref so that you can preserve the
-- handle state conveniently in the handle wrapper when you
-- run api functions
data Handle = Handle (IORef InterpreterState)

newHandle :: IO Handle
newHandle = do
    s <- emptyInterpreterState
    modifyIORef (isEnv s) (defaultEnv ++)
    modifyIORef (isForeignFunctions s) (defaultFF ++)
    h <- newIORef s
    pure $ Handle h

showHandleState :: Handle -> IO String
showHandleState (Handle h) = do
    hh <- readIORef h
    showState hh

runInterp :: Handle -> Interpreter a -> IO a
runInterp (Handle h) f = do
    h' <- readIORef h
    (v,h'') <- runReaderT (do
          res <- f
          st <- ask
          pure (res,st)) h'
    writeIORef h h''
    pure v

defaultEnv :: [(String,Value)]
defaultEnv =
    [("true", BoolV True)
    ,("false", BoolV False)
    ,("==", ForeignFunV "==")
    ,("+", ForeignFunV "+")
    ,("-", ForeignFunV "-")
    ,("*", ForeignFunV "*")
    ,("make-variant", ForeignFunV "make-variant")
    ,("variant-tag", ForeignFunV "variant-tag")
    ,("is-tuple", ForeignFunV "is-tuple")
    ,("is-record", ForeignFunV "is-record")
    ,("empty", VariantV "empty" [])
    ,("link", ForeignFunV "link")
    ,("nothing", VariantV "nothing" [])
    ,("is-empty", ForeignFunV "is-empty")
    ,("is-link", ForeignFunV "is-link")
    ,("is-List", ForeignFunV "is-List")
    ,("is-nothing", ForeignFunV "is-nothing")
    ,("is-Nothing", ForeignFunV "is-Nothing")
    ,("print", ForeignFunV "print")
   ]

defaultFF :: [(String, [Value] -> Interpreter Value)]
defaultFF =
    [("==", \[a,b] -> pure $ BoolV $ a == b)
    ,("+", \[NumV a,NumV b] -> pure $ NumV $ a + b)
    ,("-", \[NumV a,NumV b] -> pure $ NumV $ a - b)
    ,("*", \[NumV a,NumV b] -> pure $ NumV $ a * b)
    ,("make-variant", makeVariant)
    ,("variant-tag", variantTag)
    ,("is-tuple", isVariant "tuple")
    ,("is-record", isVariant "record")
    ,("link", listLink)
    ,("is-empty", isVariant "empty")
    ,("is-link", isVariant "link")
    ,("is-nothing", isVariant "nothing")
    ,("is-Nothing", isAgdt "Nothing" ["nothing"])
    ,("is-List", isAgdt "List" ["empty", "link"])
    ,("print", bPrint)
    ]

makeVariant :: [Value] -> Interpreter Value
makeVariant (TextV nm:as) =
    VariantV nm <$> f as
  where
    f [] = pure []
    f (TextV fnm : v : as') = ((fnm,v):) <$> f as'
    f x = error $ "wrong args to make-variant: " ++ show x
makeVariant x = error $ "wrong args to make-variant: " ++ show x

isVariant :: String -> [Value] -> Interpreter Value
isVariant tg [VariantV vt _] = pure $ BoolV $ tg == vt
isVariant tg _ = error $ "wrong number of args to is-" ++ tg

isAgdt :: String -> [String] -> [Value] -> Interpreter Value
isAgdt _ty tgs [VariantV vt _] = pure $ BoolV (vt `elem` tgs)
isAgdt ty _ _ = error $ "wrong number of args to is-" ++ ty


listLink :: [Value] -> Interpreter Value
listLink [a,b] = pure $ VariantV "link" [("first", a), ("rest", b)]
listLink _ = error $ "wrong number of args to link"


variantTag :: [Value] -> Interpreter Value
variantTag [VariantV t _] = pure $ TextV t
variantTag _ = pure $ VariantV "nothing" []

bPrint :: [Value] -> Interpreter Value
bPrint [v] = do
    liftIO $ putStrLn $ show v --todo: use torepr
    pure nothing

bPrint _ = error $ "wrong number of args to print"

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

interp (App f es) = do
    fv <- interp f
    vs <- mapM interp es
    app fv vs


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
    v0 <- interp e0
    v1 <- interp e1
    opv <- interp $ Iden op
    app opv [v0,v1]

interp (Lam ps e) = do
    env <- askEnv
    pure $ FunV (map unPat ps) e env

interp (Let bs e) = do
    let newEnv [] = interp e
        newEnv ((b,ex):bs') = do
            v <- interp ex
            localEnv (extendEnv [(unPat b,v)]) $ newEnv bs'
    newEnv bs

interp (Block ss) =
    localEnv id $ interpStatements ss

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
        VariantV _ fs | Just fv <- lookup f fs -> pure fv
                      | otherwise -> error $ "field not found in dotexpr " ++ show v ++ " . " ++ f
        _ -> error $ "dot called on non variant: " ++ show v

interp (Cases _ty e cs els) = do
    v <- interp e
    matchb v cs
  where
    matchb v ((p,ce) : cs') =
        case matches p v ce of
            Just f -> f
            Nothing -> matchb v cs'
    matchb v [] = case els of
                      Just ee -> interp ee
                      Nothing -> error $ "no cases match and no else " ++ show v ++ " " ++ show cs
    -- return a maybe let which provides the bindings needed
    -- todo: instead of using last ts, look up the tag for the pattern
    -- to match the whole ts, by using some modification of the tag as an identifier
    -- this will handle modules and aliases and stuff
    matches (IdenP (PatName _ s)) (VariantV tag []) ce | tag == s = Just $ interp ce
    -- todo: ignores the qualifier if there is one
    matches (VariantP _ s nms) (VariantV tag fs) ce | tag == s = Just $ do
        let letvs = zipWith (\(PatName _ n) (_,v) -> (n,v)) nms fs
        localEnv (extendEnv letvs) $ interp ce
    matches _ _  _ = Nothing

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

makeBList :: [Value] -> Value
makeBList [] = VariantV "empty" []
makeBList (x:xs) = VariantV "link" [("first", x),("rest", makeBList xs)]



unPat :: PatName -> String
unPat (PatName _ nm) = nm

app :: Value -> [Value] -> Interpreter Value
app fv vs =
    case fv of
        FunV ps bdy env -> do
            as <- safeZip ps vs
            localEnv (const $ extendEnv as env) $ interp bdy
        ForeignFunV nm -> do
            ffs <- askFF
            case lookup nm ffs of
                Just f -> f vs
                Nothing -> error $ "internal error, foreign function not found: " ++ nm
        _ -> error $ "app called on non function value: " ++ show fv
  where
    safeZip ps xs | length ps == length xs  = pure $ zip ps xs
                  | otherwise = error $ "wrong number of args: " ++ show ps ++ ", " ++ show xs


letValue :: String -> Value -> Interpreter ()
letValue nm v = do
    modifyEnv (extendEnv [(nm,v)])

interpStatements :: [Stmt] -> Interpreter Value
interpStatements [] = pure $ VariantV "nothing" []

interpStatements (s@(StmtExpr (BinOp e0 "is" e1)) : ss) = do
    doit
    interpStatements ss
  where
    msg = prettyStmt s
    catchit :: Interpreter Value -> Interpreter (Either String Value)
    catchit f =
        catch (Right <$> f) $ \ex -> pure $ Left $ show (ex :: SomeException)
    doit :: Interpreter ()
    doit = do
        v0 <- catchit $ interp e0
        v1 <- catchit $ interp e1
        case (v0,v1) of
            (Right v0', Right v1') -> addTestResult $ TestResult msg (v0' == v1')
            (Left er0, Right {}) -> addTestResult $ TestResult (msg ++ "\n" ++ prettyExpr e0 ++ " failed: " ++ er0) False
            (Right {}, Left er1) -> addTestResult $ TestResult (msg ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1) False
            (Left er0, Left er1) -> addTestResult $ TestResult (msg ++ "\n" ++ prettyExpr e0 ++ " failed: " ++ er0 ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1) False
        pure ()

interpStatements [StmtExpr e] = interp e
interpStatements [Check _ ss] = interpStatements ss
interpStatements (LetDecl b e : ss) = do
    v <- interp e
    letValue (unPat b) v
    interpStatements ss
interpStatements (StmtExpr e : ss) = do
    _ <- interp e
    interpStatements ss
interpStatements (Check _ ss' : ss) = do
    _ <- interpStatements ss'
    interpStatements ss

interpStatements (VarDecl b e : ss) = do
    v <- interp e
    vr <- liftIO $ newIORef v
    letValue (unPat b) (BoxV vr)
    interpStatements ss

interpStatements (SetVar nm e : ss) = do
    mv <- lookupEnv nm
    let vr = case mv of
                 Just (BoxV b) -> b
                 Just x -> error $ "attempt to assign to something which isn't a var: " ++ show x
                 Nothing -> error $ "identifier not found: " ++ nm
    v <- interp e
    liftIO $ writeIORef vr v
    interpStatements ss

{-

data decl:

an is-x function for each variant
an is-dat function for the data type
a make function for each variant, with the name of the variant, e.g.
pt(...)

TODO: use iden + tag value to identify variants

-}

interpStatements (DataDecl dnm vs whr : ss) = do
    -- make them call make variant
    -- pass a list
    -- add haskell helper functions for working with lists in burdock
    let makeMake (VariantDecl vnm []) =
            letDecl vnm
            $ appN "make-variant" [Text vnm]
        makeMake (VariantDecl vnm fs) =
            letDecl vnm
            $ lam (map snd fs)
            $ appN "make-variant" (Text vnm : concat (map ((\x -> [Text x, Iden x]) . snd) fs))
        makeIs (VariantDecl vnm _) = 
            letDecl ("is-" ++ vnm)
            $ lam ["x"]
            $ eqE (appN "variant-tag" [Iden "x"]) (Text vnm)
        callIs (VariantDecl vnm _) = appN ("is-" ++ vnm) [Iden "x"]
        makeIsDat =
            letDecl ("is-" ++ dnm)
            $ lam ["x"]
            $ foldl1 orE $ map callIs vs
        chk = maybe [] (\w -> [Check (Just dnm) w]) whr
    interpStatements (map makeMake vs ++ map makeIs vs ++ [makeIsDat] ++ chk ++ ss)
  where
    letDecl nm v = LetDecl (PatName NoShadow nm) v
    lam as e = Lam (map (PatName NoShadow) as) e
    --letE bs e = Let (flip map bs $ \(b,v) -> (PatName NoShadow b, v)) e
    appN nm as = App (Iden nm) as
    eqE a b = BinOp a "==" b
    orE a b = BinOp a "or" b
    
interpStatements ss | (recbs@(_:_),chks, ss') <- getRecs [] [] ss = do
    interpStatements (doLetRec recbs ++ chks ++ ss')
  where
    getRecs accdecls accchks (RecDecl nm bdy : ss') = getRecs ((nm,bdy):accdecls) accchks ss'
    getRecs accdecls accchks (FunDecl nm args bdy whr : ss') =
        let accchks' = maybe accchks (\w -> Check (Just $ unPat nm) w : accchks) whr
        in getRecs ((nm, Lam args bdy):accdecls) accchks' ss'
    getRecs accdecls accchks ss' = (reverse accdecls, reverse accchks, ss')

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

doLetRec :: [(PatName, Expr)] -> [Stmt]
doLetRec bs = 
    let vars = map makeVar bs
        assigned = map makeAssign bs
    in vars ++ assigned
  where
    makeVar (v,_) = VarDecl v $ Lam []
        $ App (Iden "raise")
            [Text "internal error: uninitialized letrec implementation var"]
    makeAssign (b,v) = SetVar (unPat b) v

