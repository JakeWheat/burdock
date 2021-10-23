

module Burdock.Interpreter
    (TestResult(..)
    ,executeScriptWithTests
    ,interpretExpr
    ) where

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            )

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

import Control.Exception.Safe (catch
                              ,SomeException)


--import Debug.Trace (trace)

------------------------------------------------------------------------------

-- public api

data TestResult = TestResult String Bool

executeScriptWithTests :: Script -> IO [TestResult]
executeScriptWithTests (Script ss) = runInterp $ do
    _ <- interpStatements ss
    st <- ask
    liftIO (reverse <$> readIORef (isTestResults st))

interpretExpr :: Expr -> IO Value
interpretExpr e = runInterp (interp e)


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

------------------------------------------------------------------------------

-- environment, interpreter state, intepreter monad helper functions

type Env = [(String, Value)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: [(String,Value)] -> Env -> Env
extendEnv bs env = bs ++ env

data InterpreterState =
    InterpreterState {isEnv :: IORef Env
                     ,isTestResults :: IORef [TestResult]}

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
    pure $ lookup k e 

-- saves a test result to the interpreter state
addTestResult :: TestResult -> Interpreter ()
addTestResult tr = do
    st <- ask
    liftIO $ modifyIORef (isTestResults st) (tr:)

emptyInterpreterState :: IO InterpreterState
emptyInterpreterState = do
    a <- newIORef emptyEnv
    b <- newIORef []
    pure $ InterpreterState a b

type Interpreter = ReaderT InterpreterState IO

-- create the run function

runInterp :: Interpreter a -> IO a
runInterp f = do
    s <- emptyInterpreterState
    modifyIORef (isEnv s) (defaultEnv ++)
    runReaderT f s

defaultEnv :: [(String,Value)]
defaultEnv =
    [("true", BoolV True)
    ,("false", BoolV False)
    ]

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

interp (BinOp e0 "or" e1) = do
    x <- interp e0
    case x of
        BoolV True -> pure x
        BoolV False -> interp e1



interp (BinOp e0 op e1) = do
    -- todo: look up operators in the env
    v0 <- interp e0
    v1 <- interp e1
    case (v0,op,v1) of
        (NumV a, "+", NumV b) -> pure $ NumV $ a + b
        (NumV a, "-", NumV b) -> pure $ NumV $ a - b
        (NumV a, "*", NumV b) -> pure $ NumV $ a * b
        (a, "==", b) -> pure $ BoolV $ a == b
        _ -> error $ "operator not supported: " ++ show (op,v0,v1)

interp (Lam ps e) = do
    env <- askEnv
    pure $ FunV (map unPat ps) e env

interp (Let bs e) = do
    let newEnv [] = interp e
        newEnv ((b,ex):bs') = do
            v <- interp ex
            localEnv (extendEnv [(unPat b,v)]) $ newEnv bs'
    newEnv bs

interp (Block ss) = interpStatements ss

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
  a := ...
  b := ...
  ...

-}
interp (LetRec bs e) =
    let vars = map makeVar bs
        assigned = map makeAssign bs
        desugared = Block (vars ++ assigned ++ [StmtExpr e])
        
    in {-trace (prettyExpr desugared) $ -} interp desugared
  where
    makeVar (v,_) = VarDecl v $ Lam []
        $ App (Iden "raise")
            [Text "internal error: uninitialized letrec implementation var"]
    makeAssign (b,v) = SetVar (unPat b) v

unPat :: PatName -> String
unPat (PatName _ nm) = nm

app :: Value -> [Value] -> Interpreter Value
app fv vs =
    case fv of
        FunV ps bdy env -> do
            as <- safeZip ps vs
            localEnv (const $ extendEnv as env) $ interp bdy
        _ -> error $ "app called on non function value: " ++ show fv
  where
    safeZip ps xs | length ps == length xs  = pure $ zip ps xs
                  | otherwise = error $ "wrong number of args: " ++ show ps ++ ", " ++ show xs


interpStatements :: [Stmt] -> Interpreter Value
interpStatements [] = pure $ VariantV "nothing" []
interpStatements [LetDecl {}] = pure $ VariantV "nothing" []

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
    localEnv (extendEnv [(unPat b,v)]) $ interpStatements ss
interpStatements (StmtExpr e : ss) = do
    _ <- interp e
    interpStatements ss
interpStatements (Check _ ss' : ss) = do
    _ <- interpStatements ss'
    interpStatements ss

interpStatements (VarDecl b e : ss) = do
    v <- interp e
    vr <- liftIO $ newIORef v
    localEnv (extendEnv [(unPat b,BoxV vr)]) $ interpStatements ss

interpStatements (SetVar nm e : ss) = do
    mv <- lookupEnv nm
    let vr = case mv of
                 Just (BoxV b) -> b
                 Just x -> error $ "attempt to assign to something which isn't a var: " ++ show x
                 Nothing -> error $ "identifier not found: " ++ nm
    v <- interp e
    liftIO $ writeIORef vr v
    interpStatements ss
    

