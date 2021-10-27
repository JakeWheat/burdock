
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

import Control.Monad (forM_
                     ,void
                     ,when
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
import Burdock.Parse (parseScript, parseExpr)

import Control.Exception.Safe (catch
                              ,SomeException)

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
runScript h mfn lenv src = runInterp h $ do
    let Script ast = either error id $ parseScript (maybe "" id mfn) src
    -- todo: how to make this local to this call only
    forM_ lenv $ \(n,v) -> letValue n v
    interpStatements ast

    
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
                     ,isTestResults :: IORef [TestResult]
                     ,isForeignFunctions :: IORef ForeignFunctions
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
    local (const $ st {isScriptEnv = e1}) f

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

_modifySystemEnv :: (Env -> Env) -> Interpreter ()
_modifySystemEnv f = do
    st <- ask
    liftIO $ modifyIORef (isSystemEnv st) f


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
    b <- newIORef emptyEnv
    c <- newIORef []
    d <- newIORef []
    pure $ InterpreterState a b c d



newHandle :: IO Handle
newHandle = do
    s <- emptyInterpreterState
    modifyIORef (isSystemEnv s) (defaultEnv ++)
    modifyIORef (isForeignFunctions s) (defaultFF ++)
    h <- newIORef s
    pure $ Handle h

runInterp :: Handle -> Interpreter a -> IO a
runInterp (Handle h) f = do
    h' <- readIORef h
    (v,h'') <- runReaderT (do
          res <- f
          st <- ask
          pure (res,st)) h'
    writeIORef h h''
    pure v

------------------------------------------------------------------------------

-- built in functions implemented in haskell:

defaultEnv :: [(String,Value)]
defaultEnv =
    [("_system", VariantV "record" [("modules", VariantV "record" [])])
    ,("true", BoolV True)
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
    ,("load-module", ForeignFunV "load-module")
    ,("show-handle-state", ForeignFunV "show-handle-state")
    ,("torepr", ForeignFunV "torepr")
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
    ,("load-module", bLoadModule)
    ,("show-handle-state", bShowHandleState)
    ,("torepr", torepr)
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
    liftIO (putStrLn =<< torepr' v)
    pure nothing

bPrint _ = error $ "wrong number of args to print"

-- load module at filepath
bLoadModule :: [Value] -> Interpreter Value
bLoadModule [TextV moduleName, TextV fn] = do
    loadModule moduleName fn
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

xSep :: String -> [P.Doc a] -> P.Doc a
xSep x ds = P.sep $ P.punctuate (P.pretty x) ds

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
        localScriptEnv (extendEnv letvs) $ interp ce
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

-- return value if it's the last statement
interpStatements [s] = interpStatement s
interpStatements (s:ss) = interpStatement s >> interpStatements ss
interpStatements [] = pure $ VariantV "nothing" []


interpStatement :: Stmt -> Interpreter Value

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

interpStatement (RecDecl {}) = error $ "internal error, rec decl not captured by letrec desugaring"
interpStatement (FunDecl {}) = error $ "internal error, fun decl not captured by letrec desugaring"

interpStatement s@(StmtExpr (BinOp e0 "is" e1)) = do
    v0 <- catchit $ interp e0
    v1 <- catchit $ interp e1
    case (v0,v1) of
        (Right v0', Right v1') ->
            if v0' == v1'
            then addTestResult $ TestResult msg True
            else do
                p0 <- liftIO $ torepr' v0'
                p1 <- liftIO $ torepr' v1'
                addTestResult $
                  TestResult (msg ++ "\n" ++ p0 ++ "\n!=\n" ++ p1) False
        (Left er0, Right {}) -> addTestResult $ TestResult (msg ++ "\n" ++ prettyExpr e0 ++ " failed: " ++ er0) False
        (Right {}, Left er1) -> addTestResult $ TestResult (msg ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1) False
        (Left er0, Left er1) -> addTestResult $ TestResult (msg ++ "\n" ++ prettyExpr e0 ++ " failed: " ++ er0 ++ "\n" ++ prettyExpr e1 ++ " failed: " ++ er1) False
    pure nothing
  where
    msg = prettyStmt s
    catchit :: Interpreter Value -> Interpreter (Either String Value)
    catchit f =
        catch (Right <$> f) $ \ex -> pure $ Left $ show (ex :: SomeException)

interpStatement (StmtExpr e) = interp e
interpStatement (Check _ ss) = interpStatements ss
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

TODO: use iden + tag value to identify variants

-}

interpStatement (DataDecl dnm vs whr) = do
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
    interpStatements (map makeMake vs ++ map makeIs vs ++ [makeIsDat] ++ chk)
  where
    letDecl nm v = LetDecl (PatName NoShadow nm) v
    lam as e = Lam (map (PatName NoShadow) as) e
    --letE bs e = Let (flip map bs $ \(b,v) -> (PatName NoShadow b, v)) e
    appN nm as = App (Iden nm) as
    eqE a b = BinOp a "==" b
    orE a b = BinOp a "or" b

---------------------------------------

-- prelude statements

{-
import file("file.tea") as X
import string-dict as X

->
resolve the path to the module, if it isn't loaded, load it
alias it locally
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
provide uses a special wrapper, put it in interpStatements

it will continue the module with a wrapper function, and when it's
done, instead of putting the whole module top level, it will transform
it first according to the provide names

it has to work with the default implicit provide *
-> either disable that when it activates
or piggyback onto it?

provide: * end
provide: a end
provide: a,b end
provide: a as b end

-}

interpStatement (Provide {}) = error "todo: provide"
------------------------------------------------------------------------------

-- module loading support



resolveImportPath :: [FilePath] -> ImportSource -> Interpreter (String, FilePath)
resolveImportPath _moduleSearchPath is = do
    case is of
        ImportSpecial "file" [fn] ->
            pure (dropExtension $ takeFileName fn, fn)
        ImportSpecial {} -> error "unsupported import"
        -- todo: set this path in a sensible and flexible way
        ImportName nm -> pure (nm, "data" </> "built-ins" </> nm <.> "bur")

ensureModuleLoaded :: String -> FilePath -> Interpreter ()
ensureModuleLoaded moduleName moduleFile = do
    st <- ask
    se <- liftIO $ readIORef (isSystemEnv st)
    when (moduleName `notElem` map fst se) $ do
        liftIO $ putStrLn $ "loading module: " ++ moduleFile
        loadModule moduleName moduleFile

loadModule :: String -> FilePath -> Interpreter ()
loadModule moduleName fn = do
    src <- liftIO $ readFile fn
    let Script ast = either error id $ parseScript fn src
    localScriptEnv (const emptyEnv) $ do
        void $ interpStatements ast
        moduleEnv <- askScriptEnv
        -- make a record from the env
        let moduleRecord = VariantV "record" moduleEnv
            sp = modulePath moduleName
        modifySystemExtendRecord sp moduleRecord

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
    apis (ProvideAlias nm k) = case lookup k rc of
        Nothing -> error $ "provide alias source not found: " ++ k
        Just v -> [(nm,v)]
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

