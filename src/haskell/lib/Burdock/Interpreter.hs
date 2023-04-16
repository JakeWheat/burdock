
{-

Interpreter is the code which takes desugared (todo) syntax and
executes it on the runtime.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Interpreter
    (interpBurdock
    ) where

import qualified Burdock.Syntax as S
import Burdock.Runtime
    (Value(..)
    ,runBurdock
    ,Runtime
    ,liftIO

    --,ffimethodapp
    --,RuntimeState
    ,emptyRuntimeState
    --,addFFIType

    ,getMember
    ,app

    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding
    ,captureClosure
    
    ,makeValue
    ,extractValue
    --,makeFunctionValue
    --,Type(..)
    --,Scientific
    )

import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
import qualified Data.Text as T

import Burdock.DefaultRuntime (initRuntime)
import Control.Monad (forM_)

interpBurdock :: S.Script -> IO Value
interpBurdock (S.Script ss) = do
    st <- emptyRuntimeState
    runBurdock st $ do
        initRuntime
        interpStmts ss

interpStmts :: [S.Stmt] -> Runtime Value
interpStmts [] = error "no statements"
interpStmts [s] = interpStmt s
interpStmts (s:ss) = interpStmt s *> interpStmts ss

interpStmt :: S.Stmt -> Runtime Value
interpStmt (S.Check _ _ ss) = interpStmts ss

interpStmt (S.LetDecl _ b e) = do
    letExprs [(b, e)]
    pure VNothing

interpStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) = do
    -- todo: do most of this in desugaring
    v1 <- interpExpr e1
    v2 <- interpExpr e2

    eqm <- getMember v1 "_equals" 
    res <- app eqm [v2]
    let res' = case extractValue res of
                   Just (y :: Bool) -> y
                   Nothing -> error $ "wrong return type for equals"
    liftIO $ putStrLn $ (if res' then "PASS" else "FAIL") <> " " <> prettyExpr e1 <> " is " <> prettyExpr e2
    pure VNothing

interpStmt (S.StmtExpr _ e) = interpExpr e

interpStmt s = error $ "interpStmt: " ++ show s

interpExpr :: S.Expr -> Runtime Value

-- desugaring if e1 then True else e2
interpExpr (S.BinOp _ e1 "or" e2) =
    interpExpr (S.If Nothing [(e1, [S.StmtExpr Nothing $ S.Iden Nothing "true"])] (Just [S.StmtExpr Nothing e2]))

-- if e1 then e2 else False
interpExpr (S.BinOp _ e1 "and" e2) =
    interpExpr (S.If Nothing [(e1, [S.StmtExpr Nothing e2])] (Just [S.StmtExpr Nothing $ S.Iden Nothing "false"]))

               
interpExpr (S.BinOp _ e1 "+" e2) = do
    -- todo: move to desugar phase
    interpExpr $ S.App Nothing (S.DotExpr Nothing e1 "_plus") [e2]

interpExpr (S.BinOp _ e1 "*" e2) = do
    -- todo: move to desugar phase
    interpExpr $ S.App Nothing (S.DotExpr Nothing e1 "_times") [e2]

interpExpr (S.DotExpr _ e1 fld) = do
    v1 <- interpExpr e1
    getMember v1 (T.pack fld)

interpExpr (S.RecordSel _ fs) = do
    vs <- mapM (\(n,e) -> (T.pack n,) <$> interpExpr e) fs
    -- todo: "record" has to be namespaced
    pure $ VariantV "record" vs


interpExpr (S.App _ ef es) = do
    vs <- mapM interpExpr es
    f <- interpExpr ef
    app f vs

interpExpr (S.Lam _ (S.FunHeader _ bs _) bdy) = do
    env <- captureClosure $ freeVars (S.Block Nothing bdy) -- [] -- todo: get the free vars
    -- todo: how do you test the freevars function?
    -- how do you test that the right closures are captured?
    let runF :: [Value] -> Runtime Value
        runF vs = do
            -- todo: check lists same length
            let bs' = zip bs vs
            withNewEnv env $ do
                letValues bs'
                interpStmts bdy
    pure $ VFun runF

interpExpr (S.Num _ n) =
    {- todo: you either have to look up "number" in the runtime environment
       or keep a token from when the number type was created, this is so type
       names are namespaced and scoped, e.g. if you have two modules which have
       a type with the same name as each other
    -}
    pure $ makeValue "number" n

interpExpr (S.Text _ t) =
    pure $ makeValue "string" $ T.pack t

interpExpr (S.Let _ bs e) = withScope $ do
    letExprs bs
    interpStmts e

interpExpr (S.Iden _ nm) = do
    b <- lookupBinding (T.pack nm)
    case b of
        Nothing -> error $ "binding not found: " ++ nm
        Just v -> pure v

interpExpr (S.Block _ sts) = withScope $ interpStmts sts

interpExpr (S.Parens _ e) = interpExpr e

interpExpr (S.If _ cs els) =
    let m ((t,e):cs') = do
            tv <- interpExpr t
            case extractValue tv of
                Nothing -> error "non boolean in if test"
                Just v ->
                    if v
                    then interpStmts e
                    else m cs'
        m [] = do
            case els of
                Nothing -> error "no if branches matched and no else"
                Just e -> interpStmts e
    in m cs

interpExpr (S.MethodExpr _ (S.Method (S.FunHeader _ts (a:as) _mty) bdy)) =
    MethodV <$> interpExpr (S.Lam Nothing (S.FunHeader [] [a] Nothing)
                        [S.StmtExpr Nothing $ S.Lam Nothing (S.FunHeader [] as Nothing) bdy])
interpExpr (S.MethodExpr _ (S.Method (S.FunHeader _ts [] _mty) _bdy)) =
    error $ "method declaration should accept at least one argument"


interpExpr x = error $ "interpExpr: " ++ show x


letExprs :: [(S.Binding, S.Expr)] -> Runtime ()
letExprs bs = do
    bs' <- mapM (\(nm,e) -> (nm,) <$> interpExpr e) bs
    letValues bs'

letValues :: [(S.Binding, Value)] -> Runtime ()
letValues bs = do
    forM_ bs $ \case
        (S.NameBinding _ nm, v) -> addBinding (T.pack nm) v
        (S.ShadowBinding _ nm, v) -> addBinding (T.pack nm) v
        (x,_) -> error $ "unsupported binding: " ++ show x


freeVars :: S.Expr -> [Text]
freeVars e = freeVars' [] e

freeVars' :: [Text] -> S.Expr -> [Text]
freeVars' bs (S.Iden _ a)
    | T.pack a `elem` bs = []
    | otherwise = [T.pack a]

freeVars' bs (S.Block _ sts) = concatMap (freeVarsSt' bs) sts

freeVars' bs (S.BinOp _ a _ b) = freeVars' bs a ++ freeVars' bs b

freeVars' bs (S.DotExpr _ e _) = freeVars' bs e

freeVars' _ e = error $ "freeVars': " ++ show e

freeVarsSt' :: [Text] -> S.Stmt -> [Text]

freeVarsSt' bs (S.StmtExpr _ e) = freeVars' bs e

freeVarsSt' _bs st = error $ "freeVarsSt': " ++ show st
