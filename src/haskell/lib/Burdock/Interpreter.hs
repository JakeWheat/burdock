
{-

Interpreter is the code which takes desugared (todo) syntax and
executes it on the runtime.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
    ,addBinding
    ,lookupBinding
    
    ,makeValue
    --,extractValue
    --,makeFunctionValue
    --,Type(..)
    --,Scientific
    )

import Burdock.Pretty (prettyExpr)
--import Data.Text (Text)
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
    letBindings [(b, e)]
    pure VNothing

interpStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) = do
    -- todo: do most of this in desugaring
    v1 <- interpExpr e1
    v2 <- interpExpr e2

    eqm <- getMember v1 "_equals" 
    res <- app eqm [v2]
    let res' = case res of
                   VBool x -> x
                   _x -> error $ "wrong return type for equals"
    liftIO $ putStrLn $ (if res' then "PASS" else "FAIL") <> " " <> prettyExpr e1 <> " is " <> prettyExpr e2
    pure VNothing

interpStmt (S.StmtExpr _ e) = interpExpr e


interpStmt s = error $ "interpStmt: " ++ show s

interpExpr :: S.Expr -> Runtime Value
interpExpr (S.BinOp _ e1 "+" e2) = do
    -- todo: move to desugar phase
    interpExpr $ S.App Nothing (S.DotExpr Nothing e1 "_plus") [e2]

interpExpr (S.DotExpr _ e1 fld) = do
    v1 <- interpExpr e1
    getMember v1 (T.pack fld)

interpExpr (S.App _ ef es) = do
    vs <- mapM interpExpr es
    f <- interpExpr ef
    app f vs
    
interpExpr (S.Num _ n) =
    {- todo: you either have to look up "number" in the runtime environment
       or keep a token from when the number type was created, this is so type
       names are namespaced and scoped, e.g. if you have two modules which have
       a type with the same name as each other
    -}
    pure $ makeValue "number" n

interpExpr (S.Let _ bs e) = withScope $ do
    letBindings bs
    interpStmts e

interpExpr (S.Iden _ nm) = do
    b <- lookupBinding (T.pack nm)
    case b of
        Nothing -> error $ "binding not found: " ++ nm
        Just v -> pure v

interpExpr x = error $ "interpExpr: " ++ show x

letBindings :: [(S.Binding, S.Expr)] -> Runtime ()
letBindings bs = do
    forM_ bs $ \case
        (S.NameBinding _ nm, ex) -> do
            vx <- interpExpr ex
            addBinding (T.pack nm) vx
        (S.ShadowBinding _ nm, ex) -> do
            vx <- interpExpr ex
            addBinding (T.pack nm) vx
        x -> error $ "unsupported binding: " ++ show x
    
