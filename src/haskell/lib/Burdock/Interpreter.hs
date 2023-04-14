
{-

Interpreter is the code which takes desugared (todo) syntax and
executes it on the runtime.

-}

module Burdock.Interpreter
    (interpBurdock
    ) where

import qualified Burdock.Syntax as S
import Burdock.Runtime
    (Value(..)
    ,runBurdock
    ,Runtime
    ,liftIO
    )

import Burdock.Pretty (prettyExpr)


interpBurdock :: S.Script -> IO Value
interpBurdock (S.Script ss) = runBurdock $ interpStmts ss

interpStmts :: [S.Stmt] -> Runtime Value
interpStmts [] = error "no statements"
interpStmts [s] = interpStmt s
interpStmts (s:ss) = interpStmt s *> interpStmts ss

interpStmt :: S.Stmt -> Runtime Value
interpStmt (S.Check _ _ ss) = interpStmts ss

interpStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) = do
    v1 <- interpExpr e1
    v2 <- interpExpr e2
    let res = v1 == v2
    liftIO $ putStrLn $ (if res then "PASS" else "FAIL") <> " " <> prettyExpr e1 <> " is " <> prettyExpr e2
    pure Value

interpStmt s = error $ show s

interpExpr :: S.Expr -> Runtime Value
interpExpr _x = pure $ Value
