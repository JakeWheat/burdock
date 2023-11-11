
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Desugar
    (desugarScript
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)

import Data.Text (Text)
import qualified Data.Text.Lazy as L

import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Burdock.StaticError (StaticError)

import qualified Burdock.Pretty as P



desugarScript :: Text
              -> S.Script
              -> Either [StaticError] [I.Stmt]
desugarScript _fn (S.Script stmts) = pure $ desugarStmts stmts

desugarStmts :: [S.Stmt] -> [I.Stmt]
desugarStmts ss = map desugarStmt ss

desugarStmt :: S.Stmt -> I.Stmt
desugarStmt (S.Check sp _ bdy) = desugarStmt $ S.StmtExpr sp $ S.Block sp bdy
desugarStmt (S.StmtExpr sp e) = I.StmtExpr sp $ desugarExpr e
desugarStmt (S.LetDecl sp b e) = I.LetDecl sp (desugarBinding b) (desugarExpr e)

desugarStmt s = error $ "desugarStmt " <> show s

desugarExpr :: S.Expr -> I.Expr
desugarExpr (S.BinOp sp e0 "is" e1) = do
    let msg = L.toStrict $ P.prettyExpr e0 <> " is " <> P.prettyExpr e1
    desugarExpr $ app "run-binary-test"
        [S.Text sp msg
        ,wrapit e0
        ,wrapit e1
        ,lam ["a", "b"] (S.BinOp sp (S.Iden sp "a") "==" (S.Iden sp "b"))
        ,S.Text sp "!="
        ]
  where
    app nm as = S.App sp (S.DotExpr sp (S.Iden sp "_system") nm) as
    wrapit e = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp e]
    lam as e = S.Lam sp (S.FunHeader [] (flip map as $ S.NameBinding sp) Nothing) [S.StmtExpr sp e]

desugarExpr (S.BinOp sp e0 "is-not" e1) = do
    let msg = L.toStrict $ P.prettyExpr e0 <> " is " <> P.prettyExpr e1
    desugarExpr $ app "run-binary-test"
        [S.Text sp msg
        ,wrapit e0
        ,wrapit e1
        ,lam ["a", "b"] (S.BinOp sp (S.Iden sp "a") "!=" (S.Iden sp "b"))
        ,S.Text sp "=="
        ]
  where
    app nm as = S.App sp (S.DotExpr sp (S.Iden sp "_system") nm) as
    wrapit e = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp e]
    lam as e = S.Lam sp (S.FunHeader [] (flip map as $ S.NameBinding sp) Nothing) [S.StmtExpr sp e]


desugarExpr (S.Block sp bdy) = I.Block sp $ desugarStmts bdy
desugarExpr (S.App sp f as) = I.App sp (desugarExpr f) (map desugarExpr as)
desugarExpr (S.DotExpr sp e f) = I.DotExpr sp (desugarExpr e) f
desugarExpr (S.Iden sp i) = I.Iden sp i
desugarExpr (S.Text sp t) = I.IString sp t
desugarExpr (S.Num sp n) = I.Num sp n
desugarExpr (S.Parens _ e) = desugarExpr e
desugarExpr (S.Lam sp (S.FunHeader [] bs Nothing) bdy)
    = I.Lam sp [] (map desugarBinding bs) $ desugarStmts bdy


desugarExpr (S.BinOp sp e0 op e1) | Just op' <- lookup op methOps =
    desugarExpr (S.App sp (S.DotExpr sp e0 op') [e1])
  where
    methOps =
        [("==", "_equals")
        ,("<=", "_lessequal")
        ,(">=", "_greaterequal")
        ,("<", "_lessthan")
        ,(">", "_greaterthan")
        ,("+", "_plus")
        ,("-", "_minus")
        ,("*", "_times")
        ,("/", "_divide")
        ]

desugarExpr (S.If sp ts els) =
    I.If sp (flip map ts $ \(e,bdy) -> (desugarExpr e, desugarStmts bdy)) $ fmap desugarStmts els

desugarExpr e = error $ "desugarExpr " <> show e

------------------------------------------------------------------------------

desugarBinding :: S.Binding -> I.Binding
desugarBinding (S.NameBinding sp nm) = I.NameBinding sp nm
desugarBinding (S.ShadowBinding sp nm) = I.NameBinding sp nm
desugarBinding b = error $ "desugarBinding " <> show b
