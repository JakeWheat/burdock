
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

import Control.Arrow (first)

------------------------------------------------------------------------------

desugarScript :: Text
              -> S.Script
              -> Either [StaticError] [I.Stmt]
desugarScript _fn (S.Script stmts) = pure $ desugarStmts stmts

------------------------------------------------------------------------------

desugarStmts :: [S.Stmt] -> [I.Stmt]
desugarStmts ss = map desugarStmt $ desugarRecs ss


------------------------------------------------------------------------------

desugarStmt :: S.Stmt -> I.Stmt
desugarStmt (S.Check sp _ bdy) = desugarStmt $ S.StmtExpr sp $ S.Block sp bdy
desugarStmt (S.StmtExpr sp e) = I.StmtExpr sp $ desugarExpr e
desugarStmt (S.LetDecl sp b e) = I.LetDecl sp (desugarBinding b) (desugarExpr e)
desugarStmt (S.VarDecl sp (S.SimpleBinding _ _ nm Nothing) e) = I.VarDecl sp nm (desugarExpr e)
desugarStmt (S.SetVar sp (S.Iden _ nm) e) = I.SetVar sp [nm] (desugarExpr e)

desugarStmt s = error $ "desugarStmt " <> show s

------------------------------------------------------------------------------

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
    app nm as = S.App sp (S.DotExpr sp (S.Iden sp "_bootstrap") nm) as
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
    app nm as = S.App sp (S.DotExpr sp (S.Iden sp "_bootstrap") nm) as
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

------------------------------------------------------------------------------

desugarRecs :: [S.Stmt] -> [S.Stmt]
desugarRecs [] = []
desugarRecs ss =
    let (rs,ctu) = getRecs ss
    in if null rs
       then case ctu of
            (t:ts) -> t : desugarRecs ts
            [] -> []
       else let (vars,sets) = unzip $ flip map rs $ \(a,b,c) -> makeRec a b c
            in vars ++ sets ++ desugarRecs ctu
  where
    getRecs :: [S.Stmt] -> ([(S.SourcePosition, Text, S.Expr)], [S.Stmt])
    getRecs (S.RecDecl sp (S.NameBinding _ nm) e : ts) = first ((sp,nm,e):) $ getRecs ts
    getRecs (S.FunDecl sp (S.SimpleBinding _ S.NoShadow nm Nothing) fh Nothing bdy Nothing : ts)
        = first ((sp,nm,S.Lam sp fh bdy) :) $ getRecs ts
    getRecs [] = ([],[])
    getRecs ts = ([],ts)

makeRec :: S.SourcePosition -> Text -> S.Expr -> (S.Stmt, S.Stmt)
makeRec sp nm e =
        let placeholder = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp $ S.App sp (S.Iden sp "raise") [S.Text sp "internal: recursive var not initialized"]]
        in (S.VarDecl sp (S.SimpleBinding sp S.NoShadow nm Nothing) placeholder
           ,S.SetVar sp (S.Iden sp nm) e)
