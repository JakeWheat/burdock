


{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.Syntax where

import Data.Data (Data)

import Burdock.Scientific

data Stmt =
      StmtExpr Expr
    | LetDecl PatName Expr
    | Check (Maybe String) [Stmt]
    deriving (Eq,Show,Data)

data Expr =
      Num Scientific
    | Text String
    | Iden String
    | Parens Expr
    | App Expr [Expr]
    | BinOp Expr String Expr
    | Lam [PatName] Expr
    | Let [(PatName,Expr)] Expr
    | LetRec [(PatName,Expr)] Expr
    | Block [Stmt]
    deriving (Eq,Show,Data)

type PatName = String

data Script =
      Script [Stmt]
    deriving (Eq,Show,Data)

{-
          | RecDecl PatName Expr
          | FunDecl PatName -- name
                    [PatName] -- args
                    Expr -- body
                    (Maybe [Stmt]) -- test block
-}
