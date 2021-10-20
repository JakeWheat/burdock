


{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.Syntax where

import Data.Data (Data)

import Burdock.Scientific

data Stmt =
      StmtExpr Expr
    | LetDecl PatName Expr
    | Check (Maybe String) [Stmt]
    | VarDecl PatName Expr
    | SetVar String Expr
    deriving (Eq,Show,Data)

data Expr =
      Num Scientific
    | Text String
    | Iden String
    | Parens Expr
    | If [(Expr,Expr)] (Maybe Expr)
    | App Expr [Expr]
    | BinOp Expr String Expr
    | Lam [PatName] Expr
    | Let [(PatName,Expr)] Expr
    | LetRec [(PatName,Expr)] Expr
    | Block [Stmt]
    deriving (Eq,Show,Data)

data PatName =
      PatName Shadow String
    deriving (Eq,Show,Data)

data Shadow = NoShadow | Shadow
          deriving (Eq,Show,Data) 


data Script =
      Script [Stmt]
    deriving (Eq,Show,Data)
