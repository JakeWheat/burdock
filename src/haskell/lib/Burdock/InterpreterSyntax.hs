
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.InterpreterSyntax
    (Stmt(..)
    ,Expr(..)
    ) where

import Data.Text (Text)
import Burdock.Scientific (Scientific)
import Data.Data (Data)

data Stmt
    = LetDecl Text Expr
    | StmtExpr Expr
    deriving (Eq,Show,Data)

data Expr
    = Block [Stmt]
    | If [(Expr,[Stmt])] (Maybe [Stmt])
    | DotExpr Expr Text
    | App Expr [Expr]
    | RecordSel [(Text,Expr)]
    | Lam [Text] [Text] [Stmt]
    | Num Scientific
    | IString Text
    | Iden Text
    | MethodExpr Expr
    deriving (Eq,Show,Data)
