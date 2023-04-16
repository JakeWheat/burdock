
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.InterpreterSyntax
    (Stmt(..)
    ,Expr(..)
    ,Binding(..)
    ) where

import Data.Text (Text)
import Burdock.Scientific (Scientific)
import Data.Data (Data)

data Stmt
    = LetDecl Binding Expr
    | StmtExpr Expr
    deriving (Eq,Show,Data)

data Expr
    = Block [Stmt]
    | If [(Expr,[Stmt])] (Maybe [Stmt])
    | DotExpr Expr Text
    | App Expr [Expr]
    | RecordSel [(Text,Expr)]
    -- free vars, param bindings, body
    | Lam [Text] [Binding] [Stmt]
    | Num Scientific
    | IString Text
    | Iden Text
    | MethodExpr Expr
    | RunTask Expr
    | Cases Expr [(Binding, [Stmt])]
    deriving (Eq,Show,Data)


data Binding
    = NameBinding Text
    | WildcardBinding
    | VariantBinding Text [Binding]
    deriving (Eq,Show,Data)

