
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.InterpreterSyntax
    (Stmt(..)
    ,Expr(..)
    ,Binding(..)
    ,SP
    ) where

import Data.Text (Text)
import Burdock.Scientific (Scientific)
import Data.Data (Data)

type SP = Maybe (Text, Int, Int)

data Stmt
    = LetDecl SP Binding Expr
    | StmtExpr SP Expr
    | VarDecl SP Text Expr
    | SetVar SP [Text] Expr
    deriving (Eq,Show,Data)

data Expr
    = Block SP [Stmt]
    | If SP [(Expr,[Stmt])] (Maybe [Stmt])
    | DotExpr SP Expr Text
    -- source pos, fn, args
    | App SP Expr [Expr]
    | RecordSel SP [(Text,Expr)]
    | TupleSel SP [Expr]
    -- free vars, param bindings, body
    | Lam SP [Text] [Binding] [Stmt]
    | Num SP Scientific
    | IString SP Text
    | Iden SP Text
    | MethodExpr SP Expr
    | RunTask SP Bool Expr
    | Cases SP Expr [(Binding, [Stmt])]
    | TupleGet SP Expr Int
    deriving (Eq,Show,Data)

data Binding
    = NameBinding SP Text
    | WildcardBinding SP
    | VariantBinding SP Text [Binding]
    | TupleBinding SP [Binding]
    deriving (Eq,Show,Data)
