
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.InterpreterSyntax
    (Stmt(..)
    ,Expr(..)
    ,Binding(..)
    ) where

import Data.Text (Text)
import Burdock.Scientific (Scientific)
import Data.Data (Data)

type SourcePos = Maybe Text

data Stmt
    = LetDecl Binding Expr
    | StmtExpr Expr
    | VarDecl Text Expr
    | SetVar Text Expr
    deriving (Eq,Show,Data)

data Expr
    = Block [Stmt]
    | If [(Expr,[Stmt])] (Maybe [Stmt])
    | DotExpr Expr Text
    -- source pos, fn, args
    | App SourcePos Expr [Expr]
    | RecordSel [(Text,Expr)]
    | TupleSel [Expr]
    -- free vars, param bindings, body
    | Lam [Text] [Binding] [Stmt]
    | Num Scientific
    | IString Text
    | Iden Text
    | MethodExpr Expr
    | RunTask Bool Expr
    | Cases SourcePos Expr [(Binding, [Stmt])]
    deriving (Eq,Show,Data)

data Binding
    = NameBinding Text
    | WildcardBinding
    | VariantBinding Text [Binding]
    | TupleBinding [Binding]
    deriving (Eq,Show,Data)
