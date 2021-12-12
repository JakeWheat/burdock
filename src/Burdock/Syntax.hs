
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.Syntax where

import Data.Data (Data)

import Burdock.Scientific

type SourcePosition = Maybe (FilePath, Int, Int)

data Script
    = Script [Stmt]
    deriving (Eq,Show,Data)

data Stmt
    = StmtExpr Expr
    | When Expr [Stmt]
    | LetDecl Binding Expr
    | Check (Maybe String) [Stmt]
    | VarDecl SimpleBinding Expr
    | SetVar String Expr
    | SetRef Expr [(String,Expr)]
    -- name, ty params, variants, shared methods, where block
    | DataDecl String [String] [VariantDecl] [(String,Method)] (Maybe [Stmt])
    | RecDecl Binding Expr
    | FunDecl
        SimpleBinding -- name
        FunHeader -- args and return type
        (Maybe String) -- doc string
        [Stmt] -- body
        (Maybe [Stmt]) -- test block
    | TypeStmt TypeDecl
    | FFITypeStmt String String
    | Contract String Ann
    | Provide [ProvideItem]
    | Import ImportSource String
    | Include ImportSource
    | IncludeFrom String [ProvideItem]
    | ImportFrom ImportSource [ProvideItem]
    | UsePackage FilePath
    deriving (Eq,Show,Data)

-- ty params, args, return ann
data FunHeader
    = FunHeader [String] [Binding] (Maybe Ann)
    deriving (Eq,Show,Data)

data VariantDecl
    = VariantDecl String [(Ref,SimpleBinding)] [(String,Method)]
    deriving (Eq,Show,Data) 

data Ref
    = Ref | Con
    deriving (Eq,Show,Data) 

data ProvideItem
    = ProvideAll
    | ProvideAlias String String
    | ProvideName String
    deriving (Eq,Show,Data) 

data ImportSource
    = ImportSpecial String [String]
    | ImportName String
    deriving (Eq,Show,Data) 

data Expr
    = Num Scientific
    | Text String
    | Iden String
    | Parens Expr
    | If [(Expr,[Stmt])] (Maybe [Stmt])
    | Ask [(Expr,[Stmt])] (Maybe [Stmt])
    | App SourcePosition Expr [Expr]
    | InstExpr Expr [Ann]
    | BinOp Expr String Expr
    | UnaryMinus Expr
    | Lam FunHeader [Stmt]
    | CurlyLam FunHeader [Stmt]
    | Let [(Binding,Expr)] [Stmt]
    | LetRec [(Binding,Expr)] [Stmt]
    | Block [Stmt]
    | DotExpr Expr String
    | Cases Expr (Maybe Ann) [(Binding, Maybe Expr, [Stmt])] (Maybe [Stmt])
    | TupleSel [Expr]
    | RecordSel [(String,Expr)]
    | TableSel [String] [RowSel]
    | TupleGet Expr Int
    | Construct [String] [Expr]
    | AssertTypeCompat Expr Ann
    | TypeLet [TypeDecl] [Stmt]
    | Template SourcePosition
    | UnboxRef Expr String
    | Receive [(Binding, Maybe Expr, [Stmt])] (Maybe (Expr, [Stmt]))
    | For Expr [(Binding, Expr)] (Maybe Ann) [Stmt]
    | MethodExpr Method
    deriving (Eq,Show,Data)

data Method
    = Method FunHeader [Stmt]
    deriving (Eq,Show,Data)

data TypeDecl
    = TypeDecl String [String] Ann
    deriving (Eq,Show,Data)

data Binding
    = NameBinding String
    | VariantBinding [String] [Binding]
    | TypedBinding Binding Ann
    | ShadowBinding String
    | WildcardBinding
    | AsBinding Binding String
    | TupleBinding [Binding]
    | NumberLitBinding Scientific
    | StringLitBinding String
    deriving (Eq,Show,Data)

data SimpleBinding
    = SimpleBinding Shadow String (Maybe Ann)
    deriving (Eq,Show,Data)

data Shadow
    = NoShadow | Shadow
    deriving (Eq,Show,Data) 

data RowSel
    = RowSel [Expr]
    deriving (Eq,Show,Data) 

data Ann
    = TName [String]
    | TTuple [Ann]
    | TRecord [(String,Ann)]
    | TParam [String] [Ann]
    | TArrow [Ann] Ann
    | TNamedArrow [(String,Ann)] Ann
    | TParens Ann
    deriving (Eq,Show,Data)
