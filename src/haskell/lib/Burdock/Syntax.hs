
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.Syntax where

import Data.Data (Data)

import Burdock.Scientific

type SourcePosition = Maybe (FilePath, Int, Int)

data Script
    = Script [Stmt]
    deriving (Eq,Show,Data)

data Stmt
    = StmtExpr SourcePosition Expr
    | When SourcePosition Expr [Stmt]
    | LetDecl SourcePosition Binding Expr
    | Check SourcePosition (Maybe String) [Stmt]
    | VarDecl SourcePosition SimpleBinding Expr
    | SetVar SourcePosition Expr Expr
    | SetRef SourcePosition Expr [(String,Expr)]
    -- name, ty params, variants, shared methods, where block
    | DataDecl SourcePosition String [String] [VariantDecl] [(String,Method)] (Maybe [Stmt])
    | RecDecl SourcePosition Binding Expr
    | FunDecl
        SourcePosition
        SimpleBinding -- name
        FunHeader -- args and return type
        (Maybe String) -- doc string
        [Stmt] -- body
        (Maybe [Stmt]) -- test block
    | TypeStmt SourcePosition TypeDecl
    | FFITypeStmt SourcePosition String String
    | Contract SourcePosition String Ann
    | Provide SourcePosition [ProvideItem]
    | Import SourcePosition ImportSource String
    | Include SourcePosition ImportSource
    | IncludeFrom SourcePosition String [ProvideItem]
    | ImportFrom SourcePosition ImportSource [ProvideItem]
    | UsePackage SourcePosition FilePath
    deriving (Eq,Show,Data)

-- ty params, args, return ann
data FunHeader
    = FunHeader [String] [Binding] (Maybe Ann)
    deriving (Eq,Show,Data)

data VariantDecl
    = VariantDecl SourcePosition String [(Ref,SimpleBinding)] [(String,Method)]
    deriving (Eq,Show,Data) 

data Ref
    = Ref | Con
    deriving (Eq,Show,Data) 

data ProvideItem
    = ProvideAll SourcePosition 
    | ProvideAlias SourcePosition String String
    | ProvideName SourcePosition String
    | ProvideType SourcePosition String
    | ProvideData SourcePosition String
    deriving (Eq,Show,Data) 

data ImportSource
    = ImportSpecial String [String]
    | ImportName String
    deriving (Eq,Show,Data) 

data Expr
    = Num SourcePosition Scientific
    | Text SourcePosition String
    | Iden SourcePosition String
    | Parens SourcePosition Expr
    | If SourcePosition [(Expr,[Stmt])] (Maybe [Stmt])
    | Ask SourcePosition [(Expr,[Stmt])] (Maybe [Stmt])
    | App SourcePosition Expr [Expr]
    | InstExpr SourcePosition Expr [Ann]
    | BinOp SourcePosition Expr String Expr
    | UnaryMinus SourcePosition Expr
    | Lam SourcePosition FunHeader [Stmt]
    | CurlyLam SourcePosition FunHeader [Stmt]
    | Let SourcePosition [(Binding,Expr)] [Stmt]
    | LetRec SourcePosition [(Binding,Expr)] [Stmt]
    | Block SourcePosition [Stmt]
    | DotExpr SourcePosition Expr String
    | Cases SourcePosition Expr (Maybe Ann) [(Binding, Maybe Expr, [Stmt])] (Maybe [Stmt])
    | TupleSel SourcePosition [Expr]
    | RecordSel SourcePosition [(String,Expr)]
    | Extend SourcePosition Expr [(String,Expr)]
    | TableSel SourcePosition [String] [RowSel]
    | TupleGet SourcePosition Expr Int
    | Construct SourcePosition [String] [Expr]
    | AssertTypeCompat SourcePosition Expr Ann
    | TypeLet SourcePosition [TypeDecl] [Stmt]
    | Template SourcePosition
    | UnboxRef SourcePosition Expr String
    | Receive SourcePosition [(Binding, Maybe Expr, [Stmt])] (Maybe (Expr, [Stmt]))
    | For SourcePosition Expr [(Binding, Expr)] (Maybe Ann) [Stmt]
    | MethodExpr SourcePosition Method
    deriving (Eq,Show,Data)

data Method
    = Method FunHeader [Stmt]
    deriving (Eq,Show,Data)

data TypeDecl
    = TypeDecl String [String] Ann
    deriving (Eq,Show,Data)

data Binding
    = NameBinding SourcePosition String
    | VariantBinding SourcePosition [String] [Binding]
    | TypedBinding SourcePosition Binding Ann
    | ShadowBinding SourcePosition String
    | WildcardBinding SourcePosition
    | AsBinding SourcePosition Binding Shadow String
    | TupleBinding SourcePosition [Binding]
    | NumberLitBinding SourcePosition Scientific
    | StringLitBinding SourcePosition String
    deriving (Eq,Show,Data)

data SimpleBinding
    = SimpleBinding SourcePosition Shadow String (Maybe Ann)
    deriving (Eq,Show,Data)

data Shadow
    = NoShadow | Shadow
    deriving (Eq,Show,Data) 

data RowSel
    = RowSel SourcePosition [Expr]
    deriving (Eq,Show,Data) 

data Ann
    = TName SourcePosition [String]
    | TTuple SourcePosition [Ann]
    | TRecord SourcePosition [(String,Ann)]
    | TParam SourcePosition [String] [Ann]
    | TArrow SourcePosition [Ann] Ann
    | TNamedArrow SourcePosition [(String,Ann)] Ann
    | TParens SourcePosition Ann
    deriving (Eq,Show,Data)
