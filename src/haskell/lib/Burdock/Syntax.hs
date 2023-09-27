
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Syntax where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)

import Burdock.Scientific

import Data.Text (Text)

type SourcePosition = Maybe (Text, Int, Int)

resetSourcePositions :: Data a => a -> a
resetSourcePositions = transformBi $ \case
    (_ :: Maybe (Text,Int,Int)) -> Nothing

data Script
    = Script [Stmt]
    deriving (Eq,Show,Data)

data Stmt
    = StmtExpr SourcePosition Expr
    | When SourcePosition Expr [Stmt]
    | LetDecl SourcePosition Binding Expr
    | VarDecl SourcePosition SimpleBinding Expr
    -- name, ty params, variants, shared methods, where block
    | DataDecl SourcePosition Text [Text] [VariantDecl] [(Text,Method)] (Maybe [Stmt])
    | RecDecl SourcePosition Binding Expr
    | FunDecl
        SourcePosition
        SimpleBinding -- name
        FunHeader -- args and return type
        (Maybe Text) -- doc string
        [Stmt] -- body
        (Maybe [Stmt]) -- test block
    | TypeStmt SourcePosition TypeDecl
    | Contract SourcePosition Text Ann
    | FFITypeStmt SourcePosition Text Text
    | SetVar SourcePosition Expr Expr
    | SetRef SourcePosition Expr [(Text,Expr)]
    | Check SourcePosition (Maybe Text) [Stmt]
    | Provide SourcePosition [ProvideItem]
    | ProvideFrom SourcePosition Text [ProvideItem]
    | Import SourcePosition ImportSource Text
    | Include SourcePosition ImportSource
    | IncludeFrom SourcePosition Text [ProvideItem]
    | ImportFrom SourcePosition ImportSource [ProvideItem]
    | UsePackage SourcePosition Text
    deriving (Eq,Show,Data)

-- ty params, args, return ann
data FunHeader
    = FunHeader [Text] [Binding] (Maybe Ann)
    deriving (Eq,Show,Data)

data VariantDecl
    = VariantDecl SourcePosition Text [(Ref,SimpleBinding)] [(Text,Method)]
    deriving (Eq,Show,Data) 

data Ref
    = Ref | Con
    deriving (Eq,Show,Data) 

data ProvideItem
    = ProvideName SourcePosition [Text]
    | ProvideAlias SourcePosition [Text] Text
    | ProvideAll SourcePosition
    | ProvideHiding SourcePosition [Text]
    
    | ProvideType SourcePosition [Text]
    | ProvideTypeAlias SourcePosition [Text] Text
    | ProvideTypeAll SourcePosition
    | ProvideTypeHiding SourcePosition [Text]
    
    | ProvideData SourcePosition [Text]
    | ProvideDataHiding SourcePosition [Text] [Text]
    | ProvideDataAll SourcePosition
    
    | ProvideModule SourcePosition [Text]
    | ProvideModuleAlias SourcePosition [Text] Text
    deriving (Eq,Show,Data) 

data ImportSource
    = ImportSpecial Text [Text]
    | ImportName Text
    deriving (Eq,Show,Data) 

data Expr
    = Num SourcePosition Scientific
    | Text SourcePosition Text
    | Iden SourcePosition Text
    | Parens SourcePosition Expr
    | If SourcePosition [(Expr,[Stmt])] (Maybe [Stmt])
    | Ask SourcePosition [(Expr,[Stmt])] (Maybe [Stmt])
    | App SourcePosition Expr [Expr]
    | InstExpr SourcePosition Expr [Ann]
    | BinOp SourcePosition Expr Text Expr
    | UnaryMinus SourcePosition Expr
    | Lam SourcePosition FunHeader [Stmt]
    | CurlyLam SourcePosition FunHeader [Stmt]
    | Let SourcePosition [(Binding,Expr)] [Stmt]
    | LetRec SourcePosition [(Binding,Expr)] [Stmt]
    | Block SourcePosition [Stmt]
    | DotExpr SourcePosition Expr Text
    | Cases SourcePosition Expr (Maybe Ann) [(Binding, Maybe Expr, [Stmt])] (Maybe [Stmt])
    | TupleSel SourcePosition [Expr]
    | RecordSel SourcePosition [(Text,Expr)]
    | Extend SourcePosition Expr [(Text,Expr)]
    | TableSel SourcePosition [Text] [RowSel]
    | TupleGet SourcePosition Expr Int
    | Construct SourcePosition [Text] [Expr]
    | AssertTypeCompat SourcePosition Expr Ann
    | TypeLet SourcePosition [TypeDecl] [Stmt]
    | Template SourcePosition
    | UnboxRef SourcePosition Expr Text
    | Receive SourcePosition [(Binding, Maybe Expr, [Stmt])] (Maybe (Expr, [Stmt]))
    | For SourcePosition Expr [(Binding, Expr)] (Maybe Ann) [Stmt]
    | MethodExpr SourcePosition Method
    deriving (Eq,Show,Data)

data Method
    = Method FunHeader [Stmt]
    deriving (Eq,Show,Data)

data TypeDecl
    = TypeDecl Text [Text] Ann
    deriving (Eq,Show,Data)

data Binding
    = NameBinding SourcePosition Text
    | ShadowBinding SourcePosition Text
    | VariantBinding SourcePosition [Text] [Binding]
    | TypedBinding SourcePosition Binding Ann
    | TupleBinding SourcePosition [Binding]
    | AsBinding SourcePosition Binding Shadow Text
    | WildcardBinding SourcePosition
    | NumberLitBinding SourcePosition Scientific
    | StringLitBinding SourcePosition Text
    deriving (Eq,Show,Data)

data SimpleBinding
    = SimpleBinding SourcePosition Shadow Text (Maybe Ann)
    deriving (Eq,Show,Data)

data Shadow
    = NoShadow | Shadow
    deriving (Eq,Show,Data) 

data RowSel
    = RowSel SourcePosition [Expr]
    deriving (Eq,Show,Data) 

data Ann
    = TName SourcePosition [Text]
    | TTuple SourcePosition [Ann]
    | TRecord SourcePosition [(Text,Ann)]
    | TParam SourcePosition [Text] [Ann]
    | TArrow SourcePosition [Ann] Ann
    | TNamedArrow SourcePosition [(Text,Ann)] Ann
    | TParens SourcePosition Ann
    deriving (Eq,Show,Data)
