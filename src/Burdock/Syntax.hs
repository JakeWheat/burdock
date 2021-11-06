


{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.Syntax where

import Data.Data (Data)

import Burdock.Scientific

type SourcePosition = Maybe (FilePath, Int, Int)

data Script =
      Script [Stmt]
    deriving (Eq,Show,Data)

data Stmt =
      StmtExpr Expr
    | LetDecl Binding Expr
    | Check (Maybe String) [Stmt]
    | VarDecl Binding Expr
    | SetVar String Expr
    | DataDecl String [String] [VariantDecl] (Maybe [Stmt])
    | RecDecl Binding Expr
    | FunDecl
        Binding -- name
        FunHeader
        Expr -- body
        (Maybe [Stmt]) -- test block
    | TypeDecl String [String] Type
    | Provide [ProvideItem]
    | Import ImportSource String
    | Include ImportSource
    | IncludeFrom String [ProvideItem]
    deriving (Eq,Show,Data)

-- ty params args return ann
data FunHeader = FunHeader [String] [Binding] (Maybe Type)
    deriving (Eq,Show,Data)

data VariantDecl = VariantDecl String [(Ref,Binding)]
                 deriving (Eq,Show,Data) 

data Ref = Ref | Con
         deriving (Eq,Show,Data) 

data ProvideItem = ProvideAll
                 | ProvideAlias String String
                 | ProvideName String
                 deriving (Eq,Show,Data) 

data ImportSource = ImportSpecial String [String]
                  | ImportName String
                  deriving (Eq,Show,Data) 
----

data Expr =
      Num Scientific
    | Text String
    | Iden String
    | Parens Expr
    | If [(Expr,Expr)] (Maybe Expr)
    | App SourcePosition Expr [Type] [Expr]
    | BinOp Expr String Expr
    | Lam FunHeader Expr
    | Let [(Binding,Expr)] Expr
    | LetRec [(Binding,Expr)] Expr
    | Block [Stmt]
    | DotExpr Expr String
    | Cases Type Expr [(CaseBinding, Expr)] (Maybe Expr)
    | TupleSel [Expr]
    | RecordSel [(String,Expr)]
    | TupleGet Expr Int
    | Construct Expr [Expr]
    | AssertTypeCompat Expr Type
    deriving (Eq,Show,Data)

data CaseBinding = CaseBinding [String] [Binding]
                 deriving (Eq,Show,Data) 

data Binding =
      NameBinding Shadow String (Maybe Type)
    deriving (Eq,Show,Data)

data Shadow = NoShadow | Shadow
          deriving (Eq,Show,Data) 

data Type = TName [String]
          | TTuple [Type]
          | TRecord [(String,Type)]
          | TParam [String] [Type]
          | TArrow [Type] Type
          | TNamedArrow [(String,Type)] Type
          | TParens Type
          deriving (Eq,Show,Data)
