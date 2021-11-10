


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
    | When Expr Expr
    | LetDecl Binding Expr
    | Check (Maybe String) [Stmt]
    | VarDecl Binding Expr
    | SetVar String Expr
    | SetRef Expr [(String,Expr)]
    | DataDecl String [String] [VariantDecl] (Maybe [Stmt])
    | RecDecl Binding Expr
    | FunDecl
        Binding -- name
        FunHeader -- args and return type
        (Maybe String) -- doc string
        Expr -- body
        (Maybe [Stmt]) -- test block
    | TypeStmt TypeDecl
    | Contract String Ann
    | Provide [ProvideItem]
    | Import ImportSource String
    | Include ImportSource
    | IncludeFrom String [ProvideItem]
    deriving (Eq,Show,Data)

-- ty params args return ann
data FunHeader = FunHeader [String] [Binding] (Maybe Ann)
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
    | Ask [(Expr,Expr)] (Maybe Expr)
    | App SourcePosition Expr [Expr]
    | InstExpr Expr [Ann]
    | BinOp Expr String Expr
    | UnaryMinus Expr
    | Lam FunHeader Expr
    | CurlyLam FunHeader Expr
    | Let [(Binding,Expr)] Expr
    | LetRec [(Binding,Expr)] Expr
    | Block [Stmt]
    | DotExpr Expr String
    | Cases Expr (Maybe Ann) [(CaseBinding, Expr)] (Maybe Expr)
    | TupleSel [Expr]
    | RecordSel [(String,Expr)]
    | TupleGet Expr Int
    | Construct [String] [Expr]
    | AssertTypeCompat Expr Ann
    | TypeLet [TypeDecl] Expr
    | Template SourcePosition
    | UnboxRef Expr String
    deriving (Eq,Show,Data)

data TypeDecl = TypeDecl String [String] Ann
              deriving (Eq,Show,Data)

data CaseBinding = CaseBinding [String] [Binding]
                 deriving (Eq,Show,Data) 

data Binding =
      NameBinding Shadow String (Maybe Ann)
    deriving (Eq,Show,Data)

data Shadow = NoShadow | Shadow
          deriving (Eq,Show,Data) 

data Ann = TName [String]
                    | TTuple [Ann]
                    | TRecord [(String,Ann)]
                    | TParam [String] [Ann]
                    | TArrow [Ann] Ann
                    | TNamedArrow [(String,Ann)] Ann
                    | TParens Ann
                    deriving (Eq,Show,Data)
