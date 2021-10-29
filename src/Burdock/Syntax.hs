


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
    | LetDecl PatName Expr
    | Check (Maybe String) [Stmt]
    | VarDecl PatName Expr
    | SetVar String Expr
    | DataDecl String [VariantDecl] (Maybe [Stmt])
    | RecDecl PatName Expr
    | FunDecl
        PatName -- name
        [PatName] -- args
        Expr -- body
        (Maybe [Stmt]) -- test block
    | Provide [ProvideItem]
    | Import ImportSource String
    | Include ImportSource
    | IncludeFrom String [ProvideItem]
    deriving (Eq,Show,Data)

data VariantDecl = VariantDecl String [(Ref,String)]
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
    | App SourcePosition Expr [Expr]
    | BinOp Expr String Expr
    | Lam [PatName] Expr
    | Let [(PatName,Expr)] Expr
    | LetRec [(PatName,Expr)] Expr
    | Block [Stmt]
    | DotExpr Expr String
    | Cases String Expr [(Pat, Expr)] (Maybe Expr)
    | TupleSel [Expr]
    | RecordSel [(String,Expr)]
    | TupleGet Expr Int
    | Construct Expr [Expr]
    deriving (Eq,Show,Data)

data Pat = IdenP PatName
         | VariantP (Maybe String) String [PatName]
          deriving (Eq,Show,Data) 

data PatName =
      PatName Shadow String
    deriving (Eq,Show,Data)

data Shadow = NoShadow | Shadow
          deriving (Eq,Show,Data) 
