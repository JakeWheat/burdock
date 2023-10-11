{-# LANGUAGE OverloadedStrings #-}
module Burdock.InterpreterPretty
    (prettyStmts
    ) where


import Burdock.InterpreterSyntax
import Burdock.Scientific (showScientific)


import Prettyprinter (Doc
                     ,parens
                     ,nest
                     ,(<+>)
                     ,sep
                     ,punctuate
                     ,comma
                     ,dquotes
                     ,vsep
                     --,hsep
                     ,layoutPretty
                     ,defaultLayoutOptions
                     ,brackets
                     ,braces
                     )

import qualified Prettyprinter as P

import Prettyprinter.Render.Text
    (renderLazy)

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

prettyStmts :: [Stmt] -> L.Text
prettyStmts ss = renderLazy $ layoutPretty defaultLayoutOptions $ stmts ss

stmts :: [Stmt] -> Doc a
stmts = vsep . map stmt

stmt :: Stmt -> Doc a

stmt (LetDecl b e) = nest 2 (bindExpr b e)
stmt (StmtExpr e) = expr e
stmt (VarDecl pn e) = pretty "var" <+> pretty pn <+> pretty "=" <+> expr e
stmt (SetVar n e) = pretty n <+> pretty ":=" <+> nest 2 (expr e)



expr :: Expr -> Doc a
expr (Block ss) = prettyBlocklike vsep
    [pretty "block:"
    ,stmts ss]
expr (If cs els) = vsep (prettyCs cs ++ pel els ++ [pretty "end"])
  where
    prettyCs [] = []
    prettyCs ((c,t):cs') = [pretty "if" <+> expr c <> pretty ":"
                           ,nest 2 (stmts t)]
                           ++ concat (map prettyEx cs')
    prettyEx (c,t) = [pretty "else" <+> pretty "if" <+> expr c <> pretty ":"
                     ,nest 2 (stmts t)]
    pel Nothing = []
    pel (Just e) = [pretty "else:"
                   ,nest 2 (stmts e)]
expr (DotExpr e i) = expr e <> pretty "." <> pretty i
expr (App _ e es) = expr e <> nest 2 (parens (commaSep $ map expr es))
expr (RecordSel es) =
    braces (commaSep $ map fld es)
  where
    fld (n,e) = pretty n <> pretty ":" <+> expr e
expr (TupleSel es) =
    braces (xSep ";" $ map expr es)
    
expr (Lam fv fh e) = prettyBlocklike vsep
    [pretty "lam"
     <> nest 2 (brackets (commaSep $ map pretty fv))
     <> parens (commaSep $ map binding fh) <> pretty ":"
    ,stmts e]
expr (Num n) = pretty $ showScientific n
expr (IString s) | '\n' `T.elem` s = pretty "```" <> pretty s <> pretty "```"
expr (IString s) = dquotes (pretty s)
  {-where
    escape ('\n':xs) = '\\':'n':escape xs
    escape ('\\':xs) = '\\':'\\':escape xs
    escape (x:xs) = x : escape xs
    escape [] = []-}
expr (Iden n) = pretty n
expr (MethodExpr m) = pretty "method"
    <> prettyBlocklike sep [expr m]
expr (RunTask _e es) = pretty "run-task" <> parens (expr es)
expr (Cases _ e mats) =
    prettyBlocklike vsep
    [pretty "cases" <+> expr e <> pretty ":"
    ,vsep (map mf mats)]
  where
    mf (p, e1) = pretty "|" <+> binding p <+> pretty "=>" <+> stmts e1

bindExpr :: Binding -> Expr -> Doc a
bindExpr n e =
    binding n <+> pretty "=" <+> nest 2 (expr e)

binding :: Binding -> Doc a
binding (NameBinding s) = pretty s
binding (WildcardBinding) = pretty "_"
binding (VariantBinding nms []) = pretty nms
binding (VariantBinding nms bs) =
    pretty nms <> parens (commaSep $ map binding bs)
binding (TupleBinding bs) =
    pretty "{"
    <> nest 2 (xSep ";" $ map binding bs)
    <> pretty "}"

prettyBlocklike :: ([Doc a] -> Doc a) ->  [Doc a] -> Doc a
prettyBlocklike sp bdy =
    sp [(nest 2 $ sp bdy), pretty "end"]

commaSep :: [Doc a] -> Doc a
commaSep = sep . punctuate comma

xSep :: T.Text -> [Doc a] -> Doc a
xSep s = sep . punctuate (pretty s)


pretty :: T.Text -> Doc ann
pretty = P.pretty
