
module Burdock.Pretty where

import Prettyprinter (pretty
                     ,Doc
                     ,parens
                     ,nest
                     ,(<+>)
                     ,sep
                     ,punctuate
                     ,comma
                     ,dquotes
                     ,vsep
                     )

{-import Data.Text (pack
                 ,unpack
                 ,Text)-}

import Burdock.Syntax


commaSep :: [Doc a] -> Doc a
commaSep = sep . punctuate comma



prettyExpr :: Expr -> String
prettyExpr e = show $ expr e

prettyScript :: Script -> String
prettyScript s = show $ script s

prettyStmt :: Stmt -> String
prettyStmt s = show $ stmt s



expr :: Expr -> Doc a
expr (Num n) = pretty $ show n
expr (Text s) = dquotes (pretty $ escape s)
  where
    escape ('\n':xs) = '\\':'n':escape xs
    escape ('\\':xs) = '\\':'\\':escape xs
    escape (x:xs) = x : escape xs
    escape [] = []
expr (Iden n) = pretty n
expr (Parens e) = parens (expr e)
expr (App e es) = expr e <> parens (commaSep $ map expr es)
expr (BinOp a op b) = expr a <+> pretty op <+> expr b
expr (Lam bs e) = prettyBlocklike sep
    [pretty "lam" <> parens (commaSep $ map pretty bs) <> pretty ":"
    ,expr e]
expr (Let bs e) = prettyBlocklike sep
    [pretty "let" <+> bs' <> pretty ":"
    ,expr e]
  where
    bs' | [(n,v)] <- bs = binding n v
        | otherwise = commaSep $ map (uncurry binding) bs
expr (LetRec bs e) = prettyBlocklike sep
    [pretty "letrec" <+> nest 2 (commaSep $ map (uncurry binding) bs) <> pretty ":"
    ,expr e]
expr (Block ss) = prettyBlocklike vsep
    [pretty "block:"
    ,stmts ss]

prettyBlocklike :: ([Doc a] -> Doc a) ->  [Doc a] -> Doc a
prettyBlocklike sp bdy =
    sp [(nest 2 $ sp bdy), pretty "end"]


stmt :: Stmt -> Doc a
stmt (StmtExpr e) = expr e
stmt (LetDecl b e) = nest 2 (binding b e)
stmt (Check nm s) = prettyBlocklike vsep 
        [case nm of
                Nothing -> pretty "check:"
                Just nm' -> pretty "check" <+> (expr $ Text nm') <> pretty ":"
        ,stmts s]

stmts :: [Stmt] -> Doc a
stmts = vsep . map stmt

binding :: PatName -> Expr -> Doc a
binding n e =
    pretty n <+> pretty "=" <+> nest 2 (expr e)

script :: Script -> Doc a
script (Script iss) = stmts iss

