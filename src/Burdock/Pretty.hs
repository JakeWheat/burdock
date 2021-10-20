
module Burdock.Pretty
    (prettyExpr
    ,prettyScript
    ,prettyStmt
    ) where

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

import Burdock.Syntax


---------------------------------------

-- api

prettyExpr :: Expr -> String
prettyExpr e = show $ expr e

prettyScript :: Script -> String
prettyScript s = show $ script s

prettyStmt :: Stmt -> String
prettyStmt s = show $ stmt s

---------------------------------------

-- expressions


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
expr (If cs els) = sep (prettyCs cs ++ pel els ++ [pretty "end"])
  where
    prettyCs [] = []
    prettyCs ((c,t):cs') = [pretty "if" <+> expr c <> pretty ":"
                           ,nest 2 (expr t)]
                           ++ concat (map prettyEx cs')
    prettyEx (c,t) = [pretty "else" <+> pretty "if" <+> expr c <> pretty ":"
                     ,nest 2 (expr t)]
    pel Nothing = []
    pel (Just e) = [pretty "else:"
                   ,nest 2 (expr e)]
    
binding :: PatName -> Expr -> Doc a
binding n e =
    pretty n <+> pretty "=" <+> nest 2 (expr e)


-- first line
--    bdy lines -> can pass sep, or vsep to always have it vertical
--    even if it could fit on a line
-- end
prettyBlocklike :: ([Doc a] -> Doc a) ->  [Doc a] -> Doc a
prettyBlocklike sp bdy =
    sp [(nest 2 $ sp bdy), pretty "end"]


---------------------------------------

-- statements

stmt :: Stmt -> Doc a
stmt (StmtExpr e) = expr e
stmt (LetDecl b e) = nest 2 (binding b e)
stmt (Check nm s) = prettyBlocklike vsep 
        [case nm of
                Nothing -> pretty "check:"
                Just nm' -> pretty "check" <+> (expr $ Text nm') <> pretty ":"
        ,stmts s]
stmt (VarDecl pn e) = pretty "var" <+> pretty pn <+> pretty "=" <+> expr e
stmt (SetVar n e) = pretty n <+> pretty ":=" <+> nest 2 (expr e)

stmts :: [Stmt] -> Doc a
stmts = vsep . map stmt


script :: Script -> Doc a
script (Script iss) = stmts iss



commaSep :: [Doc a] -> Doc a
commaSep = sep . punctuate comma
