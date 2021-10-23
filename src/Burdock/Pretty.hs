
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
    [pretty "lam" <> parens (commaSep $ map patName bs) <> pretty ":"
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
expr (DotExpr e i) = expr e <> pretty "." <> pretty i
expr (Cases ty e mats els) =
    prettyBlocklike vsep
    [pretty "cases" <> parens (pretty ty) <+> expr e <> pretty ":"
    ,vsep (map mf mats ++
           [maybe mempty (\x -> pretty "|" <+> pretty "else" <+> pretty "=>" <+> expr x) els])]
  where
    mf (p, e1) = pretty "|" <+> pat p <+> pretty "=>" <+> expr e1

    
binding :: PatName -> Expr -> Doc a
binding n e =
    patName n <+> pretty "=" <+> nest 2 (expr e)

pat :: Pat -> Doc a
pat (IdenP pn) = patName pn
pat (VariantP q c ps) = maybe mempty (\a -> pretty a <> pretty ".") q
                        <> pretty c <> parens (commaSep $ map patName ps)

patName :: PatName -> Doc a
patName (PatName s nm) =
    case s of
        Shadow -> pretty "shadow" <+> pretty nm
        NoShadow -> pretty nm

whereBlock :: [Stmt] -> Doc a
whereBlock ts = vsep
    [pretty "where:"
    ,nest 2 (stmts ts)]



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
stmt (VarDecl pn e) = pretty "var" <+> patName pn <+> pretty "=" <+> expr e
stmt (SetVar n e) = pretty n <+> pretty ":=" <+> nest 2 (expr e)

stmt (DataDecl nm vs w) =
    prettyBlocklike vsep
    [pretty "data" <+> pretty nm <+> pretty ":"
    ,vsep $ map vf vs
    ,maybe mempty whereBlock w
    ]
  where
      vf (VariantDecl vnm fs) =
          pretty "|" <+> pretty vnm <> case fs of
              [] -> mempty
              _ -> parens (commaSep $ map f fs)
      f (m, x) = (case m of
                     Ref -> pretty "ref"
                     _ -> mempty)
                 <+> pretty x

stmt (RecDecl n e) = pretty "rec" <+> binding n e
stmt (FunDecl pn as e w) =
    prettyBlocklike sep
     [pretty "fun" <+> patName pn <+> parens (commaSep $ map patName as) <> pretty ":"
     ,expr e
     ,maybe mempty whereBlock w]



stmts :: [Stmt] -> Doc a
stmts = vsep . map stmt


script :: Script -> Doc a
script (Script iss) = stmts iss



commaSep :: [Doc a] -> Doc a
commaSep = sep . punctuate comma
