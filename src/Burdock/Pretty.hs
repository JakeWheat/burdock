
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
import Data.Maybe (catMaybes)

import Burdock.Scientific (showScientific)

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
expr (Num n) = pretty $ showScientific n

-- todo handle parsing and printing escape chars properly
expr (Text s) | '\n' `elem` s = pretty "```" <> pretty s <> pretty "```"
expr (Text s) = dquotes (pretty $ escape s)
  where
    escape ('\n':xs) = '\\':'n':escape xs
    escape ('\\':xs) = '\\':'\\':escape xs
    escape (x:xs) = x : escape xs
    escape [] = []
expr (Iden n) = pretty n
expr (Parens e) = parens (expr e)
expr (InstExpr e ps) = expr e <> pretty "<" <> commaSep (map typ ps) <> pretty ">"
expr (App _sp e es) = expr e <> parens (commaSep $ map expr es)
expr (BinOp a op b) = expr a <+> pretty op <+> expr b
expr (UnaryMinus e) = pretty "-" <> expr e
expr (Lam fh e) = prettyBlocklike sep
    [pretty "lam" <> funHeader fh <> pretty ":"
    ,stmts e]
expr (CurlyLam fh e) =
    pretty "{" <> funHeader fh <> pretty ":" <+> stmts e <> pretty "}"

expr (Let bs e) = prettyBlocklike sep
    [pretty "let" <+> bs' <> pretty ":"
    ,stmts e]
  where
    bs' | [(n,v)] <- bs = bindExpr n v
        | otherwise = commaSep $ map (uncurry bindExpr) bs
expr (LetRec bs e) = prettyBlocklike sep
    [pretty "letrec" <+> nest 2 (commaSep $ map (uncurry bindExpr) bs) <> pretty ":"
    ,stmts e]
expr (Block ss) = prettyBlocklike vsep
    [pretty "block:"
    ,stmts ss]
expr (If cs els) = sep (prettyCs cs ++ pel els ++ [pretty "end"])
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
expr (Ask cs el) = prettyBlocklike vsep
    (pretty "ask:" : map prettyC cs ++ pel el)
  where
    prettyC (c,t) = pretty "|" <+> expr c <+> pretty "then:"
                    <+> nest 2 (stmts t)
    pel Nothing = []
    pel (Just e) = [pretty "|" <+> pretty "otherwise:" <+> nest 2 (stmts e)]
expr (DotExpr e i) = expr e <> pretty "." <> pretty i
expr (Cases e ty mats els) =
    prettyBlocklike vsep
    [pretty "cases" <+> expr e
     <> maybe mempty (\x -> pretty " ::" <+> typ x) ty <> pretty ":"
    ,vsep (map mf mats ++
           [maybe mempty (\x -> pretty "|" <+> pretty "else" <+> pretty "=>" <+> stmts x) els])]
  where
    mf (p, mw, e1) = pretty "|" <+> binding p <+> maybe mempty (\x -> pretty "when" <+> expr x) mw <+> pretty "=>" <+> stmts e1
expr (TupleSel es) = pretty "{" <> nest 2 (xSep ";" (map expr es) <> pretty "}")
expr (RecordSel flds) = pretty "{" <> nest 2 (commaSep (map fld flds) <> pretty "}")
  where
    fld (n,e) = pretty n <> pretty ":" <+> expr e
expr (TableSel cs rs) = prettyBlocklike sep
    (pretty "table" <+> commaSep (map pretty cs) <> pretty ":"
    : map rl rs)
  where
    rl (RowSel es) = pretty "row:" <+> commaSep (map expr  es)
expr (For ex args mrty bdy) =
    prettyBlocklike vsep
    [pretty "for" <+> expr ex
     <> parens (nest 2 $ commaSep (map f args))
     <> maybe mempty (\rty -> pretty " ->" <+> typ rty) mrty
     <> pretty ":"
    ,stmts bdy]
  where
    f (b,e) = binding b <+> pretty "from" <+> expr e
expr (TupleGet e n) = expr e <> pretty ".{" <> pretty (show n) <> pretty "}"
expr (Construct e as) =
    pretty "[" <> xSep "." (map pretty e) <> pretty ":"
    <+> nest 2 (commaSep $ map expr as) <> pretty "]"
expr (AssertTypeCompat e ty) =
    pretty "assert-type-compat(" <> nest 2 (expr e <+> pretty "::" <+> typ ty) <> pretty ")"
expr (TypeLet tds e) =
    prettyBlocklike sep
    [pretty "type-let" <+> commaSep (map typeDecl tds) <> pretty ":"
    ,stmts e]
expr (Template _sp) = pretty "..."
expr (UnboxRef e f) = expr e <> pretty "!" <> pretty f
expr (Receive mats after) =
    prettyBlocklike vsep
    [pretty "receive:"
    ,vsep (map mf mats ++
           [maybe mempty aft after])]
  where
    mf (p, mw, e1) =
        pretty "|" <+> binding p
        <+> (maybe mempty (\x -> pretty "when" <+> expr x) mw)
        <+> pretty "=>" <+> stmts e1
    aft (a, e) =
        pretty "|" <+> pretty "after" <+> expr a <+> pretty "=>" <+> stmts e
expr (MethodExpr m) = pretty "method" <+> method m

method :: Method -> Doc a
method (Method fh bdy) = prettyBlocklike sep
    [funHeader fh <> pretty ":"
    ,stmts bdy]

bindExpr :: Binding -> Expr -> Doc a
bindExpr n e =
    binding n <+> pretty "=" <+> nest 2 (expr e)

binding :: Binding -> Doc a
binding (NameBinding s) = pretty s
binding WildcardBinding = pretty "_"
binding (VariantBinding nms []) = xSep "." $ map pretty nms
binding (VariantBinding nms bs) =
    xSep "." (map pretty nms) <> parens (commaSep $ map binding bs)
binding (TypedBinding b t) = binding b <+> pretty "::" <+> typ t
binding (ShadowBinding s) = pretty "shadow" <+> pretty s
binding (AsBinding b as) = binding b <+> pretty "as" <+> pretty as
binding (TupleBinding bs) =
    pretty "{"
    <> nest 2 (xSep ";" $ map binding bs)
    <> pretty "}"
binding (NumberLitBinding n) = expr (Num n)
binding (StringLitBinding t) = expr (Text t)

simpleBinding :: SimpleBinding -> Doc a
simpleBinding (SimpleBinding s nm ty) =
    sep $ catMaybes [case s of
                        Shadow -> Just $ pretty "shadow"
                        NoShadow -> Nothing
                   ,Just $ pretty nm
                   ,fmap (\t -> pretty "::" <+> typ t) ty]

whereBlock :: [Stmt] -> Doc a
whereBlock ts = vsep
    [pretty "where:"
    ,nest 2 (stmts ts)]


typ :: Ann -> Doc a
typ (TName nms) = xSep "." $ map pretty nms
typ (TTuple ts) = pretty "{" <> nest 2 (xSep ";" $ map typ ts) <> pretty "}"
typ (TRecord fs) = pretty "{" <> nest 2 (xSep "," $ map f fs) <> pretty "}"
  where
    f(n,t) = pretty n <+> pretty "::" <+> typ t
typ (TParam t as) =
    (xSep "." $ map pretty t)
    <> pretty "<" <> nest 2 (xSep "," $ map typ as) <> pretty ">"
typ (TArrow ts t) = xSep "," (map typ ts) <+> pretty "->" <+> typ t
typ (TNamedArrow ts t) = pretty "(" <> xSep "," (map f ts) <> pretty ")" <+> pretty "->" <+> typ t
  where
    f(n,u) = pretty n <+> pretty "::" <+> typ u
typ (TParens t) = pretty "(" <> typ t <> pretty ")"

typeDecl :: TypeDecl -> Doc a
typeDecl (TypeDecl nm ps v) =
    pretty nm <>
       (case ps of
           [] -> mempty
           _ -> pretty "<" <> commaSep (map pretty ps) <> pretty ">")
    <+> pretty "=" <+> typ v


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
stmt (When c t) =
    pretty "when" <+> expr c <> pretty ":" <+> nest 2 (stmts t) <+> pretty "end"
stmt (LetDecl b e) = nest 2 (bindExpr b e)
stmt (Check nm s) = prettyBlocklike vsep 
        [case nm of
                Nothing -> pretty "check:"
                Just nm' -> pretty "check" <+> (expr $ Text nm') <> pretty ":"
        ,stmts s]
stmt (VarDecl pn e) = pretty "var" <+> simpleBinding pn <+> pretty "=" <+> expr e
stmt (SetVar n e) = pretty n <+> pretty ":=" <+> nest 2 (expr e)
stmt (SetRef e fs) = expr e <> pretty "!{" <> commaSep (map f fs) <> pretty "}"
  where
    f (n,v) = pretty n <> pretty ":" <+> expr v

stmt (DataDecl nm ts vs shr w) =
    prettyBlocklike vsep
    [pretty "data" <+> pretty nm <> tnl ts <> pretty ":"
    ,vsep $ map vf vs
    ,if null shr then mempty else sharing shr
    ,maybe mempty whereBlock w
    ]
  where
      vf (VariantDecl vnm fs meth) =
          pretty "|" <+> pretty vnm <> (case fs of
              [] -> mempty
              _ -> parens (commaSep $ map f fs))
          <> if null meth then mempty else wth meth
      f (r, x) = (case r of
                     Ref -> pretty "ref"
                     _ -> mempty)
                 <+> simpleBinding x
      tnl [] = mempty
      tnl xs = pretty "<" <> commaSep (map pretty xs) <> pretty ">"
      wth ms = pretty "" <+> pretty "with:" <+> nest 2 (commaSep $ map m ms)
      m (mnm, Method fh bdy) = pretty "method"
          <+> pretty mnm <> funHeader fh <> pretty ":"
          <+> prettyBlocklike vsep [(stmts bdy)]
      sharing s = vsep
          [pretty "sharing:"
          ,nest 2 $ commaSep $ map m s]


stmt (RecDecl n e) = pretty "rec" <+> bindExpr n e
stmt (FunDecl pn hdr ds e w) =
    prettyBlocklike sep
     [pretty "fun" <+> simpleBinding pn <> funHeader hdr <> pretty ":"
     ,maybe mempty (\x -> pretty "doc: " <+> expr (Text x)) ds
     ,stmts e
     ,maybe mempty whereBlock w]
stmt (TypeStmt td) = 
    pretty "type" <+> typeDecl td
stmt (FFITypeStmt nm ty) = 
    pretty "ffitype" <+> pretty nm <+> pretty "=" <+> dquotes (pretty ty)
stmt (Contract nm ty) = pretty nm <+> pretty "::" <+> typ ty

stmt (Provide pis) =
    prettyBlocklike vsep
         [pretty "provide:"
         ,commaSep $ map provideItem pis]
stmt (Include s) = pretty "include" <+> importSource s
stmt (IncludeFrom a pis) =
    prettyBlocklike vsep
         [pretty "include" <+> pretty "from" <+> pretty a <> pretty ":"
         ,nest 2 $ commaSep $ map provideItem pis]
stmt (Import is a) = pretty "import" <+> importSource is <+> pretty "as" <+> pretty a
stmt (ImportFrom is pis) =
    prettyBlocklike vsep
         [pretty "import" <+> pretty "from" <+> importSource is <> pretty ":"
         ,nest 2 $ commaSep $ map provideItem pis]
stmt (UsePackage d) = pretty "use" <+> pretty "package" <+> dquotes (pretty d)

funHeader :: FunHeader -> Doc a
funHeader (FunHeader ts as rt) =
    (case ts of
         [] -> mempty
         _ -> pretty "<" <> commaSep (map pretty ts) <> pretty ">")
    <> parens (commaSep $ map binding as)
    <> maybe mempty (\t -> pretty " ->" <+> typ t) rt

provideItem :: ProvideItem -> Doc a
provideItem ProvideAll = pretty "*"
provideItem (ProvideName n) = pretty n
provideItem (ProvideAlias n a) = pretty n <+> pretty "as" <+> pretty a

importSource :: ImportSource -> Doc a
importSource (ImportSpecial nm as) = pretty nm <> parens (commaSep $ map (dquotes . pretty) as)
importSource (ImportName s) = pretty s



stmts :: [Stmt] -> Doc a
stmts = vsep . map stmt


script :: Script -> Doc a
script (Script iss) = stmts iss



commaSep :: [Doc a] -> Doc a
commaSep = sep . punctuate comma

xSep :: String -> [Doc a] -> Doc a
xSep s = sep . punctuate (pretty s)
