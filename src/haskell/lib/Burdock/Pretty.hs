
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Pretty
    (prettyExpr
    ,prettyScript
    ,prettyStmt
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (show)

import Prettyprinter (Doc
                     ,parens
                     ,nest
                     ,(<+>)
                     ,sep
                     ,punctuate
                     ,comma
                     ,dquotes
                     ,vsep
                     ,hsep
                     ,layoutPretty
                     ,defaultLayoutOptions
                     )
import qualified Prettyprinter as P

import Prettyprinter.Render.Text (renderLazy)


import Data.Maybe (catMaybes)

import Burdock.Scientific (showScientific)

import Burdock.Syntax

import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Text (Text)

---------------------------------------

-- api

prettyExpr :: Expr -> L.Text
prettyExpr e = render $ expr e

prettyScript :: Script -> L.Text
prettyScript s = render $ script s

prettyStmt :: Stmt -> L.Text
prettyStmt s = render $ stmt s

render :: Doc a -> L.Text
render = renderLazy . layoutPretty defaultLayoutOptions

---------------------------------------

-- expressions


expr :: Expr -> Doc a

expr (Num _ n) = pretty $ showScientific n

-- todo handle parsing and printing escape chars properly
expr (Text _ s) | '\n' `T.elem` s = pretty "```" <> pretty s <> pretty "```"
expr (Text _ s) = dquotes (pretty $ T.pack $ escape $ T.unpack s)
  where
    escape ('\n':xs) = '\\':'n':escape xs
    escape ('\\':xs) = '\\':'\\':escape xs
    escape (x:xs) = x : escape xs
    escape [] = []
expr (Iden _ n) = pretty n
expr (Parens _ e) = parens (expr e)
expr (InstExpr _ e ps) = expr e <> pretty "<" <> commaSep (map typ ps) <> pretty ">"
expr (App _sp e es) = expr e <> parens (commaSep $ map expr es)
expr (BinOp _ a op b) = expr a <+> pretty op <+> expr b
expr (UnaryMinus _ e) = pretty "-" <> expr e
expr (Lam _ fh e) = prettyBlocklike vsep
    [pretty "lam" <> funHeader fh <> pretty ":"
    ,stmts e]
expr (CurlyLam _ fh e) =
    pretty "{" <> funHeader fh <> pretty ":" <+> stmts e <> pretty "}"

expr (Let _ bs e) = prettyBlocklike vsep
    [pretty "let" <+> bs' <> pretty ":"
    ,stmts e]
  where
    bs' | [(n,v)] <- bs = bindExpr n v
        | otherwise = commaSep $ map (uncurry bindExpr) bs
expr (LetRec _ bs e) = prettyBlocklike vsep
    [pretty "letrec" <+> nest 2 (commaSep $ map (uncurry bindExpr) bs) <> pretty ":"
    ,stmts e]
expr (Block _ ss) = prettyBlocklike vsep
    [pretty "block:"
    ,stmts ss]
expr (If _ cs els) = vsep (prettyCs cs ++ pel els ++ [pretty "end"])
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
expr (Ask _ cs el) = prettyBlocklike vsep
    (pretty "ask:" : map prettyC cs ++ pel el)
  where
    prettyC (c,t) = pretty "|" <+> expr c <+> pretty "then:"
                    <+> nest 2 (stmts t)
    pel Nothing = []
    pel (Just e) = [pretty "|" <+> pretty "otherwise:" <+> nest 2 (stmts e)]
expr (DotExpr _ e i) = expr e <> pretty "." <> pretty i
expr (Cases _ e ty mats els) =
    prettyBlocklike vsep
    [pretty "cases" <+> expr e
     <> maybe mempty (\x -> pretty " ::" <+> typ x) ty <> pretty ":"
    ,vsep (map mf mats ++
           [maybe mempty (\x -> pretty "|" <+> pretty "else" <+> pretty "=>" <+> stmts x) els])]
  where
    mf (p, mw, e1) = pretty "|" <+> binding p <+> maybe mempty (\x -> pretty "when" <+> expr x) mw <+> pretty "=>" <+> stmts e1
expr (TupleSel _ es) = pretty "{" <> nest 2 (xSep ";" (map expr es) <> pretty "}")
expr (RecordSel _ flds) = pretty "{" <> nest 2 (commaSep (map fld flds) <> pretty "}")
  where
    fld (n,e) = pretty n <> pretty ":" <+> expr e
expr (Extend _ v flds) = expr v <> pretty ".{" <> nest 2 (commaSep (map fld flds) <> pretty "}")
  where
    fld (n,e) = pretty n <> pretty ":" <+> expr e
expr (TableSel _ cs rs) = prettyBlocklike vsep
    (pretty "table" <+> commaSep (map pretty cs) <> pretty ":"
    : map rl rs)
  where
    rl (RowSel _ es) = pretty "row:" <+> commaSep (map expr  es)
expr (For _ ex args mrty bdy) =
    prettyBlocklike vsep
    [pretty "for" <+> expr ex
     <> parens (nest 2 $ commaSep (map f args))
     <> maybe mempty (\rty -> pretty " ->" <+> typ rty) mrty
     <> pretty ":"
    ,stmts bdy]
  where
    f (b,e) = binding b <+> pretty "from" <+> expr e
expr (TupleGet _ e n) = expr e <> pretty ".{" <> pretty (show n) <> pretty "}"
expr (Construct _ e as) =
    pretty "[" <> dotIden e <> pretty ":"
    <+> nest 2 (commaSep $ map expr as) <> pretty "]"
expr (AssertTypeCompat _ e ty) =
    pretty "assert-type-compat(" <> nest 2 (expr e <+> pretty "::" <+> typ ty) <> pretty ")"
expr (TypeLet _ tds e) =
    prettyBlocklike vsep
    [pretty "type-let" <+> commaSep (map typeDecl tds) <> pretty ":"
    ,stmts e]
expr (Template _sp) = pretty "..."
expr (UnboxRef _ e f) = expr e <> pretty "!" <> pretty f
expr (Receive _ mats after) =
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
expr (MethodExpr _ m) = pretty "method" <+> method m

method :: Method -> Doc a
method (Method fh bdy) = prettyBlocklike vsep
    [funHeader fh <> pretty ":"
    ,stmts bdy]

bindExpr :: Binding -> Expr -> Doc a
bindExpr n e =
    binding n <+> pretty "=" <+> nest 2 (expr e)

binding :: Binding -> Doc a
binding (NameBinding _ s) = pretty s
binding (WildcardBinding _) = pretty "_"
binding (VariantBinding _ nms []) = dotIden nms
binding (VariantBinding _ nms bs) =
    dotIden nms <> parens (commaSep $ map binding bs)
binding (TypedBinding _ b t) = binding b <+> pretty "::" <+> typ t
binding (ShadowBinding _ s) = pretty "shadow" <+> pretty s
binding (AsBinding _ b s as) =
    hsep $ catMaybes
    [Just $ binding b
    ,Just $ pretty "as"
    ,case s of
         Shadow -> Just $ pretty "shadow"
         NoShadow -> Nothing
    ,Just $ pretty as]
binding (TupleBinding _ bs) =
    pretty "{"
    <> nest 2 (xSep ";" $ map binding bs)
    <> pretty "}"
binding (NumberLitBinding _ n) = expr (Num np n)
binding (StringLitBinding _ t) = expr (Text np t)

np :: SourcePosition
np = Nothing

simpleBinding :: SimpleBinding -> Doc a
simpleBinding (SimpleBinding _ s nm ty) =
    vsep $ catMaybes [case s of
                        Shadow -> Just $ pretty "shadow"
                        NoShadow -> Nothing
                    ,Just $ pretty nm
                    ,fmap (\t -> pretty "::" <+> typ t) ty]


whereBlock :: [Stmt] -> Doc a
whereBlock ts = vsep
    [pretty "where:"
    ,nest 2 (stmts ts)]


typ :: Ann -> Doc a
typ (TName _ nms) = dotIden nms
typ (TTuple _ ts) = pretty "{" <> nest 2 (xSep ";" $ map typ ts) <> pretty "}"
typ (TRecord _ fs) = pretty "{" <> nest 2 (xSep "," $ map f fs) <> pretty "}"
  where
    f(n,t) = pretty n <+> pretty "::" <+> typ t
typ (TParam _ t as) =
    dotIden t
    <> pretty "<" <> nest 2 (xSep "," $ map typ as) <> pretty ">"
typ (TArrow _ ts t) = xSep "," (map typ ts) <+> pretty "->" <+> typ t
typ (TNamedArrow _ ts t) = pretty "(" <> xSep "," (map f ts) <> pretty ")" <+> pretty "->" <+> typ t
  where
    f(n,u) = pretty n <+> pretty "::" <+> typ u
typ (TParens _ t) = pretty "(" <> typ t <> pretty ")"

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
stmt (StmtExpr _ e) = expr e
stmt (When _ c t) =
    pretty "when" <+> expr c <> pretty ":" <+> nest 2 (stmts t) <+> pretty "end"
stmt (LetDecl _ b e) = nest 2 (bindExpr b e)
stmt (Check _ nm s) = prettyBlocklike vsep 
        [case nm of
                Nothing -> pretty "check:"
                Just nm' -> pretty "check" <+> (expr $ Text np nm') <> pretty ":"
        ,stmts s]
stmt (VarDecl _ pn e) = pretty "var" <+> simpleBinding pn <+> pretty "=" <+> expr e
stmt (SetVar _ n e) = expr n <+> pretty ":=" <+> nest 2 (expr e)
stmt (SetRef _ e fs) = expr e <> pretty "!{" <> commaSep (map f fs) <> pretty "}"
  where
    f (n,v) = pretty n <> pretty ":" <+> expr v

stmt (DataDecl _ nm ts vs shr w) =
    prettyBlocklike vsep
    [pretty "data" <+> pretty nm <> tnl ts <> pretty ":"
    ,vsep $ map vf vs
    ,if null shr then mempty else sharing shr
    ,maybe mempty whereBlock w
    ]
  where
      vf (VariantDecl _ vnm fs meth) =
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


stmt (RecDecl _ n e) = pretty "rec" <+> bindExpr n e
stmt (FunDecl _ pn hdr ds e w) =
    prettyBlocklike vsep
     [pretty "fun" <+> simpleBinding pn <> funHeader hdr <> pretty ":"
     ,maybe mempty (\x -> pretty "doc: " <+> expr (Text np x)) ds
     ,stmts e
     ,maybe mempty whereBlock w]
stmt (TypeStmt _ td) = 
    pretty "type" <+> typeDecl td
stmt (FFITypeStmt _ nm ty) = 
    pretty "ffitype" <+> pretty nm <+> pretty "=" <+> dquotes (pretty ty)
stmt (Contract _ nm ty) = pretty nm <+> pretty "::" <+> typ ty

stmt (Provide _ pis) =
    prettyBlocklike vsep
         [pretty "provide:"
         ,commaSep $ map provideItem pis]
stmt (ProvideFrom _ al pis) =
    prettyBlocklike vsep
         [pretty "provide"<+> pretty "from" <+> pretty al <> pretty ":"
         ,commaSep $ map provideItem pis]

stmt (Include _ s) = pretty "include" <+> importSource s
stmt (IncludeFrom _ a pis) =
    prettyBlocklike vsep
         [pretty "include" <+> pretty "from" <+> pretty a <> pretty ":"
         ,nest 2 $ commaSep $ map provideItem pis]
stmt (Import _ is a) = pretty "import" <+> importSource is <+> pretty "as" <+> pretty a
stmt (ImportFrom _ is pis) =
    prettyBlocklike vsep
         [pretty "import" <+> pretty "from" <+> importSource is <> pretty ":"
         ,nest 2 $ commaSep $ map provideItem pis]
stmt (UsePackage _ d) = pretty "use" <+> pretty "package" <+> dquotes (pretty d)

funHeader :: FunHeader -> Doc a
funHeader (FunHeader ts as rt) =
    (case ts of
         [] -> mempty
         _ -> pretty "<" <> commaSep (map pretty ts) <> pretty ">")
    <> parens (commaSep $ map binding as)
    <> maybe mempty (\t -> pretty " ->" <+> typ t) rt

provideItem :: ProvideItem -> Doc a
provideItem (ProvideName _ n) = dotIden n
provideItem (ProvideAlias _ n a) = dotIden n <+> pretty "as" <+> pretty a
provideItem (ProvideAll _) = pretty "*"
provideItem (ProvideHiding _ ns) =
    pretty "*" <+> pretty "hiding" <> (parens $ commaSep $ map pretty ns)

provideItem (ProvideType _ n) = pretty "type" <+> dotIden n
provideItem (ProvideTypeAlias _ n a) = pretty "type" <+> dotIden n <+> pretty "as" <+> pretty a
provideItem (ProvideTypeAll _) = pretty "type" <+> pretty "*"
provideItem (ProvideTypeHiding _ ns) =
    pretty "type" <+> pretty "*" <+> pretty "hiding" <> (parens $ commaSep $ map pretty ns)

provideItem (ProvideData _ n) = pretty "data" <+> dotIden n
provideItem (ProvideDataHiding _ n ns) =
    pretty "data" <+> dotIden n <+> pretty "hiding" <> (parens $ commaSep $ map pretty ns)
provideItem (ProvideDataAll _) = pretty "data" <+> pretty "*"

provideItem (ProvideModule _ ns) = pretty "module" <+> dotIden ns
provideItem (ProvideModuleAlias _ ns a) =
    pretty "module" <+> dotIden ns <+> pretty "as" <+> pretty a

importSource :: ImportSource -> Doc a
importSource (ImportSpecial nm as) = pretty nm <> parens (commaSep $ map (dquotes . pretty) as)
importSource (ImportName s) = xSep "." $ map pretty s

stmts :: [Stmt] -> Doc a
stmts = vsep . map stmt

script :: Script -> Doc a
script (Script iss) = stmts iss

-- regular pretty completely defeats the type checker when you want
-- to change the ast and get type errors, instead it just produces
-- incorrect code.
pretty :: Text -> Doc a
pretty = P.pretty

commaSep :: [Doc a] -> Doc a
commaSep = sep . punctuate comma

xSep :: Text -> [Doc a] -> Doc a
xSep s = sep . punctuate (pretty s)

dotIden :: [Text] -> Doc ann
dotIden ts = sep $ punctuate (pretty ".") $ map pretty ts

