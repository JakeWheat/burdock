
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Desugar
    (desugarScript
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)

import Data.Text (Text)
import qualified Data.Text.Lazy as L

import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Burdock.StaticError (StaticError)

import qualified Burdock.Pretty as P

import Control.Arrow (first)

------------------------------------------------------------------------------

desugarScript :: Text
              -> S.Script
              -> Either [StaticError] [I.Stmt]
desugarScript _fn (S.Script stmts) = pure $ desugarStmts stmts

------------------------------------------------------------------------------

desugarStmts :: [S.Stmt] -> [I.Stmt]
desugarStmts ss = map desugarStmt $ desugarRecs $ desugarDataDecls ss

------------------------------------------------------------------------------

desugarStmt :: S.Stmt -> I.Stmt
desugarStmt (S.Check sp _ bdy) = desugarStmt $ S.StmtExpr sp $ S.Block sp bdy
desugarStmt (S.StmtExpr sp e) = I.StmtExpr sp $ desugarExpr e
desugarStmt (S.LetDecl sp b e) = I.LetDecl sp (desugarBinding b) (desugarExpr e)
desugarStmt (S.VarDecl sp (S.SimpleBinding _ _ nm Nothing) e) = I.VarDecl sp nm (desugarExpr e)
desugarStmt (S.SetVar sp (S.Iden _ nm) e) = I.SetVar sp [nm] (desugarExpr e)

desugarStmt (S.When sp e bdy) =
    desugarStmt $ S.StmtExpr sp $ S.If sp [(e,bdy)] (Just [S.StmtExpr sp $ S.DotExpr sp (S.Iden sp "_bootstrap") "nothing"])

desugarStmt s = error $ "desugarStmt " <> show s

------------------------------------------------------------------------------

desugarExpr :: S.Expr -> I.Expr
desugarExpr (S.BinOp sp e0 "is" e1) = do
    let msg = L.toStrict $ P.prettyExpr e0 <> " is " <> P.prettyExpr e1
    desugarExpr $ app "run-binary-test"
        [S.Text sp msg
        ,wrapit e0
        ,wrapit e1
        ,lam ["a", "b"] (S.BinOp sp (S.Iden sp "a") "==" (S.Iden sp "b"))
        ,S.Text sp "!="
        ]
  where
    app nm as = S.App sp (S.DotExpr sp (S.Iden sp "_bootstrap") nm) as
    wrapit e = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp e]
    lam as e = S.Lam sp (S.FunHeader [] (flip map as $ S.NameBinding sp) Nothing) [S.StmtExpr sp e]

desugarExpr (S.BinOp sp e0 "is-not" e1) = do
    let msg = L.toStrict $ P.prettyExpr e0 <> " is " <> P.prettyExpr e1
    desugarExpr $ app "run-binary-test"
        [S.Text sp msg
        ,wrapit e0
        ,wrapit e1
        ,lam ["a", "b"] (S.BinOp sp (S.Iden sp "a") "!=" (S.Iden sp "b"))
        ,S.Text sp "=="
        ]
  where
    app nm as = S.App sp (S.DotExpr sp (S.Iden sp "_bootstrap") nm) as
    wrapit e = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp e]
    lam as e = S.Lam sp (S.FunHeader [] (flip map as $ S.NameBinding sp) Nothing) [S.StmtExpr sp e]


desugarExpr (S.Block sp bdy) = I.Block sp $ desugarStmts bdy
desugarExpr (S.App sp f as) = I.App sp (desugarExpr f) (map desugarExpr as)
desugarExpr (S.DotExpr sp e f) = I.DotExpr sp (desugarExpr e) f
desugarExpr (S.Iden sp i) = I.Iden sp i
desugarExpr (S.Text sp t) = I.IString sp t
desugarExpr (S.Num sp n) = I.Num sp n
desugarExpr (S.Parens _ e) = desugarExpr e
desugarExpr (S.Lam sp (S.FunHeader [] bs Nothing) bdy)
    = I.Lam sp [] (map desugarBinding bs) $ desugarStmts bdy


desugarExpr (S.BinOp sp e0 op e1) | Just op' <- lookup op methOps =
    desugarExpr (S.App sp (S.DotExpr sp e0 op') [e1])
  where
    methOps =
        [("==", "_equals")
        ,("<=", "_lessequal")
        ,(">=", "_greaterequal")
        ,("<", "_lessthan")
        ,(">", "_greaterthan")
        ,("+", "_plus")
        ,("-", "_minus")
        ,("*", "_times")
        ,("/", "_divide")
        ]

desugarExpr (S.If sp ts els) =
    I.If sp (flip map ts $ \(e,bdy) -> (desugarExpr e, desugarStmts bdy)) $ fmap desugarStmts els

desugarExpr e = error $ "desugarExpr " <> show e

------------------------------------------------------------------------------

desugarBinding :: S.Binding -> I.Binding
desugarBinding (S.NameBinding sp nm) = I.NameBinding sp nm
desugarBinding (S.ShadowBinding sp nm) = I.NameBinding sp nm
desugarBinding b = error $ "desugarBinding " <> show b

------------------------------------------------------------------------------

desugarRecs :: [S.Stmt] -> [S.Stmt]
desugarRecs [] = []
desugarRecs ss =
    let (rs,ctu) = getRecs ss
    in if null rs
       then case ctu of
            (t:ts) -> t : desugarRecs ts
            [] -> []
       else let (vars,sets) = unzip $ flip map rs $ \(a,b,c) -> makeRec a b c
            in vars ++ sets ++ desugarRecs ctu
  where
    getRecs :: [S.Stmt] -> ([(S.SourcePosition, Text, S.Expr)], [S.Stmt])
    getRecs (S.RecDecl sp (S.NameBinding _ nm) e : ts) = first ((sp,nm,e):) $ getRecs ts
    getRecs (S.FunDecl sp (S.SimpleBinding _ S.NoShadow nm Nothing) fh Nothing bdy Nothing : ts)
        = first ((sp,nm,S.Lam sp fh bdy) :) $ getRecs ts
    getRecs [] = ([],[])
    getRecs ts = ([],ts)

makeRec :: S.SourcePosition -> Text -> S.Expr -> (S.Stmt, S.Stmt)
makeRec sp nm e =
        let placeholder = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp $ S.App sp (S.Iden sp "raise") [S.Text sp "internal: recursive var not initialized"]]
        in (S.VarDecl sp (S.SimpleBinding sp S.NoShadow nm Nothing) placeholder
           ,S.SetVar sp (S.Iden sp nm) e)

------------------------------------------------------------------------------

{-

a data decl desugars to:
a type letdecl. this is renamed as a hack for now, because contruct implementations
  can have the same name as a type and not conflict
pattern letdecls - these contain the info needed to check a pattern match on a variant value
letdecls for is-type, is-variants
letdecls for the constructors
  these will be letrec, because they can refer to each other

-}

desugarDataDecl :: S.Stmt -> [S.Stmt]
desugarDataDecl (S.DataDecl dsp dnm _params variants _meths _where) =
    concat [
    [--_type-name = make-data-decl-tag(...)
      slet dsp ("_type-" <> dnm) $ sapp dsp "make-data-decl-tag" [S.Text dsp dnm]
     --   is-name = lam(x): _is-type(_type-name, x)
     ,slet dsp ("is-" <> dnm) $ slam dsp ["x"] $ sapp dsp "is-type" [S.Iden dsp ("_type-" <> dnm)
                                                                    ,S.Iden dsp "x"]]
    --variants.each:
    --  _variant-variantname = make-variant-tag(_type-name, ...)
    ,map makeVariantInfo variants
    --variants.each:
    --  is-variant-name(x) = _is-variant(_variant-variantname, x)
    ,map makeIsVariant variants
    --rec variants.each:
    --  variant-name = lam(args): make-variant(_variant-variantname, args)
    ,map makeVariantCtor variants
    ]
  where
    makeVariantInfo (S.VariantDecl vsp vnm _fs _meths) =
        slet vsp ("_variant-" <> vnm) $ sapp vsp "make-variant-tag" [S.Iden vsp ("_type-" <> dnm)
                                                                    ,S.Text vsp vnm]
    makeIsVariant (S.VariantDecl vsp vnm _fs _meths) =
        slet vsp ("is-" <> vnm) $ slam vsp ["x"] $ sapp vsp "is-variant" [S.Iden vsp ("_variant-" <> vnm)
                                                                          ,S.Iden vsp "x"]
    makeVariantCtor (S.VariantDecl vsp vnm [] _meths) =
        slet vsp vnm $ sapp vsp "make-variant" [S.Iden vsp ("_variant-" <> vnm)
                                               ,hl vsp []
                                               ,hl vsp []
                                               ]
    makeVariantCtor (S.VariantDecl vsp vnm fs _meths) =
        let fnms = flip map fs $ \(_,S.SimpleBinding _ _ nm _) -> nm
        in slet vsp vnm $ slam vsp fnms $ sapp vsp "make-variant"
           [S.Iden vsp ("_variant-" <> vnm)
           ,hl vsp $ map (S.Text vsp) fnms
           ,hl vsp $ map (S.Iden vsp) fnms]
    slet sp nm e = S.LetDecl sp (S.NameBinding sp nm) e
    sapp sp nm args = S.App sp (S.DotExpr sp (S.Iden sp "_bootstrap") nm) args
    slam sp as e =
      let bs = flip map as $ \a -> S.NameBinding sp a
      in S.Lam sp (S.FunHeader [] bs Nothing) [S.StmtExpr sp e]
    hl sp as = sapp sp "make-haskell-list" as

desugarDataDecl x = [x]

desugarDataDecls :: [S.Stmt] -> [S.Stmt]
desugarDataDecls = concatMap desugarDataDecl
