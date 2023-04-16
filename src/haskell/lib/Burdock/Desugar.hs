
{-

Desugar from parsed syntax ast which represents most of the concrete
syntax, to the properly abstract ast which the interpreter runs on

this code also adds tags for the free vars in lambda bodies

later it will also be where the lexical binding checks and static type
checker run

-}

{-# LANGUAGE LambdaCase #-}
module Burdock.Desugar
    (desugar
    ) where

import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Data.Text (Text)
import qualified Data.Text as T
import Burdock.Pretty (prettyExpr)
import Data.List (nub,sort)

desugar :: S.Script -> [I.Stmt]
desugar (S.Script ss) = desugarStmts ss

desugarStmts :: [S.Stmt] -> [I.Stmt]
desugarStmts ss = concatMap desugarStmt ss

desugarStmt :: S.Stmt -> [I.Stmt]
desugarStmt (S.Check _ _ ss) = desugarStmts ss

desugarStmt (S.LetDecl _ b e) =
    let nm = simpleBindingName b
    in [I.LetDecl nm $ desugarExpr e]

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) =
    let m1 = S.Text Nothing $ prettyExpr e1
        m2 = S.Text Nothing $ prettyExpr e2
    in desugarStmt (S.StmtExpr Nothing $ S.App Nothing (S.Iden Nothing "do-is-test") [e1,e2,m1,m2])

desugarStmt (S.StmtExpr _ e) = [I.StmtExpr $ desugarExpr e]

{-

data decl desugars to:
make function for each variant
is-x function for each variant
is-dat function
support functions for cases
support for dynamic type checks
support for modules * syntax

-}
desugarStmt (S.DataDecl _ dnm _ vs [] Nothing) =
    desugarStmts $ concatMap varF vs ++ [isDat]
  where
    varF (S.VariantDecl _ vnm bs []) =
        -- my-pt = lam(ps...) = make-variant("my-pt", [list:vnms...], [list:ps]) end
        [if null bs
         then letDecl vnm $ S.App n (S.Iden n "make-variant") [S.Text n vnm, lst [], lst []]
         else let ps = map sbNm bs
              in letDecl vnm $
                 S.Lam n (fh $ map (S.NameBinding n) ps)
            [S.StmtExpr n $
             S.App n (S.Iden n "make-variant") [S.Text n vnm
                                               , lst $ map (S.Text n) ps
                                               , lst $ map (S.Iden n) ps]]
        
                 -- is-my-pt = lam(x): is-variant("my-pt", x) end
        ,letDecl ("is-" <> vnm) $ lam ["x"] [S.StmtExpr n $
                                   S.App n (S.Iden n "is-variant")
                                   [S.Text n vnm, S.Iden n "x"]]
        ]
    varF v = error $ "unsupported variant decl: " ++ show v
    lst es = S.Construct n ["list"] es
    callIs (S.VariantDecl _ vnm _ _) = S.App n (S.Iden n $ "is-" ++ vnm) [S.Iden n "x"]
    isDat = letDecl ("is-" ++ dnm)
            $ lam ["x"]
           [S.StmtExpr n $ foldl1 orE $ map callIs vs]
    sbNm (_,S.SimpleBinding _ _ nm _) = nm
    n = Nothing
    letDecl nm v = S.LetDecl n (mnm nm) v
    lam as e = S.Lam n (fh $ map mnm as) e
    fh as = S.FunHeader [] as Nothing
    orE a b = S.BinOp n a "or" b
    mnm x = S.NameBinding n x

desugarStmt x = error $ "desugarStmt " ++ show x

desugarExpr :: S.Expr -> I.Expr

desugarExpr (S.Block _ ss) = I.Block $ desugarStmts ss

desugarExpr (S.If _ ts els) =
    I.If (map f ts) (desugarStmts <$> els)
  where
    f (a,b) = (desugarExpr a, desugarStmts b)

desugarExpr (S.DotExpr _ e fld) =
    I.DotExpr (desugarExpr e) (T.pack fld)
     
desugarExpr (S.RecordSel _ fs) =
    I.RecordSel $ map (\(a,b) -> (T.pack a, desugarExpr b)) fs

desugarExpr (S.App _ f es) =
    I.App (desugarExpr f) $ map desugarExpr es

desugarExpr (S.Lam _ (S.FunHeader _ bs _) bdy) =
    let bdy' = desugarStmts bdy
        pnms = map simpleBindingName bs
        fv = nub $ sort $ freeVarsStmts pnms bdy'
    in I.Lam fv pnms bdy'
desugarExpr (S.Text _ s) = I.IString $ T.pack s
desugarExpr (S.Num _ s) = I.Num s

-- iden
desugarExpr (S.Iden _ i) = I.Iden $ T.pack i
-- methods
desugarExpr (S.MethodExpr _ (S.Method (S.FunHeader _ts (a:as) _mty) bdy))
    = I.MethodExpr $ desugarExpr (S.Lam Nothing (S.FunHeader [] [a] Nothing)
                        [S.StmtExpr Nothing $ S.Lam Nothing (S.FunHeader [] as Nothing) bdy])

-- if e1 then True else e2
desugarExpr (S.BinOp _ e1 "or" e2) =
    desugarExpr (S.If Nothing [(e1, [S.StmtExpr Nothing $ S.Iden Nothing "true"])] (Just [S.StmtExpr Nothing e2]))

-- if e1 then e2 else False
desugarExpr (S.BinOp _ e1 "and" e2) =
    desugarExpr (S.If Nothing [(e1, [S.StmtExpr Nothing e2])] (Just [S.StmtExpr Nothing $ S.Iden Nothing "false"]))

desugarExpr (S.BinOp _ e1 op e2) | Just op' <- lookup op methOps =
    desugarExpr (S.App Nothing (S.DotExpr Nothing e1 op') [e2])
  where
    methOps =
        [("==", "_equals")
        ,("+", "_plus")
        ,("*", "_times")]
    
desugarExpr (S.Let _ bs e) =
    desugarExpr $ S.Block Nothing
        $ (map (\(b,e1) -> S.LetDecl Nothing b e1) bs) ++ e -- [S.StmtExpr Nothing e]
desugarExpr (S.Parens _ e) = desugarExpr e

desugarExpr (S.Construct _ ["list"] es) =
    desugarExpr $ S.App Nothing (S.Iden Nothing "make-list") es


desugarExpr x = error $ "desugarExpr " ++ show x

simpleBindingName :: S.Binding -> Text
simpleBindingName = \case
        S.NameBinding _ nm -> T.pack nm
        S.ShadowBinding _ nm -> T.pack nm
        x -> error $ "unsupported binding: " ++ show x


freeVarsExpr :: [Text] -> I.Expr -> [Text]
freeVarsExpr bs (I.Block sts) = freeVarsStmts bs sts

freeVarsExpr bs (I.DotExpr e _) = freeVarsExpr bs e

freeVarsExpr bs (I.App f es) = freeVarsExpr bs f ++ concatMap (freeVarsExpr bs) es

freeVarsExpr bs (I.Iden i) = if i `elem` bs
                             then []
                             else [i]

freeVarsExpr _bs (I.IString {}) = []
freeVarsExpr bs (I.If ts els) =
    concatMap (\(a,b) -> freeVarsExpr bs a ++ freeVarsStmts bs b) ts
    ++ maybe [] (freeVarsStmts bs) els

freeVarsExpr _ e = error $ "freeVarsExpr: " ++ show e


freeVarsStmt :: [Text] -> I.Stmt -> [Text]

freeVarsStmt bs (I.StmtExpr e) = freeVarsExpr bs e

freeVarsStmt _bs st = error $ "freeVarsStmt': " ++ show st

freeVarsStmts :: [Text] -> [I.Stmt] -> [Text]
freeVarsStmts bs ss = concatMap (freeVarsStmt bs) ss
