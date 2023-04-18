
{-

Desugar from parsed syntax ast which represents most of the concrete
syntax, to the properly abstract ast which the interpreter runs on

this code also adds tags for the free vars in lambda bodies

later it will also be where the lexical binding checks and static type
checker run

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Desugar
    (desugar
    ,prelude
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
--import Data.Text.IO (putStrLn)

import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Data.Text (Text)
import qualified Data.Text as T
import Burdock.Pretty (prettyExpr)
import Data.List (nub,sort)

import Burdock.DefaultRuntime (prelude)

desugar :: S.Script -> [I.Stmt]
desugar (S.Script ss) = desugarStmts ss

desugarStmts :: [S.Stmt] -> [I.Stmt]
desugarStmts ss = concatMap desugarStmt ss

desugarStmt :: S.Stmt -> [I.Stmt]
desugarStmt (S.Check _ _ ss) = desugarStmts ss

desugarStmt (S.LetDecl _ b e) =
    let nm = desugarBinding b
    in [I.LetDecl nm $ desugarExpr e]

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) =
    let m1 = S.Text n $ prettyExpr e1
        m2 = S.Text n $ prettyExpr e2
        rt e = S.App n (S.Iden n "run-task") [e]
    in desugarStmt (S.StmtExpr n $ S.App n (S.Iden n "do-is-test") [rt e1,rt e2,m1,m2])
  where
    n = Nothing

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is-not" e2)) =
    let m1 = S.Text n $ prettyExpr e1
        m2 = S.Text n $ prettyExpr e2
        rt e = S.App n (S.Iden n "run-task") [e]
    in desugarStmt (S.StmtExpr n $ S.App n (S.Iden n "do-is-not-test") [rt e1,rt e2,m1,m2])
  where
    n = Nothing

desugarStmt (S.StmtExpr _ e) = [I.StmtExpr $ desugarExpr e]

{-

data decl desugars to:
make function for each variant
is-x function for each variant
is-dat function
support functions for cases
support for dynamic type checks
support for modules * syntax

how do you generate an _equals function?
if the user supplies one, don't generate one
otherwise, it should check if both sides are the same variant,
  then check all the non method fields are the same (maybe also non
  functions as well?)
->
check-variants-equal(on-fields :: list<string>,a,b)
   check they are the same variant
   then go through each given field and check it matches


-}
desugarStmt (S.DataDecl _ dnm _ vs [] Nothing) =
    desugarStmts $ concatMap varF vs ++ [isDat]
  where
    varF (S.VariantDecl _ vnm bs []) =
        -- my-pt = lam(ps...) = make-variant("my-pt", [list:vnms...], [list:ps]) end
        [if null bs
         then
             let 
             in letDecl vnm
                $ S.App n (S.Iden n "make-variant")
                [S.Text n vnm
                , lst [S.Text n "_equals", S.Text n "_torepr"]
                , let eqm = S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a", "b"])
                            [S.StmtExpr n $ S.App n (S.Iden n "check-variants-equal")
                                [S.Construct n ["list"] []
                                , S.Iden n "a"
                                , S.Iden n "b"]]
                      trm = S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a"])
                            [S.StmtExpr n $ S.App n (S.Iden n "show-variant")
                                [S.Iden n "a"]
                            ]
                  in lst [eqm,trm]
                ]
         else let ps = map sbNm bs
              in letDecl vnm
                 $ S.Lam n (fh $ map (S.NameBinding n) ps)
                 [S.StmtExpr n $
                  S.App n (S.Iden n "make-variant")
                  [S.Text n vnm
                  , lst $ map (S.Text n) ("_equals" : "_torepr" : ps)
                  , let eqm = S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a", "b"])
                            [letDecl "flds" $ S.Construct n ["list"] $ map (S.Text n) ps
                            ,S.StmtExpr n $ S.App n (S.Iden n "check-variants-equal")
                                [S.Iden n "flds", S.Iden n "a", S.Iden n "b"]]
                        trm = S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a"])
                            [S.StmtExpr n $ S.App n (S.Iden n "show-variant")
                                [S.Iden n "a"]
                            ]    
                    in lst (eqm : trm : map (S.Iden n) ps)]]
        
                 -- is-my-pt = lam(x): is-variant("my-pt", x) end
        ,letDecl ("is-" <> vnm) $ lam ["x"] [S.StmtExpr n $
                                   S.App n (S.Iden n "is-variant")
                                   [S.Text n vnm, S.Iden n "x"]]
        ]
    varF v = error $ "unsupported variant decl: " <> show v
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

desugarStmt x = error $ "desugarStmt " <> show x

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

-- what's the right way to find run-task, so it can be implemented
-- differently at runtime from a regular function
-- it has to be looked up in the environment. the desugarer should have
-- the info to do this
desugarExpr (S.App _ (S.Iden _ "run-task") [e]) =
    I.RunTask $ desugarExpr e

desugarExpr (S.App sp f es) =
    let spx = case sp of
            Nothing -> "nothing"
            Just (n,i,j) -> "(" <> T.pack n <> "," <> show i <> "," <> show j <> ")"
    in I.App (Just spx) (desugarExpr f) $ map desugarExpr es

desugarExpr (S.Lam _ (S.FunHeader _ bs _) bdy) =
    let bdy' = desugarStmts bdy
        pnms = map desugarBinding bs
        pnmsx = concatMap getBindingNames pnms
        fv = nub $ sort $ freeVarsStmts pnmsx bdy'
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


desugarExpr (S.Cases _ tst _ bs mels) =
    let tst' = desugarExpr tst
        bs' = flip map bs $ \(bm,_,bdy) -> (desugarBinding bm, desugarStmts bdy)
        mels' = case mels of
                    Nothing -> [] -- todo: insert explicit raise here
                    Just bdy -> [(I.WildcardBinding, desugarStmts bdy)]
    in I.Cases tst' (bs' ++ mels')
      

--                | Cases Expr [(Binding, [Stmt])]

desugarExpr x = error $ "desugarExpr " <> show x

desugarBinding :: S.Binding -> I.Binding
desugarBinding = \case
    S.NameBinding _ nm -> I.NameBinding $ T.pack nm
    S.ShadowBinding _ nm -> I.NameBinding $ T.pack nm
    S.VariantBinding _ [vnm] bs -> I.VariantBinding (T.pack vnm) $ map desugarBinding bs
    x -> error $ "unsupported binding: " <> show x
    
    
getBindingNames :: I.Binding -> [Text]
getBindingNames (I.NameBinding nm) = [nm]
getBindingNames I.WildcardBinding = []
getBindingNames (I.VariantBinding _ bs) = concatMap getBindingNames bs

freeVarsExpr :: [Text] -> I.Expr -> [Text]
freeVarsExpr bs (I.Block sts) = freeVarsStmts bs sts

freeVarsExpr bs (I.DotExpr e _) = freeVarsExpr bs e

freeVarsExpr bs (I.App _ f es) = freeVarsExpr bs f ++ concatMap (freeVarsExpr bs) es

freeVarsExpr bs (I.Iden i) = if i `elem` bs
                             then []
                             else [i]

freeVarsExpr _bs (I.IString {}) = []
freeVarsExpr _bs (I.Num {}) = []
freeVarsExpr bs (I.If ts els) =
    concatMap (\(a,b) -> freeVarsExpr bs a ++ freeVarsStmts bs b) ts
    ++ maybe [] (freeVarsStmts bs) els

freeVarsExpr _bs (I.Lam fv _as _bdy) = fv
freeVarsExpr bs (I.MethodExpr e) = freeVarsExpr bs e
freeVarsExpr bs (I.RunTask e) = "left" : "right" : freeVarsExpr bs e

freeVarsExpr _ e = error $ "freeVarsExpr: " <> show e

freeVarsStmts :: [Text] -> [I.Stmt] -> [Text]
freeVarsStmts _bs [] = []
freeVarsStmts bs (I.StmtExpr e : ss) =
    freeVarsExpr bs e ++ freeVarsStmts bs ss

freeVarsStmts bs (I.LetDecl nm e : ss) =
    freeVarsExpr bs e
    ++ let bbs = getBindingNames nm ++ bs
       in freeVarsStmts bbs ss

