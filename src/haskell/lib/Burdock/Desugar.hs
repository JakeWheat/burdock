
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

desugarStmt (S.RecDecl _ b e) =
    desugarStmts $ doLetRec [(b,e)]

desugarStmt (S.FunDecl _ (S.SimpleBinding _ _ nm _) fh _ bdy _) =
    desugarStmt (S.RecDecl n (S.NameBinding n nm) $ S.Lam n fh bdy)
  where
    n = Nothing

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) =
    let m1 = S.Text n $ prettyExpr e1
        m2 = S.Text n $ prettyExpr e2
        rt e = S.App n (S.Iden n "run-task") [e]
    in desugarStmt (S.StmtExpr n $ S.App n (S.Iden n "do-is-test") [m1,m2,rt e1,rt e2])
  where
    n = Nothing

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is-not" e2)) =
    let m1 = S.Text n $ prettyExpr e1
        m2 = S.Text n $ prettyExpr e2
        rt e = S.App n (S.Iden n "run-task") [e]
    in desugarStmt (S.StmtExpr n $ S.App n (S.Iden n "do-is-not-test") [m1,m2,rt e1,rt e2])
  where
    n = Nothing

desugarStmt (S.When _ t b) =
    desugarStmt $ S.StmtExpr n $ S.If n [(t, b)] (Just [S.StmtExpr n $ S.Iden n "nothing"])
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

desugarStmt (S.VarDecl _ (S.SimpleBinding _ _ nm _) e) =
    [I.VarDecl (T.pack nm) $ desugarExpr e]

desugarStmt (S.SetVar _ (S.Iden _ nm) e) =
    [I.SetVar (T.pack nm) $ desugarExpr e]

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
    let trm = ("_torepr",desugarExpr $ S.Iden n "_record_torepr")
        eqm = ("_equals",desugarExpr $ S.Iden n "_record_equals")
    in I.VariantSel "record" (trm : eqm : map (\(a,b) -> (T.pack a, desugarExpr b)) fs)
  where
    n = Nothing

desugarExpr (S.TupleSel _ fs) =
    let fs1 = zip (map show [(0::Int)..]) fs
        trm = ("_torepr"
              ,desugarExpr $ S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a"])
                            [S.StmtExpr n $ S.App n (S.Iden n "show-tuple")
                                [S.Iden n "a"]
                            ])
        eqm = ("_equals"
              ,desugarExpr $ S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a", "b"])
                            [S.StmtExpr n $ S.App n (S.Iden n "check-variants-equal")
                                [S.Construct n ["list"] (map (S.Text n . T.unpack . fst) $ fs1)
                                , S.Iden n "a"
                                , S.Iden n "b"]])
    in I.VariantSel "tuple" (trm : eqm : map (\(a,b) -> (a,desugarExpr b)) fs1)
  where
    n = Nothing
    fh as = S.FunHeader [] as Nothing
    mnm x = S.NameBinding n x
    
desugarExpr (S.TupleGet sp v n) =
    desugarExpr (S.DotExpr sp v (T.unpack $ show n))

-- what's the right way to find run-task, so it can be implemented
-- differently at runtime from a regular function
-- it has to be looked up in the environment. the desugarer should have
-- the info to do this
desugarExpr (S.App _ (S.Iden _ "run-task") [e]) =
    desugarExpr $ S.App n (S.Iden n "_run-task-fixup") [S.App n (S.Iden n "run-task-cs") [e]]
  where
    n = Nothing
desugarExpr (S.App _ (S.Iden _ "run-task-cs") [e]) =
    I.RunTask False $ desugarExpr e

desugarExpr (S.App _ (S.Iden _ "run-task-cs-async") [e]) =
    I.RunTask True $ desugarExpr e


desugarExpr (S.App sp f es) =
    let spx = case sp of
            Nothing -> "nothing"
            Just (n,i,j) -> "(" <> T.pack n <> "," <> show i <> "," <> show j <> ")"
    in I.App (Just spx) (desugarExpr f) $ map desugarExpr es

desugarExpr (S.Lam _ (S.FunHeader _ bs _) bdy) =
    let bdy' = desugarStmts bdy
        pnms = map desugarBinding bs
        pnmsx = concatMap getBindingNames pnms
        -- adding names as a temp hack before capturing closures properly
        -- in all the bootstrap and built in code
        fv = nub $ sort ("_record_torepr" : "_record_equals" : freeVarsStmts pnmsx bdy')
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
        ,("<=", "_lessequal")
        ,("<", "_lessthan")
        ,("+", "_plus")
        ,("-", "_minus")
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

desugarExpr x = error $ "desugarExpr " <> show x


doLetRec :: [(S.Binding, S.Expr)] -> [S.Stmt]
doLetRec bs = 
    let vars = map (makeVar . fst) bs
        assigned = map makeAssign bs
    in vars ++ assigned
  where
    makeVar (S.NameBinding _ nm) = makeVar1 (S.NoShadow, nm)
    makeVar x = error $ "unsupported binding in recursive let: " <> show x
    makeVar1 (sh, nm) =
        S.VarDecl n (S.SimpleBinding n sh nm Nothing) $ S.Lam n (S.FunHeader [] [] Nothing)
        [S.StmtExpr n $ S.App n (S.Iden n "raise")
            [S.Text n "internal error: uninitialized letrec implementation var"]]
    makeAssign (S.NameBinding _ nm, v) = makeAssign1 (nm,v)
    makeAssign x = error $ "unsupported binding in recursive let: " <> show x
    makeAssign1 (nm,v) = S.SetVar n (S.Iden n nm) v
    n = Nothing


desugarBinding :: S.Binding -> I.Binding
desugarBinding = \case
    S.NameBinding _ nm -> I.NameBinding $ T.pack nm
    S.ShadowBinding _ nm -> I.NameBinding $ T.pack nm
    S.VariantBinding _ [vnm] bs -> I.VariantBinding (T.pack vnm) $ map desugarBinding bs
    S.WildcardBinding _ -> I.WildcardBinding
    S.TupleBinding _ bs -> I.TupleBinding $ map desugarBinding bs
    x -> error $ "unsupported binding: " <> show x
    
getBindingNames :: I.Binding -> [Text]
getBindingNames (I.NameBinding nm) = [nm]
getBindingNames I.WildcardBinding = []
getBindingNames (I.VariantBinding _ bs) = concatMap getBindingNames bs
getBindingNames (I.TupleBinding bs) = concatMap getBindingNames bs

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
freeVarsExpr bs (I.RunTask _ e) = "left" : "right" : freeVarsExpr bs e

freeVarsExpr bs (I.Cases e ts) =
    freeVarsExpr bs e ++ concatMap fv ts
  where
    fv (p,s) = let nbs = getBindingNames p
               in freeVarsStmts (nbs ++bs) s

freeVarsExpr bs (I.VariantSel _ fs) =
    concatMap (freeVarsExpr bs . snd) fs

--freeVarsExpr _ e = error $ "freeVarsExpr: " <> show e

freeVarsStmts :: [Text] -> [I.Stmt] -> [Text]
freeVarsStmts _bs [] = []
freeVarsStmts bs (I.StmtExpr e : ss) =
    freeVarsExpr bs e ++ freeVarsStmts bs ss

freeVarsStmts bs (I.LetDecl nm e : ss) =
    freeVarsExpr bs e
    ++ let bbs = getBindingNames nm ++ bs
       in freeVarsStmts bbs ss

freeVarsStmts _ (s:_) = error $ "freeVarsStmts: " <> show s

