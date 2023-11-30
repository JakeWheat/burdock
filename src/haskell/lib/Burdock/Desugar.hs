
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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

{-
What is really wanted is a writer
and peek to read the current value
with censor which is applied at the point of censor,
  so it doesn't affect nested peeks
this models a kind of inherited ag attribute
feels more like an inverse of reader too
-}
import Control.Monad.Writer
    (Writer
    ,runWriter
    ,tell
    )

import Data.List (nub, (\\))

import Burdock.Rename (intMod, testingMod)

------------------------------------------------------------------------------

desugarScript :: Text
              -> S.Script
              -> Either [StaticError] [I.Stmt]
desugarScript _fn (S.Script stmts) =
    pure $ fst $ runWriter $ desugarStmts stmts

------------------------------------------------------------------------------

type Desugar = Writer [Text]

desugarStmts :: [S.Stmt] -> Desugar [I.Stmt]
desugarStmts ss = desugarStmtsFilterFree $ desugarRecs $ desugarDataDecls ss

------------------------------------------------------------------------------

desugarStmtsFilterFree :: [S.Stmt] -> Desugar [I.Stmt]
desugarStmtsFilterFree st = do
    -- todo: run in subwriter
    -- look through the introduced bindings from letdecl and vardecl and tell
    -- what's left in the outer
    let (st',freeCands) = runWriter (mapM desugarStmt st)
        intros = concat $ map getIntros st'
        frees = nub freeCands \\ intros
    tell frees
    pure st'
  where
    getIntros :: I.Stmt -> [Text]
    getIntros (I.LetDecl _ b _) = getBoundNames b
    getIntros (I.VarDecl _ nm _) = [nm]
    getIntros _ = []

getBoundNames :: I.Binding -> [Text]
getBoundNames (I.NameBinding _ nm) = [nm]
getBoundNames (I.WildcardBinding {}) = []
getBoundNames (I.VariantBinding _ _ bs) = concat $ map getBoundNames bs
getBoundNames (I.TupleBinding _ bs) = concat $ map getBoundNames bs

------------------------------------------------------------------------------

desugarStmt :: S.Stmt -> Desugar I.Stmt

-- S -> S

desugarStmt (S.Check sp _ bdy) = desugarStmt $ S.StmtExpr sp $ S.Block sp bdy
desugarStmt (S.When sp e bdy) =
    desugarStmt $ S.StmtExpr sp
    $ S.If sp [(e,bdy)] (Just [S.StmtExpr sp $ S.DotExpr sp (S.Iden sp intMod) "nothing"])

-- S -> I

desugarStmt (S.Import sp (S.ImportSpecial nm as) al) =
    pure $ I.ImportAs sp al (nm,as)

desugarStmt (S.StmtExpr sp e) = I.StmtExpr sp <$> desugarExpr e
desugarStmt (S.LetDecl sp b e) = I.LetDecl sp <$> desugarBinding b <*> desugarExpr e
desugarStmt (S.VarDecl sp (S.SimpleBinding _ _ nm Nothing) e) = I.VarDecl sp nm <$> desugarExpr e
desugarStmt (S.SetVar sp (S.Iden _ nm) e) = do
    tell [nm]
    I.SetVar sp [nm] <$> desugarExpr e
-- todo: generalize
desugarStmt (S.SetVar sp (S.DotExpr _ (S.Iden _ nm) nm1) e) = do
    tell [nm]
    I.SetVar sp [nm,nm1] <$> desugarExpr e

desugarStmt s = error $ "desugarStmt " <> show s

------------------------------------------------------------------------------

desugarExpr :: S.Expr -> Desugar I.Expr

-- S -> S
desugarExpr (S.BinOp sp e0 "is" e1) = do
    let msg = L.toStrict $ P.prettyExpr e0 <> " is " <> P.prettyExpr e1
    desugarExpr $ tapp "run-binary-test"
        [S.Text sp msg
        ,wrapit e0
        ,wrapit e1
        ,lam ["a", "b"] (S.BinOp sp (S.Iden sp "a") "==" (S.Iden sp "b"))
        ,S.Text sp "<>"
        ]
  where
    tapp nm as = S.App sp (S.DotExpr sp (S.Iden sp testingMod) nm) as
    wrapit e = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp e]
    lam as e = S.Lam sp (S.FunHeader [] (flip map as $ S.NameBinding sp) Nothing) [S.StmtExpr sp e]

desugarExpr (S.BinOp sp e0 "is-not" e1) = do
    let msg = L.toStrict $ P.prettyExpr e0 <> " is " <> P.prettyExpr e1
    desugarExpr $ tapp "run-binary-test"
        [S.Text sp msg
        ,wrapit e0
        ,wrapit e1
        ,lam ["a", "b"] (S.BinOp sp (S.Iden sp "a") "<>" (S.Iden sp "b"))
        ,S.Text sp "=="
        ]
  where
    tapp nm as = S.App sp (S.DotExpr sp (S.Iden sp testingMod) nm) as
    wrapit e = S.Lam sp (S.FunHeader [] [] Nothing) [S.StmtExpr sp e]
    lam as e = S.Lam sp (S.FunHeader [] (flip map as $ S.NameBinding sp) Nothing) [S.StmtExpr sp e]

desugarExpr (S.Parens _ e) = desugarExpr e

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

desugarExpr (S.BinOp sp e0 "<>" e1) =
    desugarExpr (S.App sp (S.DotExpr sp (S.Iden sp intMod) "not") [(S.BinOp sp e0 "==" e1)])

-- todo1: the renamer will give this a canonical name, then special
-- case this name
desugarExpr (S.Construct sp ["list"] es) =
    desugarExpr $ S.App sp (S.DotExpr sp (S.Iden sp intMod) "make-burdock-list") es

desugarExpr (S.Construct sp [nm] es) =
    desugarExpr $ S.App sp (S.DotExpr sp (S.Iden sp nm) "make") [S.Construct sp ["list"] es]

-- S -> I

desugarExpr (S.Block sp bdy) = I.Block sp <$> desugarStmts bdy

-- special case for runtask
-- todo: renamer will give this a canonical name, pick up on this
-- this approach is suspect because aliasing will break it
-- but run-task is an 'internal feature' so maybe this is fine
-- for the way users should be catching exceptions, plan to use
-- the industrial pyret design that was on the google group
desugarExpr (S.App sp (S.Iden _ "run-task") [a]) = do
    tell [intMod]
    I.RunTask sp <$> desugarExpr a

desugarExpr (S.App sp f as) = I.App sp <$> desugarExpr f <*> mapM desugarExpr as

desugarExpr (S.DotExpr sp e f) = I.DotExpr sp <$> desugarExpr e <*> pure f
desugarExpr (S.Iden sp i) = do
    tell [i]
    pure $ I.Iden sp i
desugarExpr (S.Text sp t) = pure $ I.IString sp t
desugarExpr (S.Num sp n) = do
    tell [intMod]
    pure $ I.Num sp n
desugarExpr (S.Lam sp (S.FunHeader [] bs Nothing) bdy) = do
    let ((bs',bdy'), candFrees) =
            runWriter $ (,) <$> mapM desugarBinding bs <*> desugarStmts bdy
        bounds = concat $ map getBoundNames bs'
        frees = nub candFrees \\ bounds
    -- pass free vars up the chain
    tell frees
    pure $ I.Lam sp frees bs' bdy'

desugarExpr (S.If sp ts els) = do
    I.If sp <$> flip mapM ts (\(e,bdy) -> (,) <$> desugarExpr e <*> desugarStmts bdy) <*> els'
  where
    els' = case els of
        Nothing -> pure Nothing
        Just e' -> Just <$> desugarStmts e'

desugarExpr (S.Cases sp e Nothing bs Nothing) = do
    I.Cases sp <$> desugarExpr e <*> mapM branch bs
  where
    branch :: (S.Binding, b, [S.Stmt]) -> Desugar (I.Binding, [I.Stmt])
    branch (b,_,bdy) = do
        let ((b', bdy'),candFrees) = runWriter $ (,) <$> desugarBinding b <*> desugarStmts bdy
            bounds = getBoundNames b'
            frees = nub candFrees \\ bounds
        tell frees
        pure (b', bdy')

desugarExpr (S.MethodExpr sp (S.Method (S.FunHeader _ts (a:as) _mty) bdy)) = do
    I.MethodExpr sp <$> desugarExpr (S.Lam Nothing (S.FunHeader [] [a] Nothing)
                        [S.StmtExpr Nothing $ S.Lam Nothing (S.FunHeader [] as Nothing) bdy])

desugarExpr (S.RecordSel sp fs) =
    I.RecordSel sp <$> flip mapM fs (\(nm,e) -> (nm,) <$> desugarExpr e)

desugarExpr (S.TupleSel sp es) =
    I.TupleSel sp <$> mapM desugarExpr es

desugarExpr (S.TupleGet sp e i) =
    I.TupleGet sp <$> desugarExpr e <*> pure i

desugarExpr e = error $ "desugarExpr: " <> show e

------------------------------------------------------------------------------

desugarBinding :: S.Binding -> Desugar I.Binding
desugarBinding (S.NameBinding sp nm) = do
    -- temp: might be a variant name
    pure $ I.NameBinding sp nm
desugarBinding (S.ShadowBinding sp nm) = pure $ I.NameBinding sp nm
desugarBinding (S.VariantBinding sp nm@(n:_) bs) = do
    tell [intMod, n]
    I.VariantBinding sp nm <$> mapM desugarBinding bs
desugarBinding (S.VariantBinding sp nm@[] bs) =
    -- not sure if should error here or let it be someone else's
    -- problem
    I.VariantBinding sp nm <$> mapM desugarBinding bs

desugarBinding (S.WildcardBinding sp) = pure $ I.WildcardBinding sp

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
    let placeholder = S.Lam sp (S.FunHeader [] [] Nothing)
            [S.StmtExpr sp $ S.App sp (S.DotExpr sp (S.Iden sp intMod) "raise")
                [S.Text sp "internal: recursive var not initialized"]]
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
                                               ,hl vsp [S.Text vsp "_equals", S.Text vsp "_torepr"]
                                               ,hl vsp [eqf [], torp []]]
    makeVariantCtor (S.VariantDecl vsp vnm fs _meths) =
        let fnms = flip map fs $ \(_,S.SimpleBinding _ _ nm _) -> nm
            ifs = map (S.Text dsp) fnms
        in slet vsp vnm $ slam vsp fnms $ sapp vsp "make-variant"
           [S.Iden vsp ("_variant-" <> vnm)
           -- hack, put methods at the end, tryApplyBinding relies on this atm
           ,hl vsp (map (S.Text vsp) (fnms ++ ["_equals", "_torepr"]))
           ,hl vsp (map (S.Iden vsp) fnms ++ [eqf ifs, torp ifs])]
    eqf ifs = smeth dsp ["a","b"] (sapp dsp "variants-equal" [sapp dsp "make-haskell-list" ifs
                                                         ,S.Iden dsp "a"
                                                         ,S.Iden dsp "b"])
    torp ifs = smeth dsp ["a"] (sapp dsp "show-variant" [sapp dsp "make-haskell-list" ifs
                                                        ,S.Iden dsp "a"])
    slet sp nm e = S.LetDecl sp (S.NameBinding sp nm) e
    sapp sp nm args = S.App sp (S.DotExpr sp (S.Iden sp intMod) nm) args
    smeth sp as e =
      let bs = flip map as $ \a -> S.NameBinding sp a
      in S.MethodExpr sp (S.Method (S.FunHeader [] bs Nothing) [S.StmtExpr sp e])
    slam sp as e =
      let bs = flip map as $ \a -> S.NameBinding sp a
      in S.Lam sp (S.FunHeader [] bs Nothing) [S.StmtExpr sp e]
    hl sp as = sapp sp "make-haskell-list" as

desugarDataDecl x = [x]

desugarDataDecls :: [S.Stmt] -> [S.Stmt]
desugarDataDecls = concatMap desugarDataDecl
