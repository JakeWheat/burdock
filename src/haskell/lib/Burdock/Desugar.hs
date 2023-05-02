
{-

Desugar from parsed syntax ast which represents most of the concrete
syntax, to the properly abstract ast which the interpreter runs on

this code also adds tags for the free vars in lambda bodies

uses a simple attribute grammar system

later it will also be where the lexical binding checks and static type
checker run

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Text.Lazy as L

import Burdock.Pretty (prettyExpr)
import Data.List
    (nub
    ,(\\)
    )

import Burdock.DefaultRuntime (prelude)

import Control.Monad.Reader
    (runReader
    ,Reader
    --,ask
    ,asks
    ,local
    )

import Lens.Micro.TH
    (makeLenses
    --,makeFields
    )
import Lens.Micro
    (over
    ,set
    )

import Lens.Micro.Extras
    (view)

------------------------------------------------------------------------------

{-

Inh is the inherited attributes

These go from nodes to children nodes, the Inh value is passed around
the tree in a Reader monad

The code uses local to update the Inh values in the tree

Syn are the synthesizes attributes

These go from nodes to parent nodes. They are passed around the tree
in the return values to the desugar functions.

TODO: explain the different fields in these two data types.

explain the logic of how they are changed? with helpers, or
does this go in the individual nodes

-}

data Inh
    = Inh
    {_inhCtors :: [Text]}

data Syn e
    = Syn
    {_synTree :: e
    ,_synFreeVars :: [Text]
    ,_synNewBindings :: [Text]
    ,_synNewCtors :: [Text]
    }
    deriving (Show, Functor)

makeLenses ''Inh
makeLenses ''Syn

type Desugar = Reader Inh

combineSyns :: [Syn ()] -> Syn a -> Syn a
combineSyns as a =
    set synFreeVars (nub $ concat (view synFreeVars a : map (view synFreeVars) as)) a

-- future proofing, not sure if worth it
combineSynsNoFreeVars :: [Syn ()] -> Syn a -> Syn a
combineSynsNoFreeVars _as a = a

combineSynsWithNewBindings :: [Syn ()] -> Syn a -> Syn a
combineSynsWithNewBindings as a =
    set synFreeVars (nub $ concat (view synFreeVars a : map (view synFreeVars) as))
    $ set synNewBindings (concat (view synNewBindings a : map (view synNewBindings) as))
    a

ns :: Syn a -> Syn ()
ns = fmap (const ())

mkSyn :: e -> Syn e
mkSyn e = Syn e [] [] []

------------------

desugar :: S.Script -> [I.Stmt]
desugar (S.Script ss) =
    view synTree $ runReader (desugarStmts ss) (Inh [])
 
desugarStmts :: [S.Stmt] -> Desugar (Syn [I.Stmt])

desugarStmts [] = pure $ mkSyn []

desugarStmts (s:ss) = do
    s' <- desugarStmt s
    let addCtors = over inhCtors (view synNewCtors s' ++)
    ss' <- local addCtors $ desugarStmts ss

    -- get the free vars from ss
    -- remove the newbindings from s
    -- pass this up the chain
    let remainingFreeVars = nub $ (view synFreeVars ss' \\ view synNewBindings s') ++ view synFreeVars s'

    pure $ set synFreeVars remainingFreeVars
         $ combineSynsNoFreeVars [ns s', ns ss']
         $ mkSyn (_synTree s' ++ _synTree ss')

desugarStmt :: S.Stmt -> Desugar (Syn [I.Stmt])

------------------

-- S -> S for desugarStmt
-- these are desugaring which only transform the S syntax into
-- other S syntax and feeds it back into desugar

desugarStmt (S.Check _ _ ss) = desugarStmts ss

desugarStmt (S.RecDecl _ b e) =
    desugarStmts $ doLetRec [(b,e)]

desugarStmt (S.FunDecl _ (S.SimpleBinding _ _ nm _) fh _ bdy _) =
    desugarStmt (S.RecDecl n (S.NameBinding n nm) $ S.Lam n fh bdy)
  where
    n = Nothing

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is" e2)) =
    let m1 = S.Text n $ L.toStrict $ prettyExpr e1
        m2 = S.Text n $ L.toStrict $ prettyExpr e2
        rt e = S.App n (S.Iden n "run-task") [e]
    in desugarStmt (S.StmtExpr n $ S.App n (S.Iden n "do-is-test") [m1,m2,rt e1,rt e2])
  where
    n = Nothing

desugarStmt (S.StmtExpr _ (S.BinOp _ e1 "is-not" e2)) =
    let m1 = S.Text n $ L.toStrict $ prettyExpr e1
        m2 = S.Text n $ L.toStrict $ prettyExpr e2
        rt e = S.App n (S.Iden n "run-task") [e]
    in desugarStmt (S.StmtExpr n $ S.App n (S.Iden n "do-is-not-test") [m1,m2,rt e1,rt e2])
  where
    n = Nothing

desugarStmt (S.When _ t b) =
    desugarStmt $ S.StmtExpr n $ S.If n [(t, b)] (Just [S.StmtExpr n $ S.Iden n "nothing"])
  where
    n = Nothing

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
desugarStmt (S.DataDecl _ dnm _ vs shr Nothing) = do
    let --newCtors
        addNewCtors = over synNewCtors (newCtors ++)
    addNewCtors <$> (desugarStmts $ concatMap varF vs ++ [isDat])
  where
    newCtors = flip map vs $ \case
        S.VariantDecl _ nm _ _ -> nm
    varF (S.VariantDecl _ vnm bs meths) =
        let extraMeths ps =
                [(S.Text n "_equals"
                 ,S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a", "b"])
                            [letDecl "flds" $ S.Construct n ["list"] $ map (S.Text n) ps
                            ,S.StmtExpr n $ S.App n (S.Iden n "check-variants-equal")
                                [S.Iden n "flds", S.Iden n "a", S.Iden n "b"]])
                ,(S.Text n "_torepr"
                 , S.MethodExpr n $ S.Method
                            (fh $ map mnm ["a"])
                            [S.StmtExpr n $ S.App n (S.Iden n "show-variant")
                                [S.Iden n "a"]
                            ])
                ] ++ flip map (meths ++ shr) (\(nm, m) -> (S.Text n nm, S.MethodExpr n m))
            callMakeVariant ps =
                S.App n (S.Iden n "make-variant")
                  [S.Text n vnm
                  , lst $ map fst (extraMeths ps) ++ map (S.Text n) ps
                  , lst $ map snd (extraMeths ps) ++ map (S.Iden n) ps]
        in
        [if null bs
         then recDecl vnm $ callMakeVariant []
         else let ps = map sbNm bs
              in recDecl vnm
                 $ S.Lam n (fh $ map (S.NameBinding n) ps)
                 [S.StmtExpr n $ callMakeVariant ps]
        ,letDecl ("is-" <> vnm) $ lam ["x"] [S.StmtExpr n $
                                   S.App n (S.Iden n "is-variant")
                                   [S.Text n vnm, S.Iden n "x"]]
        ]
    --varF v = error $ "unsupported variant decl: " <> show v
    lst es = S.Construct n ["list"] es
    callIs (S.VariantDecl _ vnm _ _) = S.App n (S.Iden n $ "is-" <> vnm) [S.Iden n "x"]
    isDat = letDecl ("is-" <> dnm)
            $ lam ["x"]
           [S.StmtExpr n $ foldl1 orE $ map callIs vs]
    sbNm (_,S.SimpleBinding _ _ nm _) = nm
    n = Nothing
    letDecl nm v = S.LetDecl n (mnm nm) v
    recDecl nm v = S.RecDecl n (mnm nm) v
    lam as e = S.Lam n (fh $ map mnm as) e
    fh as = S.FunHeader [] as Nothing
    orE a b = S.BinOp n a "or" b
    mnm x = S.NameBinding n x

------------------
-- S -> I desugarStmt

-- these are the functions which contains the code that actually
-- converts to the I syntax

desugarStmt (S.LetDecl _ b e) = do
    nm <- desugarBinding b
    e' <- desugarExpr e
    pure $ set synNewBindings (view synNewBindings nm)
        $ combineSyns [ns nm, ns e']
        $ mkSyn [I.LetDecl (_synTree nm) $ _synTree e']

desugarStmt (S.StmtExpr _ e) = do
    e' <- desugarExpr e
    pure $ combineSyns [ns e'] $ mkSyn [I.StmtExpr $ _synTree e']

desugarStmt (S.VarDecl _ (S.SimpleBinding _ _ nm _) e) = do
    e' <- desugarExpr e
    pure $ set synNewBindings [nm]
        $ combineSyns [ns e']
        $ mkSyn [I.VarDecl nm $ _synTree e']

desugarStmt (S.SetVar _ (S.Iden _ nm) e) = do
    e' <- desugarExpr e
    pure $ combineSyns [ns e'] $ mkSyn [I.SetVar nm $ _synTree e']

desugarStmt x = error $ "desugarStmt " <> show x

---------------------------------------

desugarExpr :: S.Expr -> Desugar (Syn I.Expr)

------------------
-- S -> S

desugarExpr (S.TupleGet sp v n) =
    desugarExpr (S.DotExpr sp v $ show n)

-- what's the right way to find run-task, so it can be implemented
-- differently at runtime from a regular function
-- it has to be looked up in the environment. the desugarer should have
-- the info to do this
desugarExpr (S.App _ (S.Iden _ "run-task") [e]) =
    desugarExpr $ S.App n (S.Iden n "_run-task-fixup") [S.App n (S.Iden n "run-task-cs") [e]]
  where
    n = Nothing

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
        ,(">", "_greaterthan")
        ,("+", "_plus")
        ,("-", "_minus")
        ,("*", "_times")]
    
desugarExpr (S.Let _ bs e) =
    desugarExpr $ S.Block Nothing
    $ (map (\(b,e1) -> S.LetDecl Nothing b e1) bs) ++ e

desugarExpr (S.Parens _ e) = desugarExpr e

desugarExpr (S.Construct _ ["list"] es) =
    desugarExpr $ S.App Nothing (S.Iden Nothing "make-list") es

------------------
-- S -> I

desugarExpr (S.Block _ ss) = do
    ss' <- desugarStmts ss
    pure $ combineSyns [ns ss'] $ mkSyn $ I.Block $ _synTree ss'

desugarExpr (S.If _ ts els) = do
    ts' <- mapM f ts
    els' <- g els
    -- there has to be an easier way to do this
    let stTs = flip map ts' $ (\(a,b) -> (_synTree a, _synTree b))
        stEls = case els' of
                   Nothing -> Nothing
                   Just x -> Just $ _synTree x
        cs = combineSyns $
             flip concatMap ts' (\(a,b) -> [ns a, ns b])
             ++ (maybe [] ((:[]) . ns) els')
    pure $ cs $ mkSyn $ I.If stTs stEls
  where
    g Nothing = pure Nothing
    g (Just x) = Just <$> desugarStmts x
    f (a,b) = (,) <$> desugarExpr a <*> desugarStmts b

desugarExpr (S.DotExpr _ e fld) = do
    e' <- desugarExpr e
    pure $ combineSyns [ns e'] $  mkSyn $ I.DotExpr (_synTree e') fld
     
desugarExpr (S.RecordSel _ fs) = do
    trm <- ("_torepr",) <$> desugarExpr (S.Iden n "_record_torepr")
    eqm <- ("_equals",) <$> desugarExpr (S.Iden n "_record_equals")
    fs' <- mapM (\(a,b) -> (a,) <$> desugarExpr b) fs
    let fs'' = trm : eqm : fs'
        addHelpers = over synFreeVars (nub . (\x -> "_record_torepr" : "_record_equals" : x))
    pure $ addHelpers $ combineSyns (map (ns . snd) fs'')
        $ mkSyn $ I.VariantSel "record" $ map stp fs''
        
  where
    stp (a,b) = (a,_synTree b)
    n = Nothing

desugarExpr (S.TupleSel _ fs) = do
    trm <- ("_torepr",) <$> desugarExpr (S.Iden n "_tuple_torepr")
    eqm <- ("_equals",) <$> desugarExpr (S.Iden n "_tuple_equals")
    let fs1 = zip (map show [(0::Int)..]) fs
    fs' <- mapM (\(a,b) -> (a,) <$> desugarExpr b) fs1
    let fs'' = trm : eqm : fs'
        addHelpers = over synFreeVars (nub . (\x -> "_record_torepr" : "_record_equals" : x))
    let x = addHelpers $ combineSyns (map (ns . snd) fs'')
            $ mkSyn $ I.VariantSel "tuple" $ map stp fs''
    pure x
  where
    stp (a,b) = (a,_synTree b)
    n = Nothing

desugarExpr (S.App _ (S.Iden _ "run-task-cs") [e]) = do
    e' <- desugarExpr e
    pure $ combineSyns [ns e'] $ mkSyn $ I.RunTask False (_synTree e')

desugarExpr (S.App _ (S.Iden _ "run-task-cs-async") [e]) = do
    e' <- desugarExpr e
    pure $ combineSyns [ns e'] $ mkSyn $ I.RunTask True (_synTree e')

desugarExpr (S.App sp f es) = do
    let spx = case sp of
            Nothing -> "nothing"
            Just (n,i,j) -> "(" <> n <> "," <> show i <> "," <> show j <> ")"
    f' <- desugarExpr f
    es' <- mapM desugarExpr es
    pure $ combineSyns (ns f' : map ns es')
        $ mkSyn $ I.App (Just spx) (_synTree f') (map _synTree es')

desugarExpr (S.Lam _ (S.FunHeader _ bs _) bdy) = do
    bdy' <- desugarStmts bdy
    bs' <- mapM desugarBinding bs
    -- get the freevars from desugar bindings to pass up
    -- get the freevars from body to pass up
    --   remove the newbindings from bindings before passing up
    -- how to refactor this into a nice helper?
    let nb = concatMap (view synNewBindings) bs'
        bodyFreeVars = view synFreeVars bdy' \\ nb
        bindingsFreeVars = concatMap (view synFreeVars) bs'
        -- add a few bonus things for now, later will get the closure capture
        -- sorted and remove these
        allFreeVars = nub $ bodyFreeVars ++ bindingsFreeVars ++ ["make-list", "make-variant", "left", "right"]
        setFreeVars = set synFreeVars allFreeVars
    pure $ setFreeVars
         $ combineSynsNoFreeVars (map ns bs' ++ [ns bdy'])
         $ mkSyn $ I.Lam allFreeVars (map _synTree bs') (_synTree bdy')

desugarExpr (S.Text _ s) = pure $ mkSyn $ I.IString s
desugarExpr (S.Num _ s) = pure $ mkSyn $ I.Num s

-- iden
desugarExpr (S.Iden _ i) =
    pure (mkSyn $ I.Iden i) {_synFreeVars = [i]}
-- methods
desugarExpr (S.MethodExpr _ (S.Method (S.FunHeader _ts (a:as) _mty) bdy)) = do
    e <- desugarExpr (S.Lam Nothing (S.FunHeader [] [a] Nothing)
                        [S.StmtExpr Nothing $ S.Lam Nothing (S.FunHeader [] as Nothing) bdy])
    pure $ combineSyns [ns e] $  mkSyn $ I.MethodExpr (_synTree e)

desugarExpr (S.Cases pos tst _ bs mels) = do
    tst' <- desugarExpr tst
    let bsx = bs ++ case mels of
            Nothing -> [(S.NameBinding Nothing "asdfsdfs", Nothing
                        ,[S.StmtExpr n $ S.App n (S.Iden n "raise")
                          [S.BinOp n (S.Text n (show pos <> " cases no branch matched: ")) "+"
                              (S.App n (S.Iden n "torepr") [S.Iden n "asdfsdfs"])]])]
            Just el -> [(S.WildcardBinding Nothing, Nothing, el)]
    bs' <- mapM desugarBranch bsx
    pure $ combineSyns (ns tst' : map ns bs')
         $ mkSyn $ I.Cases (_synTree tst') (map _synTree bs')
  where
    desugarBranch (bm,_,bdy) = do
        bm' <- desugarBinding bm
        bdy' <- desugarStmts bdy
        pure $ set synFreeVars (nub (view synFreeVars bdy' \\ view synNewBindings bm'))
            $ combineSynsNoFreeVars [ns bm', ns bdy']
            $ mkSyn (_synTree bm', _synTree bdy')
    n = Nothing

desugarExpr x = error $ "desugarExpr " <> show x

---------------------------------------

-- let rec implemented as a macro using variables in the usual scheme
-- style

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

---------------------------------------

desugarBinding :: S.Binding -> Desugar (Syn I.Binding)
desugarBinding = \case
    S.NameBinding _ nm -> do
        x <- isCtor nm
        -- if it's a no arg ctor, convert to variant binding
        if x
            then addName nm $ mkSyn (I.VariantBinding nm [])
            else addName nm $ mkSyn (I.NameBinding nm)
        --addName nm $ mkSyn (I.NameBinding nm)
    S.ShadowBinding _ nm -> addName nm $ mkSyn (I.NameBinding nm)
    S.VariantBinding _ [vnm] bs -> do
        bs' <- mapM desugarBinding bs
        addName vnm $ combineSynsWithNewBindings (map ns bs') $ mkSyn $ I.VariantBinding vnm $ map _synTree bs'
    S.WildcardBinding _ -> pure $ mkSyn $ I.WildcardBinding
    S.TupleBinding _ bs -> do
        -- combine the subbindings
        bs' <- mapM desugarBinding bs
        pure $ combineSynsWithNewBindings (map ns bs') $ mkSyn $ I.TupleBinding $ map _synTree bs'
    x -> error $ "unsupported binding: " <> show x
  where
    addName nm b = do
        x <- isCtor nm
        -- if nm is a ctor add to freevars otherwise add to newbindings
        let l = if x
                then synFreeVars
                else synNewBindings
        pure $ over l (nm:) b
    isCtor :: Text -> Desugar Bool
    isCtor nm = asks ((nm `elem`) .  view inhCtors)
