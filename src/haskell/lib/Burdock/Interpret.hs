
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.Interpret
    (interpBurdock
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
--import Data.Text.IO (putStrLn)
--import qualified Data.Text.Lazy.IO as L

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Data.IORef
    (readIORef)

import qualified Burdock.InterpreterSyntax as I
--import qualified Burdock.PrettyInterpreter as I
import qualified Burdock.Runtime as R

import Burdock.Rename (intMod)
import Control.Monad (void)

------------------------------------------------------------------------------

interpBurdock :: [I.Stmt] -> R.Runtime R.Value
interpBurdock = interpStmts

interpStmts :: [I.Stmt] -> R.Runtime R.Value
interpStmts [] = pure R.BNothing

-- the other solution here, is to not have letdecl, vardecl, import as
-- as interpreter syntax, but only a let <bindings> in expression form
-- a remaining reason why originally moved away from let expression style,
-- was that it took a bunch of hacks to make the pretty printed interpreter
-- syntax readable again. not sure if this is a good enough reason to
-- stick with this statement form
-- but there's not that much difference in the end between the two approaches
-- the interp here currently is just actually simulating the let expression
-- style when given the letdecl style syntax

interpStmts (I.LetDecl _ b e : ss) = do
    v <- interpExpr e
    letValues [(b,v)] $ interpStmts ss

-- todo: make vars abstract, move to runtime
interpStmts (I.VarDecl sp nm e : ss) = do
    v <- interpExpr e
    r <- R.makeVar v
    letValues [(I.NameBinding sp nm, r)] $ interpStmts ss

interpStmts (I.ImportAs sp nm (p,as) : ss) = do
    v <- R.getModuleValue (R.ModuleID p as)
    letValues [(I.NameBinding sp nm, v)] $ interpStmts ss
    
interpStmts [s] = interpStmt s
interpStmts (s:ss) = interpStmt s *> interpStmts ss

interpStmt :: I.Stmt -> R.Runtime R.Value

interpStmt (I.SetVar sp [nm] e) = do
    v <- interpExpr e
    Just bx <- R.lookupBinding nm
    R.setVar sp bx v
    pure R.BNothing
-- todo: generalize
interpStmt (I.SetVar sp [nm,nm1] e) = do
    v <- interpExpr e
    Just bx <- R.lookupBinding nm
    case bx of
        R.Module _fs -> error $ "implement me"
        R.FFIValue {} -> do
            m <- R.getMember sp bx "_assign"
            void $ R.app sp m [R.BString nm1, v]
        _ -> error $ "bad assign target"
    pure R.BNothing


interpStmt (I.StmtExpr _ e) = interpExpr e

interpStmt s = error $ "interpStmt: " <> show s

------------------------------------------------------------------------------

interpExpr :: I.Expr -> R.Runtime R.Value

interpExpr (I.Num _sp n) = do
    --liftIO $ putStrLn $ show sp
    Just bs <- R.lookupBinding intMod
    bnum <- R.getMember Nothing bs "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (nti :: R.FFITypeInfo) <- R.extractFFIValue ti bnum
    R.makeFFIValue nti n
    -- pure $ R.Number n
interpExpr (I.IString _ n) = pure $ R.BString n

interpExpr (I.DotExpr sp e1 fld) = do
    v1 <- interpExpr e1
    R.getMember sp v1 fld

interpExpr (I.Iden sp nm) = do
    b <- R.lookupBinding nm
    case b of
        Nothing -> --do
            --bs <- R.debugGetBindings
            error $ show sp <> " binding not found: " <> nm --  <> "\n" <> T.unlines (map fst bs)
        Just (R.Box v) -> liftIO $ readIORef v
        Just v -> pure v

interpExpr (I.App sp ef es) = do
    vs <- mapM interpExpr es
    f <- interpExpr ef
    R.app sp f vs

interpExpr (I.MethodExpr _sp e) = R.MethodV <$> interpExpr e

interpExpr (I.Lam _sp fvs bs bdy) = do
    env <- R.captureClosure fvs
    let runF :: [R.Value] -> R.Runtime R.Value
        runF vs = do
            -- todo: check lists same length
            let bs' = zip bs vs
            R.withNewEnv env $ letValues bs' $ interpStmts bdy
    pure $ R.Fun runF

interpExpr (I.If sp bs' mels) =
    let f [] = case mels of
            Nothing -> error $ show sp <> " no if branches match and no else"
            Just els -> R.withScope $ interpStmts els
        f ((t,b):bs) = do
            tv <- interpExpr t
            case tv of
                R.Boolean False -> f bs
                R.Boolean True -> R.withScope $ interpStmts b
                x -> error $ show sp <> "expected boolean, got " <> R.debugShowValue x
    in f bs'

interpExpr (I.Cases sp e bs) = do
    v <- interpExpr e
    let f [] = error $ "no case branches matched"
        f ((t,bdy):bs') = do
            res <- tryApplyBinding sp t v
            case res of
                Nothing -> f bs'
                Just ls -> R.withScope $ do
                    R.localEnv (ls++) $ interpStmts bdy
    f bs

interpExpr (I.Block _ stmts) = R.withScope $ interpStmts stmts

interpExpr (I.RecordSel sp fs) = do
    fs' <- flip mapM fs $ \(n,e) -> (n,) <$> interpExpr e
    mbstp <- R.lookupBinding intMod
    bstp <- case mbstp of
            Just x -> pure x
            Nothing -> do
                ds <- R.debugGetBindings
                error $ "internal interp error no binding for " <> intMod <> ": " <> T.unlines (map fst ds)
    vvti <- R.getMember Nothing bstp "_type-variant-tag"
    ffiti <- R.getFFITypeInfoTypeInfo
    Right vvti' <- R.extractFFIValue ffiti vvti
    ttagB <- R.getMember Nothing bstp "_variant-record"
    Right ttag <- R.extractFFIValue vvti' ttagB

    let lam cl as e = I.Lam sp cl (map (I.NameBinding sp) as) ([I.StmtExpr sp e])
        app f as = I.App sp (I.DotExpr sp (I.Iden sp intMod) f) as
        nms = map ((I.IString sp) . fst) fs
        eqs = I.MethodExpr sp
            $ lam [intMod] ["a"]
            $ lam [intMod, "a"] ["b"]
            $ app "variants-equal" [app "make-haskell-list" nms
                                   ,I.Iden sp "a"
                                   ,I.Iden sp "b"]
        tor = I.MethodExpr sp
            $ lam [intMod] ["a"]
            $ lam [intMod, "a"] []
            $ app "show-record" [I.Iden sp "a"]
    equals <- interpExpr eqs
    torepr <- interpExpr tor
    pure $ R.Variant ttag (fs' ++ [("_equals", equals), ("_torepr", torepr)])

interpExpr (I.RunTask _ e) = do
    r <- R.runTask $ interpExpr e
    Just bstp <- R.lookupBinding intMod
    bright <- R.getMember Nothing bstp "right"
    bleft <- R.getMember Nothing bstp "left"
    case r of
        Right v -> R.app Nothing bright [v]
        Left err -> R.app Nothing bleft [err]

interpExpr (I.TupleSel sp es) = do
    vs <- mapM interpExpr es
    Just bstp <- R.lookupBinding intMod
    vvti <- R.getMember Nothing bstp "_type-variant-tag"
    ffiti <- R.getFFITypeInfoTypeInfo
    Right vvti' <- R.extractFFIValue ffiti vvti
    ttagB <- R.getMember Nothing bstp "_variant-tuple"
    Right ttag <- R.extractFFIValue vvti'  ttagB

    let fs = zipWith (\n v -> (show n, v)) [(0::Int)..] vs
    -- todo: add _equals and _torepr methods
    -- just cheat and recursively call interpExpr, can do it properly later
    -- method(a,b): variants-equal(make-haskell-list(map fst fs), a, b) end
    let lam cl as e = I.Lam sp cl (map (I.NameBinding sp) as) ([I.StmtExpr sp e])
        app f as = I.App sp (I.DotExpr sp (I.Iden sp intMod) f) as
        nms = map ((I.IString sp) . fst) fs
        eqs = I.MethodExpr sp
            $ lam [intMod] ["a"]
            $ lam [intMod, "a"] ["b"]
            $ app "variants-equal" [app "make-haskell-list" nms
                                   ,I.Iden sp "a"
                                   ,I.Iden sp "b"]
        tor = I.MethodExpr sp
            $ lam [intMod] ["a"]
            $ lam [intMod, "a"] []
            $ app "show-tuple" [I.Iden sp "a"]
            
    -- liftIO $ L.putStrLn $ I.prettyStmts [I.StmtExpr Nothing x]
    equals <- interpExpr eqs
    torepr <- interpExpr tor
    pure $ R.Variant ttag (fs ++ [("_equals", equals), ("_torepr", torepr)])

interpExpr (I.TupleGet _ e n) = do
    v <- interpExpr e
    -- todo check it's tuple type, need the closure capture/boilerplate
    -- work else it's tedious
    case v of
        R.Variant _ _ -> R.getMember Nothing v (show n)
        _ -> error $ "wrong value for tuple get: " <> R.debugShowValue v

--interpExpr e = error $ "interpExpr: " <> show e

------------------------------------------------------------------------------

letValues :: [(I.Binding, R.Value)] -> R.Runtime a -> R.Runtime a
letValues [] f = f
letValues ((b,v):bs) f = do
    mbs <- tryApplyBinding Nothing b v
    case mbs of
        Nothing -> error $ "couldn't bind " <> R.debugShowValue v <> " to " <> show b
        Just bs' -> letSimple bs' $ letValues bs f

letSimple :: [(Text, R.Value)] -> R.Runtime a-> R.Runtime a
letSimple [] f = f
letSimple ((nm,v):bs) f = R.addBinding nm v $ letSimple bs f

------------------------------------------------------------------------------

tryApplyBinding :: I.SP -> I.Binding -> R.Value -> R.Runtime (Maybe [(Text,R.Value)])

tryApplyBinding _ (I.NameBinding _sp nm) v = pure $ Just [(nm,v)]

tryApplyBinding _ (I.VariantBinding _sp nm bs) (R.Variant (R.VariantTag vddt vvnm) vfs) = do
    mvtg <- lookupVariantByName nm
    case mvtg of
        Just (R.VariantTag pddt pvnm) | (pddt,pvnm) == (vddt,vvnm) -> do
            x <- flip mapM (zip bs vfs) $ \(b, vf) ->
                tryApplyBinding Nothing b (snd vf)
            let y :: Maybe [(Text, R.Value)]
                y = concat <$> sequence x
            pure y
        _ -> pure $ Nothing

tryApplyBinding _ (I.VariantBinding {}) _ = pure Nothing    

tryApplyBinding _ (I.WildcardBinding {}) _ = pure $ Just []

tryApplyBinding _ b _ = error $ show b

lookupNames :: [Text] -> R.Runtime (Maybe R.Value)
lookupNames [] = error $ "empty name"
lookupNames [n] = R.lookupBinding n
lookupNames (n:ns) = do
    let f _ [] = error $ "empty name2"
        f v [m] = Just <$> R.getMember Nothing v m
        f v (m:ms) = do
            v' <- R.getMember Nothing v m
            f v' ms
    vx <- R.lookupBinding n
    case vx of
        Just vx' -> f vx' ns
        Nothing -> pure Nothing

lookupVariantByName :: [Text] -> R.Runtime (Maybe R.VariantTag)
lookupVariantByName nm = do
    bv <- lookupNames nm
    case bv of
        -- if it matches a variant in scope, we commit to it
        -- if it matches something else in scope, this should be a shadowing
        -- issue, which will be handled elsewhere, so assume it's ok to shadow
        Just vtg -> do
            -- vtg is a value, inside this value we want to find a variant tag
            Just bstp <- R.lookupBinding intMod
            vvti <- R.getMember Nothing bstp "_type-variant-tag"
            ffiti <- R.getFFITypeInfoTypeInfo
            Right vvti' <- R.extractFFIValue ffiti vvti
            cand <- R.extractFFIValue vvti' vtg
            case cand of
                Left _ -> pure Nothing
                Right (r :: R.VariantTag) -> pure $ Just r
        _ -> pure Nothing
