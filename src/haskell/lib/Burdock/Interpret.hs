
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
--import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Data.IORef
    (readIORef)

import qualified Burdock.InterpreterSyntax as I
--import qualified Burdock.PrettyInterpreter as I
import qualified Burdock.Runtime as R

------------------------------------------------------------------------------

interpBurdock :: [I.Stmt] -> R.Runtime R.Value
interpBurdock = interpStmts

interpStmts :: [I.Stmt] -> R.Runtime R.Value
interpStmts [] = error "no statements" -- todo: change to return nothing
interpStmts [s] = interpStmt s
interpStmts (s:ss) = interpStmt s *> interpStmts ss

interpStmt :: I.Stmt -> R.Runtime R.Value

interpStmt (I.LetDecl _ b e) = do
    v <- interpExpr e
    letValues True [(b,v)]
    pure R.BNothing

-- todo: make vars abstract, move to runtime
interpStmt (I.VarDecl sp nm e) = do
    v <- interpExpr e
    r <- R.makeVar v
    letValues True [(I.NameBinding sp nm, r)]
    pure R.BNothing

interpStmt (I.SetVar sp [nm] e) = do
    v <- interpExpr e
    Just bx <- R.lookupBinding nm
    R.setVar sp bx v
    pure R.BNothing

interpStmt (I.StmtExpr _ e) = interpExpr e

interpStmt (I.ImportAs sp nm (p,as)) = do
    v <- R.getModuleValue (R.ModuleID p as)
    letValues True [(I.NameBinding sp nm, v)]
    pure R.BNothing

interpStmt s = error $ "interpStmt: " <> show s

------------------------------------------------------------------------------

interpExpr :: I.Expr -> R.Runtime R.Value

interpExpr (I.Num _sp n) = do
    --liftIO $ putStrLn $ show sp
    Just bs <- R.lookupBinding "_interpreter"
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

interpExpr (I.MethodExpr _sp e) = R.Method <$> interpExpr e

interpExpr (I.Lam _sp fvs bs bdy) = do
    env <- R.captureClosure fvs
    let runF :: [R.Value] -> R.Runtime R.Value
        runF vs = do
            -- todo: check lists same length
            let bs' = zip bs vs
            R.withNewEnv env $ do
                letValues True bs'
                interpStmts bdy
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
            res <- tryApplyBinding sp False t v
            case res of
                Nothing -> f bs'
                Just ls -> R.withScope $ do
                    R.localEnv (ls++) $ interpStmts bdy
    f bs

interpExpr (I.Block _ stmts) = R.withScope $ interpStmts stmts

interpExpr (I.RecordSel sp fs) = do
    fs' <- flip mapM fs $ \(n,e) -> (n,) <$> interpExpr e

    Just bstp <- R.lookupBinding "_interpreter"
    vvti <- R.getMember Nothing bstp "_type-variant-tag"
    ffiti <- R.getFFITypeInfoTypeInfo
    Right vvti' <- R.extractFFIValue ffiti vvti
    ttagB <- R.getMember Nothing bstp "_variant-record"
    Right ttag <- R.extractFFIValue vvti' ttagB

    let lam cl as e = I.Lam sp cl (map (I.NameBinding sp) as) ([I.StmtExpr sp e])
        app f as = I.App sp (I.DotExpr sp (I.Iden sp "_interpreter") f) as
        nms = map ((I.IString sp) . fst) fs
        eqs = I.MethodExpr sp
            $ lam ["_interpreter"] ["a"]
            $ lam ["_interpreter", "a"] ["b"]
            $ app "variants-equal" [app "make-haskell-list" nms
                                   ,I.Iden sp "a"
                                   ,I.Iden sp "b"]
        tor = I.MethodExpr sp
            $ lam ["_interpreter"] ["a"]
            $ lam ["_interpreter", "a"] []
            $ app "show-record" [I.Iden sp "a"]
    equals <- interpExpr eqs
    torepr <- interpExpr tor
    pure $ R.Variant ttag (fs' ++ [("_equals", equals), ("_torepr", torepr)])

interpExpr (I.RunTask _ e) = do
    r <- R.runTask $ interpExpr e
    Just bstp <- R.lookupBinding "_interpreter"
    bright <- R.getMember Nothing bstp "right"
    bleft <- R.getMember Nothing bstp "left"
    case r of
        Right v -> R.app Nothing bright [v]
        Left err -> R.app Nothing bleft [err]

interpExpr (I.TupleSel sp es) = do
    vs <- mapM interpExpr es
    Just bstp <- R.lookupBinding "_interpreter"
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
        app f as = I.App sp (I.DotExpr sp (I.Iden sp "_interpreter") f) as
        nms = map ((I.IString sp) . fst) fs
        eqs = I.MethodExpr sp
            $ lam ["_interpreter"] ["a"]
            $ lam ["_interpreter", "a"] ["b"]
            $ app "variants-equal" [app "make-haskell-list" nms
                                   ,I.Iden sp "a"
                                   ,I.Iden sp "b"]
        tor = I.MethodExpr sp
            $ lam ["_interpreter"] ["a"]
            $ lam ["_interpreter", "a"] []
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

letValues :: Bool -> [(I.Binding, R.Value)] -> R.Runtime ()
letValues isSimple bs = flip mapM_ bs $ \(b,v) -> do
    mbs <- tryApplyBinding Nothing isSimple b v
    case mbs of
        Nothing -> error $ "couldn't bind " <> R.debugShowValue v <> " to " <> show b
        Just bs' -> mapM_ (uncurry R.addBinding) bs'

------------------------------------------------------------------------------

-- second arg is issimple, which allows variants
tryApplyBinding :: I.SP -> Bool -> I.Binding -> R.Value -> R.Runtime (Maybe [(Text,R.Value)])

-- the bool says if this is a simple binding - so a name cannot match a 0 arg variant
-- need a better solution for this
-- just removing it causes data decl desugaring to fail to execute

tryApplyBinding _ True (I.NameBinding _sp nm) v = pure $ Just [(nm,v)]
    
tryApplyBinding _ False (I.NameBinding _sp nm) v = do
    -- try to match 0 arg constructor
    mvtg <- lookupVariantByName nm
    case mvtg of
        Just (R.VariantTag pddt pvnm) -> do
            case v of
                R.Variant (R.VariantTag vddt vvnm) _ | (pddt,pvnm) == (vddt,vvnm) ->
                    pure $ Just []
                _ -> pure $ Nothing
        Nothing -> pure $ Just [(nm,v)]

tryApplyBinding _ _ (I.VariantBinding _sp [nm] bs) (R.Variant (R.VariantTag vddt vvnm) vfs) = do
    mvtg <- lookupVariantByName nm
    case mvtg of
        Just (R.VariantTag pddt pvnm) | (pddt,pvnm) == (vddt,vvnm) -> do
            x <- flip mapM (zip bs vfs) $ \(b, vf) ->
                tryApplyBinding Nothing False b (snd vf)
            let y :: Maybe [(Text, R.Value)]
                y = concat <$> sequence x
            pure y
        _ -> pure $ Nothing

tryApplyBinding _ _ (I.VariantBinding {}) _ = pure Nothing    

tryApplyBinding _ _ (I.WildcardBinding {}) _ = pure $ Just []

tryApplyBinding _ _ b _ = error $ show b

lookupVariantByName :: Text -> R.Runtime (Maybe R.VariantTag)
lookupVariantByName nm = do
    bv <- R.lookupBinding ("_variant-" <> nm)
    case bv of
        -- if it matches a variant in scope, we commit to it
        -- if it matches something else in scope, this should be a shadowing
        -- issue, which will be handled elsewhere, so assume it's ok to shadow
        Just vtg -> do
            -- vtg is a value, inside this value we want to find a variant tag
            Just bstp <- R.lookupBinding "_interpreter"
            vvti <- R.getMember Nothing bstp "_type-variant-tag"
            ffiti <- R.getFFITypeInfoTypeInfo
            Right vvti' <- R.extractFFIValue ffiti vvti
            cand <- R.extractFFIValue vvti' vtg
            case cand of
                Left _ -> pure Nothing
                Right (r :: R.VariantTag) -> pure $ Just r
        _ -> pure Nothing
