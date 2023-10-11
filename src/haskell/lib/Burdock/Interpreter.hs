
{-

Interpreter is the code which takes desugared (todo) syntax and
executes it on the runtime.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Interpreter
    (interpBurdock
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import qualified Burdock.InterpreterSyntax as I
import Burdock.RuntimeBootstrap
    (Runtime
    ,Value(..)

    ,withScope
    ,lookupBinding
    ,getMember
    ,runTask
    ,app
    ,addBinding
    ,captureClosure
    ,withNewEnv
    
    ,liftIO

    ,makeString
    ,makeNumber
    ,makeRecord
    ,makeTuple
    ,makeBurdockList

    ,extractTuple
    ,extractValue

    ,variantName
    ,variantValueFields
    
    ,nothingValue
    ,debugShowValue
    
    )

import Data.Text (Text)
import qualified Data.Text as T
    
import Control.Monad
    (forM_
    ,when
    ,zipWithM
    )

import Data.IORef
    (newIORef
    ,writeIORef
    ,readIORef
    )

------------------------------------------------------------------------------

interpBurdock :: [I.Stmt] -> Runtime Value
interpBurdock ss = interpStmts ss

interpStmts :: [I.Stmt] -> Runtime Value
interpStmts [] = error "no statements"
interpStmts [s] = interpStmt s
interpStmts (s:ss) = interpStmt s *> interpStmts ss

interpStmt :: I.Stmt -> Runtime Value

interpStmt (I.LetDecl b e) = do
    letExprs [(b, e)]
    nothingValue

-- not sure about the withScope - if there are bindings nested in e,
-- they can escape without this, but maybe the withscope needs to be
-- moved to more precise places (e.g. in if branch bodies)
interpStmt (I.StmtExpr e) = withScope $ interpExpr e

interpStmt (I.VarDecl nm e) = do
    v <- interpExpr e
    vr <- liftIO $ newIORef v
    letSimple [(nm, BoxV vr)]
    nothingValue

interpStmt (I.SetVar nm e) = do
    v <- interpExpr e
    vrb <- lookupBinding nm
    case vrb of
        Just (BoxV vr) -> liftIO $ writeIORef vr v
        Nothing -> error $ "iden not found: " <> nm
        Just x -> error $ "set var on non variable: " <> debugShowValue x
    nothingValue

--interpStmt s = error $ "interpStmt: " ++ show s

---------------------------------------

interpExpr :: I.Expr -> Runtime Value

interpExpr (I.DotExpr e1 fld) = do
    v1 <- interpExpr e1
    getMember v1 fld

interpExpr (I.RecordSel fs) = do
    vs <- flip mapM fs $ \(n,e) -> (n,) <$> interpExpr e
    makeRecord vs

interpExpr (I.TupleSel fs) = do
    vs <- mapM interpExpr fs
    makeTuple vs

interpExpr (I.RunTask catchAsync tsk) = do
    x <- runTask catchAsync $ interpExpr tsk
    case x of
        -- a left left of a string is an show'n arbitrary haskell exception
        Left (Left e, st) -> do
            left <- interpExpr (I.Iden "left")
            st' <- makeBurdockList =<< mapM (makeString . maybe "nothing" id) st
            e' <- makeString e
            ret <- makeRecord [("exception", e')
                              ,("callstack", st')]
            app Nothing left [ret]
        -- a left right v is a burdock value that has been raised from burdock
        -- or ffi code
        Left (Right v, st) -> do
            left <- interpExpr (I.Iden "left")
            st' <- makeBurdockList =<< mapM (makeString . maybe "nothing" id) st
            ret <- makeRecord [("exception", v)
                              ,("callstack", st')]
            app Nothing left [ret]
        Right v -> do
            right <- interpExpr (I.Iden "right")
            app Nothing right [v]

interpExpr (I.App sp ef es) = do
    vs <- mapM interpExpr es
    f <- interpExpr ef
    app sp f vs

interpExpr (I.Lam fvs bs bdy) = do
    env <- captureClosure fvs
    -- todo: how do you test the freevars function?
    -- how do you test that the right closures are captured?
    --   now can check the desugared syntax directly for these
    let runF :: [Value] -> Runtime Value
        runF vs = do
            -- todo: check lists same length
            let bs' = zip bs vs
            withNewEnv env $ do
                letValues bs'
                interpStmts bdy
    pure $ VFun runF

interpExpr (I.Num n) = makeNumber n

interpExpr (I.IString t) = makeString t

interpExpr (I.Iden nm) = do
    b <- lookupBinding nm
    case b of
        Nothing -> error $ "binding not found: " <> nm
        Just (BoxV v) -> liftIO $ readIORef v
        Just v -> pure v

interpExpr (I.Block sts) = withScope $ interpStmts sts

interpExpr (I.If cs els) =
    let m ((t,e):cs') = do
            tv <- interpExpr t
            case extractValue tv of
                Nothing -> error "non boolean in if test"
                Just v ->
                    if v
                    then interpStmts e
                    else m cs'
        m [] = do
            case els of
                Nothing -> error "no if branches matched and no else"
                Just e -> interpStmts e
    in m cs

interpExpr (I.Cases e bs) = do
    v <- interpExpr e
    let f [] = error $ "internal error, cases fall through"
        f ((b,bdy):bs') = do
            b' <- tryApplyBinding b v
            case b' of
                Nothing -> f bs'
                Just ls -> withScope $ do
                    letSimple ls
                    interpStmts bdy
    f bs

interpExpr (I.MethodExpr e) =
    MethodV <$> interpExpr e
    -- todo: need to check coding and user errors produce useful error messages
    -- when the e's value isn't what it should be

--interpExpr x = error $ "interpExpr: " ++ show x

------------------------------------------------------------------------------


letExprs :: [(I.Binding, I.Expr)] -> Runtime ()
letExprs bs = do
    bs' <- mapM (\(nm,e) -> (nm,) <$> interpExpr e) bs
    letValues bs'

-- revisit when do the full binding, want to decide who has responsibility
-- for the different features of binding
-- if it's purely syntax specific, put it here
-- if it's something e.g. other ffi might want to use also, put it in the
-- runtime
-- or create a separate helper module to keep syntax out of the runtime but
-- not duplicate a bunch of non-syntax syntax for the runtime
letValues :: [(I.Binding, Value)] -> Runtime ()
letValues bs = forM_ bs $ \(b,v) -> do
    mbs <- tryApplyBinding b v
    case mbs of
        Nothing -> error $ "couldn't bind " <> debugShowValue v <> " to " <> show b
        Just bs' -> mapM_ (uncurry addBinding) bs'

letSimple :: [(Text, Value)] -> Runtime ()
letSimple bs = mapM_ (uncurry addBinding) bs


------------------------------------------------------------------------------

{-

function which takes binding syntax, and a value, and returns just if they match,
returning all the simple binding names and the corresponding values to apply
as letdecls
later it will also return the shadow-age of each binding

-}

tryApplyBinding :: I.Binding -> Value -> Runtime (Maybe [(Text,Value)])


-- temp? hack for boolean literals
tryApplyBinding (I.VariantBinding "_patterninfo-true" []) v | Just True <- extractValue v = pure $ Just []
tryApplyBinding (I.VariantBinding "_patterninfo-true" []) _ = pure Nothing
tryApplyBinding (I.VariantBinding "_patterninfo-false" []) v | Just False <- extractValue v = pure $ Just []
tryApplyBinding (I.VariantBinding "_patterninfo-false" []) _ = pure Nothing

tryApplyBinding (I.NameBinding nm) v = pure $ Just [(nm,v)]
tryApplyBinding I.WildcardBinding _ = pure $ Just []

tryApplyBinding (I.TupleBinding bs) v = do
    vs' <- extractTuple v
    case vs' of
        Nothing -> pure Nothing
        Just vs ->
            if length bs /= length vs
            then pure Nothing
            else do
                x <- zipWithM tryApplyBinding bs vs
                pure $ (concat <$> sequence x)

tryApplyBinding (I.VariantBinding vnm' flds) v = do
    -- temp quick hack
    let vnm = T.drop (T.length ("_patterninfo-"::Text)) vnm'
    -- check v is a variant
    vt' <- variantName v 
    case vt' of
        Nothing -> pure Nothing
        Just vt ->
            -- check it's tag matches vnm
            if vt /= vnm
            then pure Nothing
            else do
                -- todo: use a single function that gets the tag and the fields as a maybe
                vfs <- maybe (error "impossible? tryApplyBinding I.VariantBinding variantValueFields is Nothing")
                             id <$> variantValueFields v
                -- check there's the right number of flds
                when (length vfs /= length flds) $ error $ "wrong number of args to variant binding " <> vnm <> " expected " <> show (length vfs) <> ", got " <> show (length flds)
                   <> "\n" <> show (flip map vfs $ \(n,v1)-> (n, debugShowValue v1), flds)
                -- gather the tryapplies recursively for the v fields
                x :: [Maybe [(Text, Value)]] <- mapM (uncurry tryApplyBinding) (zip flds $ map snd vfs)
                pure (concat <$> sequence x)
--tryApplyBinding _ = Nothing
