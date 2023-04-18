
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
    ,createHandle
    ,runBurdock
    ,getTempTestsPass
    ,liftIO
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
--import Data.Text.IO (putStrLn)

--import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Burdock.Runtime
    (Value(..)
    ,runBurdock
    ,Runtime
    ,RuntimeState
    ,getRuntimeState
    ,getTempTestsPass
    ,liftIO

    --,ffimethodapp
    --,RuntimeState
    ,emptyRuntimeState
    --,addFFIType

    ,variantTag
    ,variantFields
    
    ,getMember
    ,app

    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding
    ,captureClosure
    
    ,makeValue
    ,extractValue

    ,debugShowValue
    
    ,catchEither
    --,makeFunctionValue
    --,Type(..)
    --,Scientific
    )

--import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
--import qualified Data.Text as T

import Burdock.DefaultRuntime (initRuntime)
import Control.Monad
    (forM_
    ,when)

createHandle :: IO RuntimeState
createHandle = do
    st <- emptyRuntimeState
    runBurdock st $ do
        initRuntime
        getRuntimeState

interpBurdock :: [I.Stmt] -> Runtime Value
interpBurdock ss = interpStmts ss

interpStmts :: [I.Stmt] -> Runtime Value
interpStmts [] = error "no statements"
interpStmts [s] = interpStmt s
interpStmts (s:ss) = interpStmt s *> interpStmts ss

interpStmt :: I.Stmt -> Runtime Value

interpStmt (I.LetDecl b e) = do
    letExprs [(b, e)]
    pure VNothing

interpStmt (I.StmtExpr e) = interpExpr e

--interpStmt s = error $ "interpStmt: " ++ show s

interpExpr :: I.Expr -> Runtime Value

interpExpr (I.DotExpr e1 fld) = do
    v1 <- interpExpr e1
    getMember v1 fld

interpExpr (I.RecordSel fs) = do
    vs <- mapM (\(n,e) -> (n,) <$> interpExpr e) fs
    -- todo: "record" has to be namespaced
    pure $ VariantV "record" vs

interpExpr (I.RunTask tsk) = do
    x <- catchEither $ interpExpr tsk
    case x of
        -- a left left of a string is an show'n arbitrary haskell exception
        Left (Left e) -> do
            left <- interpExpr (I.Iden "left")
            app Nothing left [makeValue "string" e]
        -- a left right v is a burdock value that has been raised from burdock
        -- or ffi code
        Left (Right v) -> do
            left <- interpExpr (I.Iden "left")
            app Nothing left [v]
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

interpExpr (I.Num n) =
    {- todo: you either have to look up "number" in the runtime environment
       or keep a token from when the number type was created, this is so type
       names are namespaced and scoped, e.g. if you have two modules which have
       a type with the same name as each other
    -}
    pure $ makeValue "number" n

interpExpr (I.IString t) =
    pure $ makeValue "string" t

interpExpr (I.Iden nm) = do
    b <- lookupBinding nm
    case b of
        Nothing -> error $ "binding not found: " <> nm
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



{-

function which takes binding syntax, and a value, and returns just if they match,
returning all the simple binding names and the corresponding values to apply
as letdecls
later it will also return the shadow-age of each binding

-}

tryApplyBinding :: I.Binding -> Value -> Runtime (Maybe [(Text,Value)])


tryApplyBinding (I.NameBinding nm) v = pure $ Just [(nm,v)]
tryApplyBinding I.WildcardBinding _ = pure $ Just []
tryApplyBinding (I.VariantBinding vnm flds) v = do
    -- check v is a variant
    vt' <- variantTag v 
    case vt' of
        Nothing -> pure Nothing
        Just vt ->
            -- check it's tag matches vnm
            if vt /= vnm
            then pure Nothing
            else do
                -- todo: use a single function that gets the tag and the fields as a maybe
                vfs' <- maybe (error "impossible? tryApplyBinding I.VariantBinding variant fields Nothing") id
                       <$> variantFields v
                -- dirty hack
                -- todo: generate something internal to be able to get the fields like this
                let vfs = filter ((`notElem` ["_equals", "_torepr"]) . fst) vfs'
                -- check there's the right number of flds
                when (length vfs /= length flds) $ error $ "wrong number of args to variant binding " <> vnm <> " expected " <> show (length vfs) <> ", got " <> show (length flds)
                   <> "\n" <> show (flip map vfs $ \(n,v1)-> (n, debugShowValue v1), flds)
                -- gather the tryapplies recursively for the v fields
                x :: [Maybe [(Text, Value)]] <- mapM (uncurry tryApplyBinding) (zip flds $ map snd vfs)
                pure (concat <$> sequence x)
--tryApplyBinding _ = Nothing
