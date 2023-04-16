
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

--import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Burdock.Runtime
    (Value(..)
    ,runBurdock
    ,Runtime
    --,liftIO

    --,ffimethodapp
    --,RuntimeState
    ,emptyRuntimeState
    --,addFFIType

    ,getMember
    ,app

    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding
    ,captureClosure
    
    ,makeValue
    ,extractValue

    ,catchEither
    --,makeFunctionValue
    --,Type(..)
    --,Scientific
    )

--import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
import qualified Data.Text as T

import Burdock.DefaultRuntime (initRuntime)
import Control.Monad (forM_)

interpBurdock :: [I.Stmt] -> IO Value
interpBurdock ss = do
    st <- emptyRuntimeState
    runBurdock st $ do
        initRuntime
        interpStmts ss

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
            app left [makeValue "string" e]
        -- a left right v is a burdock value that has been raised from burdock
        -- or ffi code
        Left (Right v) -> do
            left <- interpExpr (I.Iden "left")
            app left [v]
        Right v -> do
            right <- interpExpr (I.Iden "right")
            app right [v]

interpExpr (I.App ef es) = do
    vs <- mapM interpExpr es
    f <- interpExpr ef
    app f vs

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
        Nothing -> error $ "binding not found: " ++ T.unpack nm
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

interpExpr (I.MethodExpr e) =
    MethodV <$> interpExpr e
    -- todo: need to check coding and user errors produce useful error messages
    -- when the e's value isn't what it should be

--interpExpr x = error $ "interpExpr: " ++ show x


letExprs :: [(Text, I.Expr)] -> Runtime ()
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
letValues :: [(Text, Value)] -> Runtime ()
letValues bs = forM_ bs $ \(nm,v) -> addBinding nm v

