
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Interpreter
    (interpBurdock
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
--import Data.Text.IO (putStrLn)

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import Data.IORef
    (readIORef)

import qualified Burdock.InterpreterSyntax as I
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
    letValues [(b,v)]
    pure R.BNothing

interpStmt (I.StmtExpr _ e) = interpExpr e
interpStmt s = error $ "interpStmt: " <> show s

------------------------------------------------------------------------------

interpExpr :: I.Expr -> R.Runtime R.Value

interpExpr (I.Num _sp n) = do
    --liftIO $ putStrLn $ show sp
    Just bs <- R.lookupBinding "_bootstrap"
    bnum <- R.getMember Nothing bs "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (nti :: R.FFITypeInfo) <- R.extractFFIValue ti bnum
    R.makeFFIValue nti n
    -- pure $ R.Number n
interpExpr (I.IString _ n) = pure $ R.BText n

interpExpr (I.Iden _ "true") = pure $ R.Boolean True
interpExpr (I.Iden _ "false") = pure $ R.Boolean False

interpExpr (I.DotExpr sp e1 fld) = do
    v1 <- interpExpr e1
    R.getMember sp v1 fld

interpExpr (I.Iden sp nm) = do
    b <- R.lookupBinding nm
    case b of
        Nothing -> error $ show sp <> " binding not found: " <> nm
        Just (R.Box v) -> liftIO $ readIORef v
        Just v -> pure v

interpExpr (I.App sp ef es) = do
    vs <- mapM interpExpr es
    f <- interpExpr ef
    R.app sp f vs

interpExpr (I.Lam _sp fvs bs bdy) = do
    env <- R.captureClosure -- fvs
    let runF :: [R.Value] -> R.Runtime R.Value
        runF vs = do
            -- todo: check lists same length
            let bs' = zip bs vs
            R.withNewEnv env $ do
                letValues bs'
                interpStmts bdy
    pure $ R.Fun runF

interpExpr (I.Block _ stmts) = R.withScope $ interpStmts stmts

interpExpr e = error $ "interpExpr: " <> show e

------------------------------------------------------------------------------

letValues :: [(I.Binding, R.Value)] -> R.Runtime ()
letValues bs = flip mapM_ bs $ \(b,v) -> do
    mbs <- tryApplyBinding Nothing b v
    case mbs of
        Nothing -> error $ "couldn't bind " <> R.debugShowValue v <> " to " <> show b
        Just bs' -> mapM_ (uncurry R.addBinding) bs'

------------------------------------------------------------------------------

tryApplyBinding :: I.SP -> I.Binding -> R.Value -> R.Runtime (Maybe [(Text,R.Value)])

tryApplyBinding _ (I.NameBinding _sp nm) v = do
    pure $ Just [(nm,v)]

tryApplyBinding _sp b _v = error $ "tryApplyBinding " <> show b
