
{-

Interpreter is the code which takes desugared (todo) syntax and
executes it on the runtime.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Interpreter
    (createHandle
    ,runRuntime
    ,runScript
    ,getModuleValue
    ,getTestResults
    ,liftIO
    ,runTask
    ,debugShowValue
    ,extractValue
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

--import qualified Burdock.Syntax as S
import qualified Burdock.InterpreterSyntax as I
import Burdock.Runtime
    (Value(..)
    ,runRuntime
    ,Runtime
    ,RuntimeState
    ,getRuntimeState
    ,getTestResults
    ,liftIO
    ,setBootstrapRecTup
    ,BootstrapValues(..)
    ,nothingValue

    ,ModuleMetadata
    ,RuntimeImportSource(..)
    ,ModulePlugin(..)
    ,getModuleMetadata
    ,getModuleValue
    ,addModulePlugin
    
    --,ffimethodapp
    --,RuntimeState
    ,emptyRuntimeState
    --,addFFIType

    ,variantName
    ,variantValueFields
    
    ,getMember
    ,app

    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding
    ,captureClosure
    
    ,makeValue
    ,extractValue
    ,makeBurdockList
    ,makeRecord
    ,extractTuple
    ,DataDeclTypeTag(..)
    ,ValueTypeTag(..)

    ,debugShowValue
    
    ,runTask
    --,makeFunctionValue
    --,Type(..)
    --,Scientific

    ,getTempEnvStage
    ,setTempEnvStage
    )

--import Burdock.Pretty (prettyExpr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Burdock.Parse (parseScript)
import Burdock.Desugar
    (desugarScript
    ,desugarModule
    ,getSourceDependencies
    )

import qualified Burdock.Syntax as S

import Burdock.DefaultRuntime
    (initRuntime
    ,internals
    ,bootstrap
    )
    
import Control.Monad
    (forM_
    ,when
    ,zipWithM
    ,forM
    )

import Data.IORef
    (IORef
    ,newIORef
    ,writeIORef
    ,readIORef
    ,modifyIORef
    )

import Text.Show.Pretty (ppShow)
import Burdock.InterpreterPretty (prettyStmts)

import System.Directory
    (canonicalizePath
    ,getCurrentDirectory
    )
import System.FilePath
    (takeDirectory
    ,(</>)
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..))

------------------------------------------------------------------------------

debugPrintUserScript :: Bool
debugPrintUserScript = False

debugPrintBootstrap :: Bool
debugPrintBootstrap = False

debugPrintInternals :: Bool
debugPrintInternals = False

------------------------------------------------------------------------------

createHandle :: IO RuntimeState
createHandle = do
    st <- emptyRuntimeState
    runRuntime st $ do
        mp <- burdockModulePlugin
        addModulePlugin "file" mp

        tmpHackMetadata <- initRuntime

        setTempEnvStage tmpHackMetadata
        (bmm,_) <- runScript' debugPrintBootstrap (Just "_bootstrap") bootstrap

        let lkpf f = maybe (error $ "_bootstrap " <> f <> " not found") id <$> lookupBinding f

        setBootstrapRecTup =<< BootstrapValues
            <$> lkpf "_tuple_equals"
            <*> lkpf "_tuple_torepr"
            <*> lkpf "_record_equals"
            <*> lkpf "_record_torepr"
            <*> lkpf "empty"
            <*> lkpf "link"
            <*> lkpf "nothing"
        
        -- temp: add the stuff in initRuntime + bootstrap
        let tempCombineModuleMetadata (ModuleMetadata a) (ModuleMetadata b) = ModuleMetadata (a ++ b)
        setTempEnvStage $ tempCombineModuleMetadata tmpHackMetadata bmm
        (imm,_) <- runScript' debugPrintInternals (Just "_internals") internals

        -- temp: add the stuff in initRuntime + bootstrap + internals
        setTempEnvStage $ tempCombineModuleMetadata (tempCombineModuleMetadata tmpHackMetadata bmm) imm

            -- todo: tests in the internals?
        getRuntimeState

runScript :: Maybe T.Text -> L.Text -> Runtime Value
runScript fn src = snd <$> runScript' debugPrintUserScript fn src

------------------------------------------------------------------------------

runScript' :: Bool -> Maybe T.Text -> L.Text -> Runtime (ModuleMetadata, Value)
runScript' debugPrint fn' src = do
    -- filename either comes from bootstrap or from user
    fn <- T.pack <$> liftIO (canonicalizePath $ maybe "unnamed" T.unpack fn')
    let ast = either error id $ parseScript fn src
    ms <- recurseMetadata (Just fn) ast
    --liftIO $ putStrLn $ "desugar script"
    tmpHack <- getTempEnvStage
    let (mm,dast) = desugarScript tmpHack fn ms ast
    when False $ liftIO $ putStrLn $ T.pack $ ppShow dast
    when debugPrint $ liftIO $ L.putStrLn $ prettyStmts dast
    (mm,) <$> interpBurdock dast

loadAndDesugarModule :: Text -> Runtime (ModuleMetadata, [I.Stmt])
loadAndDesugarModule fn = do
    --liftIO $ putStrLn $ "load " <>  fn
    src <- liftIO $ L.readFile (T.unpack fn)
    let ast = either error id $ parseScript fn src
    ms <- recurseMetadata (Just fn) ast
    --liftIO $ putStrLn $ "desugar " <> fn
    tmpHack <- getTempEnvStage
    pure $ desugarModule tmpHack fn ms ast

recurseMetadata :: Maybe Text -> S.Script -> Runtime [(Text, ModuleMetadata)]
recurseMetadata ctx ast = do
    let deps = getSourceDependencies ast
    forM deps $ \case
        (nm,args@(fn:_)) -> (fn,) <$> getModuleMetadata ctx (RuntimeImportSource nm args)
        x -> error $ "desugar unsupported import source: " <> show x

------------------------------------------------------------------------------

data BurdockModulePluginCache
    = BurdockModulePluginCache
    {cacheCompiled :: IORef [(Text, (ModuleMetadata, [I.Stmt]))]
    ,cacheModuleValues :: IORef [(Text, Value)]
    }

burdockModulePlugin :: Runtime ModulePlugin
burdockModulePlugin = do
    let nx = liftIO $ newIORef []
    pc <- BurdockModulePluginCache <$> nx <*> nx
    let --resolveModulePath :: Maybe Text -> Text -> Runtime Text
        resolveModulePath ctx fn = do
            ctx' <- case ctx of
                Nothing -> liftIO $ getCurrentDirectory
                Just x -> pure $ takeDirectory $ T.unpack x
            liftIO (T.pack <$> (canonicalizePath (ctx' </> T.unpack fn)))
            
        compileCacheModule fn = do
            v <- liftIO $ readIORef (cacheCompiled pc)
            case lookup fn v of
                Just v' -> pure v'
                Nothing -> do
                    vs <- loadAndDesugarModule fn
                    liftIO $ modifyIORef (cacheCompiled pc) ((fn,vs):)
                    pure vs
        getMetadata' ctx ri = do
            fn <- resolveModulePath ctx $ getRiFile ri
            (m,_) <- compileCacheModule fn
            pure m
        getModuleValue' ctx ri = do
            fn <- resolveModulePath ctx $ getRiFile ri
            v <- liftIO $ readIORef (cacheModuleValues pc)
            case lookup fn v of
                Just v' -> pure v'
                Nothing -> do
                    (_,dast) <- compileCacheModule fn
                    v' <- interpBurdock dast
                    liftIO $ modifyIORef (cacheModuleValues pc) ((fn,v'):)
                    pure v'
    pure $ ModulePlugin getMetadata' getModuleValue'
  where
    getRiFile = \case 
        RuntimeImportSource _ [fn] -> fn
        RuntimeImportSource _ as -> error $ "bad args to burdock module source: " <> show as
        
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

interpExpr (I.VariantSel nm fs) = do
    vs <- mapM (\(n,e) -> (n,) <$> interpExpr e) fs
    -- todo: type tag
    pure $ VariantV (DataDeclTypeTag $ error "interpExpr VariantSel: fixmenm") nm vs

interpExpr (I.RunTask catchAsync tsk) = do
    x <- runTask catchAsync $ interpExpr tsk
    case x of
        -- a left left of a string is an show'n arbitrary haskell exception
        Left (Left e, st) -> do
            left <- interpExpr (I.Iden "left")
            st' <- makeBurdockList $ map (makeValue (ValueTypeTag "string") . maybe "nothing" id) st
            ret <- makeRecord [("exception", makeValue (ValueTypeTag "string") e)
                              ,("callstack", st')]
            app Nothing left [ret]
        -- a left right v is a burdock value that has been raised from burdock
        -- or ffi code
        Left (Right v, st) -> do
            left <- interpExpr (I.Iden "left")
            st' <- makeBurdockList $ map (makeValue (ValueTypeTag "string") . maybe "nothing" id) st
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

interpExpr (I.Num n) =
    {- todo: you either have to look up "number" in the runtime environment
       or keep a token from when the number type was created, this is so type
       names are namespaced and scoped, e.g. if you have two modules which have
       a type with the same name as each other
    -}
    pure $ makeValue (ValueTypeTag "number") n

interpExpr (I.IString t) =
    pure $ makeValue (ValueTypeTag "string") t

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
tryApplyBinding (I.VariantBinding "true" []) v | Just True <- extractValue v = pure $ Just []
tryApplyBinding (I.VariantBinding "true" []) _ = pure Nothing
tryApplyBinding (I.VariantBinding "false" []) v | Just False <- extractValue v = pure $ Just []
tryApplyBinding (I.VariantBinding "false" []) _ = pure Nothing

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

tryApplyBinding (I.VariantBinding vnm flds) v = do
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
