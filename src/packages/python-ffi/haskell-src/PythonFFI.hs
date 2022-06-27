
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module PythonFFI (pythonFFIPackage) where

{-import Data.Dynamic
    (Dynamic
    ,fromDynamic
    --,Typeable
    )-}

import Burdock.Interpreter
    (Interpreter
    ,Value(..)
    ,FFIPackage(..)
    ,FFITypeInfo(..)
    ,makeFFIValue
    ,unmakeFFIValue
    ,nothing
    ,liftIO
    ,makeFFIMemberFunction
    ,fromBList
    ,makeBList
    ,makeBTuple
    ,fromBTuple
    ,app
    ,FMD(..)
    ,ffiMemberDispatcher
    ,ffiSingleArgMethod
    ,ffiNoArgMethod
    ,ffiNoArgValue
    )

import Burdock.Scientific (extractInt)

import qualified PyWrap as P
import qualified Data.Text as T
import Control.Monad (void, when)

import Control.Concurrent.STM.TVar
    (TVar
    ,readTVar
    ,newTVarIO
    ,writeTVar
    )

import Control.Concurrent.STM (atomically, retry)
import System.IO.Unsafe (unsafePerformIO)

pythonFFIPackage :: FFIPackage
pythonFFIPackage = FFIPackage
    {ffiPackageTypes =
     [("pyobject"
      , FFITypeInfo (makeFFIMemberFunction "_member-pyobject"))]
    ,ffiPackageFunctions = 
     [("py-block_", pyBlock_)
     ,("py-block", pyBlock)
     ,("py-block-with-binds", pyBlockWithBinds)
     ,("py-app", pyAppS)
     ,("safe-python-initialize", safePythonInitialize)
     ,("_pyobject-equals", pyObjectEquals)
     ,("_pyobject-lessthan", pyObjectLessThan)
     ,("_pyobject-torepr", pyObjectToRepr)
     ,("_pyobject-app", pyObjectApp)
     ,("_pyobject-generic-member", pyObjectGenericMember)
     ,("py-for", pyFor)
     ,("_member-pyobject"
      ,ffiMemberDispatcher $ FMD
       {fmdLkp = [("_equals", ffiSingleArgMethod "_pyobject-equals")
                 ,("_lessthan", ffiSingleArgMethod "_pyobject-lessthan")
                 ,("_torepr", ffiNoArgMethod "_pyobject-torepr")
                 ,("_app", ffiSingleArgMethod "_pyobject-app")]
       ,fmdFallback = Just $ ffiNoArgValue "_pyobject-generic-member"})
     ]}


-- hopefully this works
-- guard it with a helper function
-- since initializing happens after the atomic block
-- to update the tvar
globalStartIsPythonInitialized :: TVar Bool
globalStartIsPythonInitialized = unsafePerformIO $ newTVarIO False
{-# NOINLINE globalStartIsPythonInitialized #-}

globalEndIsPythonInitialized :: TVar Bool
globalEndIsPythonInitialized = unsafePerformIO $ newTVarIO False
{-# NOINLINE globalEndIsPythonInitialized #-}

safePythonInitialize :: [Value] -> Interpreter Value
safePythonInitialize [] = do
    -- I'm sure this can be done much more simply with a TMVar
    i <- liftIO $ atomically $ do
        st <- readTVar globalStartIsPythonInitialized
        if st
            then do
                e <- readTVar globalEndIsPythonInitialized
                if e
                    then pure True
                    else retry
            else do
                writeTVar globalStartIsPythonInitialized True
                pure False
    when (not i) $ do
        liftIO P.initialize
        liftIO $ atomically $ writeTVar globalEndIsPythonInitialized True
    pure nothing
safePythonInitialize _ = error "bad args to safePythonInitialize"

pyBlock_ :: [Value] -> Interpreter Value
pyBlock_ [TextV src] = do
    liftIO $ void $ P.script $ T.pack src
    pure nothing
pyBlock_ _ = error "bad args to pyBlock_"

pyBlock :: [Value] -> Interpreter Value
pyBlock [TextV src] = do
    o <- liftIO $ P.script $ T.pack src
    autoConvPyObject $ either (error . show) id o
pyBlock _ = error "bad args to pyBlock"

pyBlockWithBinds :: [Value] -> Interpreter Value
pyBlockWithBinds [bs, TextV src]
    | Just bs' <- fromBList bs = do
    bs'' <- maybe (error "bad args to py-block-with-binds") id . sequence <$> mapM getBind bs'
    o <- liftIO $ P.scriptWithBinds bs'' $ T.pack src
    autoConvPyObject $ either (error . show) id o
  where
    getBind o | Just [TextV nm, v] <- fromBTuple o
       = Just <$> ((T.pack nm,) <$> autoConvBurdockValue v)
    getBind _ = pure Nothing
pyBlockWithBinds _ = error "bad args to py-block-with-binds"

autoConvBurdockValue :: Value -> Interpreter P.PyObject
autoConvBurdockValue v = do
    case v of
        BoolV True ->  either (error . show) id <$> liftIO (P.eval "True")
        BoolV False -> either (error . show) id <$> liftIO (P.eval "False")
        TextV str -> liftIO (P.toPyObject (T.pack str))
        NumV n -> do
            let x = extractInt n
            case x of
                Nothing -> either (error . show) id <$> liftIO (P.eval $ T.pack $ show n)
                Just n' -> either (error . show) id <$> liftIO (P.eval $ T.pack $ show n')
        VariantV _ "nothing" [] -> either (error . show) id <$> liftIO (P.eval "None")
        _ | Just ls <- fromBList v -> do
                vs <- mapM autoConvBurdockValue ls
                either (error . show) id <$> liftIO (P.makePyList vs)
        _ | Just ls <- fromBTuple v -> do
                vs <- mapM autoConvBurdockValue ls
                either (error . show) id <$>  liftIO (P.makePyTuple vs)
        _ -> do
            v' <- unmakeFFIValue "pyobject" v
            case v' of
                Just v'' -> pure v''
                Nothing -> error $ "python: not a supported value: " ++ show v

autoConvPyObject :: P.PyObject -> Interpreter Value
autoConvPyObject o = do
    t <- liftIO $ typName o
    case t of
        "str" -> do
            x <- either (error . show) id <$> liftIO (P.fromPyObject o)
            pure $ TextV $ T.unpack x
        "bool" -> do
            x  <- either (error . show) id <$> liftIO (P.fromPyObject o)
            pure $ BoolV x
        "int" -> do
            (x :: Int)  <- either (error . show) id <$> liftIO (P.fromPyObject o)
            pure $ NumV $ fromIntegral x
        "float" -> do
            (x :: Double)  <- either (error . show) id <$> liftIO (P.fromPyObject o)
            pure $ NumV $ realToFrac x
        "NoneType" -> pure nothing
        "tuple" -> do
            os <- either (error . show) id <$> liftIO (P.pyTupleToList o)
            ls <- mapM autoConvPyObject os
            pure $ makeBTuple ls
        "list" -> do
            os <- either (error . show) id <$> liftIO (P.pyListToList o)
            ls <- mapM autoConvPyObject os
            pure $ makeBList ls
        _ -> makeFFIValue "pyobject" o
  where
    typName :: P.PyObject -> IO T.Text
    typName x = do
        y <- either (error . show) id <$> P.appText "type" [x]
        z <- P.getAttr y "__name__"
        either (error . show) id <$> P.fromPyObject (either (error . show) id z)
            
pyAppS :: [Value] -> Interpreter Value
pyAppS [TextV pyfname, argsList] = do
    let as = maybe (error "bad args to py-app") id $ fromBList argsList
    aso <- mapM autoConvBurdockValue as
    o <- liftIO $ either (error . show) id <$> P.appText (T.pack pyfname) aso
    autoConvPyObject o -- makeFFIValue "pyobject" o
pyAppS _ = error "bad args to py-app"
    
pyObjectEquals :: [Value] -> Interpreter Value
pyObjectEquals [a,b] = do
    a' <- maybe (error "bad args to pyObjectEquals") id <$> unmakeFFIValue "pyobject" a
    b' <- maybe (error "bad args to pyObjectEquals") id <$> unmakeFFIValue "pyobject" b
    o <- liftIO $ P.scriptWithBinds [("a", a'),("b", b')] "a == b"
    autoConvPyObject $ either (error . show) id o
pyObjectEquals _ = error "bad args to pyObjectEquals"

pyObjectLessThan :: [Value] -> Interpreter Value
pyObjectLessThan [a,b] = do
    a' <- maybe (error "bad args to pyObjectLessThan") id <$> unmakeFFIValue "pyobject" a
    b' <- maybe (error "bad args to pyObjectLessThan") id <$> unmakeFFIValue "pyobject" b
    o <- liftIO $ P.scriptWithBinds [("a", a'),("b", b')] "a < b"
    autoConvPyObject $ either (error . show) id o
pyObjectLessThan _ = error "bad args to pyObjectLessThan"

pyObjectToRepr :: [Value] -> Interpreter Value
pyObjectToRepr [v] = do
    x :: P.PyObject <- maybe (error "bad args to pyObjectToRepr") id <$> unmakeFFIValue "pyobject" v
    y <- liftIO $ either (error . show) id <$> P.appText "str" [x]
    autoConvPyObject y
pyObjectToRepr _ = error "bad args to pyObjectToRepr"

pyObjectApp :: [Value] -> Interpreter Value
pyObjectApp [f,args] = do
    f' <- maybe (error "bad args to pyObjectApp") id <$> unmakeFFIValue "pyobject" f
    let args' = maybe (error "bad args to pyObjectApp") id $ fromBList args
    aso <- mapM autoConvBurdockValue args'
    o <- liftIO $ P.app f' aso
    autoConvPyObject $ either (error . show) id o
pyObjectApp _ = error "bad args to pyObjectApp"

pyObjectGenericMember :: [Value] -> Interpreter Value
pyObjectGenericMember [TextV fld, v] = do
    v' <- maybe (error "bad args to pyObjectGenericMember") id <$> unmakeFFIValue "pyobject" v
    m <- liftIO $ P.getAttr v' (T.pack fld)
    autoConvPyObject $ either (error . show) id m
pyObjectGenericMember _ = error "bad args to pyObjectGenericMember"

pyFor :: [Value] -> Interpreter Value
pyFor [cb, ml] | Just l <- fromBList ml = do
    las <- mapM autoConvBurdockValue l
    pyiter' <- liftIO $ either (error . show) id <$> P.makePyList las
    let cb' a = do
            a' <- autoConvPyObject a
            app cb [a']
    as <- P.for pyiter' cb'
    pure $ makeBList $ either (error . show) id as

pyFor [cb, pyiter] = do
    pyiter' <- maybe (error "bad args to pyFor") id <$> unmakeFFIValue "pyobject" pyiter
    let cb' a = do
            a' <- autoConvPyObject a
            app cb [a']
    as <- P.for pyiter' cb'
    pure $ makeBList $ either (error . show) id as
pyFor _ = error "bad args to pyFor"
