
{-

plugin to load python modules and access them from burdock

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.PythonModulePlugin
    (addPythonPackage
    ) where

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,modifyIORef
    ,writeIORef
    )

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import qualified Burdock.Runtime as R
import qualified PyWrap as P
import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    ,BindingMeta(..)
    )

import Burdock.Runtime (liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Data.Maybe (catMaybes)

import Control.Monad (unless)


addPythonPackage :: (Text -> (ModuleMetadata, R.Value) -> R.Runtime ()) -> R.Runtime ()
addPythonPackage bootstrapLoadModule = do

        -- create pyobject ffi type
    ffiTypeInfo <- R.getFFITypeInfoTypeInfo
    pyTy <- makePyObjectType
    pyObjTag <- R.makeFFIValue ffiTypeInfo pyTy
    --isPythonInitialized <- liftIO $ newIORef False
    liftIO P.initialize
    --(ModuleMetadata ms,pv) <- loadPythonModule pyTy "string"
    --liftIO $ putStrLn $ T.unlines $ flip map ms $ \(nm,_,_,_) -> nm
    --printable <- R.getMember Nothing pv "printable"
    --printable' <- either (error . show) id <$> R.extractFFIValue pyTy printable
    --printable'' <- either (error . show) id <$> liftIO (P.fromPyObject printable')
    --liftIO $ putStrLn $ "printable: " <> printable''
    --liftIO $ putStrLn "what"
    let getMeta (ModuleID _ [nm]) =
            fst <$> loadPythonModule pyTy nm
        getVal (ModuleID _ [nm]) =
            snd <$> loadPythonModule pyTy nm

    let pythonModulePlugin = R.ModulePlugin (\_ i -> pure i) getMeta getVal
    let pythonModulePluginSupportModule = do
            let bs' = [(BEType 0, ("pyobject", pyObjTag))]
                      ++ map (BEIdentifier,)
                      [("py-block", R.Fun $ pyBlock (R.makeFFIValue pyTy))
                      ,("py-block-with-binds", R.Fun $ pyBlockWithBinds (R.makeFFIValue pyTy) (R.extractFFIValue pyTy))
                      ]
            let (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
                    ((n, Nothing, t, ("<python>",Nothing)), (n,v))

            pure (ModuleMetadata ms, (R.Module bs))
    
    R.addModulePlugin "python" $ pythonModulePlugin
    bootstrapLoadModule "python" =<< pythonModulePluginSupportModule


loadPythonModule :: R.FFITypeInfo -> Text -> R.Runtime (ModuleMetadata,R.Value)
loadPythonModule pyObjType name = do
    pym <- fuckOffErrors <$> liftIO (P.script $ "import " <> name <> "\n" <> name)
    members' <- fuckOffErrors <$> 
        (liftIO $ P.script $ "import inspect\nimport " <> name <> "\n"
            <> "[i[0] for i in inspect.getmembers(" <> name <> ")]")
    members <- extractStringList members'
    pymv <- R.makeFFIValue pyObjType pym
    let ms = flip map members $ \n -> (n,Nothing, BEIdentifier, ("<python>",Nothing))
    pure (R.ModuleMetadata ms, pymv)
  where
    extractStringList :: P.PyObject -> R.Runtime [Text]
    extractStringList l = do
        is <- fuckOffErrors <$> liftIO (P.pyListToList l)
        mapM extractText is
    extractText i = fuckOffErrors <$> liftIO (P.fromPyObject i)

fuckOffErrors = either (error . show) id

-- todo: a pyobject should be able to support:
-- arbitrary members
-- _app
-- torepr, compare
-- _assign
-- not sure what else, 
makePyObjectType :: R.Runtime R.FFITypeInfo
makePyObjectType = do
    R.makeFFIType ["python", "pyobject"]
        [
         R.ToRepr $ \(v :: P.PyObject) -> do
                t <- fuckOffErrors <$> liftIO (P.appText "str" [v])
                u <- fuckOffErrors <$> liftIO (P.fromPyObject t)
                pure u
         -- todo: fix these
        ,R.Equals $ \_ _  -> pure False
        ,R.CatchAll $ \mkV exV nm pyObj -> do
                v <- fuckOffErrors <$> P.getAttr pyObj nm
                autoConvPyObject mkV v
                --mkV v
        ]

autoConvBurdockValue :: (R.Value -> R.Runtime (Either Text P.PyObject)) -> R.Value -> R.Runtime P.PyObject
autoConvBurdockValue exV v = do
    case v of
        R.Boolean True -> fuckOffErrors <$> liftIO (P.eval "True")
        R.Boolean False -> fuckOffErrors <$> liftIO (P.eval "False")
        R.BString str -> liftIO (P.toPyObject str)
        R.BNothing -> fuckOffErrors <$> liftIO (P.eval "None")
        {-
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
                either (error . show) id <$>  liftIO (P.makePyTuple vs)-}
        _ -> either (error . show) id <$> exV v

autoConvPyObject :: (P.PyObject -> R.Runtime R.Value) -> P.PyObject -> R.Runtime R.Value
autoConvPyObject mkV o = do
    t <- liftIO $ typName o
    case t of
        "str" -> do
            x <- fuckOffErrors <$> liftIO (P.fromPyObject o)
            pure $ R.BString x
        "bool" -> do
            x  <- fuckOffErrors <$> liftIO (P.fromPyObject o)
            pure $ R.Boolean x
        "NoneType" -> pure R.BNothing
        {-
        "int" -> do
            (x :: Int)  <- either (error . show) id <$> liftIO (P.fromPyObject o)
            pure $ NumV $ fromIntegral x
        "float" -> do
            (x :: Double)  <- either (error . show) id <$> liftIO (P.fromPyObject o)
            pure $ NumV $ realToFrac x
        "tuple" -> do
            os <- either (error . show) id <$> liftIO (P.pyTupleToList o)
            ls <- mapM autoConvPyObject os
            pure $ makeBTuple ls
        "list" -> do
            os <- either (error . show) id <$> liftIO (P.pyListToList o)
            ls <- mapM autoConvPyObject os
            pure $ makeBList ls-}
        _ -> mkV o
  where
    typName :: P.PyObject -> IO T.Text
    typName x = do
        y <- fuckOffErrors <$> P.appText "type" [x]
        z <- fuckOffErrors <$> P.getAttr y "__name__"
        fuckOffErrors <$> P.fromPyObject z



------------------------------------------------------------------------------

pyBlock :: (P.PyObject -> R.Runtime R.Value) -> [R.Value] -> R.Runtime R.Value
pyBlock mkV [R.BString scr] = do
    r <- fuckOffErrors <$> P.script scr
    autoConvPyObject mkV r

pyBlockWithBinds :: (P.PyObject -> R.Runtime R.Value) -> (R.Value -> R.Runtime (Either Text P.PyObject)) -> [R.Value] -> R.Runtime R.Value
pyBlockWithBinds mkV _ [R.BString scr] = do
    r <- fuckOffErrors <$> P.script scr
    autoConvPyObject mkV r
pyBlockWithBinds mkV exV [R.BString scr, R.Variant _ fs] = do
    fs' <- flip mapM fs $ \(n,v) -> case v of
            R.MethodV {} -> pure Nothing
            _ -> Just . (n,) <$> autoConvBurdockValue exV v
    r <- fuckOffErrors <$> P.scriptWithBinds (catMaybes fs') scr
    autoConvPyObject mkV r
