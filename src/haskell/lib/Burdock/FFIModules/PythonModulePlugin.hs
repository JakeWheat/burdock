
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

import Burdock.Scientific (Scientific, extractInt)

data PythonFFIClosure
    = PythonFFIClosure
    {pfcFfiTypeInfo :: R.FFITypeInfo
    ,pfcNumFfiTypeInfo :: R.FFITypeInfo
    ,pfcVariantLink :: R.VariantTag
    ,pfcVariantEmpty :: R.VariantTag
    ,pfcLinkCtor :: R.Value
    ,pfcEmptyCtor :: R.Value
    ,pfcVariantTuple :: R.VariantTag
    ,pfcVariantRecord :: R.VariantTag
    }


addPythonPackage :: (Text -> (ModuleMetadata, R.Value) -> R.Runtime ()) -> R.Runtime ()
addPythonPackage bootstrapLoadModule = do

    -- create pyobject ffi type
    ffiTypeInfo <- R.getFFITypeInfoTypeInfo
    pythonFFIClosure <- do
        -- load _internals
        -- todo: decide which modules ffi code should import to get access
        -- to these things, probably globals or something like that
        internals <- R.getModuleValue (ModuleID "haskell" ["_bootstrap"])
        -- todo: add an ffi helper in Runtime which gets a type by it's name
        -- and returns a typeinfo
        -- so the _type- business will be completely hidden from user code
        bnti <- R.getMember Nothing internals "_type-number"
        -- extract it to a type info
        nti <- either (error . show) id <$> R.extractFFIValue ffiTypeInfo bnti
        pure $ PythonFFIClosure
            {pfcFfiTypeInfo = ffiTypeInfo
            ,pfcNumFfiTypeInfo = nti
            ,pfcVariantLink = undefined
            ,pfcVariantEmpty = undefined
            ,pfcLinkCtor = undefined
            ,pfcEmptyCtor = undefined
            ,pfcVariantTuple = undefined
            ,pfcVariantRecord = undefined
            }
    pyTy <- makePyObjectType pythonFFIClosure
    pyObjTag <- R.makeFFIValue ffiTypeInfo pyTy
    
    liftIO P.initialize
    -- todo: call loadPythonModule once per module
    -- but I don't think it's likely to be a big bottleneck
    let getMeta (ModuleID _ [nm]) =
            fst <$> loadPythonModule pyTy nm
        getVal (ModuleID _ [nm]) =
            snd <$> loadPythonModule pyTy nm
    let pythonModulePlugin = R.ModulePlugin (\_ i -> pure i) getMeta getVal
    -- an ffi module which implements helper functions used to work
    -- with python imports and values from burdock code
    let pythonModulePluginSupportModule = do
            let bs' = [(BEType 0, ("pyobject", pyObjTag))]
                      ++ map (BEIdentifier,)
                      [("py-block", R.Fun $ pyBlock pythonFFIClosure pyTy)
                      ,("py-block-with-binds", R.Fun $ pyBlockWithBinds pythonFFIClosure pyTy)
                      ]
            let (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
                    ((n, Nothing, t, ("<python>",Nothing)), (n,v))

            pure (ModuleMetadata ms, (R.Module bs))
    R.addModulePlugin "python" $ pythonModulePlugin
    bootstrapLoadModule "python" =<< pythonModulePluginSupportModule

-- load a specific python module and bind it to a burdock name
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
makePyObjectType :: PythonFFIClosure -> R.Runtime R.FFITypeInfo
makePyObjectType pfc = do
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
                autoConvPyObject pfc mkV v
        ]

-- "auto" conversion between PyObjects and burdock Values
-- if it's one of the blessed types, convert between native python and burdock
-- otherwise wrap in pyobject ffi value
-- todo: opaque values being passed from burdock to python and back again
-- come back to this when implementing callbacks passed from burdock to python
autoConvBurdockValue :: PythonFFIClosure ->  (R.Value -> R.Runtime (Either Text P.PyObject)) -> R.Value -> R.Runtime P.PyObject
autoConvBurdockValue pfc exV v = do
    case v of
        R.Boolean True -> fuckOffErrors <$> liftIO (P.eval "True")
        R.Boolean False -> fuckOffErrors <$> liftIO (P.eval "False")
        R.BString str -> liftIO (P.toPyObject str)
        R.BNothing -> fuckOffErrors <$> liftIO (P.eval "None")
        R.FFIValue {} -> do
            v' <- R.extractFFIValue (pfcNumFfiTypeInfo pfc) v 
            case v' of
                Right (n :: Scientific) ->
                    case extractInt n of
                        Nothing -> either (error . show) id <$> liftIO (P.eval $ show n)
                        Just n' -> either (error . show) id <$> liftIO (P.eval $ show n')
        {-
        _ | Just ls <- fromBList v -> do
                vs <- mapM autoConvBurdockValue ls
                either (error . show) id <$> liftIO (P.makePyList vs)
        _ | Just ls <- fromBTuple v -> do
                vs <- mapM autoConvBurdockValue ls
                either (error . show) id <$>  liftIO (P.makePyTuple vs)-}
        _ -> either (error . show) id <$> exV v

autoConvPyObject :: PythonFFIClosure -> (P.PyObject -> R.Runtime R.Value) -> P.PyObject -> R.Runtime R.Value
autoConvPyObject pfc mkV o = do
    t <- liftIO $ typName o
    case t of
        "str" -> do
            x <- fuckOffErrors <$> liftIO (P.fromPyObject o)
            pure $ R.BString x
        "bool" -> do
            x  <- fuckOffErrors <$> liftIO (P.fromPyObject o)
            pure $ R.Boolean x
        "NoneType" -> pure R.BNothing
        "int" -> do
            (x :: Int) <- either (error . show) id <$> liftIO (P.fromPyObject o)
            R.makeFFIValue (pfcNumFfiTypeInfo pfc) (fromIntegral x :: Scientific)
        "float" -> do
            (x :: Double)  <- either (error . show) id <$> liftIO (P.fromPyObject o)
            R.makeFFIValue (pfcNumFfiTypeInfo pfc) (realToFrac x :: Scientific)
        {-"tuple" -> do
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

pyBlock :: PythonFFIClosure -> R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
pyBlock pfc pyTi [R.BString scr] = do
    r <- fuckOffErrors <$> P.script scr
    autoConvPyObject pfc (R.makeFFIValue pyTi) r

pyBlockWithBinds :: PythonFFIClosure -> R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
pyBlockWithBinds pfc pyTi [R.BString scr] = do
    r <- fuckOffErrors <$> P.script scr
    autoConvPyObject pfc (R.makeFFIValue pyTi) r
pyBlockWithBinds pfc pyTi [R.BString scr, R.Variant _ fs] = do
    fs' <- flip mapM fs $ \(n,v) -> case v of
            R.MethodV {} -> pure Nothing
            _ -> Just . (n,) <$> autoConvBurdockValue pfc (R.extractFFIValue pyTi) v
    r <- fuckOffErrors <$> P.scriptWithBinds (catMaybes fs') scr
    autoConvPyObject pfc (R.makeFFIValue pyTi) r
