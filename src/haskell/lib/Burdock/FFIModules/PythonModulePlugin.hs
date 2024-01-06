
{-

plugin to load python modules and access them from burdock

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
module Burdock.FFIModules.PythonModulePlugin
    (addPythonPackage
    ) where

{-import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,modifyIORef
    ,writeIORef
    )-}

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
--import Data.Text.IO (putStrLn)
import Data.Maybe (catMaybes)

--import Control.Monad (unless)

import Burdock.Scientific (Scientific, extractInt)

data PythonFFIClosure
    = PythonFFIClosure
    {pfcFfiTypeInfo :: R.FFITypeInfo
    ,pfcNumFfiTypeInfo :: R.FFITypeInfo
    ,pfcVariantEmpty :: R.VariantTag
    ,pfcVariantLink :: R.VariantTag
    ,pfcEmptyCtor :: R.Value
    ,pfcLinkCtor :: R.Value
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
        interpreterMod <- R.getModuleValue (ModuleID "haskell" ["_interpreter"])
        -- todo: add an ffi helper in Runtime which gets a type by it's name
        -- and returns a typeinfo
        -- so the _type- business will be completely hidden from user code
        bnti <- R.getMember Nothing interpreterMod "_type-number"
        -- extract it to a type info
        nti <- leftError <$> R.extractFFIValue ffiTypeInfo bnti

        link <- R.getMember Nothing interpreterMod "link"
        empty <- R.getMember Nothing interpreterMod "empty"

        vvti <- R.getMember Nothing interpreterMod "_type-variant-tag"
        Right vvti' <- R.extractFFIValue ffiTypeInfo vvti
    
        let getVariantTag nm = do
                tg <- R.getMember Nothing interpreterMod ("_variant-" <> nm)
                R.extractFFIValue vvti' tg

        emptyTag <- leftError <$> getVariantTag "empty"
        linkTag <- leftError <$> getVariantTag "link"

        tupleTag <- leftError <$> getVariantTag "tuple"
        recordTag <- leftError <$> getVariantTag "record"
        
        pure $ PythonFFIClosure
            {pfcFfiTypeInfo = ffiTypeInfo
            ,pfcNumFfiTypeInfo = nti
            ,pfcVariantEmpty = emptyTag
            ,pfcVariantLink = linkTag
            ,pfcEmptyCtor = empty
            ,pfcLinkCtor = link
            ,pfcVariantTuple = tupleTag
            ,pfcVariantRecord = recordTag
            }
    pyTy <- makePyObjectType pythonFFIClosure
    pyObjTag <- R.makeFFIValue ffiTypeInfo pyTy
    
    liftIO P.initialize
    -- todo: call loadPythonModule once per module
    -- but I don't think it's likely to be a big bottleneck
    let getMeta (ModuleID _ [nm]) =
            fst <$> loadPythonModule pyTy nm
        getMeta x = error $ "addPythonPackage getMeta" <> show x
        getVal (ModuleID _ [nm]) =
            snd <$> loadPythonModule pyTy nm
        getVal x = error $ "addPythonPackage getVal" <> show x
    let pythonModulePlugin = R.ModulePlugin (\_ i -> pure i) getMeta getVal
    -- an ffi module which implements helper functions used to work
    -- with python imports and values from burdock code
    let pythonModulePluginSupportModule = do
            let bs' = [(BEType 0, ("pyobject", pyObjTag))]
                      ++ map (BEIdentifier,)
                      [("py-block", R.Fun $ pyBlock pythonFFIClosure pyTy)
                      ,("py-block-with-binds", R.Fun $ pyBlockWithBinds pythonFFIClosure pyTy)
                      ,("py-for", R.Fun $ pyFor pythonFFIClosure (R.makeFFIValue pyTy) (R.extractFFIValue pyTy))
                      ]
            let (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
                    ((n, Nothing, t, ("<python>",Nothing)), (n,v))

            pure (ModuleMetadata ms, (R.Module bs))
    R.addModulePlugin "python" $ pythonModulePlugin
    bootstrapLoadModule "python" =<< pythonModulePluginSupportModule

-- load a specific python module and bind it to a burdock name
loadPythonModule :: R.FFITypeInfo -> Text -> R.Runtime (ModuleMetadata,R.Value)
loadPythonModule pyObjType name = do
    pym <- leftError <$> liftIO (P.script $ "import " <> name <> "\n" <> name)
    members' <- leftError <$> 
        (liftIO $ P.script $ "import inspect\nimport " <> name <> "\n"
            <> "[i[0] for i in inspect.getmembers(" <> name <> ")]")
    members <- extractStringList members'
    pymv <- R.makeFFIValue pyObjType pym
    let ms = flip map members $ \n -> (n,Nothing, BEIdentifier, ("<python>",Nothing))
    pure (R.ModuleMetadata ms, pymv)
  where
    extractStringList :: P.PyObject -> R.Runtime [Text]
    extractStringList l = do
        is <- leftError <$> liftIO (P.pyListToList l)
        mapM extractText is
    extractText i = leftError <$> liftIO (P.fromPyObject i)

leftError :: Show e => Either e a -> a
leftError = either (error . show) id

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
                t <- leftError <$> liftIO (P.appText "str" [v])
                u <- leftError <$> liftIO (P.fromPyObject t)
                pure u
        ,R.App $ \v as -> do
             let exV = error "please add exV to the quick ffi app method"
                 mkV = error "please add mkV to the quick ffi app method"
             as' <- mapM (autoConvBurdockValue pfc exV) as
             res <- P.app v as'
             case res of
                 Left e -> error (show e)
                 Right res' -> autoConvPyObject pfc mkV res'
         -- todo: fix these
        ,R.Equals $ \_ _  -> pure False
        ,R.CatchAll $ \mkV _exV nm pyObj -> do
                v <- leftError <$> P.getAttr pyObj nm
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
        R.Boolean True -> leftError <$> liftIO (P.eval "True")
        R.Boolean False -> leftError <$> liftIO (P.eval "False")
        R.BString str -> liftIO (P.toPyObject str)
        R.BNothing -> leftError <$> liftIO (P.eval "None")
        R.FFIValue {} -> do
            v' <- R.extractFFIValue (pfcNumFfiTypeInfo pfc) v 
            case v' of
                Right (n :: Scientific) ->
                    case extractInt n of
                        Nothing -> leftError <$> liftIO (P.eval $ show n)
                        Just n' -> leftError <$> liftIO (P.eval $ show n')
                _ -> do
                    q' <- exV v
                    case q' of
                        Right x -> pure x
                        Left {} -> error $ "passing this value python not currently supported: " <> R.debugShowValue v
        {-
        _ | Just ls <- fromBList v -> do
                vs <- mapM autoConvBurdockValue ls
                leftError <$> liftIO (P.makePyList vs)
        _ | Just ls <- fromBTuple v -> do
                vs <- mapM autoConvBurdockValue ls
                leftError <$>  liftIO (P.makePyTuple vs)-}
        _ -> error $ "passing this value python not currently supported: " <> R.debugShowValue v

autoConvPyObject :: PythonFFIClosure -> (P.PyObject -> R.Runtime R.Value) -> P.PyObject -> R.Runtime R.Value
autoConvPyObject pfc mkV o = do
    t <- liftIO $ typName o
    case t of
        "str" -> do
            x <- leftError <$> liftIO (P.fromPyObject o)
            pure $ R.BString x
        "bool" -> do
            x  <- leftError <$> liftIO (P.fromPyObject o)
            pure $ R.Boolean x
        "NoneType" -> pure R.BNothing
        "int" -> do
            (x :: Int) <- leftError <$> liftIO (P.fromPyObject o)
            R.makeFFIValue (pfcNumFfiTypeInfo pfc) (fromIntegral x :: Scientific)
        "float" -> do
            (x :: Double)  <- leftError <$> liftIO (P.fromPyObject o)
            R.makeFFIValue (pfcNumFfiTypeInfo pfc) (realToFrac x :: Scientific)
        {-"tuple" -> do
            os <- leftError <$> liftIO (P.pyTupleToList o)
            ls <- mapM autoConvPyObject os
            pure $ makeBTuple ls
        "list" -> do
            os <- leftError <$> liftIO (P.pyListToList o)
            ls <- mapM autoConvPyObject os
            pure $ makeBList ls-}
        _ -> mkV o
  where
    typName :: P.PyObject -> IO T.Text
    typName x = do
        y <- leftError <$> P.appText "type" [x]
        z <- leftError <$> P.getAttr y "__name__"
        leftError <$> P.fromPyObject z


------------------------------------------------------------------------------

-- not sure about the right way to do this
-- want to think about an iterable like concept for burdock first
-- I think it's essentially adding a lazily evaluated list to an eager
-- language with a little bit of 'good enough' design

pyFor :: PythonFFIClosure
      -> (P.PyObject -> R.Runtime R.Value)
      -> (R.Value -> R.Runtime (Either Text P.PyObject))
      -> [R.Value] -> R.Runtime R.Value
pyFor pfc mkV exV [cb', as] = do
    let cb a = do
            a' <- autoConvPyObject pfc mkV a
            R.app Nothing cb' [a']
    pyiter <- do
        mas <- extractBurdockList (pfcVariantLink pfc) (pfcVariantEmpty pfc) as
        case mas of
            -- allow passing a burdock list as a python iterator - convert it to a python list
            Just as' -> do
                as'' <- mapM (autoConvBurdockValue pfc exV) as'
                liftIO (leftError <$> P.makePyList as'')
            -- if not a list, assume is a pyobject of a python iterator
            Nothing -> leftError <$> exV as
    (ras :: [R.Value]) <- leftError <$> P.for pyiter cb
    makeBurdockList (pfcLinkCtor pfc) (pfcEmptyCtor pfc) ras
pyFor _ _ _ _ = error "bad args to pyFor"

------------------------------------------------------------------------------

pyBlock :: PythonFFIClosure -> R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
pyBlock pfc pyTi [R.BString scr] = do
    r <- leftError <$> P.script scr
    autoConvPyObject pfc (R.makeFFIValue pyTi) r

pyBlock _ _ _ = error "bad args to py-block"

pyBlockWithBinds :: PythonFFIClosure -> R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
pyBlockWithBinds pfc pyTi [R.BString scr] = do
    r <- leftError <$> P.script scr
    autoConvPyObject pfc (R.makeFFIValue pyTi) r
pyBlockWithBinds pfc pyTi [R.BString scr, R.Variant _ fs] = do
    fs' <- flip mapM fs $ \(n,v) -> case v of
            R.MethodV {} -> pure Nothing
            _ -> Just . (n,) <$> autoConvBurdockValue pfc (R.extractFFIValue pyTi) v
    r <- leftError <$> P.scriptWithBinds (catMaybes fs') scr
    autoConvPyObject pfc (R.makeFFIValue pyTi) r
pyBlockWithBinds _ _ _ = error "bad args to py-block-with-binds"

------------------------------------------------------------------------------

makeBurdockList :: R.Value -> R.Value -> ([R.Value] -> R.Runtime R.Value)
makeBurdockList link empty us = f us
  where
    f [] = pure empty
    f (v:vs) = do
        vs' <- f vs
        R.app Nothing link [v, vs']

extractBurdockList :: R.VariantTag -> R.VariantTag -> R.Value -> R.Runtime (Maybe [R.Value])
extractBurdockList linkTag emptyTag as = do
    case R.destructureVariant as of
        Just (tg, _) | tg == emptyTag ->
            pure $ Just []
        Just (tg, vs) | tg == linkTag
                      , Just a <- lookup "first" vs
                      , Just b <- lookup "rest" vs  -> do
            b' <- extractBurdockList linkTag emptyTag b
            pure ((a:) <$> b')
        Just {} -> pure Nothing
        Nothing -> pure Nothing
    
    -- see if as is an empty
    -- if so, return Just []
    -- see if as is a link(a,b)
    -- if so, return (a :) <$> extractburdock b

