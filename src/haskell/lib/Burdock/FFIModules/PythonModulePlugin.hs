
{-

plugin to load python modules and access them from burdock

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.PythonModulePlugin
    (pythonModulePlugin
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


import Control.Monad (unless)

loadPythonModule :: R.FFITypeInfo -> Text -> R.Runtime (ModuleMetadata,R.Value)
loadPythonModule pyObjType name = do
    pym <- either (error . show) id <$> liftIO (P.script $ "import " <> name <> "\n" <> name)
    members' <- either (error . show) id <$>
        (liftIO $ P.script $ "import inspect\nimport " <> name <> "\n"
            <> "[i[0] for i in inspect.getmembers(" <> name <> ")]")
    members <- extractStringList members'
    pymv <- R.makeFFIValue pyObjType pym
    let ms = flip map members $ \n -> (n,Nothing, BEIdentifier, ("<python>",Nothing))
    pure (R.ModuleMetadata ms, pymv)
  where
    extractStringList :: P.PyObject -> R.Runtime [Text]
    extractStringList l = do
        is <- either (error . show) id <$> liftIO (P.pyListToList l)
        mapM extractText is
    extractText i = either (error . show) id <$> liftIO (P.fromPyObject i)

pythonModulePlugin :: R.Runtime R.ModulePlugin
pythonModulePlugin = do
    -- create pyobject ffi type
    pyTy <- makePyObjectType
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
    pure $ R.ModulePlugin (\_ i -> pure i) getMeta getVal

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
                s <- either (error . show) id <$> P.script "str"
                t <- either (error . show) id <$> P.app s [v]
                u <- either (error . show) id <$> liftIO (P.fromPyObject t)
                pure u
         -- todo: fix these
        ,R.Equals $ \_ _  -> pure False
        ,R.CatchAll $ \mkV _exV nm pyObj -> do
                v <- either (error . show) id <$> P.getAttr pyObj nm
                mkV v
        ]
