

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}
module PyWrap
    (initialize

    -- main api
    ,script
    ,scriptWithBinds
    ,eval
    ,app
    ,appText
    ,getAttr
    ,for
    -- could make like exec, and then have another function that splits
    -- for efficiency?
    
    ,PythonError(..)

    -- conversion to and from python values for some basic values
    ,PyObject
    ,ToPyObject
    ,toPyObject
    ,FromPyObject
    ,fromPyObject

    ,isPyNone
    ,pyNone
    ,pyTrue
    ,pyFalse

    ,makePyList
    ,makePyTuple

    ,pyTupleToList
    ,pyListToList

     -- internals for testing
    ,_splitStatements
    
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Data.Text as T
import Foreign.Ptr
--import Control.Monad (void, when) --, when, forM_, forM)
-- --import Data.Char (isSpace)
import qualified Text.RawString.QQ as R
import Foreign.ForeignPtr
import Control.Monad.Except

-- import Control.Monad.IO.Class (MonadIO, liftIO)

------------------------------------------------------------------------------

-- initialize

-- call exactly once before using python. it should be called in a bound thread
-- and all the subsequent calls to this api should be in this thread

foreign import ccall "Py_Initialize" pyInitialize :: IO ()
--foreign import ccall "init_helper_functions" cinitHelperFunctions :: IO CInt
-- int PyRun_SimpleString(const char *command)
--foreign import ccall "PyRun_SimpleString" pyRunSimpleString :: CString -> IO CInt

initialize :: IO ()
initialize = do
    pyInitialize
    x <- runExceptT (pyEvalEvalCodeWrap Nothing pyFileInput pythonHelpersSource)
    -- todo: change this into an either like the rest of the code
    case x of
        Left e -> error $ "python ffi: initializing helper functions failed " ++ show e
        Right {} -> pure ()
    --void cinitHelperFunctions

pythonHelpersSource :: T.Text
pythonHelpersSource = [R.r|
import copy
import ast
fn_template = ast.parse('''
def f():
    pass''')

# if the last line in the list of statements is an expression
# break it off, and return the prefix, and this expression separately
# else return the code passed in and None
def split_statements(y):
    yp = ast.parse(y)
    if len(yp.body) == 0:
        return ('', None)
    if isinstance(yp.body[-1], ast.Expr):
        return (ast.unparse(yp.body[:-1]), ast.unparse(yp.body[-1]))
    else:
        return (ast.unparse(yp), None)

# def print_type(y):
#    print(f'{y} {type(y)}');
|]


------------------------------------------------------------------------------

-- split statements

-- takes python source. if the source ends with an expression, split it out to
-- a separate string
-- it uses python parsing and ast manipulation functions wrapped in a
-- c helper function

foreign import ccall "split_statements"
  csplitStatements :: CString
                 -> Ptr CString
                 -> (Ptr (Ptr CPyObject))
                 -> IO CString

_splitStatements :: T.Text -> IO (Either PythonError (T.Text, Maybe T.Text))
_splitStatements src =
    liftIO $ withCString (T.unpack src) $ \src' ->
    alloca $ \ex -> 
    alloca $ \resResource -> runExceptT $ do
       --liftIO $ poke ex nullPtr
       -- the res and ex are owned by python, scoped according
       -- to the lifetime of the resResource pyobject
       res <- liftIO $ csplitStatements src' ex resResource
       -- wrap the resources before error checking to avoid resource leak
       resRc <- liftIO (safePeekPyObject =<< peek resResource)
       ExceptT checkPythonError
       when (res == nullPtr)
          $ throwError $ PythonError "PyWrapError" "internal error: expected error in split statements, didn't find one"
       v0 <- T.pack <$> liftIO (peekCString res)
       v1 <- pk ex
       -- make sure resRc isn't freed until we've read res and ex
       liftIO $ maybe (pure ()) touchPyObject resRc
       pure (v0,v1)
  where
    pk s = liftIO $ do
      cs <- peek s
      if cs == nullPtr
          then pure Nothing
          else Just <$> T.pack <$> peekCString cs

------------------------------------------------------------------------------

data PythonError = PythonError T.Text T.Text
    deriving (Eq,Show)

type PyWrap = ExceptT PythonError IO

checkPythonError :: IO (Either PythonError ())
checkPythonError = runExceptT $ do
    x <- liftIO pyErrOccurred
    when x $ do
        (mptype, mpvalue,_mptrackback) <- liftIO pyErrFetch
        ptype <- maybe
            (throwError $ PythonError "PyWrapError" "expected pyErrFetch to return a type but it returned null")
            pure
            mptype
        ptypeName <- ExceptT $ getAttr ptype "__name__"
        nm <- ExceptT $ fromPyObject ptypeName
        vl <- maybe (pure "") appStr mpvalue
        throwError (PythonError nm vl)
  where
    appStr :: PyObject -> PyWrap T.Text
    appStr o = do
        fn <- pyEvalEvalCodeWrap Nothing pyEvalInput "str"
        v <- ExceptT $ app fn [o]
        ExceptT $ fromPyObject v

------------------------------------------------------------------------------

-- basic api

script :: MonadIO m => T.Text -> m (Either PythonError PyObject)
script src = liftIO $ runExceptT $ pyScript' Nothing src

scriptWithBinds :: MonadIO m => [(T.Text, PyObject)] -> T.Text -> m (Either PythonError PyObject)
scriptWithBinds bs src = liftIO $ runExceptT $ do
    lo <- makeDict bs
    pyScript' (Just lo) src
  where
    --makeDict :: [(T.Text, PyObject)] -> PyWrap PyObject
    makeDict bsx = do
        d <- pyDictNew
        forM_ bsx $ \(n,v) -> pyDictSetItemString d n v
        pure d

getAttr :: MonadIO m => PyObject -> T.Text -> m (Either PythonError PyObject)
getAttr o f = liftIO $ runExceptT $ pyObjectGetAttrString o f

app :: MonadIO m => PyObject -> [PyObject] -> m (Either PythonError PyObject)
app func args = liftIO $ runExceptT $ do
    args' <- ExceptT $ makePyTuple args
    pyObjectCall func args'

-- can't work out how to re-enter the 'm' monad from regular exceptT wrapper
for :: MonadIO m => PyObject -> (PyObject -> m a) -> m (Either PythonError [a])
for it cb = do
    pyit' <- meth it "__iter__"
    case pyit' of
        Left e -> pure $ Left e
        Right pyit -> doiterate pyit
  where
    doiterate pyit = do
        nv' <- meth pyit "__next__"
        case nv' of
            Left (PythonError "StopIteration" "") -> pure $ Right []
            Left e -> pure $ Left e
            Right nv -> do
                v <- cb nv
                ctu <- doiterate pyit
                case ctu of
                    Left e -> pure $ Left e
                    Right vs -> pure $ Right (v:vs)
    meth o f = runExceptT $ do
        v <- ExceptT $ getAttr o f
        ExceptT $ app v []

appText :: T.Text -> [PyObject] -> IO (Either PythonError PyObject)
appText funcnm args = runExceptT $ do
    func <- pyEvalEvalCodeWrap Nothing pyEvalInput funcnm
    ExceptT $ app func args

------------------------------------------------------------------------------

-- running scripts and evaluating expressions

pyScript' :: Maybe PyObject -> T.Text -> PyWrap PyObject
pyScript' lo src = do
    (ex, rt) <- ExceptT $ _splitStatements src
    void $ pyEvalEvalCodeWrap lo pyFileInput ex
    case rt of
        Nothing -> pyNone
        Just rt' -> pyEvalEvalCodeWrap lo pyEvalInput rt'

-- TODO: do this properly
-- #define Py_file_input 257
-- #define Py_eval_input 258
pyFileInput :: CInt
pyFileInput = 257
pyEvalInput :: CInt
pyEvalInput = 258

pyEvalEvalCodeWrap :: Maybe PyObject -> CInt -> T.Text -> PyWrap PyObject
pyEvalEvalCodeWrap lo start src = do
    co <- pyCompileString src "" start
    modMain <- pyImportAddModule "__main__"
    gd <- pyModuleGetDict modMain
    let ld = maybe gd id lo
    -- TODO: by using a local dict here, it implements the local
    -- feature that want - so don't need the wrap in function thing anymore
    --ld <- pyDictNew
    -- todo: this isn't right - using extra local binds should not
    -- affect if the new binds are local or global
    -- will work as a hack for now
    pyEvalEvalCode co gd ld

eval :: T.Text -> IO (Either PythonError PyObject)
eval ex = runExceptT $ pyEvalEvalCodeWrap Nothing pyEvalInput ex

------------------------------------------------------------------------------

-- python values wrapped in haskell

-- raw PyObject*
data CPyObject = CPyObject
    deriving Show

-- haskell wrapper
data PyObject = PyObject (ForeignPtr CPyObject)
    deriving Show

touchPyObject :: PyObject -> IO ()
touchPyObject (PyObject p) = touchForeignPtr p

safePeekPyObject :: Ptr CPyObject -> IO (Maybe PyObject)
safePeekPyObject ptr =
    if ptr == nullPtr
    then pure Nothing
    else Just . PyObject <$> newForeignPtr cPyDecrefFP ptr


objFromMaybe :: T.Text -> Maybe PyObject -> IO (Either PythonError PyObject)
objFromMaybe fn mobj = runExceptT $
    maybe (throwError $ PythonError "PyWrap" (T.concat ["expected ", fn, " to return a cpyobject but it returned null"])) pure mobj

withCPO' :: PyObject -> (Ptr CPyObject -> PyWrap a) -> PyWrap a
withCPO' (PyObject p) f =
    ExceptT $ liftIO $ withForeignPtr p $ \o' -> runExceptT $ f o'

foreign import ccall "cPy_DECREF" cPyDecref :: Ptr CPyObject -> IO ()
foreign import ccall "&cPy_DECREF" cPyDecrefFP :: FunPtr (Ptr CPyObject -> IO ())

foreign import ccall "cPy_INCREF" cPyIncref :: Ptr CPyObject -> IO ()

withCPO :: PyObject -> (Ptr CPyObject -> IO a) -> IO a
withCPO (PyObject p) f = withForeignPtr p f

class FromPyObject a where
    fromPyObject :: PyObject -> IO (Either PythonError a)

class ToPyObject a where
    toPyObject :: a -> IO PyObject

wrapWithError :: PyWrap a -> IO a
wrapWithError f = do
    x <- runExceptT f
    case x of
        Left e -> error $ show e
        Right x' -> pure x'

---------------------------------------

-- check object types directly
-- otherwise the error message you get from python is awful
-- could consider doing it only if get an error?
-- not sure if the conversion functions are robust against
-- python implicit casting, so have to check that also if change
checkObjectType :: T.Text -> PyObject -> PyWrap ()
checkObjectType nm obj = do
    ty <- pyType obj
    nm' <- ExceptT $ getAttr ty "__name__"
    nm'' <- pyUnicodeAsUTF8String nm'
    nm''' <- pyBytesAsString nm''
    if nm == nm'''
      then pure ()
      else throwError $ PythonError "TypeMismatch" (T.concat ["Expected ", nm, ", got ", nm'''])

instance FromPyObject T.Text where
    fromPyObject a = runExceptT $ do
        checkObjectType "str" a
        ut <- pyUnicodeAsUTF8String a
        pyBytesAsString ut

instance ToPyObject T.Text where
    toPyObject a = pyUnicodeFromString a

---------------------------------------

instance FromPyObject Int where
    fromPyObject a = runExceptT $ do
        checkObjectType "int" a
        pyLongAsLong a

instance ToPyObject Int where
    toPyObject a = pyLongFromLong a

instance FromPyObject Double where
    fromPyObject a = runExceptT $ do
        checkObjectType "float" a
        pyFloatAsDouble a

instance ToPyObject Double where
    toPyObject a = pyFloatFromDouble a

instance FromPyObject Bool where
    fromPyObject a = runExceptT $ do
        checkObjectType "bool" a
        isPyTrue a

instance ToPyObject Bool where
    toPyObject a = if a
                   then wrapWithError pyTrue
                   else wrapWithError pyFalse


---------------------------------------

makePyList :: [PyObject] -> IO (Either PythonError PyObject)
makePyList lst = makeNThing pyListNew pyListSetItem lst

makePyTuple :: [PyObject] -> IO (Either PythonError PyObject)
makePyTuple lst = makeNThing pyTupleNew pyTupleSetItem lst

makeNThing :: (Int -> PyWrap PyObject)
           -> (PyObject -> Int -> PyObject -> PyWrap ())
           -> [PyObject]
           -> IO (Either PythonError PyObject)
makeNThing new si lst = runExceptT $ do
    ret <- new (length lst)
    let lp _ [] = pure ()
        lp i (x:xs) = do
            si ret i x
            lp (i + 1) xs
    lp 0 lst
    pure ret

pyTupleToList :: PyObject -> IO (Either PythonError [PyObject])
pyTupleToList tup = runExceptT $ do
    checkObjectType "tuple" tup
    sz <- pyTupleSize tup
    if sz == 0
      then pure []
      else forM [0..sz - 1] $ \i -> pyTupleGetItem tup i

pyListToList :: PyObject -> IO (Either PythonError [PyObject])
pyListToList tup = runExceptT $ do
    checkObjectType "list" tup
    sz <- pyListSize tup
    if sz == 0
      then pure []
      else forM [0..sz - 1] $ \i -> pyListGetItem tup i

------------------------------------------------------------------------------

-- raw functions and wrappers

{-
check nulls
manage reference counts properly, including when there's an error
check for python exceptions
-}

--textWithCString :: T.Text -> (CString -> IO a) -> IO a
--textWithCString c = withCString (T.unpack c) 

textWithCString' :: T.Text -> (CString -> PyWrap a) -> PyWrap a
textWithCString' c f =
    ExceptT $ liftIO $ withCString (T.unpack c) $ \c' -> runExceptT $ f c'

checkAndReturn :: T.Text -> IO (Ptr CPyObject) -> PyWrap PyObject
checkAndReturn fn f = do -- runExceptT $ do
    r <- liftIO f
    r' <- liftIO $ safePeekPyObject r
    ExceptT checkPythonError
    ExceptT $ objFromMaybe fn r'

checkAndReturnBorrowed :: T.Text -> IO (Ptr CPyObject) -> PyWrap PyObject
checkAndReturnBorrowed fn f = do -- runExceptT $ do
    r <- liftIO f
    when (r /= nullPtr) $ liftIO $ cPyIncref r
    r' <- liftIO $ safePeekPyObject r
    ExceptT checkPythonError
    ExceptT $ objFromMaybe fn r'

-- returns a borrowed reference to the current exception's type
-- or null if there's no error
foreign import  ccall "PyErr_Occurred" pyErrOccurredRaw :: IO (Ptr CPyObject)

pyErrOccurred :: IO Bool
pyErrOccurred = do
    x <- pyErrOccurredRaw
    pure (x /= nullPtr)

foreign import ccall "PyErr_Fetch"
  pyErrFetchRaw :: Ptr (Ptr CPyObject)
                -> Ptr (Ptr CPyObject)
                -> Ptr (Ptr CPyObject)
                -> IO ()

pyErrFetch :: IO (Maybe PyObject, Maybe PyObject, Maybe PyObject)
pyErrFetch =
    alloca $ \ptype ->
    alloca $ \pvalue ->
    alloca $ \ptraceback -> do
    pyErrFetchRaw ptype pvalue ptraceback
    (,,) <$> pk ptype <*> pk pvalue <*> pk ptraceback
  where
    pk p = do
        p' <- peek p
        safePeekPyObject p'

foreign import ccall "PyObject_GetAttrString"
  pyObjectGetAttrStringRaw :: Ptr CPyObject -> CString -> IO (Ptr CPyObject)

pyObjectGetAttrString :: PyObject -> T.Text -> PyWrap PyObject
pyObjectGetAttrString o fld =
    withCPO' o $ \o' ->
    textWithCString' fld $ \fld' -> do
    r <- liftIO $ pyObjectGetAttrStringRaw o' fld'
    ret <- liftIO $ safePeekPyObject r
    ExceptT checkPythonError
    ExceptT $ objFromMaybe "PyObject_GetAttrString" ret

foreign import ccall "PyObject_Call"
  pyObjectCallRaw :: Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)

pyObjectCall :: PyObject -> PyObject -> PyWrap PyObject
pyObjectCall func args =
    withCPO' func $ \func' ->
    withCPO' args $ \args' -> do
    r <- liftIO $ pyObjectCallRaw func' args' nullPtr -- kwArgs'
    r' <- liftIO $ safePeekPyObject r
    ExceptT checkPythonError
    ExceptT $ objFromMaybe "PyObject_Call" r'
    
foreign import ccall "PyDict_New" pyDictNewRaw :: IO (Ptr CPyObject)

pyDictNew :: PyWrap PyObject
pyDictNew = do
    r <- liftIO $ pyDictNewRaw
    r1 <- liftIO $ safePeekPyObject r
    ExceptT checkPythonError
    ExceptT $ objFromMaybe "PyDict_New" r1

foreign import ccall "PyDict_SetItemString"
  pyDictSetItemStringRaw :: Ptr CPyObject -> CString -> Ptr CPyObject -> IO CInt

pyDictSetItemString :: PyObject -> T.Text -> PyObject -> PyWrap ()
pyDictSetItemString d k v =
    withCPO' d $ \d' ->
    textWithCString' k $ \k' ->
    withCPO' v $ \v' -> do
        -- should this be done before calling,
        -- or after calling and error has been checked?
        liftIO $ cPyIncref v'
        r <- liftIO $ pyDictSetItemStringRaw d' k' v'
        ExceptT checkPythonError
        when (r /= 0) $ throwError $ PythonError "PyWrap" "PyDict_SetItemString returned -1 but no python exception"
             
foreign import ccall "Py_CompileString" pyCompileStringRaw :: CString -> CString -> CInt -> IO (Ptr CPyObject)

pyCompileString :: T.Text -> T.Text -> CInt -> PyWrap PyObject
pyCompileString src fn start =
    textWithCString' src $ \src' ->
    textWithCString' fn $ \fn' ->
    checkAndReturn "Py_CompileString" (pyCompileStringRaw src' fn' start)

foreign import ccall "PyImport_AddModule" pyImportAddModuleRaw :: CString -> IO (Ptr CPyObject)

pyImportAddModule :: T.Text -> PyWrap PyObject
pyImportAddModule mo =
    textWithCString' mo $ \mo' ->
    checkAndReturnBorrowed "PyImport_AddModule" (pyImportAddModuleRaw mo') 
   
foreign import ccall "PyModule_GetDict" pyModuleGetDictRaw :: Ptr CPyObject -> IO (Ptr CPyObject)

pyModuleGetDict :: PyObject -> PyWrap PyObject
pyModuleGetDict obj =
    withCPO' obj $ \obj' -> do
    checkAndReturnBorrowed "PyModule_GetDict" (pyModuleGetDictRaw obj')

foreign import ccall "PyEval_EvalCode" pyEvalEvalCodeRaw :: Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)

pyEvalEvalCode :: PyObject -> PyObject -> PyObject -> PyWrap PyObject
pyEvalEvalCode code gdict ldict =
    withCPO' code $ \code' ->
    withCPO' gdict $ \gdict' ->
    withCPO' ldict $ \ldict' ->
    checkAndReturn "PyEval_EvalCode" (pyEvalEvalCodeRaw code' gdict' ldict')

------------------------------------------------------------------------------

-- data types

foreign import ccall "PyUnicode_AsUTF8String" pyUnicodeAsUTF8StringRaw :: Ptr CPyObject -> IO (Ptr CPyObject)

pyUnicodeAsUTF8String :: PyObject -> PyWrap PyObject
pyUnicodeAsUTF8String o =
    withCPO' o $ \o' ->
    checkAndReturn "PyUnicode_AsUTF8String" (pyUnicodeAsUTF8StringRaw o')

foreign import ccall "PyBytes_AsString" pyBytesAsStringRaw :: Ptr CPyObject -> IO CString

pyBytesAsString :: PyObject -> PyWrap T.Text
pyBytesAsString o = 
    withCPO' o $ \o' -> do
    -- cstr is readonly and owned by python
    cstr <- liftIO $ pyBytesAsStringRaw o'
    ExceptT checkPythonError
    T.pack <$> liftIO (peekCString cstr)

foreign import ccall "PyUnicode_FromString" pyUnicodeFromStringRaw :: CString -> IO (Ptr CPyObject)

pyUnicodeFromString :: T.Text -> IO PyObject
pyUnicodeFromString str = wrapWithError $
    textWithCString' str $ \str' -> do
    checkAndReturn "PyUnicode_FromString" $ pyUnicodeFromStringRaw str'

foreign import ccall "py_type" pyTypeRaw :: Ptr CPyObject -> IO (Ptr CPyObject)

pyType :: PyObject -> PyWrap PyObject
pyType o =
    withCPO' o $ \o' -> do
    r <- liftIO $ pyTypeRaw o'
    -- can't generate a python exception
    when (r /= nullPtr) $ liftIO $ cPyIncref r
    r' <- liftIO $ safePeekPyObject r
    ExceptT $ objFromMaybe "Py_TYPE" r'


foreign import ccall "PyLong_AsLong" pyLongAsLongRaw :: Ptr CPyObject -> IO CLong

pyLongAsLong :: PyObject -> PyWrap Int
pyLongAsLong obj =
    withCPO' obj $ \obj' -> do
    CLong l <- liftIO $ pyLongAsLongRaw obj'
    ExceptT checkPythonError
    pure $ fromIntegral l

foreign import ccall "PyLong_FromLong" pyLongFromLongRaw :: CInt -> IO (Ptr CPyObject)

pyLongFromLong :: Int -> IO PyObject
pyLongFromLong i = wrapWithError $
    checkAndReturn "PyLong_FromLong" (pyLongFromLongRaw (fromIntegral i))
     
foreign import ccall "PyFloat_AsDouble"
  pyFloatAsDoubleRaw :: Ptr CPyObject -> IO CDouble

pyFloatAsDouble :: PyObject -> PyWrap Double
pyFloatAsDouble o =
    withCPO' o $ \o' -> do
    CDouble d <- liftIO $ pyFloatAsDoubleRaw o'
    ExceptT checkPythonError
    pure $ realToFrac d

foreign import ccall "PyFloat_FromDouble"
  pyFloatFromDoubleRaw :: CDouble -> IO (Ptr CPyObject)

pyFloatFromDouble :: Double -> IO PyObject
pyFloatFromDouble d =
    wrapWithError $
    checkAndReturn "PyFloat_FromDouble" $ pyFloatFromDoubleRaw (realToFrac d)

foreign import ccall "is_py_true" isPyTrueRaw :: Ptr CPyObject -> IO CInt
foreign import ccall "is_py_false" isPyFalseRaw :: Ptr CPyObject -> IO CInt

-- these can't fail
isPyTrue :: PyObject -> PyWrap Bool
isPyTrue o = withCPO' o $ \o' -> do
    x <- liftIO $ isPyTrueRaw o'
    pure (x /= 0)

_isPyFalse :: PyObject -> PyWrap Bool
_isPyFalse o = withCPO' o $ \o' -> do
    x <- liftIO $ isPyFalseRaw o'
    pure (x /= 0)

foreign import ccall "py_false" pyFalseRaw :: IO (Ptr CPyObject)
foreign import ccall "py_true" pyTrueRaw :: IO (Ptr CPyObject)

pyFalse :: PyWrap PyObject
pyFalse = checkAndReturn "Py_RETURN_FALSE" pyFalseRaw

pyTrue :: PyWrap PyObject
pyTrue = checkAndReturn "Py_RETURN_TRUE" pyTrueRaw

{-foreign import ccall "cPy_None" pyNoneRaw :: IO (Ptr CPyObject)

pyNone' :: PyWrap PyObject
pyNone' = checkAndReturn "Py_RETURN_NONE" pyNoneRaw-}

foreign import ccall "cPy_None" pyNoneRaw :: IO (Ptr CPyObject)

isPyNone :: MonadIO m => PyObject -> m Bool
isPyNone p = liftIO $ withCPO p $ \p' -> do
    pn <- pyNoneRaw
    let x = p' == pn
    cPyDecref pn
    pure x

pyNone :: PyWrap PyObject
pyNone = checkAndReturn "Py_RETURN_NONE" pyNoneRaw

foreign import ccall "PyList_New" pyListNewRaw :: CSize -> IO (Ptr CPyObject)


pyListNew :: Int -> PyWrap PyObject
pyListNew n = checkAndReturn "PyList_New" $ pyListNewRaw (fromIntegral n)

foreign import ccall "PyList_SetItem" pyListSetItemRaw :: Ptr CPyObject -> CSize -> Ptr CPyObject -> IO CInt

pyListSetItem :: PyObject -> Int -> PyObject -> PyWrap ()
pyListSetItem d i v =
    withCPO' d $ \d' ->
    withCPO' v $ \v' -> do
        liftIO $ cPyIncref v'
        _ <- liftIO $ pyListSetItemRaw d' (fromIntegral i) v'
        ExceptT checkPythonError
        pure ()

foreign import ccall "PyTuple_New" pyTupleNewRaw :: CSize -> IO (Ptr CPyObject)

pyTupleNew :: Int -> PyWrap PyObject
pyTupleNew n = checkAndReturn "PyTuple_New" $ pyTupleNewRaw (fromIntegral n)

foreign import ccall "PyTuple_SetItem" pyTupleSetItemRaw :: Ptr CPyObject -> CSize -> Ptr CPyObject -> IO CInt

pyTupleSetItem :: PyObject -> Int -> PyObject -> PyWrap ()
pyTupleSetItem d i v =
    withCPO' d $ \d' ->
    withCPO' v $ \v' -> do
        liftIO $ cPyIncref v'
        _ <- liftIO $ pyTupleSetItemRaw d' (fromIntegral i) v'
        ExceptT checkPythonError
        pure ()


foreign import ccall "PyTuple_Size" pyTupleSizeRaw :: Ptr CPyObject -> IO CSize

pyTupleSize :: PyObject -> PyWrap Int
pyTupleSize o =
    withCPO' o $ \o' -> do
    n <- liftIO $ pyTupleSizeRaw o'
    ExceptT checkPythonError
    pure $ fromIntegral n
    
foreign import ccall "PyTuple_GetItem" pyTupleGetItemRaw :: Ptr CPyObject -> CSize -> IO (Ptr CPyObject)

pyTupleGetItem :: PyObject -> Int -> PyWrap PyObject
pyTupleGetItem o n =
    withCPO' o $ \o' -> do
    r <- liftIO $ pyTupleGetItemRaw o' (fromIntegral n)
    when (r /= nullPtr) $ liftIO $ cPyIncref r
    r' <- liftIO $ safePeekPyObject r
    ExceptT $ objFromMaybe "PyTuple_GetItem" r'

foreign import ccall "PyList_Size" pyListSizeRaw :: Ptr CPyObject -> IO CSize

pyListSize :: PyObject -> PyWrap Int
pyListSize o =
    withCPO' o $ \o' -> do
    n <- liftIO $ pyListSizeRaw o'
    ExceptT checkPythonError
    pure $ fromIntegral n
    
foreign import ccall "PyList_GetItem" pyListGetItemRaw :: Ptr CPyObject -> CSize -> IO (Ptr CPyObject)

pyListGetItem :: PyObject -> Int -> PyWrap PyObject
pyListGetItem o n =
    withCPO' o $ \o' -> do
    r <- liftIO $ pyListGetItemRaw o' (fromIntegral n)
    when (r /= nullPtr) $ liftIO $ cPyIncref r
    r' <- liftIO $ safePeekPyObject r
    ExceptT $ objFromMaybe "PyList_GetItem" r'
