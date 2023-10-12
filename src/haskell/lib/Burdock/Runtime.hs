
{-

The runtime is the code that allows running in a burdock context. It's
kind of like an ffi interface to haskell, it doesn't know anything
about syntax.

The interpreter takes syntax and calls functions in the runtime.
Haskell ffi code uses function in the runtime too.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Runtime
    (Value(..)

    ,ModuleMetadata
    ,ModulePlugin(..)
    ,RuntimeImportSource(..)

    ,getModuleMetadata
    ,getModuleValue
    ,addModulePlugin
    ,lookupImportSource
    ,BurdockImportSource(..)
    
    ,runRuntime
    ,Runtime
    ,liftIO
    ,Scientific

    ,debugShowValue

    ,RuntimeState
    ,emptyRuntimeState
    ,getRuntimeState
    ,addFFIType
    ,ffiTypeTagToValue
    ,makeFFIType
    
    ,setBootstrapRecTup
    ,BootstrapValues(..)
    
    ,withCallstackEntry

    ,makeFunctionValue
    ,makeValue
    ,extractValue

    ,DataDeclTypeTag(..)
    ,dataDeclTagsEqual
    ,FFITypeTag
    ,tyName
    ,Env

    ,captureClosure
    
    ,nothingValue
    ,makeBurdockList
    ,extractBurdockList

    ,makeRecord
    
    ,makeTuple
    ,extractTuple
    ,tupleGet

    ,makeVariant
    ,makeValueName
    ,makeString
    ,makeBool
    ,makeNumber

    ,variantName
    ,variantTag
    ,variantFields
    ,variantValueFields
    -- temp, should only be used for the binding in the default env

    ,makeDataDeclTag

    ,runTask
    ,throwValue

    ,getCallStack
    
    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding
    ,lookupType

    ,createBurdockRunner

    ,getMember
    ,app

    ,addTestFail
    ,addTestPass
    ,getTestResults
    ,doIsTest
    ,doIsNotTest
    ,setTestModule

    --,ffimethodapp
    -- temp
    ,getTempEnvStage
    ,setTempEnvStage
    ) where

import Prelude hiding (error, putStrLn, show)
import qualified Prelude as P
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Burdock.Scientific
import Burdock.ModuleMetadata
    (ModuleMetadata
    ,tempEmptyModuleMetadata)

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            --,MonadIO
                            )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Dynamic
    (Dynamic
    ,toDyn
    ,fromDynamic
    ,Typeable
    )

import Data.IORef
    (IORef
    ,newIORef
    ,modifyIORef
    ,readIORef
    ,writeIORef
    )

import Control.Exception.Safe (catch
                              ,SomeException
                              ,Exception
                              ,throwM
                              ,catchAsync
                              --,catchAny
                              --,fromException
                              --,try
                              ,bracket
                              )

import Data.List (sortBy)
import Data.Ord (comparing)

import Burdock.RenamerEnv (renameTypeName)

------------------------------------------------------------------------------

data RuntimeState
    = RuntimeState
    {rtBindings :: IORef [(Text, Value)]
    ,rtNumTestFailed :: IORef Int
    ,rtNumTestPassed :: IORef Int
    ,rtCallStack :: IORef [Maybe Text]
    ,rtBootstrapRecTup :: IORef BootstrapValues
    ,rtTestModule :: IORef Value
    ,rtModulePlugins :: IORef [(Text, ModulePlugin)]
    -- hack used by the renamer temporarily
    ,rtTempEnvStage :: IORef ModuleMetadata
    }

getTempEnvStage :: Runtime ModuleMetadata
getTempEnvStage = do
    x <- ask
    liftIO $ readIORef (rtTempEnvStage x)

setTempEnvStage :: ModuleMetadata -> Runtime ()
setTempEnvStage v = do
    x <- ask
    liftIO $ writeIORef (rtTempEnvStage x) v

-- todo: see if this can be refactored to work with the ffi closure
-- system
data BootstrapValues
    = BootstrapValues
    {btListEmpty :: Value
    ,btListLink :: Value
    ,btNothing :: Value
    ,btStringTypeTag :: FFITypeTag
    }

emptyRuntimeState :: IO RuntimeState
emptyRuntimeState =
    RuntimeState
        <$> newIORef []
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef []
        <*> newIORef (error "bootstrap for tuples and records not completed")
        <*> newIORef (error "testing module not loaded")
        <*> newIORef []
        <*> newIORef tempEmptyModuleMetadata

setBootstrapRecTup :: BootstrapValues -> Runtime ()
setBootstrapRecTup v = do
    st <- ask
    liftIO $ writeIORef (rtBootstrapRecTup st) v

makeFFIType :: Text -> (Text -> Value -> Runtime Value) -> Runtime FFITypeTag
makeFFIType nm memfn = pure $ FFITypeTag nm memfn

-- creates an ffi type and adds a binding to it in the current scope
-- not sure these should be combined like this
addFFIType :: Text -> (Text -> Value -> Runtime Value) -> Runtime FFITypeTag
addFFIType nm memfn = do
    ty <- makeFFIType nm memfn
    -- recursive hack
    ftg <-
        if nm == "ffitypetag"
        then pure ty
        else getFFITypeTag "ffitypetag"
    addBinding (renameTypeName nm) $ makeValue ftg ty
    pure ty

ffiTypeTagToValue :: FFITypeTag -> Runtime Value
ffiTypeTagToValue ty@(FFITypeTag nm _) = do
    -- recursive hack
    ftg <-
        if nm == "ffitypetag"
        then pure ty
        else getFFITypeTag "ffitypetag"
    pure $ makeValue ftg ty

getFFITypeTag :: Text -> Runtime FFITypeTag
getFFITypeTag nm = do
    v <- maybe (error $ "internal: ffitypetag " <> nm <> " not found") id
         <$> lookupBinding (renameTypeName nm)
    maybe (error $ "internal: ffitypetag " <> nm <> " wrong type") pure $ extractValue v

makeDataDeclTag :: Text -> Runtime Value
makeDataDeclTag v = do
    let tg = DataDeclTypeTag v
    tgx <- getFFITypeTag "datadecltag"
    pure $ makeValue tgx tg

type Runtime = ReaderT RuntimeState IO

-- todo: make this abstract
data Value = FFIValue FFITypeTag Dynamic
           | VariantV DataDeclTypeTag Text [(Text, Value)]
           | RecordV [(Text,Value)]
           | TupleV [Value]
           | MethodV Value
           | VFun ([Value] -> Runtime Value)
           | BoxV (IORef Value)

-- todo: use something more robust
data DataDeclTypeTag = DataDeclTypeTag {dtyName :: Text}

dataDeclTagsEqual :: DataDeclTypeTag -> DataDeclTypeTag -> Bool
dataDeclTagsEqual a b = dtyName a == dtyName b

data FFITypeTag
    = FFITypeTag
    {tyName :: Text
    ,tyMemberFn :: (Text -> Value -> Runtime Value)}

debugShowValue :: Value -> Text
debugShowValue (FFIValue _tg dn)
    | Just (dn' :: Scientific) <- fromDynamic dn
    = "ValueNum " <> showScientific dn'

debugShowValue (FFIValue _tg dn)
    | Just (ls :: [Value]) <- fromDynamic dn
    = "[HaskellList: " <> T.intercalate "," (map debugShowValue ls) <> "]"

debugShowValue v
    | Just ls <- extractBurdockList v
    = "[list: " <> T.intercalate "," (map debugShowValue ls) <> "]"

debugShowValue (FFIValue tg dn) = "Value " <> tyName tg <> " " <> show dn
debugShowValue (VariantV _ tf fs) =
    -- todo: show the tag also?
    "VariantV " <> tf <> " " <> T.concat (map f fs)
    where
        f (nm,v) = nm <> " " <> debugShowValue v <> ","
debugShowValue (MethodV v) = "MethodV " <> debugShowValue v
debugShowValue (VFun {}) = "VFun {}"
debugShowValue (BoxV {}) = "BoxV {}"

debugShowValue (RecordV fs) = "{" <> T.intercalate "," (map f fs) <> "}"
  where
    f (n,v) = n <> ":" <> debugShowValue v

debugShowValue (TupleV fs) = "{" <> T.intercalate ";" (map debugShowValue fs) <> "}"

getCallStack :: Runtime [Maybe Text]
getCallStack = do
    st <- ask
    liftIO $ readIORef (rtCallStack st)

data Env = Env [(Text, Value)]
    --deriving Show

makeValue :: Typeable a => FFITypeTag -> a -> Value
makeValue tg v = FFIValue tg $ toDyn v

makeBurdockList :: [Value] -> Runtime Value
makeBurdockList [] = do
    st <- ask
    btV <- liftIO $ readIORef (rtBootstrapRecTup st)
    pure $ btListEmpty btV
makeBurdockList (v:vs) = do
    st <- ask
    btV <- liftIO $ readIORef (rtBootstrapRecTup st)
    let listLink = btListLink btV
    vs' <- makeBurdockList vs
    app Nothing listLink [v,vs']

extractBurdockList :: Value -> Maybe [Value]
-- todo: check the tag is the tag for list type
extractBurdockList (VariantV _ "empty" _) = Just []
extractBurdockList (VariantV _ "link" fs)
    | Just f <- lookup "first" fs
    , Just r <- lookup "rest" fs
    = do
         r' <- extractBurdockList r
         Just (f:r')
extractBurdockList _ = Nothing

extractTuple :: Value -> Runtime (Maybe [Value])
-- todo: check the tag is tuple
extractTuple (TupleV vs) = pure $ Just vs
extractTuple _ = pure Nothing

makeTuple :: [Value] -> Runtime Value
makeTuple fs = pure $ TupleV fs

tupleGet :: Value -> Int -> Runtime Value
tupleGet (TupleV v) i = pure $ v !! i
tupleGet x _ = error $ "tupleGet called on non tuple: " <> debugShowValue x

extractValue :: Typeable a => Value -> Maybe a
extractValue (FFIValue _ v) = fromDynamic v
extractValue _x = Nothing -- error $ "can't extract value from " ++ T.unpack (debugShowValue x)

nothingValue :: Runtime Value
nothingValue = do
    st <- ask
    btV <- liftIO $ readIORef (rtBootstrapRecTup st)
    pure $ btNothing btV

makeFunctionValue :: ([Value] -> Runtime Value) -> Runtime Value
makeFunctionValue f = pure $ VFun f

makeValueName :: Typeable a => Text -> a -> Runtime Value
makeValueName tgn v = do
    tg <- getFFITypeTag tgn
    pure $ makeValue tg v

makeString :: Text -> Runtime Value
makeString s = makeValueName "string" s

makeStringInternal :: Text -> Runtime Value
makeStringInternal t = do
    st <- ask
    btV <- liftIO $ readIORef (rtBootstrapRecTup st)
    pure $ makeValue (btStringTypeTag btV) t

makeBool :: Bool -> Runtime Value
makeBool b = makeValueName "boolean" b

makeNumber :: Scientific -> Runtime Value
makeNumber n = makeValueName "number" n

makeVariant :: DataDeclTypeTag -> Text -> [(Text,Value)] -> Runtime Value
makeVariant ty n fs = pure $ VariantV ty n fs

variantName :: Value -> Runtime (Maybe Text)
variantName (VariantV _ nm _) = pure $ Just nm
variantName _ = pure Nothing -- error $ "non variant passed to variant tag"

variantTag :: Value ->  Runtime (Maybe DataDeclTypeTag)
variantTag (VariantV tg _ _) = pure $ Just tg
variantTag _ = pure Nothing

variantFields :: Value -> Runtime (Maybe [(Text, Value)])
variantFields (VariantV _ _ flds) = pure $ Just flds
variantFields _ = pure Nothing

{-
these are the fields used in variant pattern matching,
and in the default torepr, equals and other generated methods
maybe will save an explicit list in the future, so can add auxiliary
non method fields which aren't included in pattern matching e.g.
-}
variantValueFields :: Value -> Runtime (Maybe [(Text, Value)])
variantValueFields (VariantV _ _ flds) =
    pure $ Just $ flip filter flds $ \case
       (_,MethodV {}) -> False
       _ -> True
variantValueFields _ = pure Nothing

makeRecord :: [(Text,Value)] -> Runtime Value
makeRecord fs = pure $ RecordV fs
              
captureClosure :: [Text] -> Runtime Env
captureClosure nms = do
    envr <- rtBindings <$> ask
    env <- liftIO $ readIORef envr
    -- todo: check all the nms are found
    {-let missing = let there = map fst env
                      in filter (`notElem` there) nms
    when (not (null missing)) $
        liftIO $ putStrLn $ "missing closure capture names: " <> show missing-}
    pure $ Env $ filter ((`elem` nms) . fst) env

-- todo: maybe the env has a catalog of loaded types
-- but an ffivalue, the ctor Value currently, should have
-- a pointer to it's type, not go via any kind of name lookup like this
getMember :: Value -> Text -> Runtime Value
getMember v@(FFIValue tg _) fld = (tyMemberFn tg) fld v

getMember v@(VariantV _ _ fs) fld = do
    case lookup fld fs of
        Nothing -> error $ "field not found:" <> fld <> ", " <> debugShowValue v
        Just (MethodV v1) -> app Nothing v1 [v]
        Just v1 -> pure v1

getMember (MethodV {}) "_torepr" = do
    v <- makeStringInternal "<method>"
    makeFunctionValue (\_ -> pure v)
    --pure $ makeValue "string" ("<methodv>" :: Text)
getMember (VFun {}) "_torepr" = do
    v <- makeStringInternal "<function>"
    makeFunctionValue (\_ -> pure v)

getMember (BoxV v) fld = do
    v' <- liftIO $ readIORef v
    getMember v' fld

-- special cases for _torepr, _equals for record and tuple
getMember (RecordV fs) "_torepr" = makeFunctionValue $ \case
    [] -> do
        vs <- mapM f fs
        makeStringInternal $ "{" <> T.intercalate "," (map (\(n,v) -> n <> ":" <> v) vs) <> "}"
    _ -> error $ "wrong number of args to record._torepr"
  where
    f (n,v) = do
        v' <- getMember v "_torepr"
        v'' <- app Nothing v' []
        case extractValue v'' of
            Just (v''' :: Text) -> pure (n,v''')
            Nothing -> error $ "_torepr returned non string on " <> debugShowValue v''

getMember (RecordV fs) "_equals" = makeFunctionValue $ \case
    [RecordV gs] -> eq fs gs
    [_] -> makeBool False
    _ -> error $ "wrong number of args to tuple._equals"
  where
    eq as bs | length as /= length bs = makeBool False
             | otherwise = let s = sortBy (comparing fst)
                           in eq' (s as) (s bs)
    eq' [] [] = makeBool True
    eq' ((af,_):_) ((bf,_):_) | af /= bf = makeBool False
    eq' ((_,a):as) ((_,b):bs) = do
        myEq <- getMember a "_equals"
        r <- app Nothing myEq [b]
        case extractValue r of
            Just True -> eq' as bs
            Just False -> pure r
            Nothing -> error $ "_equals method returned non bool " <> debugShowValue r
    eq' _ _ = error $ "internal: record equals length check regression"      

getMember v@(RecordV fs) fld = do
    case lookup fld fs of
        Nothing -> error $ "field not found:" <> fld <> ", " <> debugShowValue v
        Just (MethodV v1) -> app Nothing v1 [v]
        Just v1 -> pure v1

getMember (TupleV fs) "_torepr" = makeFunctionValue $ \case
    [] -> do
        vs' <- mapM f fs
        makeStringInternal $ "{" <> T.intercalate ";" vs' <> "}"
    _ -> error $ "wrong number of args to _torepr on tuple"
  where
    f v = do
        v' <- getMember v "_torepr"
        v'' <- app Nothing v' []
        case extractValue v'' of
            Just (v''' :: Text) -> pure v'''
            Nothing -> error $ "_torepr returned non string on " <> debugShowValue v''

getMember (TupleV fs) "_equals" = makeFunctionValue $ \case
    [TupleV gs] -> eq fs gs
    [_] -> makeBool False
    _ -> error $ "wrong number of args to tuple._equals"
  where
    eq as bs | length as /= length bs = makeBool False
             | otherwise = eq' as bs
    eq' [] [] = makeBool True
    eq' (a:as) (b:bs) = do
        myEq <- getMember a "_equals"
        r <- app Nothing myEq [b]
        case extractValue r of
            Just True -> eq' as bs
            Just False -> pure r
            Nothing -> error $ "_equals method returned non bool " <> debugShowValue r
    eq' _ _ = error $ "internal: tuple equals length check regression"      

getMember v fld = error $ "unrecognised member " <> fld <> " on " <> debugShowValue v

app :: Maybe Text -> Value -> [Value] -> Runtime Value
app sourcePos (VFun f) args =
    withCallstackEntry sourcePos $ f args
app sp (MethodV f) args = app sp f args
app sp (BoxV v) args = do
    v' <- liftIO $ readIORef v
    app sp v' args
app sp v@(FFIValue tg _) args = do
    fn <- (tyMemberFn tg) "_app" v
    app sp fn args

app _ v _ = error $ "app called on non function value " <> debugShowValue v

-- the reason it's done like this, is because in the future,
-- will be able to asynchronously exit another thread (using throwTo
-- under the covers)
-- and want the monitor message for a thread exiting this way to
-- record the stack trace where the async exception was injected
-- it's not a great design, but quick and will hopefully do the job
-- well enough for now
-- this is why it's vital not to use bracket
-- run-task uses the same system to get the stack trace for an exception
-- possibly a nicer design could be used if it was just synchronous
-- run-task to support
withCallstackEntry :: Maybe Text -> Runtime a -> Runtime a
withCallstackEntry sourcePos f = do
    st <- ask
    -- oldCS <- liftIO $ readIORef (rtCallStack st)
    liftIO $ modifyIORef (rtCallStack st) (sourcePos:)
    r <- f
    liftIO $ modifyIORef (rtCallStack st) tail
    pure r

withScope :: Runtime a -> Runtime a
withScope f = do
    x <- rtBindings <$> ask
    x1 <- liftIO $ readIORef x
    b1 <- liftIO $ newIORef x1
    local (\y -> y{rtBindings = b1}) f

withNewEnv :: Env -> Runtime a -> Runtime a
withNewEnv (Env env) f = do
    er <- liftIO $ newIORef env
    local (\y -> y{rtBindings = er}) f

addBinding :: Text -> Value -> Runtime ()
addBinding nm v = do
    x <- rtBindings <$> ask
    liftIO $ modifyIORef x ((nm,v):)

lookupBinding :: Text -> Runtime (Maybe Value)
lookupBinding nm = do
    x <- rtBindings <$> ask
    x1 <- liftIO $ readIORef x
    pure $ lookup nm x1

lookupType :: Text -> Runtime (Maybe Value)
lookupType nm = do
    x <- rtBindings <$> ask
    x1 <- liftIO $ readIORef x
    case lookup (renameTypeName nm) x1 of
        Nothing -> liftIO $ putStrLn $ T.unlines $ map fst x1
        Just {} -> pure ()
    pure $ lookup (renameTypeName nm) x1

runRuntime :: RuntimeState -> Runtime a -> IO a
runRuntime rt f = runReaderT f rt

createBurdockRunner :: Runtime (Runtime a -> IO a)
createBurdockRunner = do
    st <- ask
    pure $ runRuntime st

runTask :: Bool -> Runtime a -> Runtime (Either (Either Text Value, [Maybe Text]) a)
runTask casync f = do
    --liftIO $ putStrLn $ "catchAsync is " <> show casync
    bracketCallstack $ catch' (catchValue (Right <$> f))
  where
    myCatch = if casync
              then catchAsync
              else catch
    -- first try to catch a specific burdock value that was thrown
    catchValue = flip catch $ \(ValueException v) -> do
        st <- doStackTraceStuff
        pure $ Left (Right v, st)
    -- then try to catch any haskell (non async) exception
    catch' = flip myCatch $ \(e :: SomeException) -> do
        st <- doStackTraceStuff
        pure $ Left (Left $ show e, st)
    doStackTraceStuff = do
        rf <- rtCallStack <$> ask
        st <- liftIO $ readIORef rf
        pure st
    bracketCallstack :: Runtime a -> Runtime a
    bracketCallstack bf = bracket
        (do
         rf <- rtCallStack <$> ask
         save <- liftIO $ readIORef rf
         pure save)
        (\save -> do
           rf <- rtCallStack <$> ask
           liftIO $ writeIORef rf save)
        (const bf)

throwValue :: Value -> Runtime a
throwValue v = throwM $ ValueException v

data RuntimeException = ValueException Value

instance Show RuntimeException where
    show (ValueException v) = "ValueException " ++ T.unpack (debugShowValue v)

instance Exception RuntimeException

addTestFail :: Runtime ()
addTestFail = do
    st <- ask
    liftIO $ modifyIORef (rtNumTestFailed st) (+ 1)

addTestPass :: Runtime ()
addTestPass = do
    st <- ask
    liftIO $ modifyIORef (rtNumTestPassed st) (+ 1)
    
getTestResults :: Runtime (Int,Int)
getTestResults = do
    st <- ask
    (,) <$> liftIO (readIORef (rtNumTestPassed st))
        <*> liftIO (readIORef (rtNumTestFailed st))

getRuntimeState :: Runtime RuntimeState
getRuntimeState = ask

doIsTest :: [Value] -> Runtime Value
doIsTest as = do
    st <- ask
    testm <- liftIO $ readIORef (rtTestModule st)
    f <- getMember testm "my-do-is-test"
    app Nothing f as

doIsNotTest :: [Value] -> Runtime Value
doIsNotTest as = do
    st <- ask
    testm <- liftIO $ readIORef (rtTestModule st)
    f <- getMember testm "my-do-is-not-test"
    app Nothing f as

setTestModule :: Value -> Runtime ()
setTestModule m = do
    st <- ask
    liftIO $ writeIORef (rtTestModule st) m


------------------------------------------------------------------------------

-- import plugins
-- the idea is to create an abstract interface for import plugins
-- this is used to implemented modules implemented in burdock
-- but also modules implemented in an ffi system
-- this includes haskell ffi code
-- and will include the python auto module system
-- and some sort of c ffi

-- a plugin should be able to take an import source, and return
-- the metadata (used for burdock static checks, other plugins
-- can decide if to do anything with this)
-- or the loaded module value for that import source
-- a plugin must cache module values so if asked for one twice,
-- it returns the same value, and doesn't rerun the module

-- a runtime import source is the name corresponding to the <import-source-name>
-- in the syntax, and the list of args, in the burdock syntax this is:
-- import <import-source-name>(args) ...

--data ModuleMetadata = ModuleMetadata
--   deriving (Eq,Show)

data RuntimeImportSource
    = RuntimeImportSource
    {risImportSourceName :: Text
    ,risArgs :: [Text]
    }
    deriving Show

data ModulePlugin
    = ModulePlugin
    {mpGetMetadata :: Maybe Text -> RuntimeImportSource -> Runtime ModuleMetadata
    ,mpGetModuleValue :: Maybe Text -> RuntimeImportSource -> Runtime Value
    }

getModuleMetadata :: Maybe Text -> RuntimeImportSource -> Runtime ModuleMetadata
getModuleMetadata ctx ri = do
    st <- ask
    c <- liftIO $ readIORef (rtModulePlugins st)
    case lookup (risImportSourceName ri) c of
        Nothing -> error $ "unrecognised runtime import source: " <> risImportSourceName ri
        Just p -> (mpGetMetadata p) ctx ri

getModuleValue :: Maybe Text -> RuntimeImportSource -> Runtime Value
getModuleValue ctx ri = do
    st <- ask
    c <- liftIO $ readIORef (rtModulePlugins st)
    case lookup (risImportSourceName ri) c of
        Nothing -> error $ "unrecognised runtime import source: " <> risImportSourceName ri
        Just p -> (mpGetModuleValue p) ctx ri

addModulePlugin :: Text -> ModulePlugin -> Runtime ()
addModulePlugin nm mp = do
    st <- ask
    liftIO $ modifyIORef (rtModulePlugins st) ((nm,mp):)

data BurdockImportSource
    = BurdockImportSpecial Text [Text]
    | BurdockImportName [Text]
    deriving Show

lookupImportSource :: BurdockImportSource -> Runtime RuntimeImportSource
lookupImportSource (BurdockImportSpecial nm as) = pure $ RuntimeImportSource nm as
lookupImportSource (BurdockImportName [nm]) =
    -- todo: need to locate the builtins a bit more robustly
    pure $ RuntimeImportSource "file" ["/home/jake/wd/burdock/2023/src/burdock/built-ins/" <> nm <> ".bur"]
lookupImportSource x = error $ "import source not supported: " <> show x
