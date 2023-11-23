
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Runtime
    (Runtime
    ,RuntimeState
    ,makeRuntimeState
    ,runRuntime

    --,Value
    ,debugShowValue
    -- temp
    ,Value(BNothing,Boolean,BString,Fun,Box,Module,Variant,Method)
    ,makeVar
    ,setVar

    --,debugGetBindings

    ,newDataDeclTag
    ,DataDeclTag(..)
    ,VariantTag(..)

    ,FFITypeInfo(..)
    ,makeFFIType
    ,makeFFIValue
    ,extractFFIValue
    ,FFIValueEntry(..)
    ,getFFITypeInfoTypeInfo

    ,withScope
    ,localEnv
    ,app
    ,captureClosure
    ,withNewEnv
    ,addBinding
    ,getMember
    ,lookupBinding
    ,runTask

    ,InterpreterException(..)
    ,throwM

    ,ModuleID(..)
    ,ModulePlugin(..)
    --,ModuleMetadata
    ,addModulePlugin

    ,getModuleMetadata
    ,getModuleValue

    ,S.ImportSource(..)
    ,lookupImportSource

    ,addHaskellModule
    ,HaskellModulePlugin(..)
    ,haskellModulePlugin
    ,HaskellModule(..)
    ,ModuleMetadata(..)

    ,liftIO
    ,getRuntimeRunner
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import qualified Prelude as P

--import Burdock.Scientific (Scientific, showScientific)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Burdock.Syntax as S (ImportSource(..))

import Control.Monad.Reader
    (ReaderT
    ,runReaderT
    ,ask
    ,local
    )

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,modifyIORef
    ,writeIORef
    )

import Control.Monad.IO.Class (liftIO)

import Control.Monad (when)

import Data.Dynamic
    (Dynamic
    ,fromDynamic
    ,toDyn
    ,Typeable
    )

import Data.List
    ((\\)
    )

import Control.Exception.Safe
    (SomeException
    ,Exception
    ,Handler(..)
    ,catches
    ,throwM
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..))

------------------------------------------------------------------------------

type SP = Maybe (Text, Int, Int)

data DataDeclTag = DataDeclTag Int Text
    deriving (Eq, Show)

data VariantTag = VariantTag DataDeclTag Text
    deriving (Eq, Show)

data Value
    = Boolean Bool
    | BString Text
    | BNothing
    
    | FFIValue FFITypeInfo Dynamic
    | Variant VariantTag [(Text,Value)]
    | Box (IORef Value)
    | Module [(Text, Value)]

    | Fun ([Value] -> Runtime Value)
    | Method Value

-- pure show for use in temporary error messages and internal errors
debugShowValue :: Value -> Text
debugShowValue (Boolean b) = show b
debugShowValue (BString t) = "\"" <> t <> "\""
debugShowValue BNothing = "nothing"
debugShowValue (Fun {}) = "<function>"
debugShowValue (Method {}) = "<method>"
debugShowValue (Box _) = "<box>"
debugShowValue (Module _) = "<module>"
debugShowValue (FFIValue _ d) = "<" <> show d <> ">"
debugShowValue (Variant (VariantTag _ nm) fs) =
    nm <> "(" <> T.intercalate "," (map (debugShowValue . snd) fs) <> ")"

newDataDeclTag :: Text -> Runtime DataDeclTag
newDataDeclTag tnm = do
    newID <- autoID rtAutoDataDeclID
    pure $ DataDeclTag newID tnm

------------------------------------------------------------------------------

data RuntimeState
    = RuntimeState
    {rtBindings :: [(Text, Value)]
    ,rtFFITypeInfoTypeInfo :: FFITypeInfo
    ,rtAutoDataDeclID :: IORef Int
    ,rtAutoFFITypeID :: IORef Int
    ,rtModulePlugins :: IORef [(Text, ModulePlugin)]
    }

makeRuntimeState :: IO RuntimeState
makeRuntimeState = do
    st <- RuntimeState [] (error "delayed initialisation")
        <$> newIORef 0
        <*> newIORef 0
        <*> newIORef []
    -- does this need to be here? try to move it to bootstrap
    tinf <- runRuntime st $
        makeFFIType ["_bootstrap","ffi-type-info"]
            [ToRepr $ \(v :: FFITypeInfo) -> pure $ "<" <> tyNameText (tyTypeID v) <> ">"
            ,Equals $ \v w -> pure $ tyTypeID v == tyTypeID w]
    pure $ st {rtFFITypeInfoTypeInfo = tinf}

type Runtime = ReaderT RuntimeState IO

runRuntime :: RuntimeState -> Runtime a -> IO a
runRuntime st f = runReaderT f st

--debugGetBindings :: Runtime [(Text, Value)]
--debugGetBindings = do
--    st <- ask
--    liftIO $ readIORef (rtBindings st)

-- this could work if everything is single threaded, will need
-- something more sophisticated with threads
-- plus, probably should be cloning the state anyway since it's not pure
-- need to reason very carefully about state
-- the rtBindings should probably be copied to a new ioref
-- the auto data decl and ffitype id -> these could be stm tvars
--   but will also need a solution for serialization and high level
--   networking, which may remove these
-- the module plugins should probably be a tvar also - so all threads
-- share the same module cache, but then this would be copied to a new
-- tvar when using the container system
getRuntimeRunner :: Runtime (Runtime a -> IO a)
getRuntimeRunner = runRuntime <$> ask

--------------------------------------

withScope :: Runtime a -> Runtime a
withScope f = localEnv id f

withNewEnv :: [(Text,Value)] -> Runtime a -> Runtime a
withNewEnv bs f = local (\y -> y {rtBindings = bs}) f

localEnv :: ([(Text,Value)] -> [(Text,Value)]) -> Runtime a -> Runtime a
localEnv f stuff = do
    rtbv <- rtBindings <$> ask
    local (\s -> s {rtBindings = f rtbv}) stuff

app :: SP -> Value -> [Value] -> Runtime Value
app _sourcePos (Fun f) args = f args
app _ v _ = error $ "bad arg to app " <> debugShowValue v

-- todo: should capture closure capture elements within a module instead
-- of just the module? not sure. it might be a performance boost
-- but maybe using indexes would achieve this too, and plan to do that
-- at some point
captureClosure :: [Text] -> Runtime [(Text,Value)]
captureClosure nms = do
    -- error if any names aren't found
    rtbv <- rtBindings <$> ask
    -- could consider making this optional if it's a performance issue, seems unlikely
    -- given how inefficient everything else is
    let missing = nms \\ map fst rtbv
        -- todo: get to the point this hack isn't needed to make it work
        -- run-task: special case in the desugarer, don't add this syntax
        missingHack = filter (`notElem` ["run-task"])
                      missing
    when (not $ null missingHack) $
        error $ "closure capture items not found: " <> show missingHack
    pure $ filter ((`elem` nms) . fst) rtbv

addBinding :: Text -> Value -> Runtime a -> Runtime a
addBinding nm v f = local (\x -> x {rtBindings = ((nm,v): rtBindings x)}) f

lookupBinding :: Text -> Runtime (Maybe Value)
lookupBinding nm = lookup nm . rtBindings <$> ask

getMember :: SP -> Value -> Text -> Runtime Value

getMember _ v@(FFIValue tg _) fld = (tyMemberFn tg) fld v

getMember _ (Module fs) "_torepr" =
    pure $ wrap $ "MODULE{" <> T.intercalate "," (map fst fs) <> "}"
  where
    wrap v = Fun $ \case
        [] -> pure $ BString v
        _ -> error "bad args to torepr"

getMember sp (Module fs) f = case lookup f fs of
    Nothing -> error $ show sp <> " module member not found: " <> f
    Just v' -> pure v'

getMember sp v@(Variant _ fs) f = case lookup f fs of
    Nothing -> error $ show sp <> " variant member not found: " <> f
    Just (Method v1) -> app Nothing v1 [v]
    Just v' -> pure v'

-- temp?
getMember _ (Boolean b) "_torepr" =
    pure $ if b then wrap "true" else wrap "false"
  where
    wrap v = Fun $ \case
        [] -> pure $ BString v
        _ -> error "bad args to torepr"

getMember _ (Boolean b) "_equals" =
    pure $ Fun $ \case
        [Boolean c] -> pure $ Boolean $ b == c
        [_] -> pure $ Boolean False
        _ -> error $ "bad args to equals"

getMember _ (BString b) "_torepr" =
    pure $ Fun $ \case
        [] -> pure $ BString b
        _ -> error "bad args to torepr"

getMember _ (BString b) "_equals" =
    pure $ Fun $ \case
        [BString c] -> pure $ Boolean $ b == c
        [_] -> pure $ Boolean False
        _ -> error $ "bad args to equals"

getMember _ (BString b) "_plus" =
    pure $ Fun $ \case
        [BString c] -> pure $ BString $ b <> c
        _ -> error $ "bad args to bstring _plus"

getMember sp v f = error $ show sp <> ": getMember: " <> debugShowValue v <> " . " <> f

runTask :: forall a. Runtime a -> Runtime (Either Value a)
runTask f = do
    (do
        x <- f
        pure $! Right $! x
        ) `catches`
        [Handler $ \case
            InterpreterException txt -> pure $ Left $ BString txt
            ValueException v -> pure $ Left v
        ,Handler $ \(ex :: SomeException) -> pure $ Left $ BString $ show ex]

makeVar :: Value -> Runtime Value
makeVar v = do
    r <- liftIO $ newIORef v
    pure $ Box r

setVar :: SP -> Value -> Value -> Runtime ()
setVar sp bx v = do
    case bx of
        Box r -> liftIO $ writeIORef r v
        _ -> error $ show sp <> " attempt to set non var: " <> debugShowValue bx

------------------------------------------------------------------------------

-- ffi values helper functions

-- when considering adding other ways to create ffi values, you'll
-- almost certainly need to use recursive definitions - since to
-- extract values or create values in any of the members, you need the
-- ffi type already created, which must contain all the
-- members/methods for the ffi value

data FFITypeID
    = FFITypeID
    {tyID :: Int
    ,tyName :: [Text]
    }
    deriving (Eq,Show, Typeable)

tyNameText :: FFITypeID -> Text
tyNameText = T.intercalate "." . tyName

data FFITypeInfo
    = FFITypeInfo
    {tyTypeID :: FFITypeID
    ,tyMemberFn :: (Text -> Value -> Runtime Value)
    }

data FFIValueEntry a
    = ToRepr (a -> Runtime Text)
    | Equals (a -> a -> Runtime Bool)
    | Compare (a -> a -> Runtime Ordering)
    | Arith {aAdd :: a -> a -> Runtime a
            ,aSub :: a -> a -> Runtime a
            ,aMult :: a -> a -> Runtime a
            ,aDiv :: a -> a -> Runtime a}

makeFFIType :: Typeable a => [Text] -> [FFIValueEntry a] -> Runtime FFITypeInfo
makeFFIType nm fs = makeFFIType2 nm $ makeMemFns fs

makeFFIType2 :: Typeable a =>
                [Text]
             -> (FFITypeID
                 -> (a -> Runtime Value)
                 -> (Value -> Runtime (Either Text a))
                 -> (Text -> Value -> Runtime Value))
             -> Runtime FFITypeInfo
makeFFIType2 nm memFn = do
    tid <- newFFITypeID nm
    let tinf = FFITypeInfo tid memFn'
        memFn' = memFn tid (makeFFIValue tinf) (extractFFIValue tinf)
    pure $ tinf

makeFFIValue :: Typeable a => FFITypeInfo -> a -> Runtime Value
makeFFIValue ti a = pure $ FFIValue ti $ toDyn a

extractFFIValue :: Typeable a => FFITypeInfo -> Value -> Runtime (Either Text a)
extractFFIValue ti v = case v of
    FFIValue vtg v' | tyTypeID ti == tyTypeID vtg
                    , Just v'' <- fromDynamic v' -> pure $ Right v''
    _ -> pure $ Left $ "wrong extract value (please fix this error message)"

getFFITypeInfoTypeInfo :: Runtime FFITypeInfo
getFFITypeInfoTypeInfo = rtFFITypeInfoTypeInfo <$> ask

--------------------------------------

makeMemFns :: [FFIValueEntry a]
           -> FFITypeID
           -> (a -> Runtime Value)
           -> (Value -> Runtime (Either Text a))
           -> (Text -> Value -> Runtime Value)
makeMemFns meths tid mkV exV =
    let eqMem fn = ("_equals", \v -> do
            v' <- either error id <$> exV v
            pure $ Fun $ \case
                [w] -> do
                    w' <- exV w
                    case w' of
                        Left _ -> pure $ Boolean False
                        Right w'' -> Boolean <$> fn v' w''
                _ -> error $ "bad args to equals")
        binMem nm fn = (nm, \v -> do
                v' <- either error id <$> exV v
                pure $ Fun $ \case
                    [w] -> do
                        w' <- exV w
                        case w' of
                            Left _ -> error $ "bad type"
                            Right w'' -> fn v' w''
                    _ -> error $ "bad args to " <> nm)
        binMem2 nm fn = (nm, \v -> do
                v' <- either error id <$> exV v
                pure $ Fun $ \case
                    [w] -> do
                        w' <- exV w
                        case w' of
                            Left _ -> error $ "bad type"
                            Right w'' -> fn v' w''
                    _ -> error $ "bad args to " <> nm)
        meths' :: [(Text, Value -> Runtime Value)]
        meths' = concat $ flip map meths $ \case
           ToRepr f -> [("_torepr", \v -> do
               v' <- either error id <$> exV v
               pure $ Fun $ \case
                   [] -> BString <$> f v'
                   _ -> error $ "bad args to torepr")] 
           Equals f -> [eqMem f]
           Compare f -> [eqMem (\a b -> (==EQ) <$> f a b)
                        ,binMem "_lessthan" (\a b -> (Boolean . (==LT)) <$> f a b)
                        ,binMem "_lessequal" (\a b -> (Boolean . (`elem` [LT,EQ])) <$> f a b)
                        ,binMem "_greaterequal" (\a b -> (Boolean . (`elem` [GT,EQ])) <$> f a b)
                        ,binMem "_greaterthan" (\a b -> (Boolean . (==GT)) <$> f a b)]
           Arith myadd mysub mymul mydiv ->
               [binMem2 "_plus" $ \a b -> mkV =<< myadd a b
               ,binMem2 "_minus" $ \a b -> mkV =<< mysub a b
               ,binMem2 "_times" $ \a b -> mkV =<< mymul a b
               ,binMem2 "_divide" $ \a b -> mkV =<< mydiv a b]
        memFn nm = case lookup nm meths' of
            Nothing -> error $ "field not found:" <> tyNameText tid <> " ." <> nm
            Just x -> x
    in memFn 

autoID :: (RuntimeState -> IORef Int) -> Runtime Int
autoID f = do
    rf <- f <$> ask
    ret <- liftIO $ readIORef rf
    liftIO $ modifyIORef rf (+1)
    pure ret

newFFITypeID :: [Text] -> Runtime FFITypeID
newFFITypeID tnm = do
    newID <- autoID rtAutoFFITypeID
    pure $ FFITypeID newID tnm

------------------------------------------------------------------------------

data InterpreterException = InterpreterException Text
                          | ValueException Value

instance Show InterpreterException where
    show (InterpreterException s) = T.unpack s
    show (ValueException v) = T.unpack $ debugShowValue v

instance Exception InterpreterException where

--interpreterExceptionToValue :: InterpreterException -> Value
--interpreterExceptionToValue (InterpreterException s) = BString s
--interpreterExceptionToValue (ValueException v) = v

------------------------------------------------------------------------------

{-

modules

Basic design:

a low level import source is name(args), like importspecial in pyret syntax

the idea is that users will use package.name or similar to usually import modules
then the system will map from these to the low level modules

then, the name in the importspecial refers to an import plugin
plugins have the following responsibilities:
  given some args:
    give an error if there's no corresponding module
    provide the metadata for that module (without loading it if possible
      - this allows a pure burdock codebase to statically check everything
      without executing any script specific code yet, for other languages
      this will be possible with caching tricks, but convenient to be
      non mandatory)
      the metadata is used to do static checks of burdock code
        perhaps it can also be used with ffi code in the other direction
    provide the module value for that module
    plus module plugins store module values for reuse - they aren't
      cached anywhere else in the runtime

currently planned to be built in are:
haskell/pre wrapped modules - this is what the system uses for the bootstrap
  modules and the ffi implemented built in modules
  and what to use for regular haskell implemented ffi modules
burdock modules - this is the plugin that handles burdock implemented modules
python
later: c ffi plugin

-}

data ModulePlugin
    = ModulePlugin
    {mpCanonicalizeID :: Maybe ModuleID -> ModuleID -> Runtime ModuleID
    ,mpGetMetadata :: ModuleID -> Runtime ModuleMetadata
    ,mpGetModuleValue :: ModuleID -> Runtime Value
    }

getModuleMetadata :: ModuleID -> Runtime ModuleMetadata
getModuleMetadata ri = do
    st <- ask
    c <- liftIO $ readIORef (rtModulePlugins st)
    case lookup (mPlugin ri) c of
        Nothing -> error $ "unrecognised runtime import source: " <> mPlugin ri
        Just p -> (mpGetMetadata p) ri

getModuleValue :: ModuleID -> Runtime Value
getModuleValue ri = do
    st <- ask
    c <- liftIO $ readIORef (rtModulePlugins st)
    case lookup (mPlugin ri) c of
        Nothing -> error $ "unrecognised runtime import source: " <> mPlugin ri
        Just p -> (mpGetModuleValue p) ri

addModulePlugin :: Text -> ModulePlugin -> Runtime ()
addModulePlugin nm mp = do
    st <- ask
    liftIO $ modifyIORef (rtModulePlugins st) ((nm,mp):)

-- the burdock import source is to provide an ffi api to load module values
-- into ffi code. it gives access to the low level module plugins,
-- or the high level module map

lookupImportSource :: Maybe ModuleID -> S.ImportSource -> Runtime ModuleID
lookupImportSource ctx (S.ImportSpecial nm as) = do
    st <- ask
    c <- liftIO $ readIORef (rtModulePlugins st)
    case lookup nm c of
        Nothing -> error $ "unrecognised runtime import source: " <> nm
        Just p -> (mpCanonicalizeID p) ctx (ModuleID nm as)
lookupImportSource ctx (S.ImportName [nm]) =
    -- todo: need to locate the builtins a bit more robustly
    -- plus implement a flexible dictionary for this mapping
    lookupImportSource ctx
        (S.ImportSpecial "file"
            ["/home/jake/wd/burdock/2023/src/burdock/built-ins/" <> nm <> ".bur"])
lookupImportSource _ x = error $ "import source not supported: " <> show x


------------------------------------------------------------------------------

-- native plugin

data HaskellModulePlugin
    = HaskellModulePlugin
    {hmmModulePlugin :: ModulePlugin
    ,hmmModules :: IORef [(Text, InternalHaskellModule)]
    }

-- the values are put in runtime, so the user can decide if to unconditionally
-- load the module before registering, or load it only on first use
data HaskellModule
    = HaskellModule
    {hmGetMetadata :: Runtime ModuleMetadata
    ,hmGetModuleValue :: Runtime Value
    }

-- there's probably a more elegant way to do this
-- the either implements the possibility of loading the module
-- only on first use
data InternalHaskellModule
    = InternalHaskellModule
    {ihmGetMetadata :: IORef (Either ModuleMetadata (Runtime ModuleMetadata))
    ,ihmGetModuleValue :: IORef (Either Value (Runtime Value))
    }

haskellModulePlugin :: Runtime HaskellModulePlugin
haskellModulePlugin = do
    modulesRef <- liftIO $ newIORef []
    let lookupPlugin ris = do
            modules <- liftIO $ readIORef modulesRef
            let mnm = case ris of
                    ModuleID _ [mnmcunt] -> mnmcunt
                    _ -> error "unsupported haskell import source " <> show ris
            pure $ maybe (error $ "haskell module not found: " <> mnm) id
                $ lookup mnm modules
        plg = ModulePlugin
            {mpCanonicalizeID = \_ zz -> pure zz
            ,mpGetMetadata = \ris -> do
                r <- lookupPlugin ris
                v <- liftIO $ readIORef (ihmGetMetadata r)
                case v of
                    Left v' -> pure v'
                    Right r1 -> do
                        v' <- r1
                        liftIO $ writeIORef  (ihmGetMetadata r) (Left v')
                        pure v'
            ,mpGetModuleValue = \ris -> do
                r <- lookupPlugin ris
                v <- liftIO $ readIORef (ihmGetModuleValue r)
                case v of
                    Left v' -> pure v'
                    Right r1 -> do
                        v' <- r1
                        liftIO $ writeIORef  (ihmGetModuleValue r) (Left v')
                        pure v'
            }
    pure $ HaskellModulePlugin plg modulesRef

addHaskellModule :: Text -> HaskellModule -> HaskellModulePlugin -> Runtime ()
addHaskellModule nm hm hmm = do
    a <- liftIO $ newIORef (Right $ hmGetMetadata hm)
    b <- liftIO $ newIORef (Right $ hmGetModuleValue hm)
    liftIO $ modifyIORef (hmmModules hmm) ((nm,InternalHaskellModule a b):)
