
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
    
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import qualified Prelude as P

--import Burdock.Scientific (Scientific, showScientific)
import Data.Text (Text)
import qualified Data.Text as T

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
    {rtBindings :: IORef [(Text, Value)]
    ,rtAutoDataDeclID :: IORef Int
    ,rtAutoFFITypeID :: IORef Int
    ,rtFFITypeInfoTypeInfo :: FFITypeInfo
    }

makeRuntimeState :: IO RuntimeState
makeRuntimeState = do
    st <- RuntimeState
        <$> newIORef []
        <*> newIORef 0
        <*> newIORef 0
        <*> pure (error "delayed initialisation")
    -- does this need to be here? try to move it to bootstrap
    tinf <- runRuntime st $
        makeFFIType ["_bootstrap","ffi-type-info"]
            [ToRepr $ \(v :: FFITypeInfo) -> pure $ "<" <> tyNameText (tyTypeID v) <> ">"
            ,Equals $ \v w -> pure $ tyTypeID v == tyTypeID w]

    pure $ st {rtFFITypeInfoTypeInfo = tinf}

type Runtime = ReaderT RuntimeState IO

runRuntime :: RuntimeState -> Runtime a -> IO a
runRuntime st f = runReaderT f st

--------------------------------------

withScope :: Runtime a -> Runtime a
withScope f = localEnv id f

withNewEnv :: [(Text,Value)] -> Runtime a -> Runtime a
withNewEnv bs f = do
    b1 <- liftIO $ newIORef bs
    local (\y -> y {rtBindings = b1}) f

localEnv :: ([(Text,Value)] -> [(Text,Value)]) -> Runtime a -> Runtime a
localEnv f stuff = do
    rtb <- rtBindings <$> ask
    rtbv <- liftIO $ readIORef rtb
    rtbvn <- liftIO $ newIORef (f rtbv)
    local (\s -> s {rtBindings = rtbvn}) stuff

app :: SP -> Value -> [Value] -> Runtime Value
app _sourcePos (Fun f) args = f args
app _ v _ = error $ "bad arg to app " <> debugShowValue v

-- todo: should capture closure capture elements within a module instead
-- of just the module? not sure. it might be a performance boost
-- but maybe using indexes would achieve this too, and plan to do that
-- at some point
captureClosure :: [Text] -> Runtime [(Text,Value)]
captureClosure nms = do
    -- todo: error if any names aren't found
    rtb <- rtBindings <$> ask
    rtbv <- liftIO (readIORef rtb)
    -- could consider making this optional if it's a performance issue, seems unlikely
    -- given how inefficient everything else is
    let missing = nms \\ map fst rtbv
        -- todo: get to the point this hack isn't needed to make it work
        -- run-task: special case in the desugarer, don't add this syntax
        -- _variant: once the renamer is working, this will be accurate and
        -- can be removed
        missingHack = filter (`notElem` ["run-task"])
                      $ filter (not . ("_variant-" `T.isPrefixOf`)) missing
    when (not $ null missingHack) $
        error $ "closure capture items not found: " <> show missingHack
    pure $ filter ((`elem` nms) . fst) rtbv

addBinding :: Text -> Value -> Runtime ()
addBinding nm v = do
    stb <- rtBindings <$> ask
    liftIO $ modifyIORef stb ((nm,v):)

lookupBinding :: Text -> Runtime (Maybe Value)
lookupBinding nm = do
    x <- rtBindings <$> ask
    x1 <- liftIO $ readIORef x
    pure $ lookup nm x1

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
