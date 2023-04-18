
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

    ,runBurdock
    ,Runtime
    ,liftIO
    ,Scientific

    ,debugShowValue

    ,RuntimeState
    ,emptyRuntimeState
    ,getRuntimeState
    ,addFFIType

    ,makeFunctionValue
    ,makeValue
    ,extractValue
    ,Type(..)
    ,Env
    ,captureClosure
    ,makeVariant
    ,variantTag
    ,variantFields
    ,variantValueFields
    ,catchEither
    ,throwValue

    -- temp, should only be used for the binding in the default env
    ,nothingValue

    ,getCallStack
    
    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding

    ,makeList
    ,extractList

    ,getMember
    ,app

    ,addTestFail
    ,getNumTestsFailed

    --,ffimethodapp
    ) where

import Prelude hiding (error, putStrLn, show)
import qualified Prelude as P
import Burdock.Utils (error, show)
--import Data.Text.IO (putStrLn)

import Burdock.Scientific

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            ,local
                            ,liftIO
                            --,MonadIO
                            )

import Control.Monad
    (zipWithM
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
    --,writeIORef
    )

import Control.Exception.Safe (catch
                              ,SomeException
                              ,Exception
                              ,throwM
                              --,catchAny
                              --,fromException
                              --,try
                              )

data RuntimeState
    = RuntimeState
    {rtFFITypes :: IORef [(Text,Type)]
    ,rtBindings :: IORef [(Text, Value)]
    ,rtNumTestFailed :: IORef Int
    ,rtCallStack :: IORef [Maybe Text]
    }

emptyRuntimeState :: IO RuntimeState
emptyRuntimeState =
    RuntimeState <$> newIORef [] <*> newIORef [] <*> newIORef 0 <*> newIORef []

addFFIType :: Text -> Type -> Runtime ()
addFFIType nm ty = do
    x <- rtFFITypes <$> ask
    liftIO $ modifyIORef x ((nm,ty) :)

type Runtime = ReaderT RuntimeState IO

-- todo: make this abstract
-- don't use the eq and show except for debugging
data Value = Value Text Dynamic
           | VariantV Text [(Text, Value)]
           | MethodV Value
           -- todo: change nothing to a variant
           | VNothing
           -- todo: change to variant, or something?
           | VList [Value]
           | VFun ([Value] -> Runtime Value)
    --deriving (Show)

debugShowValue :: Value -> Text
debugShowValue (Value tg dn) = "Value " <> show tg <> " " <> show dn
debugShowValue (VariantV tf fs) =
    "VariantV " <> tf <> " " <> T.concat (map f fs)
    where
        f (nm,v) = nm <> " " <> debugShowValue v <> ","
debugShowValue (MethodV v) = "MethodV " <> debugShowValue v
debugShowValue VNothing = "VNothing"
debugShowValue (VList vs)
    = "VList " <> T.concat (map f vs)
    where
        f v = debugShowValue v <> ","
debugShowValue (VFun {}) = "VFun {}"


getCallStack :: Runtime [Maybe Text]
getCallStack = do
    st <- ask
    liftIO $ readIORef (rtCallStack st)

data Env = Env [(Text, Value)]
    --deriving Show

makeValue :: Typeable a => Text -> a -> Value
makeValue nm v = Value nm $ toDyn v

makeList :: [Value] -> Value
makeList = VList

extractList :: Value -> Maybe [Value]
extractList (VList vs) = Just vs
extractList _ = Nothing

extractValue :: Typeable a => Value -> Maybe a
extractValue (Value _ v) = fromDynamic v
extractValue _x = Nothing -- error $ "can't extract value from " ++ T.unpack (debugShowValue x)

nothingValue :: Value
nothingValue = VNothing

makeFunctionValue :: ([Value] -> Runtime Value) -> Runtime Value
makeFunctionValue f = pure $ VFun f

makeVariant :: Text -> [(Text,Value)] -> Runtime Value
makeVariant n fs = pure $ VariantV n fs

variantTag :: Value -> Runtime (Maybe Text)
variantTag (VariantV nm _) = pure $ Just nm
variantTag _ = pure Nothing -- error $ "non variant passed to variant tag"

variantFields :: Value -> Runtime (Maybe [(Text, Value)])
variantFields (VariantV _ flds) = pure $ Just flds
variantFields _ = pure Nothing

{-
these are the fields used in variant pattern matching,
and in the default torepr, equals and other generated methods
maybe will save an explicit list in the future, so can add auxiliary
non method fields which aren't included in pattern matching e.g.
-}
variantValueFields :: Value -> Runtime (Maybe [(Text, Value)])
variantValueFields (VariantV _ flds) =
    pure $ Just $ flip filter flds $ \case
       (_,MethodV {}) -> False
       _ -> True
variantValueFields _ = pure Nothing

captureClosure :: [Text] -> Runtime Env
captureClosure nms = do
    envr <- rtBindings <$> ask
    env <- liftIO $ readIORef envr
    -- todo: check all the nms are found
    pure $ Env $ filter ((`elem` nms) . fst) env

getMember :: Value -> Text -> Runtime Value
getMember v@(Value tyNm _ ) fld = do
    st <- ask
    ty <- liftIO ((maybe (error $ "type not found " <> tyNm) id . lookup tyNm)
                  <$> readIORef (rtFFITypes st))
    (tyMemberFn ty) fld v

getMember v@(VariantV _ fs) fld = do
    case lookup fld fs of
        Nothing -> error $ "field not found:" <> fld <> ", " <> debugShowValue v
        Just (MethodV v1) -> app Nothing v1 [v]
        Just v1 -> pure v1

getMember (MethodV {}) "_torepr" = do
    let v = makeValue "string" ("<method>" :: Text)
    makeFunctionValue (\_ -> pure v)
    --pure $ makeValue "string" ("<methodv>" :: Text)
getMember (VFun {}) "_torepr" = do
    let v = makeValue "string" ("<function>" :: Text)
    makeFunctionValue (\_ -> pure v)

getMember (VNothing) "_torepr" = do
    let v = makeValue "string" ("nothing" :: Text)
    makeFunctionValue (\_ -> pure v)

getMember (VNothing) "_equals" = do
    makeFunctionValue $ \case
        [VNothing] -> pure $ makeValue "boolean" True
        [_] -> pure $ makeValue "boolean" False
        _ -> error "bad args to nothing._equals"


getMember (VList es) "_torepr" = do
    let trf e = do
            f <- getMember e "_torepr"
            v <- app Nothing f []
            pure $ maybe (error $ "non string from to _torepr: " <> debugShowValue e <> " " <> debugShowValue v) id $ extractValue v

    makeFunctionValue (\_ -> do
        strs <- mapM trf es
        let txt = "[list: " <> T.intercalate ", " strs <> "]"
        pure $ makeValue "string" txt)

getMember (VList es) "length" = do
    makeFunctionValue (\_ -> do
        pure $ makeValue "number" ((fromIntegral $ length es) :: Scientific))

getMember (VList es) "_equals" = do
    makeFunctionValue $ \case
        [VList fs] ->
            if length es /= length fs
            then pure $ makeValue "boolean" False
            else do
                let eq a b = do
                        f <- getMember a "_equals"
                        r <- app Nothing f [b]
                        case extractValue r of
                            Nothing -> error $ "non bool returned from _equals: " <> debugShowValue r <> " on "  <> debugShowValue a
                            Just x -> pure x
                        
                rs <- zipWithM eq es fs
                pure $ makeValue "boolean" $ and rs
        
        [_] -> pure $ makeValue "boolean" False
        _ -> error "bad args to nothing._equals"


getMember v fld = error $ "unrecognised member " <> fld <> " on " <> debugShowValue v

app :: Maybe Text -> Value -> [Value] -> Runtime Value
app sourcePos (VFun f) args =
    withCallstackEntry sourcePos $ f args
app sp (MethodV f) args = app sp f args
app _ _ _ = error $ "app called on non function value"

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

data Type
    = Type
    {tyMemberFn :: (Text -> Value -> Runtime Value)}

runBurdock :: RuntimeState -> Runtime a -> IO a
runBurdock rt f = runReaderT f rt

catchEither:: Runtime a -> Runtime (Either (Either Text Value) a)
catchEither f =
    catch' (catchValue (Right <$> f))
  where
    -- first try to catch a specific burdock value that was thrown
    catchValue = flip catch $ \(ValueException v) -> pure $ Left $ Right v
    -- then try to catch any haskell (non async) exception
    catch' = flip catch $ \(e :: SomeException) -> pure $ Left $ Left $ show e

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
    
getNumTestsFailed :: Runtime Int
getNumTestsFailed = do
    st <- ask
    liftIO $ readIORef (rtNumTestFailed st)

getRuntimeState :: Runtime RuntimeState
getRuntimeState = ask
