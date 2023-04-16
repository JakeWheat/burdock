
{-

The runtime is the code that allows running in a burdock context. It's
kind of like an ffi interface to haskell, it doesn't know anything
about syntax.

The interpreter takes syntax and calls functions in the runtime.
Haskell ffi code uses function in the runtime too.

-}
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
    ,addFFIType

    ,makeFunctionValue
    ,makeValue
    ,extractValue
    ,Type(..)
    ,Env
    ,captureClosure
    ,makeVariant
    ,variantTag
    
    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding

    ,makeList
    ,extractList

    ,getMember
    ,app

    --,ffimethodapp
    ) where

import Burdock.Scientific

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
    )

data RuntimeState
    = RuntimeState
    {rtFFITypes :: IORef [(Text,Type)]
    ,rtBindings :: IORef [(Text, Value)]
    }

emptyRuntimeState :: IO RuntimeState
emptyRuntimeState =
    RuntimeState <$> newIORef [] <*> newIORef []

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
debugShowValue (Value tg dn) = "Value " <> T.pack (show tg) <> " " <> T.pack (show dn)
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
extractValue _x = error $ "can't extract value from something"

makeFunctionValue :: ([Value] -> Runtime Value) -> Runtime Value
makeFunctionValue f = pure $ VFun f

makeVariant :: Text -> [(Text,Value)] -> Runtime Value
makeVariant n fs = pure $ VariantV n fs

variantTag :: Value -> Runtime (Maybe Text)
variantTag (VariantV nm _) = pure $ Just nm
variantTag _ = pure Nothing -- error $ "non variant passed to variant tag"

captureClosure :: [Text] -> Runtime Env
captureClosure nms = do
    envr <- rtBindings <$> ask
    env <- liftIO $ readIORef envr
    -- todo: check all the nms are found
    pure $ Env $ filter ((`elem` nms) . fst) env

getMember :: Value -> Text -> Runtime Value
getMember v@(Value tyNm _ ) fld = do
    st <- ask
    ty <- liftIO ((maybe (error $ "type not found " ++ T.unpack tyNm) id . lookup tyNm)
                  <$> readIORef (rtFFITypes st))
    (tyMemberFn ty) fld v

getMember v@(VariantV _ fs) fld = do
    case lookup fld fs of
        Nothing -> error $ "field not found:" ++ T.unpack fld
        Just (MethodV v1) -> app v1 [v]
        Just v1 -> pure v1

getMember _ _ = error $ "get member on wrong sort of value"

app :: Value -> [Value] -> Runtime Value
app (VFun f) args = f args
    --([Value] -> Runtime Value) = undefined
app (MethodV f) args = app f args
app _ _ = error $ "app called on non function value"


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
