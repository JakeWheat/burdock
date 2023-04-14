
{-

The runtime is the code that allows running in a burdock context. It's
kind of like an ffi interface to haskell, it doesn't know anything
about syntax.

The interpreter takes syntax and calls functions in the runtime.
Haskell ffi code uses function in the runtime too.

-}

module Burdock.Runtime
    (Value(..)

    ,runBurdock
    ,Runtime
    ,liftIO
    ,Scientific

    ,RuntimeState
    ,emptyRuntimeState
    ,addFFIType

    ,makeFunctionValue
    ,makeValue
    ,extractValue
    ,Type(..)
    ,Env
    ,captureClosure

    ,getMember
    ,app

    --,ffimethodapp
    ) where

import Burdock.Scientific

import Control.Monad.Reader (ReaderT
                            ,runReaderT
                            ,ask
                            --,local
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
    }

emptyRuntimeState :: IO RuntimeState
emptyRuntimeState =
    RuntimeState <$> newIORef []

addFFIType :: Text -> Type -> Runtime ()
addFFIType nm ty = do
    x <- rtFFITypes <$> ask
    liftIO $ modifyIORef x ((nm,ty) :)

type Runtime = ReaderT RuntimeState IO

-- todo: make this abstract
-- don't use the eq and show except for debugging
data Value = Number Scientific
           | Value Text Dynamic
           | VNothing
           | VBool Bool
           | VFun ([Value] -> Runtime Value)
    --deriving (Show)

data Env = Env [(Text, Value)]
    --deriving Show

makeValue :: Typeable a => Text -> a -> Value
makeValue nm v = Value nm $ toDyn v

extractValue :: Typeable a => Value -> Maybe a
extractValue (Value _ v) = fromDynamic v
extractValue _x = error $ "can't extract value from something"

makeFunctionValue :: ([Value] -> Runtime Value) -> Runtime Value
makeFunctionValue f = pure $ VFun f

captureClosure :: [Text] -> Runtime Env
captureClosure _ = pure $ Env []

getMember :: Value -> Text -> Runtime Value
getMember v fld = do
    st <- ask
    let tyNm = case v of
                   Value x _ -> x
                   _ -> error $ "get member on wrong sort of value"
    ty <- liftIO ((maybe (error $ "type not found " ++ T.unpack tyNm) id . lookup tyNm) <$> readIORef (rtFFITypes st))
    (tyMemberFn ty) fld v

app :: Value -> [Value] -> Runtime Value
app (VFun f) args = f args
    --([Value] -> Runtime Value) = undefined
app _ _ = error $ "app called on non function value"

data Type
    = Type
    {tyMemberFn :: (Text -> Value -> Runtime Value)}

runBurdock :: RuntimeState -> Runtime a -> IO a
runBurdock rt f = runReaderT f rt
