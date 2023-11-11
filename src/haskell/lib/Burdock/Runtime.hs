
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Runtime
    (Runtime
    ,RuntimeState
    ,makeRuntimeState
    ,runRuntime

    ,Value(..)
    ,debugShowValue
    -- temp
    ,showValue
    ,FFITypeTag(..)

    ,withScope
    ,app
    ,captureClosure
    ,withNewEnv
    ,addBinding
    ,getMember
    ,lookupBinding
    ,runTask

    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show, catchAsText)

import Burdock.Scientific (Scientific, showScientific)
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
    )

import Control.Monad.IO.Class (liftIO)

import Data.Dynamic
    (Dynamic
    )

------------------------------------------------------------------------------

type SP = Maybe (Text, Int, Int)

-- todo: make these unique, can use a counter in the runtime state for now?
data DataDeclTag = DataDeclTag Text
    deriving (Eq, Show)

data VariantTag = VariantTag DataDeclTag Text
    deriving (Eq, Show)

data FFITypeTag
    = FFITypeTag
    {tyName :: Text
    ,tyMemberFn :: (Text -> Value -> Runtime Value)}

data Value
    = Number Scientific
    | Boolean Bool
    | BText Text
    | BNothing
    
    | FFIValue FFITypeTag Dynamic

    | Variant VariantTag [(Text,Value)]
    | Box (IORef Value)
    | Module [(Text, Value)]

    | Fun ([Value] -> Runtime Value)
    | Method Value

-- pure show for use in temporary error messages and internal errors
debugShowValue :: Value -> Text
debugShowValue (Number n) = showScientific n
debugShowValue (Boolean b) = show b
debugShowValue (BText t) = "\"" <> t <> "\""
debugShowValue BNothing = "nothing"
debugShowValue (Fun {}) = "<function>"
debugShowValue (Method {}) = "<method>"
debugShowValue (Box _) = "<box>"
debugShowValue (Module _) = "<module>"
debugShowValue (FFIValue _ d) = "<" <> show d <> ">"
debugShowValue (Variant (VariantTag _ nm) fs) =
    nm <> "(" <> T.intercalate "," (map (debugShowValue . snd) fs) <> ")"

showValue :: Value -> Runtime Text
showValue (Number n) = pure $ showScientific n
showValue (Boolean b) = pure $ show b
showValue (BText t) = pure $ "\"" <> t <> "\""
showValue BNothing = pure $ "nothing"
showValue (Fun {}) = pure $ "<function>"
showValue (Method {}) = pure $ "<method>"
showValue (Box _) = pure $ "<box>"
showValue (Module _) = pure $ "<box>"
showValue (FFIValue _ d) = pure $ "<" <> show d <> ">"
showValue (Variant (VariantTag _ nm) fs) = do
    as <- flip mapM fs $ showValue . snd
    pure $ nm <> "(" <> T.intercalate "," as <> ")"

------------------------------------------------------------------------------

data RuntimeState
    = RuntimeState
    {rtBindings :: IORef [(Text, Value)]
    }

makeRuntimeState :: IO RuntimeState
makeRuntimeState =
    RuntimeState
    <$> newIORef []

type Runtime = ReaderT RuntimeState IO

runRuntime :: RuntimeState -> Runtime a -> IO a
runRuntime st f = runReaderT f st

--------------------------------------

withScope :: Runtime a -> Runtime a
withScope f = do
    rtb <- rtBindings <$> ask
    b1 <- liftIO (newIORef =<< readIORef rtb)
    local (\y -> y {rtBindings = b1}) f

withNewEnv :: [(Text,Value)] -> Runtime a -> Runtime a
withNewEnv bs f = do
    b1 <- liftIO $ newIORef bs
    local (\y -> y {rtBindings = b1}) f

app :: SP -> Value -> [Value] -> Runtime Value
app _sourcePos (Fun f) args = f args
app _ v _ = error $ "bad arg to app " <> debugShowValue v

captureClosure :: Runtime [(Text,Value)]
captureClosure = do
    rtb <- rtBindings <$> ask
    liftIO $ readIORef rtb

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

getMember sp (Module fs) f = case lookup f fs of
        Nothing -> error $ show sp <> " module member not found: " <> f
        Just v' -> pure v'
getMember sp v f = error $ show sp <> ": getMember: " <> debugShowValue v <> " . " <> f

runTask :: Runtime a -> Runtime (Either Text a)
runTask f = catchAsText (Right <$> f) (pure . Left)
