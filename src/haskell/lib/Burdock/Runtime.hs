
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Runtime
    (Runtime
    ,RuntimeState
    ,makeRuntimeState
    ,runRuntime

    ,Value
    ,debugShowValue
    -- temp
    ,showValue
    ,Value(BNothing,Boolean,BText,Fun,Box,Module)

    ,FFITypeInfo
    ,makeFFIType
    ,makeFFIValue
    ,extractFFIValue
    ,FFIValueEntry(..)
    ,getFFITypeInfoTypeInfo

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
    ,fromDynamic
    ,toDyn
    ,Typeable
    )

------------------------------------------------------------------------------

type SP = Maybe (Text, Int, Int)

data DataDeclTag = DataDeclTag Int Text
    deriving (Eq, Show)

data VariantTag = VariantTag DataDeclTag Text
    deriving (Eq, Show)

data Value
    = Boolean Bool
    | BText Text
    | BNothing
    
    | FFIValue FFITypeInfo Dynamic

    | Variant VariantTag [(Text,Value)]
    | Box (IORef Value)
    | Module [(Text, Value)]

    | Fun ([Value] -> Runtime Value)
    | Method Value

-- pure show for use in temporary error messages and internal errors
debugShowValue :: Value -> Text
--debugShowValue (Number n) = showScientific n
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
--showValue (Number n) = pure $ showScientific n
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
    ,rtAutoDataDeclID :: IORef Int
    ,rtAutoFFITypeID :: IORef Int
    ,rtFFITypeInfoTypeInfo :: FFITypeInfo
    }

makeRuntimeState :: IO RuntimeState
makeRuntimeState = do
    st <-  RuntimeState
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

-- temp?
getMember _ (Boolean b) "_torepr" =
    pure $ if b then wrap "true" else wrap "false"
  where
    wrap v = Fun $ \[] -> pure $ BText v
        
getMember sp v f = error $ show sp <> ": getMember: " <> debugShowValue v <> " . " <> f

runTask :: Runtime a -> Runtime (Either Text a)
runTask f = catchAsText (Right <$> f) (pure . Left)

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
            pure $ Fun $ \[w] -> do
                w' <- exV w
                case w' of
                    Left _ -> pure $ Boolean False
                    Right w'' -> Boolean <$> fn v' w'')
        binMem nm fn = (nm, \v -> do
                v' <- either error id <$> exV v
                pure $ Fun $ \[w] -> do
                    w' <- exV w
                    case w' of
                        Left _ -> error $ "bad type"
                        Right w'' -> fn v' w'')
        binMem2 nm fn = (nm, \v -> do
                v' <- either error id <$> exV v
                pure $ Fun $ \[w] -> do
                    w' <- exV w
                    case w' of
                        Left _ -> error $ "bad type"
                        Right w'' -> fn v' w'')
        meths' :: [(Text, Value -> Runtime Value)]
        meths' = concat $ flip map meths $ \case
           ToRepr f -> [("_torepr", \v -> do
               v' <- either error id <$> exV v
               pure $ Fun $ \[] -> BText <$> f v')] 
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
