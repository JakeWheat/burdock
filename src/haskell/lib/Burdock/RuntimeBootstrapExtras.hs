
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Burdock.RuntimeBootstrapExtras
     (addRuntimeBootstrapExtras
     ,binaryMemberLax
     ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Burdock.Runtime
    (Value
    ,Runtime
    ,makeFunctionValue
    ,doIsTest
    ,doIsNotTest
    ,addTestPass
    ,addTestFail
    ,makeValue
    ,extractValue
    ,liftIO
    ,debugShowValue
    ,nothingValue
    ,makeString
    ,getCallStack
    ,makeBurdockList
    ,extractBurdockList
    ,makeBool
    ,makeNumber
    ,makeTuple
    ,getMember
    ,app
    ,runTask
    ,makeValueName
    ,FFITypeTag
    )

import Data.Text (Text)
import qualified Data.Text as T

import Burdock.Scientific
    (Scientific
    )

import Data.Dynamic
    (Typeable
    )

import System.Process
    (readProcessWithExitCode
    
    )
import System.Exit
    (ExitCode(..)
    )

------------------------------------------------------------------------------

addRuntimeBootstrapExtras
    :: (Text -> (Text -> Value -> Runtime Value) -> Runtime FFITypeTag)
    -> (Text -> Value -> Runtime ())
    -> Runtime ()
addRuntimeBootstrapExtras addFFIType' addBinding' = do
    addBinding' "torepr" =<< makeFunctionValue myToRepr

    gremlintype <- addFFIType' "gremlintype" gremlinFFI
    --void $ addFFIType' "bytestring" ffitypetagFFI

    addBinding' "print" =<< makeFunctionValue myPrint
    addBinding' "debug-print" =<< makeFunctionValue myDebugPrint
    addBinding' "debug-show" =<< makeFunctionValue myDebugShow
    addBinding' "get-call-stack" =<< makeFunctionValue myGetCallStack
    addBinding' "tostring" =<< makeFunctionValue myToString

    addBinding' "not" =<< makeFunctionValue myNot

    addBinding' "add-test-pass" =<< makeFunctionValue myAddTestPass
    addBinding' "add-test-fail" =<< makeFunctionValue myAddTestFail
    addBinding' "indent" =<< makeFunctionValue indent

    addBinding' "do-is-test" =<< makeFunctionValue doIsTest
    addBinding' "do-is-not-test" =<< makeFunctionValue doIsNotTest

    addBinding' "torepr-debug" =<< makeFunctionValue toreprDebug

    addBinding' "gremlin" $ makeValue gremlintype False

    addBinding' "read-process" =<< makeFunctionValue myReadProcessWithExitCode

------------------------------------------------------------------------------

-- simple utility functions

myPrint :: [Value] -> Runtime Value
myPrint [v] = do
    t <- myToString [v]
    case extractValue t of
        Nothing -> liftIO $ putStrLn $ debugShowValue v
        Just s -> liftIO $ putStrLn s
    nothingValue
myPrint _ = error $ "bad args to myPrint"

myDebugPrint :: [Value] -> Runtime Value
myDebugPrint [x] = do
    liftIO $ putStrLn $ debugShowValue x
    nothingValue
myDebugPrint _ = error $ "bad args to myDebugPrint"

myDebugShow :: [Value] -> Runtime Value
myDebugShow [x] = makeString $ debugShowValue x
myDebugShow _ = error $ "bad args to myDebugShow"

myGetCallStack :: [Value] -> Runtime Value
myGetCallStack [] = do
    cs <- getCallStack
    cs' <- flip mapM cs $ \case
            Nothing -> makeString "nothing"
            Just x -> makeString x
    makeBurdockList cs' -- makeValue "string" $ T.intercalate "," cs'
myGetCallStack _ = error $ "bad args to myGetCallStack"

myNot :: [Value] -> Runtime Value
myNot [x] = case extractValue x of
    Just True -> makeBool False
    Just False -> makeBool True
    _ -> error $ "bad arg to not"
myNot _ = error $ "bad args to raise"

myToRepr :: [Value] -> Runtime Value
myToRepr [x] = do
    tf <- getMember x "_torepr" 
    app Nothing tf []
myToRepr _ = error $ "bad args to myToRepr"

myToString :: [Value] -> Runtime Value
myToString [x] = do
    case extractValue x of
        Just (_ :: Text) -> pure x
        Nothing -> myToRepr[x]
myToString _ = error $ "bad args to myToString"


toreprDebug :: [Value] -> Runtime Value
toreprDebug [x] = do
    y <- runTask False $ myToRepr [x]
    case y of
        Left _ -> makeString $ debugShowValue x
        Right v -> pure v
toreprDebug _ = error $ "bad args to toreprDebug"

------------------------------------------------------------------------------

-- running processes

{-myCallProcess :: [Value] -> Runtime Value
myCallProcess [x] = do
    let as = maybe (error $ "bad args to call-process, arg is not list")
                id $ extractList x
        as' :: [Text]
        as' = flip map as $ \f -> maybe (error $ "bad args to call-process, arg has non string in list")
              id $ extractValue f
        (b:bs) = map T.unpack as'
    liftIO $ callProcess b bs
    pure VNothing
myCallProcess _ = error $ "bad args to myCallProcess"-}

myReadProcessWithExitCode :: [Value] -> Runtime Value
myReadProcessWithExitCode [prog, args, stdinVal] = do
    let prog' = maybe (error "0: bad args to myReadProcessWithExitCode") T.unpack $ extractValue prog
        as = maybe (error $ "bad args to call-process, arg is not list")
                id $ extractBurdockList args
        args' :: [String]
        args' = flip map as $ \f -> maybe (error $ "bad args to read-process, arg has non string in list")
                T.unpack $ extractValue f

        stdinVal' = maybe (error "2: bad args to myReadProcessWithExitCode") T.unpack $ extractValue stdinVal
    (x, so, se) <- liftIO $ readProcessWithExitCode prog' args' stdinVal'
    let x1 :: Scientific
        x1 = fromIntegral $ case x of
                 ExitSuccess -> 0
                 ExitFailure n -> n
    x1' <- makeNumber x1
    so' <- makeString $ T.pack so
    se' <- makeString $ T.pack se
    makeTuple [x1', so', se']
myReadProcessWithExitCode _ = error $ "bad args to myReadProcessWithExitCode"

------------------------------------------------------------------------------

-- helpers for the testing

indent :: [Value] -> Runtime Value
indent [x] | Just t <- extractValue x = do
    let ls = T.lines t
        ls' = map ("  " <>) ls
    makeString $ T.unlines ls'
indent _ = error $ "bad args to indent"

myAddTestPass :: [Value] -> Runtime Value
myAddTestPass [] = do
    addTestPass
    nothingValue
myAddTestPass _ = error $ "bad args to myAddTestPass"

myAddTestFail :: [Value] -> Runtime Value
myAddTestFail [] = do
    addTestFail
    nothingValue
myAddTestFail _ = error $ "bad args to myAddTestFail"

------------------------------------------------------------------------------

-- testing the ffi system

gremlinFFI :: Text -> Value -> Runtime Value
gremlinFFI "_torepr" v1 = do
    case extractValue v1 of
        Just (v :: Bool) -> makeString $ "gremlin: " <> show v
        Nothing -> error $ "non gremlintytpe: " <> debugShowValue v1
gremlinFFI "_equals" v1 = do
    makeFunctionValue $ binaryMemberLax "_equals" "gremlintype" "gremlintype" ((==) :: Bool -> Bool -> Bool) v1
gremlinFFI m _ = error $ "unsupported field on gremlin: " <> m


------------------------------------------------------------------------------

binaryMemberLax :: (Typeable a, Typeable b) =>
    Text -> Text -> Text -> (a -> a -> b) -> Value -> [Value] -> Runtime Value
binaryMemberLax burName inType outType f v1 as =
    case as of
        [v2] -> do
            let n1 = maybe (error $ "bl not a " <> inType <> " " <> debugShowValue v1) id $ extractValue v1
                mn2 = extractValue v2
                -- todo: make false an arg?
            maybe (makeValueName outType False) (makeValueName outType . (f n1)) mn2
        _ -> error $ "bad args to " <> inType <> " " <> burName
