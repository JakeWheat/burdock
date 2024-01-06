
{-

some extra ffi used in the testing for the interpreter itself

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.TestingExtras
    (testingExtrasModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)

import qualified Burdock.Runtime as R
import Burdock.Runtime (liftIO)
import Burdock.Scientific (Scientific)
import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
--import Data.Text.IO (putStrLn)

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,writeIORef
    )
import Data.Maybe (catMaybes)

------------------------------------------------------------------------------

testingExtrasModule :: R.Runtime (ModuleMetadata, R.Value)
testingExtrasModule = R.withScope $ do

    imod <- R.getModuleValue (R.ModuleID "haskell" ["_interpreter"])
    bnum <- R.getMember Nothing imod "_type-number"
    ti <- R.getFFITypeInfoTypeInfo
    Right (burdockNumberTI :: R.FFITypeInfo) <- R.extractFFIValue ti bnum

    ffiTestingType <- makeFFITestingType
    ffiTestingTypeB <- R.makeFFIValue ti ffiTestingType

    let bs' = [(BEType 0, ("ffi-test-type", ffiTestingTypeB))] ++
             map (BEIdentifier,)
             [("make-ffi-testing-value", R.Fun $ makeFFITestingValue ffiTestingType)
             ,("simple-haskell-callback", R.Fun $ simpleHaskellCallback burdockNumberTI)
             ,("wrapped-haskell-callback", R.Fun $ wrappedHaskellCallback burdockNumberTI)
             ]
        (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<testing-extras>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))

------------------------------------------------------------------------------

data FFITesting = FFITesting Text (IORef Text)

-- todo: I think all these functions need explicit access to exV and mkV
makeFFITestingType :: R.Runtime R.FFITypeInfo
makeFFITestingType = do
    R.makeFFIType ["_testing-extras", "ffi-test-type"]
        [R.ToRepr $ \(FFITesting v _) -> pure v
        --,R.Equals $ (pure .) . (==)
        ,R.Member "field1" $ \(FFITesting v _) _ -> pure $ R.BString v
        ,R.Member "field2" $ \(FFITesting _ r) _ -> do
            r' <- liftIO $ readIORef r
            pure $ R.BString r'
        ,R.Method "method1" $ \(FFITesting v _) _ _ ->
             pure $ R.BString $ "got: " <> v
        ,R.Method "method2" $ \(FFITesting v _) _ x -> case x of
             [R.Boolean b] -> pure $ R.BString $ T.unwords ["got:", v, show b]
             _ -> error $ "bad args to method2"
        ,R.App $ \(FFITesting v _) as -> case as of
             [R.BString s] -> pure $ R.BString $ v <> " " <> s
             _ -> error $ "bad args to ffi app"
        ,R.Assign "field2" $ \(FFITesting _ v) _nm val -> do
             let val' = case val of
                     R.BString x -> x
                     _ -> error $ "wrong type in assign"
             liftIO $ writeIORef v val'
        ,R.AppKw $ \(FFITesting v _) as kws -> case as of
             [R.BString t] -> do
                 kwt <- showKws kws
                 pure $ R.BString $ T.unwords [v,t,kwt]
             _ -> error $ "bad args to appkw"
        ,R.CatchAll $ \_ _ nm (FFITesting v _) ->
            pure $ R.BString $ v <> " " <> nm
        ]
  where
      showKws :: [(Text,R.Value)] -> R.Runtime Text
      showKws ks = do
          ts <- flip mapM ks (\(n,v) -> case v of
              R.MethodV {} -> pure $ Nothing
              _ -> do
                  --liftIO $ putStrLn $ R.debugShowValue v
                  x <- R.getMember Nothing v "_torepr"
                  y <- R.app Nothing x []
                  case y of
                      R.BString v' -> pure $ Just (n <> " " <> v')
                      _ -> error $ "wrong type from torepr")
          pure $ T.unwords $ catMaybes ts

makeFFITestingValue :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
makeFFITestingValue ffiTestingType [R.BString s] = do
    mut <- liftIO $ newIORef (s <> " also")
    R.makeFFIValue ffiTestingType $ FFITesting s mut
makeFFITestingValue _ _ = error "bad args to makeFFITestingValue"

------------------------------------------------------------------------------

simpleHaskellCallback :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
simpleHaskellCallback burdockNumberTI [fn, nm] = do
    Right (nm' :: Scientific) <- R.extractFFIValue burdockNumberTI nm
    nm'' <- R.makeFFIValue burdockNumberTI (nm' * 2)
    void $ R.app Nothing fn [nm'']
    pure R.BNothing
simpleHaskellCallback _ _ = error "bad args to simpleHaskellCallback"

-- get a function value from burdock
-- then wrap it so it can be passed to this haskell function
-- which will call it as a callback without any burdock specific
-- monad or types
myHaskellTripleAndCallback :: Scientific -> (Scientific -> IO ()) -> IO ()
myHaskellTripleAndCallback n f = f (n * 3)

wrappedHaskellCallback :: R.FFITypeInfo -> [R.Value] -> R.Runtime R.Value
wrappedHaskellCallback burdockNumberTI [fn, nm] = do
    -- create a wrapper for fn that takes a haskell data type argument
    -- and runs in IO
    myRunBurdock <- R.getRuntimeRunner
    let w :: Scientific -> IO ()
        w n = myRunBurdock $ do
            -- there will be some boilerplate helpers that will make
            -- this wrapping/unwrapping trivial
            n' <- R.makeFFIValue burdockNumberTI n
            void $ R.app Nothing fn [n']
    -- extract the number, and call the native haskell function which
    -- takes a callback
    Right (nm' :: Scientific) <- R.extractFFIValue burdockNumberTI nm
    R.liftIO $ myHaskellTripleAndCallback nm' w
    
    pure R.BNothing
wrappedHaskellCallback _ _ = error "bad args to wrappedHaskellCallback"
