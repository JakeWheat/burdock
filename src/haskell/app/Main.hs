
{-


get the command line args

special cases:
if first arg is rename, just output rename each of the other args
if first arg is desugar, just output desugar each of the other args

for each arg, treat as script
create a handle
set tests to on
run the script
catch any exception that comes out
get the test results, display the total passed/total run


if there was an exception or not all tests pass on any of the files,
exit with non zero


-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (show, catchAsText, error)
import Data.Text.IO (putStrLn)

import Data.Text (Text)
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text as T
import System.Environment (getArgs)

import Control.Monad (void)

import Data.IORef
    (newIORef
    ,readIORef
    ,modifyIORef
    )

import System.Exit (exitFailure)

import Burdock.Burdock
    (liftIO
    ,createHandle
    ,Handle
    ,runScript
    --,Value(..)
    ,debugShowValue
    ,extractFFIValue
    ,hRuntimeState

    ,desugarScript
    ,desugarModule
    ,desugarFragment
    )
    
import Burdock.Scientific (extractInt)
import qualified Burdock.Runtime as R
import qualified Data.Text.Lazy as L

main :: IO ()
main = do
    as <- map T.pack <$> getArgs

    case as of
        ("rename": _as') -> undefined
        ("rename-module": _as') -> undefined
        ("desugar": as') -> mapM_ (doDesugar desugarScript) as'
        ("desugar-module": as') -> mapM_ (doDesugar desugarModule) as'
        ("desugar-fragment": as') -> mapM_ (doDesugar desugarFragment) as'
        ("test": as') -> runScriptWithTests as'
        ("test-all": _as') -> undefined -- do the hunit tests too
        ("--": _as) -> undefined -- runfiles
        _ -> undefined -- runfiles

doDesugar :: (Handle -> Maybe Text -> L.Text -> IO L.Text) -> Text -> IO ()
doDesugar desugarF fn = do
    putStrLn fn
    mySrc <- liftIO $ L.readFile (T.unpack fn)
    st <- createHandle
    x <- desugarF st (Just fn) mySrc
    L.putStrLn x

runScriptTest :: Text -> IO (Int,Int)
runScriptTest fn =
    let getTestResults st = do
            -- todo: create a nice user wrapper here that doesn't need all this
            -- boilerplate and internals
            passes <- runScript st Nothing "_bootstrap.get-test-passes()"
            failures <- runScript st Nothing "_bootstrap.get-test-failures()"
            ntix <- runScript st Nothing "_bootstrap._type-number"
            R.runRuntime (hRuntimeState st) $ do
                xx <- R.getFFITypeInfoTypeInfo
                Right nti <- R.extractFFIValue xx ntix
                Right passes' <- extractFFIValue nti passes
                Right failures' <- extractFFIValue nti failures
                case (extractInt passes',extractInt failures') of
                    (Just p',Just f') -> pure (p',f')
                    _ -> error $ "bad return value from get test results " <> debugShowValue passes <> " " <> debugShowValue failures
        doIt = do
            putStrLn fn
            mySrc <- liftIO $ L.readFile (T.unpack fn)
            st <- createHandle
            void $ runScript st (Just fn) mySrc
            getTestResults st
            
        onError e = do
            putStrLn $ "Exception: " <> e
            pure (0,1)
    -- todo: if it's a burdock value exception, show it nicely
    in catchAsText doIt onError
    
runScriptWithTests :: [Text] -> IO ()
runScriptWithTests fns = do

    numTestsPassed <- newIORef 0
    numTestsFailed <- newIORef 0

    flip mapM_ fns $ \fn -> do
        (p,f) <- runScriptTest fn
        liftIO $ modifyIORef numTestsPassed (p+)
        liftIO $ modifyIORef numTestsFailed (f+)

    p <- readIORef numTestsPassed
    f <- readIORef numTestsFailed
    let tot = p + f
    if f == 0
        then putStrLn $ show p <> " tests passed"
        else do
            putStrLn $ show p <> "/" <> show tot <> " tests passed"
            exitFailure
