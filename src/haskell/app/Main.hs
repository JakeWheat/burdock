
{-

quick fake test runner to bootstrap the code

-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (show)
import Data.Text.IO (putStrLn)

import Burdock.Interpreter
    (runRuntime
    ,getTestResults
    ,liftIO
    ,runScript
    ,runTask
    ,debugShowValue
    ,createHandle
    ,extractValue
    )

import qualified Data.Text as T

import Control.Monad
    (void
    --,when
    )

import System.Environment (getArgs)
import Control.Monad (forM_)

import Data.IORef
    (newIORef
    ,readIORef
    ,modifyIORef
    )
import System.Exit (exitFailure)
import qualified Data.Text.Lazy.IO as L

main :: IO ()
main = do
    args <- getArgs
    numTestsPassed <- newIORef 0
    numTestsFailed <- newIORef 0
    forM_ args $ \fn -> do
        putStrLn $ T.pack fn
        mySrc <- L.readFile fn
        st <- createHandle
        
        ee <- runRuntime st $ runTask False $ do
            void $ runScript (Just $ T.pack fn) mySrc
            --void $ interpBurdock dast
            (p,f) <- getTestResults
            liftIO $ modifyIORef numTestsPassed (p+)
            liftIO $ modifyIORef numTestsFailed (f+)
            -- get test passed state, update ioref
            
        let doError t = do
                -- runs if the script doesn't even complete
                putStrLn $ "FAIL: " <> T.pack fn <> ": " <> t
                liftIO $ modifyIORef numTestsFailed (1+)
        case ee of
            Right {} -> pure ()
            Left (Left t, _) -> doError t
            Left (Right v, _) -> do
                case extractValue v of
                    Just x -> doError x
                    _ -> doError $ debugShowValue v
                
    -- check passed ioref, if false, then exit with non zero        
    p <- readIORef numTestsPassed
    f <- readIORef numTestsFailed
    let tot = p + f
    if f == 0
        then putStrLn $ show p <> " tests passed"
        else do
            putStrLn $ show f <> "/" <> show tot <> " tests failed"
            exitFailure
