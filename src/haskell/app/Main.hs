
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

import Burdock.TestLib (runHUnitTests)
import Burdock.Tests (allTests)

import TempRenamer
    (tempRenamer
    ,tempDesugar)

---------------------------------------

main :: IO ()
main = do
    args <- getArgs

    -- todo: work on proper command line args
    -- output filename before each desugar/rename if there is more than one
    -- get the interpreter to do rename, desugar debug options
    -- implement the non test run version
    case args of
        -- output renamed scripts
        ("rename": as) -> mapM_ (tempRenamer False) (map T.pack as)
        -- output renamed modules
        ("rename-module": as) -> mapM_ (tempRenamer True) (map T.pack as)
        -- output desugared scripts
        ("desugar": as) -> mapM_ (tempDesugar False) (map T.pack as)
        -- output desugared modules
        ("desugar-module": as) -> mapM_ (tempDesugar True) (map T.pack as)
        -- run tests, including the internal hunit tests
        ("test-all": as) -> runScriptWithTests True as
        -- run the scripts with their tests
        ("test": as) -> runScriptWithTests False as
        -- run scripts without tests (needs work)
        _ -> runScripts args


runScripts :: [String] -> IO ()
runScripts args = do
    let runScriptx fn = do
            mySrc <- liftIO $ L.readFile fn
            void $ runScript (Just $ T.pack fn) mySrc

    forM_ args $ \fn -> do
        putStrLn $ T.pack fn
        st <- createHandle
        _ <- runRuntime st $ runTask False $ runScriptx fn
        pure ()
    
runScriptWithTests :: Bool -> [String] -> IO ()
runScriptWithTests runInternal args = do

    numTestsPassed <- newIORef 0
    numTestsFailed <- newIORef 0

    let runScriptTest fn = do
            mySrc <- liftIO $ L.readFile fn
            void $ runScript (Just $ T.pack fn) mySrc

    let suites = (if runInternal
                     then (("hunit tests", runHUnitTests allTests) :)
                     else id) $ map (\fn -> (fn, runScriptTest fn)) args

    forM_ suites $ \(nm,tst) -> do
        putStrLn $ T.pack nm
        st <- createHandle
        ee <- runRuntime st $ runTask False $ do
            tst
            --void $ interpBurdock dast
            (p,f) <- getTestResults
            liftIO $ modifyIORef numTestsPassed (p+)
            liftIO $ modifyIORef numTestsFailed (f+)
            -- get test passed state, update ioref
            
        let doError t = do
                -- runs if the script doesn't even complete
                putStrLn $ "FAIL: " <> T.pack nm <> ":\n" <> t
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
