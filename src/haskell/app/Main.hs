
{-

quick fake test runner to bootstrap the code

-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

--import Data.Text (Text)
--import qualified Data.Text as T
--import Text.Show.Pretty (ppShow)

import Burdock.Parse (parseScript)
--import qualified Burdock.Syntax as S

import Burdock.Interpreter
    (interpBurdock
    ,createHandle
    ,runBurdock
    ,getTestResults
    ,liftIO
    )

import Burdock.Desugar (desugar, prelude)
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

main :: IO ()
main = do
    args <- getArgs
    numTestsPassed <- newIORef 0
    numTestsFailed <- newIORef 0
    forM_ args $ \fn -> do
        mySrc <- readFile fn
        let ast = either (error . T.pack) id $ parseScript "" (T.unpack prelude <> mySrc)
            dast = desugar ast
        --putStrLn $ T.pack $ ppShow dast
        st <- createHandle
        runBurdock st $ do
            void $ interpBurdock dast
            (p,f) <- getTestResults
            liftIO $ modifyIORef numTestsPassed (p+)
            liftIO $ modifyIORef numTestsFailed (f+)
            -- get test passed state, update ioref
    -- check passed ioref, if false, then exit with non zero        
    p <- readIORef numTestsPassed
    f <- readIORef numTestsFailed
    let tot = p + f
    if f == 0
        then putStrLn $ show p <> " tests passed"
        else do
            putStrLn $ show f <> "/" <> show tot <> " tests failed"
            exitFailure
