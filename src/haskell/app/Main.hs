
{-

quick fake test runner to bootstrap the code

-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Data.Text (Text)
--import qualified Data.Text as T
--import Text.Show.Pretty (ppShow)

import Burdock.Parse (parseScript)
--import qualified Burdock.Syntax as S

import Burdock.Interpreter
    (interpBurdock
    ,createHandle
    ,runBurdock
    ,getTempTestsPass
    ,liftIO
    )

import Burdock.Desugar (desugar, prelude)
import qualified Data.Text as T

import Control.Monad
    (void
    ,when)

import System.Environment (getArgs)
import Control.Monad (forM_)

import Data.IORef
    (newIORef
    ,readIORef
    ,writeIORef
    )
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    allPassed <- newIORef True
    forM_ args $ \fn -> do
        mySrc <- readFile fn
        let ast = either error id $ parseScript "" (T.unpack prelude <> mySrc)
            dast = desugar ast
        --putStrLn $ ppShow dast
        st <- createHandle
        runBurdock st $ do
            void $ interpBurdock dast
            p <- getTempTestsPass
            when (not p) $ liftIO $ writeIORef allPassed False
            -- get test passed state, update ioref
    -- check passed ioref, if false, then exit with non zero        
    p <- readIORef allPassed
    when (not p) exitFailure
