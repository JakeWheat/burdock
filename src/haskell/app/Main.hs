
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
    (interpBurdock)

import Burdock.Desugar (desugar)


import Control.Monad
    (void)

import System.Environment (getArgs)
import Control.Monad (forM_)

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \fn -> do
        mySrc <- readFile fn
        let ast = either error id $ parseScript "" mySrc
            dast = desugar ast
        --putStrLn $ ppShow dast
        void $ interpBurdock dast 
