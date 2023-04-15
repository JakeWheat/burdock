
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

import Control.Monad
    (void)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let fn = case args of
                    [x] -> x
                    _ -> error $ "please pass name of script to test, got: " ++ show args
    mySrc <- readFile fn
    let ast = either error id $ parseScript "" mySrc
    void $ interpBurdock ast
    pure ()
